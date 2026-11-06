;;; ir.el --- Imposto de renda (IRPF): rules and the year's position  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'pj)
(require 'reconcile)

(defvar fin-conv-salary-regexps)
(defvar fin-bank-own-regexps)

(defcustom fin-ir-pro-labore nil
  "Pró-labore overriding `fin-ir-pro-labore-files': list of (YEAR . CENTS)
for the year's total, or (YEAR :monthly CENTS) counted through the last
completed month (all 12 for a past year)."
  :type '(alist :key-type integer :value-type sexp) :group 'fin)

(defcustom fin-ir-filed nil
  "Returns delivered besides the receipt files: list of (YEAR . ISO-DATE)."
  :type '(alist :key-type integer :value-type string) :group 'fin)

(defcustom fin-ir-payments-dir
  (expand-file-name "../infra/inbox/ir/"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory of Receita CSVs and summaries; nil or absent means none."
  :type '(choice directory (const nil)) :group 'fin)

(defcustom fin-ir-nfse-dir
  (expand-file-name "../infra/inbox/nfse/"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory of NFS-e index CSVs; nil or absent means none."
  :type '(choice directory (const nil)) :group 'fin)

(defcustom fin-ir-receita-items '("das" "darf")
  "Items of `fin-pj-tax-items' paid to Receita, matched case-sensitively."
  :type '(repeat string) :group 'fin)

(defcustom fin-ir-payment-labels nil
  "Receita payments missing from its lists: list of (ISO-DATE CENTS CODE)."
  :type '(repeat (list string integer string)) :group 'fin)

(defcustom fin-ir-wrong-account-known nil
  "Reviewed wrong-account tax payments: list of (ISO-DATE CENTS).
Company totals still count them."
  :type '(repeat (list string integer)) :group 'fin)

(defcustom fin-ir-payment-window 5
  "Days a bank payment may sit from the Receita payment it matches."
  :type 'integer :group 'fin)

(defconst fin-ir-receita-codes
  '(("3333" "das" pj) ("0055" "das" pj) ("1410" "inss" pj) ("4444" "inss" pj)
    ("1099" "inss" pj) ("0211" "irpf" cpf) ("5320" "irpf fine" cpf)
    ("1872" "inss complement" cpf)
    ("third-party" "third party" none))
  "Receita code -> (ITEM PAYER), PAYER pj, cpf or none (another taxpayer's
guide paid by mistake).  Unlisted codes are the company's.")

(defcustom fin-ir-defis-exempt nil
  "Exempt profit per DEFIS: list of (YEAR . CENTS).
Overrides `fin-ir-defis-files'."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-declared-exempt nil
  "Exempt company profit per filed DIRPF: list of (YEAR . CENTS).
Overrides `fin-ir-dirpf-files'."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-exclusive nil
  "Income taxed exclusively at source per DIRPF: list of (YEAR . CENTS).
Overrides the .DEC."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-assets-declared nil
  "Bens e direitos on 31/12 per DIRPF: list of (YEAR . CENTS).
Overrides the .DEC."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-open-debts nil
  "Debts Receita notified and not yet paid: list of (DUE-ISO CODE CENTS TEXT)."
  :type '(repeat (list string string integer string)) :group 'fin)

(defcustom fin-ir-full-books nil
  "Full books behind profit above the presumed, for years with no books
summary in the inbox (those use its profit).
verified: exempt; yes: exempt, flagged; no or nil: the excess is taxable."
  :type '(choice (const verified) (const yes) (const no) (const nil)) :group 'fin)

(defcustom fin-ir-partner-payees nil
  "Regexps of company payments that were the partner's own spending.
They count as profit drawn."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-ir-book-profit nil
  "Company DRE net profit: list of (YEAR . CENTS).
Overrides `fin-ir-book-files'.  Exempt up to the larger of it and the
presumed profit (LC 123 art. 14 §1)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-presumption 0.32
  "Presumed-profit rate, Lei 9.249 art. 15 (0.32 for services)."
  :type 'number :group 'fin)

(defcustom fin-ir-retirement-plan nil
  "Plan behind `retirement': pgbl (deductible), vgbl, none or nil (unknown)."
  :type '(choice (const pgbl) (const vgbl) (const none) (const nil)) :group 'fin)

(defcustom fin-ir-other-taxable nil
  "Taxable income outside bank and ledger: list of (YEAR . CENTS).
Overrides the .DEC."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-health-deductible
  '("m[eé]dic" "dentist" "odonto" "psic[oó]log" "fisio" "\\blab" "exame"
    "hospital" "cl[ií]nic")
  "Regexps of deductible `health' items, ignoring case.
Lei 9.250 art. 8 II a."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-ir-individual-categories '("extras")
  "Income categories received from individuals."
  :type '(repeat string) :group 'fin)

(defcustom fin-ir-individual-income nil
  "Nature of `fin-ir-individual-categories' income.
taxable (carnê-leão), exempt or nil (unknown)."
  :type '(choice (const taxable) (const exempt) (const nil)) :group 'fin)

(defconst fin-ir--table-2015
  '((2284776 0 0) (3391980 0.075 171358) (4501260 0.15 425757)
    (5597616 0.225 763351) (nil 0.275 1043232))
  "Annual table of anos-calendário 2015–2022 (Lei 13.149/2015):
https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2016")

(defconst fin-ir-rules
  `((2020 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2021-05-31" :inss-cap 610106)
    (2021 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2022-05-31" :inss-cap 643357)
    (2022 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2023-05-31" :inss-cap 708722)
    (2023
     :table ((2451192 0 0) (3391980 0.075 183839) (4501260 0.15 438238)
             (5597616 0.225 775832) (nil 0.275 1055713))
     :simplified-cap 1675434 :reduction nil
     :filing-taxable 3063990 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2024-05-31" :inss-cap 750749)
    (2024
     :table ((2696320 0 0) (3391980 0.075 202224) (4501260 0.15 456623)
             (5597616 0.225 794217) (nil 0.275 1074098))
     :simplified-cap 1675434 :reduction nil
     :filing-taxable 3388800 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2025-05-30" :inss-cap 778602)
    (2025
     :table ((2846720 0 0) (3391980 0.075 213504) (4501260 0.15 467903)
             (5597616 0.225 805497) (nil 0.275 1085378))
     :simplified-cap 1675434
     :reduction nil
     :filing-taxable 3558400 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2026-05-29"
     :inss-cap 815741)
    (2026
     :table ((2914560 0 0) (3391980 0.075 218592) (4501260 0.15 472991)
             (5597616 0.225 810585) (nil 0.275 1090466))
     :simplified-cap 1764000
     :reduction (6000000 269415 8820000 842973 0.095575)
     :filing-taxable nil :filing-exempt nil :filing-assets nil :deadline nil
     :inss-cap 847555))
  "IRPF rules per calendar year (ano-calendário), amounts in cents.")

(defconst fin-ir-late-fine-min 16574
  "Minimum late-filing fine (MAED), also the whole fine when no tax is due:
1% a month on the tax, at least this, at most 20% (each IN RFB, art. 10).")

(defconst fin-ir-inss-rate 0.11
  "Partner's INSS rate on pró-labore: Lei 8.212 art. 21 and 30 §4.")

(defconst fin-ir-pgbl-share 0.12
  "PGBL deduction cap, share of taxable income: Lei 9.532 art. 11.")

(defconst fin-ir-simplified-share 0.20
  "Simplified discount, share of taxable income: Lei 9.250 art. 10.")

(defconst fin-ir-das-rate-tolerance 0.015
  "DAS share of revenue a PGDAS-D month may sit from its year's median.")

(defconst fin-ir-dividend-month-limit 5000000
  "Profit one company pays one person in a month past which it withholds 10%:
Lei 15.270/2025 (Lei 9.250 art. 6-A), from 2026.")

(defconst fin-ir-minimum-tax-floor 60000000
  "Yearly total income past which the minimum tax (IRPFM) applies:
Lei 15.270/2025 (Lei 9.250 art. 16-A), from 2026.")

(defconst fin-ir-monthly-exempt 500000
  "Monthly taxable income the 2026 reduction zeroes: Lei 15.270/2025.")

(defun fin-ir-rule (year key)
  "Rule KEY of calendar YEAR."
  (let ((r (cdr (assq year fin-ir-rules))))
    (unless r (error "fin-ir: no rules for %S" year))
    (plist-get r key)))

(defun fin-ir-table-tax (year base)
  "Tax on BASE cents by YEAR's annual table, before any reduction."
  (cl-assert (integerp base))
  (if (<= base 0) 0
    (let ((band (cl-find-if (lambda (b) (or (null (car b)) (<= base (car b))))
                            (fin-ir-rule year :table))))
      (max 0 (round (- (* base (nth 1 band)) (nth 2 band)))))))

(defun fin-ir-reduction (year taxable tax)
  "Lei 15.270 reduction of TAX for TAXABLE income in YEAR, capped at TAX."
  (pcase (fin-ir-rule year :reduction)
    ('nil 0)
    (`(,full-upto ,full ,phase-upto ,a ,b)
     (min tax (cond ((<= taxable full-upto) full)
                    ((<= taxable phase-upto) (max 0 (round (- a (* b taxable)))))
                    (t 0))))))

(defun fin-ir-tax (year taxable deductions)
  "Tax due for YEAR on TAXABLE cents less DEDUCTIONS cents."
  (cl-assert (and (natnump taxable) (natnump deductions)))
  (let ((tax (fin-ir-table-tax year (- taxable deductions))))
    (- tax (fin-ir-reduction year taxable tax))))

(defun fin-ir-simplified (year taxable)
  "Simplified discount on TAXABLE cents in YEAR."
  (min (round (* fin-ir-simplified-share taxable)) (fin-ir-rule year :simplified-cap)))

(defun fin-ir--year-arg (year)
  (cl-assert (integerp year))
  (list (format "%04d" year)))

(defun fin-ir--pj-rows (year)
  "Company account rows of YEAR as (id date type amount description)."
  (fin-db-query "SELECT id, date, type, amount, description FROM bank_txn
                  WHERE account = 'pj' AND strftime('%Y', date) = ?"
                (fin-ir--year-arg year)))

(defconst fin-ir--amount-re "\\`[0-9]+\\.[0-9]\\{2\\}\\'"
  "A CSV amount: reais with two decimals.")

(defconst fin-ir--month-re "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\'"
  "A CSV month: YYYY-MM.")

(defun fin-ir--cents (amount)
  "Cents of CSV AMOUNT."
  (round (* 100 (string-to-number amount))))

(defun fin-ir--csv (dir pattern parse)
  "PARSE (LINE FILE) of each non-header line of DIR's files matching PATTERN."
  (let (out)
    (dolist (f (and dir (file-directory-p dir) (directory-files dir t pattern)))
      (with-temp-buffer
        (insert-file-contents f)
        (dolist (line (cdr (split-string (buffer-string) "\n" t)))
          (push (funcall parse line f) out))))
    (nreverse out)))

(defun fin-ir--bump (alist key cents)
  "ALIST with CENTS added to KEY, new keys last."
  (let ((cell (assoc key alist)))
    (if cell (progn (cl-incf (cdr cell) cents) alist)
      (append alist (list (cons key cents))))))

(defun fin-ir--year-total (rows year value)
  "Sum of VALUE over ROWS whose car, YYYY-MM, is in YEAR; nil when none."
  (let* ((prefix (format "%04d-" year))
         (in (cl-remove-if-not (lambda (r) (string-prefix-p prefix (car r))) rows)))
    (and in (apply #'+ (mapcar value in)))))

(defun fin-ir--payment-row (line file)
  "(ISO-DATE CODE CENTS PERIOD) of a Receita payments CSV LINE.
PERIOD is the YYYY-MM of its período de apuração."
  (pcase (split-string line ",")
    (`(,date ,code ,_doc ,pa ,total)
     (unless (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date)
                  (string-match-p "\\`[0-9]+\\'" code)
                  (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" pa)
                  (string-match-p fin-ir--amount-re total))
       (error "fin-ir: bad payment line in %s: %S" file line))
     (list date code (fin-ir--cents total) (substring pa 0 7)))
    (_ (error "fin-ir: bad payment line in %s: %S" file line))))

(defun fin-ir-payments ()
  "Receita payments: (ISO-DATE CODE CENTS PERIOD) each."
  (fin-ir--csv fin-ir-payments-dir "\\`pagamentos-.*\\.csv\\'" #'fin-ir--payment-row))

(defun fin-ir--simples-row (line file)
  "(MONTH SEQ REVENUE-CENTS DAS-CENTS) of a PGDAS-D summary CSV LINE."
  (pcase (split-string line ",")
    ((and `(,pa ,seq ,revenue ,das)
          (guard (and (string-match-p fin-ir--month-re pa) (string-match-p "\\`[0-9]+\\'" seq)
                      (string-match-p fin-ir--amount-re revenue) (string-match-p fin-ir--amount-re das))))
     (list pa (string-to-number seq) (fin-ir--cents revenue) (fin-ir--cents das)))
    (_ (error "fin-ir: bad PGDAS-D line in %s: %S" file line))))

(defun fin-ir-simples ()
  "Last PGDAS-D declaration per period: (MONTH REVENUE-CENTS DAS-CENTS).
Oldest first."
  (let (last)
    (dolist (r (fin-ir--csv fin-ir-payments-dir "\\`pgdasd-.*-resumo\\.csv\\'" #'fin-ir--simples-row))
      (let ((cell (assoc (car r) last)))
        (cond ((null cell) (push r last))
              ((= (nth 1 r) (nth 1 cell)) (error "fin-ir: PGDAS-D %s declaration %d twice" (car r) (nth 1 r)))
              ((> (nth 1 r) (nth 1 cell)) (setcdr cell (cdr r))))))
    (mapcar (lambda (r) (list (car r) (nth 2 r) (nth 3 r)))
            (sort last (lambda (a b) (string< (car a) (car b)))))))

(defun fin-ir--invoice-row (line file)
  "(MONTH CENTS ACTIVE) of an NFS-e CSV LINE, MONTH its competência."
  (pcase (split-string line ",")
    ((and `(,_system ,_number ,_issued ,month ,_doc ,_client ,value ,status ,_file)
          (guard (and (string-match-p fin-ir--month-re month) (string-match-p fin-ir--amount-re value)
                      (member status '("ativa" "cancelada")))))
     (list month (fin-ir--cents value) (equal status "ativa")))
    (_ (error "fin-ir: bad NFS-e line in %s: %S" file line))))

(defun fin-ir-invoices ()
  "Non-cancelled NFS-e per competência: (MONTH . CENTS), oldest first."
  (let (out)
    (dolist (r (fin-ir--csv fin-ir-nfse-dir "\\`nfse-.*-index\\.csv\\'" #'fin-ir--invoice-row))
      (when (nth 2 r) (setq out (fin-ir--bump out (car r) (nth 1 r)))))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun fin-ir--payment-code (b payments used)
  "Receita code of the payment in PAYMENTS that bank row B paid.
Same amount, nearest date within `fin-ir-payment-window' days, not in
hash table USED, which is updated."
  (let ((day (fin-reconcile--day (nth 1 b))) best best-gap)
    (dolist (p payments)
      (when (and (= (nth 2 p) (nth 3 b)) (not (gethash p used)))
        (let ((gap (abs (- day (fin-reconcile--day (car p))))))
          (when (and (<= gap fin-ir-payment-window) (or (null best) (< gap best-gap)))
            (setq best p best-gap gap)))))
    (when best (puthash best t used) (nth 1 best))))

(defun fin-ir--receita-rows ()
  "Bank rows paid to Receita from either account, oldest first.
Each is (id date account amount description)."
  (let ((case-fold-search nil)
        (res (mapcar #'car (cl-remove-if-not (lambda (i) (member (cdr i) fin-ir-receita-items))
                                             fin-pj-tax-items))))
    (cl-remove-if-not
     (lambda (r) (cl-some (lambda (re) (string-match-p re (nth 4 r))) res))
     (fin-db-query "SELECT id, date, account, amount, description FROM bank_txn
                     WHERE type = 'out' AND account IN ('pj', 'account') ORDER BY date, id"))))

(defun fin-ir-receita-matches (payments)
  "List of (ROW . CODE) per Receita bank row, CODE nil when unmatched.
Rows claim PAYMENTS oldest first, each payment once across all years."
  (let ((used (make-hash-table :test #'eq)))
    (mapcar (lambda (r) (cons r (or (fin-ir--payment-code r payments used)
                                    (fin-ir--labelled-code r))))
            (fin-ir--receita-rows))))

(defun fin-ir--labelled-code (r)
  "CODE `fin-ir-payment-labels' gives bank row R."
  (nth 2 (cl-find-if (lambda (l) (and (equal (car l) (nth 1 r)) (eql (nth 1 l) (nth 3 r))))
                     fin-ir-payment-labels)))

(defun fin-ir-code (code)
  "(ITEM PAYER) of Receita CODE (`fin-ir-receita-codes')."
  (or (cdr (assoc code fin-ir-receita-codes)) (list code 'pj)))

(defun fin-ir-wrong-account-p (match)
  "Non-nil when MATCH, (ROW . CODE), left the other payer's account."
  (let ((payer (and (cdr match) (nth 1 (fin-ir-code (cdr match))))))
    (and (memq payer '(pj cpf))
         (not (eq payer (if (equal (nth 2 (car match)) "pj") 'pj 'cpf))))))

(defun fin-ir-wrong-account (receita)
  "Matches of RECEITA paid from the wrong account.
Only from the company's first bank row on, less `fin-ir-wrong-account-known'."
  (let ((first (caar (fin-db-query "SELECT MIN(date) FROM bank_txn WHERE account = 'pj'"))))
    (cl-remove-if-not (lambda (m) (and first (not (string< (nth 1 (car m)) first))
                                       (fin-ir-wrong-account-p m)
                                       (not (member (list (nth 1 (car m)) (nth 3 (car m)))
                                                    fin-ir-wrong-account-known))))
                      receita)))

(defun fin-ir-unmatched (receita)
  "Company rows of RECEITA no payment matches."
  (mapcar #'car (cl-remove-if (lambda (m) (or (cdr m) (not (equal (nth 2 (car m)) "pj")))) receita)))

(defun fin-ir--company-tax (b receita)
  "(ITEM . PAYER) of company tax row B, by its RECEITA code when matched."
  (let ((code (cdr (cl-find (car b) receita :key #'caar :test #'equal))))
    (if code
        (let ((c (fin-ir-code code))) (cons (car c) (nth 1 c)))
      (cons (fin-pj-tax-item b) 'pj))))

(defun fin-ir-company (year receita)
  "Plist of YEAR's :revenue, :taxes (alist ITEM . CENTS) and :partner-taxes.
Tax rows are named by their RECEITA match; company taxes paid from the
personal account count too."
  (let ((prefix (format "%04d-" year)) (revenue 0) (partner 0) taxes)
    (dolist (b (fin-ir--pj-rows year))
      (cond ((fin-pj-income-category b) (cl-incf revenue (nth 3 b)))
            ((fin-pj-tax-item b)
             (let ((tax (fin-ir--company-tax b receita)))
               (pcase (cdr tax)
                 ('cpf (cl-incf partner (nth 3 b)))
                 ('none nil)
                 (_ (setq taxes (fin-ir--bump taxes (car tax) (nth 3 b)))))))))
    (dolist (m receita)
      (when (and (equal (nth 2 (car m)) "account") (string-prefix-p prefix (nth 1 (car m)))
                 (fin-ir-wrong-account-p m))
        (setq taxes (fin-ir--bump taxes (car (fin-ir-code (cdr m))) (nth 3 (car m))))))
    (list :revenue revenue :taxes taxes :partner-taxes partner)))

(defun fin-ir--revenue-through (year through)
  "Bank revenue cents of YEAR in the months up to THROUGH (YYYY-MM)."
  (cl-assert (string-match-p fin-ir--month-re through))
  (let ((sum 0))
    (dolist (b (fin-ir--pj-rows year) sum)
      (when (and (fin-pj-income-category b) (not (string> (substring (nth 1 b) 0 7) through)))
        (cl-incf sum (nth 3 b))))))

(defun fin-ir--through (rows through)
  "ROWS, (MONTH ...) each, of the months up to THROUGH (YYYY-MM)."
  (cl-remove-if (lambda (r) (string> (car r) through)) rows))

(defun fin-ir-returned (year)
  "Cents the partner sent back from own accounts to the company in YEAR."
  (let ((case-fold-search t) (sum 0))
    (dolist (b (fin-ir--pj-rows year) sum)
      (when (and (equal (nth 2 b) "in") (not (fin-pj-income-category b))
                 (cl-some (lambda (re) (string-match-p re (nth 4 b))) fin-bank-own-regexps))
        (cl-incf sum (nth 3 b))))))

(defun fin-ir-partner-paid (year)
  "Cents the company account paid in YEAR to `fin-ir-partner-payees'."
  (let ((case-fold-search t) (sum 0))
    (dolist (b (fin-ir--pj-rows year) sum)
      (when (and (equal (nth 2 b) "out")
                 (cl-some (lambda (re) (string-match-p re (nth 4 b))) fin-ir-partner-payees))
        (cl-incf sum (nth 3 b))))))

(defun fin-ir-withdrawals (year)
  "Alist (YYYY-MM . CENTS) the company deposited in the personal account."
  (let ((case-fold-search t) out)
    (dolist (r (fin-db-query "SELECT strftime('%Y-%m', date), amount, description FROM bank_txn
                               WHERE account = 'account' AND type = 'in' AND strftime('%Y', date) = ?
                               ORDER BY date"
                             (fin-ir--year-arg year)))
      (when (cl-some (lambda (re) (string-match-p re (nth 2 r))) fin-conv-salary-regexps)
        (let ((cell (assoc (car r) out)))
          (if cell (cl-incf (cdr cell) (nth 1 r)) (push (cons (car r) (nth 1 r)) out)))))
    (nreverse out)))

(defun fin-ir--entries (year type categories)
  "(ITEM AMOUNT DATE) entries of TYPE in CATEGORIES during YEAR."
  (cl-assert (consp categories))
  (fin-db-query (format "SELECT item, amount, date FROM entry
                          WHERE type = ? AND strftime('%%Y', date) = ? AND category IN (%s)"
                        (mapconcat (lambda (_) "?") categories ","))
                (append (list type) (fin-ir--year-arg year) categories)))

(defcustom fin-ir-health-categories '("health")
  "Ledger categories of health spending, deductible per `fin-ir-health-deductible'."
  :type '(repeat string) :group 'fin)

(defcustom fin-ir-retirement-categories '("retirement")
  "Ledger categories of retirement savings."
  :type '(repeat string) :group 'fin)

(defun fin-ir-health (year)
  "Deductible cents of YEAR's `fin-ir-health-categories' entries."
  (let ((case-fold-search t) (ok 0))
    (dolist (e (fin-ir--entries year "out" fin-ir-health-categories) ok)
      (when (and (car e) (cl-some (lambda (re) (string-match-p re (car e))) fin-ir-health-deductible))
        (cl-incf ok (nth 1 e))))))

(defun fin-ir--sum (rows)
  (apply #'+ (mapcar #'cadr rows)))

(defun fin-ir-retirement (year)
  "Cents put in `fin-ir-retirement-categories' during YEAR."
  (fin-ir--sum (fin-ir--entries year "out" fin-ir-retirement-categories)))

(defun fin-ir-individuals (year)
  "Alist (YYYY-MM . CENTS) of YEAR's income from individuals, oldest first."
  (let (out)
    (dolist (e (fin-ir--entries year "in" fin-ir-individual-categories))
      (let* ((m (substring (nth 2 e) 0 7)) (cell (assoc m out)))
        (if cell (cl-incf (cdr cell) (nth 1 e)) (push (cons m (nth 1 e)) out))))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defconst fin-ir--dec-layouts
  '((2026 :length 744 :other-taxable (456 . 469) :assets-before (274 . 287)
          :assets (287 . 300) :exclusive (326 . 339))
    (2023 :length 718 :other-taxable (456 . 469) :assets-before (274 . 287)
          :assets (287 . 300) :exclusive (326 . 339))
    (2022 :length 718 :other-taxable (456 . 469) :assets-before (274 . 287)
          :assets (287 . 300) :exclusive (326 . 339)))
  "Columns of a DIRPF .DEC totals record (type 18), by exercício.
Fields are 13-digit cents; an unlisted exercício fails loud.")

(defconst fin-ir--dec-simples-code "0013"
  "Record 84 exempt-income code of Simples profit paid to the partner.")

(defun fin-ir--dec-simples-exempt (lines)
  "Cents of record 84 LINES with `fin-ir--dec-simples-code'."
  (let ((sum 0))
    (dolist (l lines sum)
      (when (and (string-prefix-p "84" l) (>= (length l) 116)
                 (equal (substring l 25 29) fin-ir--dec-simples-code))
        (let ((digits (substring l 103 116)))
          (unless (string-match-p "\\`[0-9]+\\'" digits)
            (error "fin-ir: .DEC exempt record value not numeric: %S" digits))
          (cl-incf sum (string-to-number digits)))))))

(defun fin-ir-dec-totals (text)
  "Plist of DIRPF .DEC TEXT: :exercise, :exempt and cents of the
`fin-ir--dec-layouts' fields."
  (let* ((lines (split-string text "\r?\n" t))
         (header (or (car (cl-member-if (lambda (l) (string-prefix-p "IR" l)) lines))
                     (error "fin-ir: .DEC without an IR header")))
         (exercise (string-to-number (substring header 8 12)))
         (layout (or (cdr (assq exercise fin-ir--dec-layouts))
                     (error "fin-ir: no .DEC layout for exercício %d; add it to `fin-ir--dec-layouts'" exercise)))
         (totals (or (car (cl-member-if (lambda (l) (string-prefix-p "18" l)) lines))
                     (error "fin-ir: .DEC without a totals record"))))
    (unless (= (length totals) (plist-get layout :length))
      (error "fin-ir: .DEC %d totals record is %d long, layout says %d"
             exercise (length totals) (plist-get layout :length)))
    (append (list :exercise exercise :exempt (fin-ir--dec-simples-exempt lines))
            (cl-loop for (key cols) on (cddr layout) by #'cddr
                     append (let ((digits (substring totals (car cols) (cdr cols))))
                              (unless (string-match-p "\\`[0-9]+\\'" digits)
                                (error "fin-ir: .DEC %d field %s not numeric: %S" exercise key digits))
                              (list key (string-to-number digits)))))))

(defun fin-ir--yearly-row (line file)
  "(YEAR . CENTS) of a summary CSV LINE: year first, amount last."
  (let ((cols (split-string line ",")))
    (unless (and (cdr cols) (string-match-p "\\`[0-9]\\{4\\}\\'" (car cols))
                 (string-match-p fin-ir--amount-re (car (last cols))))
      (error "fin-ir: bad summary line in %s: %S" file line))
    (cons (string-to-number (car cols)) (fin-ir--cents (car (last cols))))))

(defun fin-ir--yearly (dir pattern)
  "Alist YEAR -> CENTS of DIR's summary CSVs matching PATTERN."
  (let (out)
    (dolist (r (fin-ir--csv dir pattern #'fin-ir--yearly-row) out)
      (when (assq (car r) out) (error "fin-ir: %d twice in %s summaries" (car r) pattern))
      (push r out))))

(defun fin-ir-defis-files ()
  "DEFIS exempt profit paid to the partner: alist YEAR -> CENTS."
  (fin-ir--yearly (and fin-ir-payments-dir (expand-file-name "defis" fin-ir-payments-dir))
                  "\\`defis-.*-resumo\\.csv\\'"))

(defun fin-ir-book-files ()
  "Company DRE net profit: alist YEAR -> CENTS."
  (fin-ir--yearly fin-ir-payments-dir "\\`dre-.*-resumo\\.csv\\'"))

(defun fin-ir-dirpf-files ()
  "Alist ANO-CALENDARIO -> `fin-ir-dec-totals' of each filed return: the
retificadora's (dirpf-cpf-YYYY-retificadora.zip) over the original's."
  (let ((dir (and fin-ir-payments-dir (expand-file-name "dirpf" fin-ir-payments-dir))) out)
    (dolist (f (and dir (file-directory-p dir)
                    (directory-files dir t "\\`dirpf-cpf-[0-9]\\{4\\}-\\(original\\|retificadora\\)\\.zip\\'"))
               out)
      (with-temp-buffer
        (let ((coding-system-for-read 'latin-1))
          (unless (zerop (call-process "unzip" nil t nil "-p" f "*.DEC"))
            (error "fin-ir: unzip failed on %s" f)))
        (let ((totals (fin-ir-dec-totals (buffer-string))))
          (setf (alist-get (1- (plist-get totals :exercise)) out) totals))))))

(defun fin-ir--last-period (payments code)
  "Latest PERIOD (YYYY-MM) of PAYMENTS paid with Receita CODE, or nil."
  (car (sort (mapcar #'cadddr (cl-remove-if-not (lambda (p) (equal (nth 1 p) code)) payments))
             (lambda (a b) (string< b a)))))

(defun fin-ir-context ()
  "Plist of the inputs every year's `fin-ir-position' shares.
:mei-until is the last período paid with the DAS-MEI code 0055."
  (let ((payments (fin-ir-payments)))
    (list :receita (fin-ir-receita-matches payments)
          :simples (fin-ir-simples) :invoices (fin-ir-invoices) :dirpf (fin-ir-dirpf-files)
          :other-payers (fin-ir-other-payers) :reported-exempt (fin-ir-reported-exempt)
          :defis (fin-ir-defis-files) :books (fin-ir-book-files)
          :mei-until (fin-ir--last-period payments "0055"))))

(defun fin-ir--declared-field (ctx year key config)
  "Cents of KEY in YEAR's filed return in CTX, CONFIG overriding.
:assets falls back to the next year's :assets-before."
  (or (cdr (assq year config))
      (plist-get (cdr (assq year (plist-get ctx :dirpf))) key)
      (and (eq key :assets)
           (plist-get (cdr (assq (1+ year) (plist-get ctx :dirpf))) :assets-before))))

(defun fin-ir--due-p (month today)
  "Non-nil when MONTH's PGDAS-D was due (20th of next month) before TODAY."
  (pcase-let ((`(,y ,m) (mapcar #'string-to-number (split-string month "-"))))
    (string< (if (= m 12) (format "%04d-01-20" (1+ y)) (format "%04d-%02d-20" y (1+ m)))
             today)))

(defun fin-ir-bank-revenue ()
  "Alist (MONTH . CENTS) of company revenue, oldest first."
  (let (out)
    (dolist (b (fin-db-query "SELECT id, date, type, amount, description FROM bank_txn
                               WHERE account = 'pj' ORDER BY date")
               out)
      (when (fin-pj-income-category b)
        (setq out (fin-ir--bump out (substring (nth 1 b) 0 7) (nth 3 b)))))))

(defun fin-ir-revenue-gaps (simples invoices today &optional bank)
  "(MONTH DECLARED INVOICED BANKED) where SIMPLES and INVOICES differ by
over a real, from the first period declared through the last declared or
due by TODAY (ISO date).  BANKED is nil when BANK lacks the month."
  (when simples
    (let ((first (caar simples)) out)
      (dolist (m (delete-dups (sort (append (mapcar #'car simples) (mapcar #'car invoices)) #'string<)))
        (when (and (not (string< m first)) (or (assoc m simples) (fin-ir--due-p m today)))
          (let ((declared (or (cadr (assoc m simples)) 0))
                (invoiced (or (cdr (assoc m invoices)) 0)))
            (when (> (abs (- declared invoiced)) 100)
              (push (list m declared invoiced (cdr (assoc m bank))) out)))))
      (nreverse out))))

(defun fin-ir--median (xs)
  "Median of the non-empty list XS."
  (cl-assert xs)
  (let* ((v (vconcat (sort (copy-sequence xs) #'<))) (n (length v)))
    (if (cl-oddp n) (aref v (/ n 2))
      (/ (+ (aref v (1- (/ n 2))) (aref v (/ n 2))) 2.0))))

(defun fin-ir-rate-outliers (simples)
  "(MONTH RATE MEDIAN REVENUE DAS) of SIMPLES months with revenue whose DAS
share is more than `fin-ir-das-rate-tolerance' off its year's median."
  (let ((rated (mapcar (lambda (s) (cons (/ (float (nth 2 s)) (nth 1 s)) s))
                       (cl-remove-if-not (lambda (s) (> (nth 1 s) 0)) simples)))
        out)
    (dolist (r rated)
      (let* ((year (substring (nth 1 r) 0 5))
             (median (fin-ir--median (mapcar #'car (cl-remove-if-not
                                                    (lambda (o) (string-prefix-p year (nth 1 o)))
                                                    rated)))))
        (when (> (abs (- (car r) median)) fin-ir-das-rate-tolerance)
          (push (list (nth 1 r) (car r) median (nth 2 r) (nth 3 r)) out))))
    (nreverse out)))

(defun fin-ir--deductions (year taxable inss health retirement)
  "Plist of complete-model deductions and the chosen model's."
  (let* ((pgbl (if (eq fin-ir-retirement-plan 'pgbl)
                   (min retirement (round (* fin-ir-pgbl-share taxable)))
                 0))
         (complete (+ inss health pgbl))
         (simple (fin-ir-simplified year taxable)))
    (list :pgbl pgbl :complete complete :simplified simple
          :model (if (> complete simple) 'complete 'simplified)
          :tax (min (fin-ir-tax year taxable complete) (fin-ir-tax year taxable simple)))))

(defun fin-ir--competence-months (year today)
  "Pró-labore months of YEAR by TODAY (ISO date).
12 for a past year, through the last completed month for the current one."
  (let ((now (string-to-number (substring today 0 4))))
    (cond ((< year now) 12)
          ((= year now) (1- (string-to-number (substring today 5 7))))
          (t 0))))

(defun fin-ir--fontes-row (line file)
  "(YEAR TAXABLE INSS EXEMPT) of a payer-income CSV LINE."
  (pcase (split-string line ",")
    ((and `(,year ,_payer ,taxable ,inss ,_irrf ,exempt . ,_)
          (guard (and (string-match-p "\\`[0-9]\\{4\\}\\'" year)
                      (string-match-p fin-ir--amount-re taxable) (string-match-p fin-ir--amount-re inss)
                      (string-match-p fin-ir--amount-re exempt))))
     (list (string-to-number year) (fin-ir--cents taxable) (fin-ir--cents inss) (fin-ir--cents exempt)))
    (_ (error "fin-ir: bad payer-income line in %s: %S" file line))))

(defun fin-ir--dctfweb-row (line file)
  "(YEAR TOTAL INSS) of a DCTFWeb CSV LINE: a month's INSS on pró-labore.
TOTAL is the implied pró-labore, nil when INSS is at the cap."
  (pcase (split-string line ";")
    ((and `(,period ,_ ,_ ,_ ,_ ,_ ,debit . ,_)
          (guard (and (string-match "\\`[0-9]\\{2\\}/\\([0-9]\\{4\\}\\)\\'" period)
                      (string-match-p fin-ir--amount-re debit))))
     (let* ((inss (fin-ir--cents debit))
            (year (string-to-number (substring period 3)))
            (capped (>= inss (round (* fin-ir-inss-rate (fin-ir-rule year :inss-cap))))))
       (list year (unless capped (round inss fin-ir-inss-rate)) inss)))
    (_ (error "fin-ir: bad DCTFWeb line in %s: %S" file line))))

(defun fin-ir--fontes ()
  "Payer-income rows: (YEAR TAXABLE INSS EXEMPT) each."
  (fin-ir--csv (and fin-ir-payments-dir (expand-file-name "fontes-pagadoras" fin-ir-payments-dir))
               "\\.csv\\'" #'fin-ir--fontes-row))

(defun fin-ir-reported-exempt ()
  "Alist YEAR -> exempt cents the payers reported.
Years with none are left out: no report and a zero report look alike."
  (let (out)
    (dolist (r (fin-ir--fontes) out)
      (when (> (nth 3 r) 0) (setq out (fin-ir--bump out (car r) (nth 3 r)))))))

(defun fin-ir-other-payers ()
  "Alist YEAR -> taxable cents from payers that withheld no INSS."
  (let (out)
    (dolist (r (fin-ir--fontes) out)
      (when (zerop (nth 2 r)) (setq out (fin-ir--bump out (car r) (nth 1 r)))))))

(defun fin-ir-pro-labore-files ()
  "Alist YEAR -> (TOTAL INSS MONTHS) of pró-labore from Receita files.
Payer-income files win for their years (12 months); other years sum DCTFWeb
months, TOTAL nil when a month's INSS is at the cap."
  (let* ((fontes (cl-remove-if-not (lambda (r) (> (nth 2 r) 0)) (fin-ir--fontes)))
         (out (mapcar (lambda (r) (list (nth 0 r) (nth 1 r) (nth 2 r) 12)) fontes)))
    (dolist (r (fin-ir--csv fin-ir-payments-dir "\\`dctfweb-.*\\.csv\\'" #'fin-ir--dctfweb-row))
      (unless (assq (car r) fontes)
        (let ((cell (assq (car r) out)))
          (if cell
              (setcdr cell (list (and (nth 1 cell) (nth 1 r) (+ (nth 1 cell) (nth 1 r)))
                                 (+ (nth 2 cell) (nth 2 r)) (1+ (nth 3 cell))))
            (push (list (car r) (nth 1 r) (nth 2 r) 1) out)))))
    out))

(defun fin-ir--pro-labore (year months)
  "(TOTAL MONTHLY INSS) of YEAR's pró-labore over MONTHS, nil if unknown.
A yearly total assumes an even pró-labore: INSS runs low when one month
went past the cap."
  (let ((cap (fin-ir-rule year :inss-cap)))
    (pcase (cdr (assq year fin-ir-pro-labore))
      ('nil (let ((f (cdr (assq year (fin-ir-pro-labore-files)))))
              (and f (nth 0 f) (list (nth 0 f) (round (nth 0 f) (nth 2 f)) (nth 1 f)))))
      ((and (pred integerp) total)
       (let ((part (round (* total months) 12)))
         (list part (round total 12) (round (* fin-ir-inss-rate (min part (* months cap)))))))
      (`(:monthly ,m)
       (cl-assert (integerp m))
       (list (* m months) m (round (* months fin-ir-inss-rate (min m cap)))))
      (v (error "fin-ir: bad pró-labore for %d: %S" year v)))))

(defun fin-ir--books-exempt-p ()
  "Non-nil when `fin-ir-full-books' makes profit above the presumed exempt."
  (pcase fin-ir-full-books
    ((or 'verified 'yes) t)
    ((or 'no 'nil) nil)
    (v (error "fin-ir: bad fin-ir-full-books: %S" v))))

(defun fin-ir--excess-tax (year base excess inss health retirement)
  "Tax EXCESS profit adds when taxed on top of BASE taxable cents."
  (- (plist-get (fin-ir--deductions year (+ base excess) inss health retirement) :tax)
     (plist-get (fin-ir--deductions year base inss health retirement) :tax)))

(defun fin-ir--declared (year simples company)
  "(REVENUE DAS) of YEAR from SIMPLES, else from the COMPANY position."
  (list (or (fin-ir--year-total simples year #'cadr) (plist-get company :revenue))
        (or (fin-ir--year-total simples year #'caddr)
            (cdr (assoc "das" (plist-get company :taxes)))
            0)))

(defun fin-ir-position (year &optional ctx)
  "Plist of YEAR's income tax position from CTX (`fin-ir-context' if nil).
Taxable parts are nil while the pró-labore is unknown.  Profit is what
reached the partner beyond the pró-labore net of its INSS."
  (let* ((ctx (or ctx (fin-ir-context)))
         (company (fin-ir-company year (plist-get ctx :receita)))
         (declared (fin-ir--declared year (plist-get ctx :simples) company))
         (wd (fin-ir-withdrawals year))
         (returned (fin-ir-returned year))
         (paid (fin-ir-partner-paid year))
         (drawn (- (+ (apply #'+ (mapcar #'cdr wd)) (plist-get company :partner-taxes) paid) returned))
         (pl (fin-ir--pro-labore year (fin-ir--competence-months year (format-time-string "%F"))))
         (individuals (fin-ir-individuals year))
         (from-individuals (apply #'+ (mapcar #'cdr individuals)))
         (base (and pl (+ (car pl)
                          (or (fin-ir--declared-field ctx year :other-taxable fin-ir-other-taxable)
                              (cdr (assq year (plist-get ctx :other-payers)))
                              0)
                          (if (eq fin-ir-individual-income 'taxable) from-individuals 0))))
         (inss (nth 2 pl))
         (profit (and pl (max 0 (- drawn (- (car pl) inss)))))
         (presumed (max 0 (- (round (* fin-ir-presumption (car declared))) (cadr declared))))
         (book (cdr (or (assq year fin-ir-book-profit) (assq year (plist-get ctx :books)))))
         (limit (max presumed (or book 0)))
         (excess (and profit (max 0 (- profit limit))))
         (books (and (not book) (fin-ir--books-exempt-p)))
         (taxable (and pl (if books base (+ base excess))))
         (health (fin-ir-health year))
         (retirement (fin-ir-retirement year))
         (through (car (car (last (fin-ir--through
                                    (cl-remove-if-not (lambda (r) (string-prefix-p (format "%04d-" year) (car r)))
                                                      (plist-get ctx :simples))
                                    (format "%04d-12" year)))))))
    (append
     (list :year year :revenue (plist-get company :revenue)
           :declared-revenue (fin-ir--year-total (plist-get ctx :simples) year #'cadr)
           :invoiced (fin-ir--year-total (plist-get ctx :invoices) year #'cdr)
           :declared-through through
           :revenue-through (and through (fin-ir--revenue-through year through))
           :invoiced-through (and through (or (fin-ir--year-total (fin-ir--through (plist-get ctx :invoices) through)
                                                                  year #'cdr)
                                              0))
           :taxes (plist-get company :taxes) :partner-taxes (plist-get company :partner-taxes)
           :returned returned :partner-paid paid
           :drawn drawn :months (length wd) :peak-month (if wd (apply #'max (mapcar #'cdr wd)) 0)
           :pro-labore (nth 1 pl) :pro-labore-total (car pl) :other-taxable (and pl (- base (car pl)))
           :taxable taxable :inss inss
           :profit profit :presumed presumed :book-profit book :excess excess
           :exempt (and profit (if books profit (min profit limit)))
           :excess-tax (and pl (fin-ir--excess-tax year base excess inss health retirement))
           :defis-exempt (fin-ir--defis-exempt ctx year)
           :reported-exempt (cdr (assq year (plist-get ctx :reported-exempt)))
           :declared-exempt (fin-ir--declared-field ctx year :exempt fin-ir-declared-exempt)
           :exclusive (fin-ir--declared-field ctx year :exclusive fin-ir-exclusive)
           :assets (fin-ir--declared-field ctx year :assets fin-ir-assets-declared)
           :individuals-exempt (if (eq fin-ir-individual-income 'exempt) from-individuals 0)
           :health health :retirement retirement
           :individuals individuals)
     (and taxable (fin-ir--deductions year taxable inss health retirement)))))

(defun fin-ir--defis-exempt (ctx year)
  "YEAR's DEFIS exempt profit: config, then CTX :defis, then 0 when a
DEFIS PDF of YEAR is on file, else nil."
  (let ((dir (and fin-ir-payments-dir (expand-file-name "defis" fin-ir-payments-dir))))
    (or (cdr (assq year fin-ir-defis-exempt))
        (cdr (assq year (plist-get ctx :defis)))
        (and dir (file-directory-p dir)
             (directory-files dir nil (format "\\`defis-cnpj-%d\\(-retificadora-[0-9]+\\)?\\.pdf\\'" year))
             0))))

(defun fin-ir--filed ()
  "Alist YEAR -> filed ISO date: `fin-ir-filed', then the receipt files."
  (let ((out (copy-sequence fin-ir-filed))
        (dir (and fin-ir-payments-dir (expand-file-name "dirpf" fin-ir-payments-dir))))
    (dolist (f (and dir (file-directory-p dir) (directory-files dir nil "-recibo-")) out)
      (when (string-match "\\`dirpf-cpf-\\([0-9]\\{4\\}\\)-recibo-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" f)
        (let ((y (string-to-number (match-string 1 f))))
          (unless (assq y out) (push (cons y (match-string 2 f)) out)))))))

(defun fin-ir-duty (pos)
  "Filing duty of closed-year POS: plist :required (t, nil or unknown),
:filed, :deadline, :exempt-total and :status (on-time late missing
not-required unknown)."
  (let* ((y (plist-get pos :year))
         (taxable (plist-get pos :taxable))
         (exempt (and (plist-get pos :exempt)
                      (+ (plist-get pos :exempt) (or (plist-get pos :exclusive) 0)
                         (or (plist-get pos :individuals-exempt) 0))))
         (assets (plist-get pos :assets))
         (deadline (fin-ir-rule y :deadline))
         (required (cond ((null taxable) 'unknown)
                         ((or (> taxable (fin-ir-rule y :filing-taxable))
                              (> exempt (fin-ir-rule y :filing-exempt))
                              (and assets (> assets (fin-ir-rule y :filing-assets))))
                          t)))
         (filed (cdr (assq y (fin-ir--filed)))))
    (cl-assert deadline nil "fin-ir: %d is not a closed year" y)
    (list :required required :filed filed :deadline deadline :exempt-total exempt
          :status (cond (filed (if (string< deadline filed) 'late 'on-time))
                        ((eq required t) 'missing)
                        ((null required) 'not-required)
                        (t 'unknown)))))

(provide 'ir)
;;; ir.el ends here
