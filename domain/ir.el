;;; ir.el --- Imposto de renda (IRPF): rules and the year's position  -*- lexical-binding: t; -*-

;; What the bank and the ledger say about the owner's income tax, as a
;; sole partner of a Simples Nacional company:
;;
;; - The company account gives revenue (client payments, `fin-pj-income-payers')
;;   and the taxes it paid (`fin-pj-tax-items'), named by the Receita
;;   payment each matches; the partner's own taxes it paid are profit.
;; - Deposits from the company into the personal account
;;   (`fin-conv-salary-regexps'), less what went back, are pró-labore,
;;   taxable, plus distributed profit, exempt up to the presumed profit (LC
;;   123 art. 14 §1: the Lei 9.249 art. 15 rate times revenue, minus the
;;   Simples tax) unless full books show more (`fin-ir-full-books').  The
;;   split is not in any statement: `fin-ir-pro-labore-files' (Receita
;;   files), overridable by `fin-ir-pro-labore'.
;; - PGDAS-D and NFS-e lists give revenue declared and invoiced per month.
;; - Deductions: INSS on pró-labore, health (`health' entries matching
;;   `fin-ir-health-deductible'), PGBL up to 12% (`retirement', when
;;   `fin-ir-retirement-plan' is pgbl).
;;
;; Rule values per calendar year are in `fin-ir-rules', each with its source.
;; Amounts are integer cents.

(require 'cl-lib)
(require 'db)
(require 'pj)
(require 'reconcile)

(defvar fin-conv-salary-regexps)
(defvar fin-bank-own-regexps)

(defcustom fin-ir-pro-labore nil
  "Pró-labore per calendar year overriding the Receita files (see
`fin-ir-pro-labore-files'): list of (YEAR . CENTS) for the year's total,
or (YEAR :monthly CENTS) for a fixed monthly amount, counted from January
through the last completed month (all 12 for a past year)."
  :type '(alist :key-type integer :value-type sexp) :group 'fin)

(defcustom fin-ir-filed nil
  "Income tax returns delivered, by calendar year: list of (YEAR . ISO-DATE),
added to those `fin-ir--filed' reads from the receipts' file names."
  :type '(alist :key-type integer :value-type string) :group 'fin)

(defcustom fin-ir-payments-dir
  (expand-file-name "../infra/inbox/ir/"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory of Receita lists: payments (pagamentos-*.csv: arrecadacao,
receita, documento, apuracao, total) and PGDAS-D summaries
(pgdasd-*-resumo.csv: pa, declaracao_seq, receita, das).  Nil or absent:
none."
  :type '(choice directory (const nil)) :group 'fin)

(defcustom fin-ir-nfse-dir
  (expand-file-name "../infra/inbox/nfse/"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory of NFS-e indexes (nfse-*-index.csv: sistema, numero, emissao,
competencia, tomador_doc, tomador, valor, situacao, arquivo).  Nil or
absent: none."
  :type '(choice directory (const nil)) :group 'fin)

(defcustom fin-ir-receita-regexps '("RECEITA FED" "DARF" "\\bDAS\\b" "Simples Nacional")
  "Bank descriptions of payments to Receita, ignoring case, in the company
and the personal account."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-ir-payment-labels nil
  "Receita payments the Receita lists do not show, labelled by hand: list of
(ISO-DATE CENTS CODE).  A bank row of that date and amount counts as paying
CODE (see `fin-ir-receita-codes')."
  :type '(repeat (list string integer string)) :group 'fin)

(defcustom fin-ir-payment-window 5
  "Days a bank payment may sit from the Receita payment it matches."
  :type 'integer :group 'fin)

(defconst fin-ir-receita-codes
  '(("3333" "das" pj) ("0055" "das" pj) ("1410" "inss" pj) ("4444" "inss" pj)
    ("0211" "irpf" cpf) ("5320" "irpf fine" cpf))
  "Receita codes (código de receita) -> (ITEM PAYER): the tax item and who
owes it, pj (the company) or cpf (the partner).  Other codes keep their
number and are the company's.")

(defcustom fin-ir-defis-exempt nil
  "Exempt profit paid to the partner per DEFIS, by calendar year: list of
(YEAR . CENTS)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-declared-exempt nil
  "Exempt profit from the company the filed DIRPF declares, by calendar
year: list of (YEAR . CENTS)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-exclusive nil
  "Income taxed exclusively at source (investment yields) per DIRPF, by
calendar year: list of (YEAR . CENTS)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-assets-declared nil
  "Bens e direitos on 31/12 per DIRPF, by calendar year: list of
(YEAR . CENTS)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-open-debts nil
  "Debts Receita notified and not yet paid: list of (DUE-ISO CODE CENTS TEXT)."
  :type '(repeat (list string string integer string)) :group 'fin)

(defcustom fin-ir-full-books nil
  "Full books (escrituração contábil) behind profit above the presumed one:
verified (balance sheet on file: all profit exempt), yes (claimed, not
verified: exempt, flagged), no or nil (unknown): only the presumed profit
is exempt, the excess is the partner's taxable income."
  :type '(choice (const verified) (const yes) (const no) (const nil)) :group 'fin)

(defcustom fin-ir-presumption 0.32
  "Lei 9.249 art. 15 presumed-profit rate of the company's activity
(0.32 for services in general)."
  :type 'number :group 'fin)

(defcustom fin-ir-retirement-plan nil
  "Plan behind `retirement' entries: pgbl (deductible), vgbl, none (plain
savings) or nil (unknown)."
  :type '(choice (const pgbl) (const vgbl) (const none) (const nil)) :group 'fin)

(defcustom fin-ir-other-taxable nil
  "Taxable income outside the bank and the ledger, cents per calendar year:
list of (YEAR . CENTS), e.g. work for individuals the return declares."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-health-deductible
  '("m[eé]dic" "dentist" "odonto" "psic[oó]log" "fisio" "\\blab" "exame"
    "hospital" "cl[ií]nic")
  "Regexps of `health' items that are deductible, ignoring case (Lei 9.250
art. 8 II a: doctors, dentists, psychologists, physiotherapists, labs,
hospitals, clinics).  Other `health' entries, item-less ones too, are not
deducted."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-ir-individual-categories '("extras")
  "Income categories received from individuals."
  :type '(repeat string) :group 'fin)

(defcustom fin-ir-individual-income nil
  "Nature of `fin-ir-individual-categories' income: taxable (work or rent:
carnê-leão), exempt (reimbursements, gifts) or nil (unknown)."
  :type '(choice (const taxable) (const exempt) (const nil)) :group 'fin)

(defconst fin-ir--table-2015
  '((2284776 0 0) (3391980 0.075 171358) (4501260 0.15 425757)
    (5597616 0.225 763351) (nil 0.275 1043232))
  "Annual table of anos-calendário 2015–2022 (Lei 13.149/2015):
https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2016")

(defconst fin-ir-rules
  ;; Annual table: (UPTO RATE DEDUCTION), UPTO nil for the top band.
  ;; Filing duty, deadline: each exercício's IN RFB (DOU), arts. 2, 3 and 7.
  ;; INSS cap: each year's Portaria (DOU).
  `((2020 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          ;; IN RFB 2.010/2021, deadline moved by IN RFB 2.020/2021.
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2021-05-31" :inss-cap 610106)
    (2021 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          ;; IN RFB 2.065/2022, deadline moved by IN RFB 2.077/2022.
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2022-05-31" :inss-cap 643357)
    (2022 :table ,fin-ir--table-2015 :simplified-cap 1675434 :reduction nil
          ;; IN RFB 2.134/2023.
          :filing-taxable 2855970 :filing-exempt 4000000 :filing-assets 30000000
          :deadline "2023-05-31" :inss-cap 708722)
    (2023
     ;; https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2023
     :table ((2451192 0 0) (3391980 0.075 183839) (4501260 0.15 438238)
             (5597616 0.225 775832) (nil 0.275 1055713))
     :simplified-cap 1675434 :reduction nil
     ;; IN RFB 2.178/2024.
     :filing-taxable 3063990 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2024-05-31" :inss-cap 750749)
    (2024
     ;; https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/copy_of_2024
     :table ((2696320 0 0) (3391980 0.075 202224) (4501260 0.15 456623)
             (5597616 0.225 794217) (nil 0.275 1074098))
     :simplified-cap 1675434 :reduction nil
     ;; IN RFB 2.255/2025.
     :filing-taxable 3388800 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2025-05-30" :inss-cap 778602)
    (2025
     ;; https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2025
     :table ((2846720 0 0) (3391980 0.075 213504) (4501260 0.15 467903)
             (5597616 0.225 805497) (nil 0.275 1085378))
     :simplified-cap 1675434
     :reduction nil
     ;; IN RFB 2.312/2026 art. 2 (filing duty) and art. 7 (deadline).
     :filing-taxable 3558400 :filing-exempt 20000000 :filing-assets 80000000
     :deadline "2026-05-29"
     ;; Portaria Interministerial MPS/MF 6/2025 (unverified: secondary sources).
     :inss-cap 815741)
    (2026
     ;; https://www.gov.br/receitafederal/pt-br/assuntos/meu-imposto-de-renda/tabelas/2026
     :table ((2914560 0 0) (3391980 0.075 218592) (4501260 0.15 472991)
             (5597616 0.225 810585) (nil 0.275 1090466))
     ;; Lei 15.270/2025: art. 10 X and art. 11-A of Lei 9.250.
     :simplified-cap 1764000
     :reduction (6000000 269415 8820000 842973 0.095575)
     ;; DIRPF 2027 rules not published yet.
     :filing-taxable nil :filing-exempt nil :filing-assets nil :deadline nil
     ;; Portaria Interministerial MPS/MF 13/2026 (unverified: secondary sources).
     :inss-cap 847555))
  "IRPF rules per calendar year (ano-calendário), amounts in cents.")

(defconst fin-ir-late-fine-min 16574
  "Minimum late-filing fine (MAED), also the whole fine when no tax is due:
1% a month on the tax, at least this, at most 20% (each IN RFB, art. 10).")

(defconst fin-ir-inss-rate 0.11
  "Partner's INSS on pró-labore paid by a company: Lei 8.212 art. 21 and 30 §4.")

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
  "Monthly taxable income the 2026 reduction brings to zero tax: Lei 15.270/2025.")

(defun fin-ir-rule (year key)
  "Rule KEY of calendar YEAR; error when YEAR has no rules."
  (let ((r (cdr (assq year fin-ir-rules))))
    (unless r (error "fin-ir: no rules for %S" year))
    (plist-get r key)))

;;; Tax arithmetic

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

;;; Facts from the bank and the ledger

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
  "Cents of CSV AMOUNT, a string matching `fin-ir--amount-re'."
  (round (* 100 (string-to-number amount))))

(defun fin-ir--csv (dir pattern parse)
  "PARSE (LINE FILE) of each line past the header of the files in DIR whose
name matches PATTERN; none when DIR is nil or absent."
  (let (out)
    (dolist (f (and dir (file-directory-p dir) (directory-files dir t pattern)))
      (with-temp-buffer
        (insert-file-contents f)
        (dolist (line (cdr (split-string (buffer-string) "\n" t)))
          (push (funcall parse line f) out))))
    (nreverse out)))

(defun fin-ir--bump (alist key cents)
  "ALIST of (KEY . CENTS) with CENTS added to KEY, new keys last."
  (let ((cell (assoc key alist)))
    (if cell (progn (cl-incf (cdr cell) cents) alist)
      (append alist (list (cons key cents))))))

(defun fin-ir--year-total (rows year value)
  "Sum of VALUE over ROWS whose car, a YYYY-MM month, is in YEAR; nil when none."
  (let* ((prefix (format "%04d-" year))
         (in (cl-remove-if-not (lambda (r) (string-prefix-p prefix (car r))) rows)))
    (and in (apply #'+ (mapcar value in)))))

(defun fin-ir--payment-row (line file)
  "(ISO-DATE CODE CENTS) of a Receita payments CSV LINE from FILE."
  (pcase (split-string line ",")
    (`(,date ,code ,_doc ,_pa ,total)
     (unless (and (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date)
                  (string-match-p "\\`[0-9]+\\'" code)
                  (string-match-p fin-ir--amount-re total))
       (error "fin-ir: bad payment line in %s: %S" file line))
     (list date code (fin-ir--cents total)))
    (_ (error "fin-ir: bad payment line in %s: %S" file line))))

(defun fin-ir-payments ()
  "Receita payments, (ISO-DATE CODE CENTS) each, from the
pagamentos-*.csv files in `fin-ir-payments-dir'."
  (fin-ir--csv fin-ir-payments-dir "\\`pagamentos-.*\\.csv\\'" #'fin-ir--payment-row))

(defun fin-ir--simples-row (line file)
  "(MONTH SEQ REVENUE-CENTS DAS-CENTS) of a PGDAS-D summary CSV LINE from FILE."
  (pcase (split-string line ",")
    ((and `(,pa ,seq ,revenue ,das)
          (guard (and (string-match-p fin-ir--month-re pa) (string-match-p "\\`[0-9]+\\'" seq)
                      (string-match-p fin-ir--amount-re revenue) (string-match-p fin-ir--amount-re das))))
     (list pa (string-to-number seq) (fin-ir--cents revenue) (fin-ir--cents das)))
    (_ (error "fin-ir: bad PGDAS-D line in %s: %S" file line))))

(defun fin-ir-simples ()
  "PGDAS-D per period of apuração, its last declaration: (MONTH REVENUE-CENTS
DAS-CENTS), oldest first, from the pgdasd-*-resumo.csv files in
`fin-ir-payments-dir'.  Two declarations with one number fail loud."
  (let (last)
    (dolist (r (fin-ir--csv fin-ir-payments-dir "\\`pgdasd-.*-resumo\\.csv\\'" #'fin-ir--simples-row))
      (let ((cell (assoc (car r) last)))
        (cond ((null cell) (push r last))
              ((= (nth 1 r) (nth 1 cell)) (error "fin-ir: PGDAS-D %s declaration %d twice" (car r) (nth 1 r)))
              ((> (nth 1 r) (nth 1 cell)) (setcdr cell (cdr r))))))
    (mapcar (lambda (r) (list (car r) (nth 2 r) (nth 3 r)))
            (sort last (lambda (a b) (string< (car a) (car b)))))))

(defun fin-ir--invoice-row (line file)
  "(MONTH CENTS ACTIVE) of an NFS-e index CSV LINE from FILE, MONTH its competência."
  (pcase (split-string line ",")
    ((and `(,_system ,_number ,_issued ,month ,_doc ,_client ,value ,status ,_file)
          (guard (and (string-match-p fin-ir--month-re month) (string-match-p fin-ir--amount-re value)
                      (member status '("ativa" "cancelada")))))
     (list month (fin-ir--cents value) (equal status "ativa")))
    (_ (error "fin-ir: bad NFS-e line in %s: %S" file line))))

(defun fin-ir-invoices ()
  "NFS-e not cancelled per competência: (MONTH . CENTS), oldest first, from
the nfse-*-index.csv files in `fin-ir-nfse-dir'."
  (let (out)
    (dolist (r (fin-ir--csv fin-ir-nfse-dir "\\`nfse-.*-index\\.csv\\'" #'fin-ir--invoice-row))
      (when (nth 2 r) (setq out (fin-ir--bump out (car r) (nth 1 r)))))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun fin-ir--payment-code (b payments used)
  "Receita code of the payment in PAYMENTS that bank row B paid: same
amount, nearest date within `fin-ir-payment-window' days, not in USED (a
hash table, updated)."
  (let ((day (fin-reconcile--day (nth 1 b))) best best-gap)
    (dolist (p payments)
      (when (and (= (nth 2 p) (nth 3 b)) (not (gethash p used)))
        (let ((gap (abs (- day (fin-reconcile--day (car p))))))
          (when (and (<= gap fin-ir-payment-window) (or (null best) (< gap best-gap)))
            (setq best p best-gap gap)))))
    (when best (puthash best t used) (nth 1 best))))

(defun fin-ir--receita-rows ()
  "Bank rows paid to Receita (`fin-ir-receita-regexps') from the company or
the personal account, oldest first: (id date account amount description)."
  (let ((case-fold-search t))
    (cl-remove-if-not
     (lambda (r) (cl-some (lambda (re) (string-match-p re (nth 4 r))) fin-ir-receita-regexps))
     (fin-db-query "SELECT id, date, account, amount, description FROM bank_txn
                     WHERE type = 'out' AND account IN ('pj', 'account') ORDER BY date, id"))))

(defun fin-ir-receita-matches (payments)
  "Each Receita bank row with the code of the payment in PAYMENTS it paid,
or nil: list of (ROW . CODE).  Rows claim payments oldest first, each
payment once across all years."
  (let ((used (make-hash-table :test #'eq)))
    (mapcar (lambda (r) (cons r (or (fin-ir--payment-code r payments used)
                                    (fin-ir--labelled-code r))))
            (fin-ir--receita-rows))))

(defun fin-ir--labelled-code (r)
  "CODE `fin-ir-payment-labels' gives bank row R, (id date account amount desc)."
  (nth 2 (cl-find-if (lambda (l) (and (equal (car l) (nth 1 r)) (eql (nth 1 l) (nth 3 r))))
                     fin-ir-payment-labels)))

(defun fin-ir-code (code)
  "(ITEM PAYER) of Receita CODE (`fin-ir-receita-codes')."
  (or (cdr (assoc code fin-ir-receita-codes)) (list code 'pj)))

(defun fin-ir-wrong-account-p (match)
  "Non-nil when MATCH, (ROW . CODE), left the account of the other payer."
  (and (cdr match)
       (not (eq (nth 1 (fin-ir-code (cdr match)))
                (if (equal (nth 2 (car match)) "pj") 'pj 'cpf)))))

(defun fin-ir-wrong-account (receita)
  "Matches of RECEITA (`fin-ir-receita-matches') paid from the wrong account
once the company had its own (from its first bank row)."
  (let ((first (caar (fin-db-query "SELECT MIN(date) FROM bank_txn WHERE account = 'pj'"))))
    (cl-remove-if-not (lambda (m) (and first (not (string< (nth 1 (car m)) first))
                                       (fin-ir-wrong-account-p m)))
                      receita)))

(defun fin-ir-unmatched (receita)
  "Company rows of RECEITA (`fin-ir-receita-matches') no payment matches."
  (mapcar #'car (cl-remove-if (lambda (m) (or (cdr m) (not (equal (nth 2 (car m)) "pj")))) receita)))

(defun fin-ir--company-tax (b receita)
  "(ITEM . PAYER) of PJ tax row B: its Receita code's when RECEITA matches
it, else its `fin-pj-tax-items' item, the company's."
  (let ((code (cdr (cl-find (car b) receita :key #'caar :test #'equal))))
    (if code
        (let ((c (fin-ir-code code))) (cons (car c) (nth 1 c)))
      (cons (fin-pj-tax-item b) 'pj))))

(defun fin-ir-company (year receita)
  "Plist of YEAR's company :revenue, :taxes, an alist (ITEM . CENTS), and
:partner-taxes, cents of the partner's own taxes it paid.  Tax rows are
named by the Receita payment RECEITA (`fin-ir-receita-matches') matches
(DAS and INSS both leave the bank as a Pix to Receita); company taxes paid
from the personal account count too."
  (let ((prefix (format "%04d-" year)) (revenue 0) (partner 0) taxes)
    (dolist (b (fin-ir--pj-rows year))
      (cond ((fin-pj-income-category b) (cl-incf revenue (nth 3 b)))
            ((fin-pj-tax-item b)
             (let ((tax (fin-ir--company-tax b receita)))
               (if (eq (cdr tax) 'cpf) (cl-incf partner (nth 3 b))
                 (setq taxes (fin-ir--bump taxes (car tax) (nth 3 b))))))))
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
  "Cents the partner sent back to the company account in YEAR: its
in-rows from own accounts (`fin-bank-own-regexps') that are not revenue."
  (let ((case-fold-search t) (sum 0))
    (dolist (b (fin-ir--pj-rows year) sum)
      (when (and (equal (nth 2 b) "in") (not (fin-pj-income-category b))
                 (cl-some (lambda (re) (string-match-p re (nth 4 b))) fin-bank-own-regexps))
        (cl-incf sum (nth 3 b))))))

(defun fin-ir-withdrawals (year)
  "(YYYY-MM . CENTS) the company deposited in the personal account in YEAR."
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

(defun fin-ir-health (year)
  "Deductible cents of YEAR's `health' entries: those whose item matches
`fin-ir-health-deductible'."
  (let ((case-fold-search t) (ok 0))
    (dolist (e (fin-ir--entries year "out" '("health")) ok)
      (when (and (car e) (cl-some (lambda (re) (string-match-p re (car e))) fin-ir-health-deductible))
        (cl-incf ok (nth 1 e))))))

(defun fin-ir--sum (rows)
  (apply #'+ (mapcar #'cadr rows)))

(defun fin-ir-retirement (year)
  "Cents put in `retirement' during YEAR."
  (fin-ir--sum (fin-ir--entries year "out" '("retirement"))))

(defun fin-ir-individuals (year)
  "(YYYY-MM . CENTS) income from individuals (`fin-ir-individual-categories')."
  (let (out)
    (dolist (e (fin-ir--entries year "in" fin-ir-individual-categories))
      (let* ((m (substring (nth 2 e) 0 7)) (cell (assoc m out)))
        (if cell (cl-incf (cdr cell) (nth 1 e)) (push (cons m (nth 1 e)) out))))
    (sort out (lambda (a b) (string< (car a) (car b))))))

;;; Revenue declared against invoiced

(defun fin-ir-context ()
  "Inputs every year's position shares, read once: :receita
(`fin-ir-receita-matches'), :simples (`fin-ir-simples') and :invoices
(`fin-ir-invoices')."
  (list :receita (fin-ir-receita-matches (fin-ir-payments))
        :simples (fin-ir-simples) :invoices (fin-ir-invoices)))

(defun fin-ir--due-p (month today)
  "Non-nil when MONTH's PGDAS-D was due (the 20th of the next month) before TODAY."
  (pcase-let ((`(,y ,m) (mapcar #'string-to-number (split-string month "-"))))
    (string< (if (= m 12) (format "%04d-01-20" (1+ y)) (format "%04d-%02d-20" y (1+ m)))
             today)))

(defun fin-ir-bank-revenue ()
  "(MONTH . CENTS) of client payments the company account received, oldest
first (`fin-pj-income-payers')."
  (let (out)
    (dolist (b (fin-db-query "SELECT id, date, type, amount, description FROM bank_txn
                               WHERE account = 'pj' ORDER BY date")
               out)
      (when (fin-pj-income-category b)
        (setq out (fin-ir--bump out (substring (nth 1 b) 0 7) (nth 3 b)))))))

(defun fin-ir-revenue-gaps (simples invoices today &optional bank)
  "(MONTH DECLARED INVOICED BANKED) cents for each month whose PGDAS-D
revenue in SIMPLES (`fin-ir-simples') is more than a real off the NFS-e in
INVOICES (`fin-ir-invoices'), from the first period declared through the
last one declared or due by TODAY (ISO date).  BANKED is the month's
revenue in BANK (`fin-ir-bank-revenue'), nil when BANK lacks the month."
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
  "Median of the non-empty list of numbers XS."
  (cl-assert xs)
  (let* ((v (vconcat (sort (copy-sequence xs) #'<))) (n (length v)))
    (if (cl-oddp n) (aref v (/ n 2))
      (/ (+ (aref v (1- (/ n 2))) (aref v (/ n 2))) 2.0))))

(defun fin-ir-rate-outliers (simples)
  "(MONTH RATE MEDIAN REVENUE DAS) for each month of SIMPLES
(`fin-ir-simples') with revenue whose DAS share of it is more than
`fin-ir-das-rate-tolerance' off its year's median."
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

;;; The year's position

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
  "Months of YEAR's pró-labore by TODAY (ISO date): 12 for a past year,
January through the last completed month for the current one."
  (let ((now (string-to-number (substring today 0 4))))
    (cond ((< year now) 12)
          ((= year now) (1- (string-to-number (substring today 5 7))))
          (t 0))))

(defun fin-ir--fontes-row (line file)
  "(YEAR TOTAL INSS) of a payer-income CSV LINE from FILE (ano, fonte,
rendimento_tributavel, prev_oficial, …) when it withheld INSS, as only
the company's pró-labore does; nil otherwise."
  (pcase (split-string line ",")
    ((and `(,year ,_payer ,taxable ,inss . ,_)
          (guard (and (string-match-p "\\`[0-9]\\{4\\}\\'" year)
                      (string-match-p fin-ir--amount-re taxable) (string-match-p fin-ir--amount-re inss))))
     (and (> (fin-ir--cents inss) 0)
          (list (string-to-number year) (fin-ir--cents taxable) (fin-ir--cents inss))))
    (_ (error "fin-ir: bad payer-income line in %s: %S" file line))))

(defun fin-ir--dctfweb-row (line file)
  "(YEAR TOTAL INSS) of a DCTFWeb CSV LINE from FILE (periodo MM/YYYY; …;
debito): the month's INSS on pró-labore, and the pró-labore it implies."
  (pcase (split-string line ";")
    ((and `(,period ,_ ,_ ,_ ,_ ,_ ,debit . ,_)
          (guard (and (string-match "\\`[0-9]\\{2\\}/\\([0-9]\\{4\\}\\)\\'" period)
                      (string-match-p fin-ir--amount-re debit))))
     (let ((inss (fin-ir--cents debit)))
       (list (string-to-number (substring period 3)) (round inss fin-ir-inss-rate) inss)))
    (_ (error "fin-ir: bad DCTFWeb line in %s: %S" file line))))

(defun fin-ir-pro-labore-files ()
  "Pró-labore per year from the Receita files in `fin-ir-payments-dir':
alist YEAR -> (TOTAL INSS MONTHS).  A year the payer-income files
\(fontes-pagadoras/*.csv, what the company reported) cover comes from them,
12 months; any other from its DCTFWeb months (dctfweb-*.csv)."
  (let* ((fontes (delq nil (fin-ir--csv (and fin-ir-payments-dir
                                              (expand-file-name "fontes-pagadoras" fin-ir-payments-dir))
                                         "\\.csv\\'" #'fin-ir--fontes-row)))
         (out (mapcar (lambda (r) (list (nth 0 r) (nth 1 r) (nth 2 r) 12)) fontes)))
    (dolist (r (fin-ir--csv fin-ir-payments-dir "\\`dctfweb-.*\\.csv\\'" #'fin-ir--dctfweb-row))
      (unless (assq (car r) fontes)
        (let ((cell (assq (car r) out)))
          (if cell
              (setcdr cell (list (+ (nth 1 cell) (nth 1 r)) (+ (nth 2 cell) (nth 2 r)) (1+ (nth 3 cell))))
            (push (list (car r) (nth 1 r) (nth 2 r) 1) out)))))
    out))

(defun fin-ir--pro-labore (year months)
  "(TOTAL MONTHLY INSS) of YEAR's pró-labore over MONTHS, or nil if unknown:
`fin-ir-pro-labore' when it has YEAR, else `fin-ir-pro-labore-files'.  A
yearly total caps INSS at 12 monthly caps: exact for an even pró-labore,
low when one month went past the cap."
  (let ((cap (fin-ir-rule year :inss-cap)))
    (pcase (cdr (assq year fin-ir-pro-labore))
      ('nil (let ((f (cdr (assq year (fin-ir-pro-labore-files)))))
              (and f (list (nth 0 f) (round (nth 0 f) (nth 2 f)) (nth 1 f)))))
      ((and (pred integerp) total)
       (list total (round total 12) (round (* fin-ir-inss-rate (min total (* 12 cap))))))
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
  "(REVENUE DAS) of YEAR: PGDAS-D sums from SIMPLES, else the COMPANY
position's bank revenue and DAS."
  (list (or (fin-ir--year-total simples year #'cadr) (plist-get company :revenue))
        (or (fin-ir--year-total simples year #'caddr)
            (cdr (assoc "das" (plist-get company :taxes)))
            0)))

(defun fin-ir-position (year &optional ctx)
  "Plist of YEAR's income tax position from CTX (`fin-ir-context', read now
when nil); taxable parts nil while `fin-ir-pro-labore' lacks YEAR.  Profit
is what reached the partner (deposits and own taxes the company paid, less
what went back) beyond the pró-labore net of its INSS."
  (let* ((ctx (or ctx (fin-ir-context)))
         (company (fin-ir-company year (plist-get ctx :receita)))
         (declared (fin-ir--declared year (plist-get ctx :simples) company))
         (wd (fin-ir-withdrawals year))
         (returned (fin-ir-returned year))
         (drawn (- (+ (apply #'+ (mapcar #'cdr wd)) (plist-get company :partner-taxes)) returned))
         (pl (fin-ir--pro-labore year (fin-ir--competence-months year (format-time-string "%F"))))
         (individuals (fin-ir-individuals year))
         (from-individuals (apply #'+ (mapcar #'cdr individuals)))
         (base (and pl (+ (car pl)
                          (or (cdr (assq year fin-ir-other-taxable)) 0)
                          (if (eq fin-ir-individual-income 'taxable) from-individuals 0))))
         (inss (nth 2 pl))
         (profit (and pl (max 0 (- drawn (- (car pl) inss)))))
         (presumed (max 0 (- (round (* fin-ir-presumption (car declared))) (cadr declared))))
         (excess (and profit (max 0 (- profit presumed))))
         (books (fin-ir--books-exempt-p))
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
           :returned returned
           :drawn drawn :months (length wd) :peak-month (if wd (apply #'max (mapcar #'cdr wd)) 0)
           :pro-labore (nth 1 pl) :taxable taxable :inss inss
           :profit profit :presumed presumed :excess excess
           :exempt (and profit (if books profit (min profit presumed)))
           :excess-tax (and pl (fin-ir--excess-tax year base excess inss health retirement))
           :defis-exempt (cdr (assq year fin-ir-defis-exempt))
           :declared-exempt (cdr (assq year fin-ir-declared-exempt))
           :exclusive (cdr (assq year fin-ir-exclusive))
           :assets (cdr (assq year fin-ir-assets-declared))
           :individuals-exempt (if (eq fin-ir-individual-income 'exempt) from-individuals 0)
           :health health :retirement retirement
           :individuals individuals)
     (and taxable (fin-ir--deductions year taxable inss health retirement)))))

(defun fin-ir--filed ()
  "Returns delivered: `fin-ir-filed', then one per receipt in the dirpf/
folder of `fin-ir-payments-dir', named dirpf-cpf-YEAR-recibo-FILED-ISO.pdf
\(YEAR the ano-calendário)."
  (let ((out (copy-sequence fin-ir-filed))
        (dir (and fin-ir-payments-dir (expand-file-name "dirpf" fin-ir-payments-dir))))
    (dolist (f (and dir (file-directory-p dir) (directory-files dir nil "-recibo-")) out)
      (when (string-match "\\`dirpf-cpf-\\([0-9]\\{4\\}\\)-recibo-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" f)
        (let ((y (string-to-number (match-string 1 f))))
          (unless (assq y out) (push (cons y (match-string 2 f)) out)))))))

(defun fin-ir-duty (pos)
  "Filing duty of closed-year position POS: plist :required (t, nil, or
unknown), :filed (ISO date or nil), :status (on-time late missing
not-required unknown), :deadline and :exempt-total (exempt profit plus
exclusive-taxation and exempt individual income)."
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
