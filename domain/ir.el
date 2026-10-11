;;; ir.el --- Imposto de renda (IRPF): rules and the year's position  -*- lexical-binding: t; -*-

;; What the bank and the ledger say about the owner's income tax, as a
;; sole partner of a Simples Nacional company:
;;
;; - The company account gives revenue (client payments, `fin-pj-income-payers')
;;   and the taxes it paid (`fin-pj-tax-items').
;; - Deposits from the company into the personal account
;;   (`fin-conv-salary-regexps') are pró-labore, taxable, plus distributed
;;   profit, exempt up to the presumed profit (LC 123 art. 14 §1: the Lei
;;   9.249 art. 15 rate times revenue, minus the Simples tax) unless full
;;   books show more.  The split is not in any statement: `fin-ir-pro-labore'.
;; - Deductions: INSS on pró-labore, health (`health' entries not matching
;;   `fin-ir-health-excluded'), PGBL up to 12% (`retirement', when
;;   `fin-ir-retirement-plan' is pgbl).
;;
;; Rule values per calendar year are in `fin-ir-rules', each with its source.
;; Amounts are integer cents.

(require 'cl-lib)
(require 'db)
(require 'pj)

(defvar fin-conv-salary-regexps)

(defcustom fin-ir-pro-labore nil
  "Pró-labore per calendar year: list of (YEAR . CENTS) for the year's total,
or (YEAR :monthly CENTS) for a fixed monthly amount, counted over the months
the company deposited in the personal account."
  :type '(alist :key-type integer :value-type sexp) :group 'fin)

(defcustom fin-ir-filed nil
  "Income tax returns delivered, by calendar year: list of (YEAR . ISO-DATE)."
  :type '(alist :key-type integer :value-type string) :group 'fin)

(defcustom fin-ir-simples-declared nil
  "What the company declared in PGDAS-D per calendar year: list of
(YEAR REVENUE-CENTS DAS-CENTS)."
  :type '(repeat (list integer integer integer)) :group 'fin)

(defcustom fin-ir-defis-exempt nil
  "Exempt profit paid to the partner per DEFIS, by calendar year: list of
(YEAR . CENTS)."
  :type '(alist :key-type integer :value-type integer) :group 'fin)

(defcustom fin-ir-open-debts nil
  "Debts Receita notified and not yet paid: list of (DUE-ISO CODE CENTS TEXT)."
  :type '(repeat (list string string integer string)) :group 'fin)

(defcustom fin-ir-full-books nil
  "Whether the company keeps full books (escrituração contábil): yes, no or
nil (unknown).  With them, profit above the presumed one is exempt too."
  :type '(choice (const yes) (const no) (const nil)) :group 'fin)

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

(defcustom fin-ir-health-excluded '("nata[cç][aã]o" "academia")
  "Regexps of `health' items that are not deductible (Lei 9.250 art. 8 II a
lists doctors, dentists, psychologists, physiotherapists, labs, hospitals)."
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

(defun fin-ir-company (year)
  "Plist of YEAR's company :revenue and :taxes, an alist (ITEM . CENTS)."
  (let ((revenue 0) taxes)
    (dolist (b (fin-ir--pj-rows year))
      (cond ((fin-pj-income-category b) (cl-incf revenue (nth 3 b)))
            ((fin-pj-tax-item b)
             (let ((cell (assoc (fin-pj-tax-item b) taxes)))
               (if cell (cl-incf (cdr cell) (nth 3 b))
                 (push (cons (fin-pj-tax-item b) (nth 3 b)) taxes))))))
    (list :revenue revenue :taxes (nreverse taxes))))

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
  "(DEDUCTIBLE . EXCLUDED) cents of YEAR's `health' entries."
  (let ((case-fold-search t) (ok 0) (no 0))
    (dolist (e (fin-ir--entries year "out" '("health")))
      (if (and (car e) (cl-some (lambda (re) (string-match-p re (car e))) fin-ir-health-excluded))
          (cl-incf no (nth 1 e))
        (cl-incf ok (nth 1 e))))
    (cons ok no)))

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

(defun fin-ir-assets ()
  "Cents in the savings accounts now (the `accounts' sheet snapshot)."
  (or (caar (fin-db-query "SELECT SUM(balance) FROM account WHERE parent IS NULL")) 0))

(defun fin-ir--pj-covered-p (year)
  "Non-nil when the company account statements cover all of YEAR."
  (let ((first (caar (fin-db-query "SELECT MIN(date) FROM bank_txn WHERE account = 'pj'"))))
    (and first (string< first (format "%04d-01-08" year)))))

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

(defun fin-ir--pro-labore (year months)
  "(TOTAL MONTHLY INSS) of YEAR's pró-labore over MONTHS, or nil if unknown."
  (let ((cap (fin-ir-rule year :inss-cap)))
    (pcase (cdr (assq year fin-ir-pro-labore))
      ('nil nil)
      ((and (pred integerp) total)
       (list total (round total 12) (round (* fin-ir-inss-rate (min total (* 12 cap))))))
      (`(:monthly ,m)
       (cl-assert (integerp m))
       (list (* m months) m (round (* months fin-ir-inss-rate (min m cap)))))
      (v (error "fin-ir: bad pró-labore for %d: %S" year v)))))

(defun fin-ir-position (year)
  "Plist of YEAR's income tax position; taxable parts nil while
`fin-ir-pro-labore' lacks YEAR.  Profit is what the company deposited beyond
the pró-labore net of its INSS."
  (let* ((company (fin-ir-company year))
         (declared (cdr (assq year fin-ir-simples-declared)))
         (revenue (if declared (car declared) (plist-get company :revenue)))
         (das (if declared (cadr declared) (or (cdr (assoc "das" (plist-get company :taxes))) 0)))
         (wd (fin-ir-withdrawals year))
         (drawn (apply #'+ (mapcar #'cdr wd)))
         (pl (fin-ir--pro-labore year (length wd)))
         (individuals (fin-ir-individuals year))
         (taxable (and pl (+ (car pl)
                             (or (cdr (assq year fin-ir-other-taxable)) 0)
                             (if (eq fin-ir-individual-income 'taxable)
                                 (apply #'+ (mapcar #'cdr individuals))
                               0))))
         (inss (nth 2 pl))
         (health (fin-ir-health year))
         (retirement (fin-ir-retirement year)))
    (append
     (list :year year :revenue (plist-get company :revenue) :declared-revenue (car declared)
           :bank-covered (fin-ir--pj-covered-p year)
           :taxes (plist-get company :taxes)
           :drawn drawn :months (length wd) :peak-month (if wd (apply #'max (mapcar #'cdr wd)) 0)
           :pro-labore (nth 1 pl) :taxable taxable :inss inss
           :profit (and pl (max 0 (- drawn (- (car pl) inss))))
           :presumed (max 0 (- (round (* fin-ir-presumption revenue)) das))
           :defis-exempt (cdr (assq year fin-ir-defis-exempt))
           :health (car health) :health-excluded (cdr health) :retirement retirement
           :individuals individuals)
     (and taxable (fin-ir--deductions year taxable inss (car health) retirement)))))

(defun fin-ir-duty (pos)
  "Filing duty of closed-year position POS: plist :required (t, nil, or
unknown), :filed (ISO date or nil), :status (on-time late missing
not-required unknown) and :deadline."
  (let* ((y (plist-get pos :year))
         (taxable (plist-get pos :taxable))
         (exempt (plist-get pos :profit))
         (deadline (fin-ir-rule y :deadline))
         (required (cond ((null taxable) 'unknown)
                         ((or (> taxable (fin-ir-rule y :filing-taxable))
                              (> exempt (fin-ir-rule y :filing-exempt)))
                          t)))
         (filed (cdr (assq y fin-ir-filed))))
    (cl-assert deadline nil "fin-ir: %d is not a closed year" y)
    (list :required required :filed filed :deadline deadline
          :status (cond (filed (if (string< deadline filed) 'late 'on-time))
                        ((eq required t) 'missing)
                        ((null required) 'not-required)
                        (t 'unknown)))))

(provide 'ir)
;;; ir.el ends here
