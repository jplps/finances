;;; health.el --- Ledger health: what to do so the ODS matches the bank  -*- lexical-binding: t; -*-

;; The cockpit's to-do list, from what the import and `fin-bank-fix' stored
;; (no reconciliation runs here):
;;
;; - Inbox: an account whose latest statement ends before last month's last
;;   day needs a new export.  An open card bill is dated at its next
;;   closing, so it counts as current.
;; - Fix: the last `fin-bank-fix' run; it is due again when bank rows were
;;   imported after it, or when it was a dry run with changes pending.
;; - Classify: large recent bank rows that run could only report.
;; - Gaps: complete months where ledger spending is off the bank's by more
;;   than `fin-health-gap-share', counted as `fin-bank-fix' counts room:
;;   bank rows normalized (refunds, fees, Pix no Crédito), ignored rows,
;;   savings moves and the PJ account's tax payments left out; ledger
;;   savings, tax and company rows left out.
;; - Cash: this month and the last, ledger income less spending less what
;;   the bank moved into savings against the cash all bank accounts gained,
;;   card bill payments left out; flagged past `fin-health-cash-slack'.
;; - Accounts: an account rebuilt from the bank (`fin-accounts-derived') off
;;   the sheet by more than the yield it could have earned since its anchor.
;; - Documents: each `fin-health-expected-docs' entry no inbox file matches;
;;   key files (certificates) that do not belong in the inbox.
;; - Open items: `fin-health-todo', kept by hand.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'bank)
(require 'bankfix)
(require 'pj)
(require 'accounts)
(require 'cashflow)

(defconst fin-health-accounts '("account" "card" "pj" "pj-card")
  "Account kinds whose statements the inbox check follows.")

(defcustom fin-health-gap-share 0.10
  "Share of a month's bank spending past which the ledger gap is flagged."
  :type 'number :group 'fin)

(defcustom fin-health-yield-rate 0.15
  "Yearly yield a rebuilt account may trail the sheet by before it is flagged."
  :type 'number :group 'fin)

(defcustom fin-health-cash-slack 10000
  "Cents a month's cash check may be off before it is flagged."
  :type 'integer :group 'fin)

(defcustom fin-health-expected-docs nil
  "Documents the inbox should hold: list of (LABEL . REGEXP), REGEXP matched
against paths relative to the inbox root."
  :type '(alist :key-type string :value-type regexp) :group 'fin)

(defcustom fin-health-key-file-regexp "\\.\\(pfx\\|p12\\|pem\\|key\\)\\'"
  "Files that carry private keys and must not sit in the inbox."
  :type 'regexp :group 'fin)

(defcustom fin-health-todo nil
  "Open items to do, kept by hand: list of strings."
  :type '(repeat string) :group 'fin)

(defun fin-health--month-start (months-back)
  "ISO first day of the month MONTHS-BACK before the current one."
  (cl-assert (natnump months-back))
  (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time)))
    (let ((k (- (+ (* y 12) (1- m)) months-back)))
      (format "%04d-%02d-01" (/ k 12) (1+ (% k 12))))))

(defun fin-health--last-month-end ()
  "ISO last day of the previous month."
  (format-time-string "%Y-%m-%d"
                      (time-subtract (date-to-time (concat (fin-health--month-start 0) " 12:00"))
                                     (days-to-time 1))))

(defun fin-health-inbox ()
  "(ACCOUNT LATEST OK) per account kind with any statement: LATEST its last
balance date, OK non-nil when it reaches last month's end."
  (let ((need (fin-health--last-month-end)))
    (cl-loop for a in fin-health-accounts
             for d = (car (fin-bankdb-latest-balance a))
             when d collect (list a d (not (string< d need))))))

(defun fin-health-fix ()
  "Plist :run-at :changes :dry :since (rows imported after) :ok of the last
`fin-bank-fix' run; nil when it never ran."
  (pcase (fin-bankdb-last-fix-run)
    (`(,at ,changes ,dry)
     (let ((since (fin-bankdb-imported-since at)))
       (list :run-at at :changes changes :dry (= dry 1) :since since
             :ok (and (zerop since) (or (= dry 0) (zerop changes))))))))

(defun fin-health--bank-spent-p (b pj)
  "Non-nil if bank row B is spending as the ledger books it.  PJ holds the
ids of the company account's rows, whose tax payments are not spending."
  (and (equal (nth 2 b) "out")
       (not (fin-bank--ignored-p b))
       (not (fin-conv--savings-p b))
       (not (and (gethash (car b) pj) (fin-pj-tax-item b)))))

(defun fin-health--pj-ids ()
  "Set of the company account's bank row ids."
  (let ((h (make-hash-table :test #'equal)))
    (dolist (i (car (fin-bankdb-account-rows "pj"))) (puthash i t h))
    h))

(defun fin-health-months (n)
  "(YYYY-MM BANK LEDGER) for the N complete months before this one."
  (cl-assert (and (integerp n) (> n 0)))
  (let* ((from (fin-health--month-start n))
         (to (fin-health--month-start 0))
         (bank (make-hash-table :test #'equal))
         (ledger (fin-bank-fix--ledger-out from))
         (pj (fin-health--pj-ids)))
    (dolist (b (plist-get (fin-conv-normalize (fin-bankdb-since from)) :rows))
      (when (and (string< (nth 1 b) to) (fin-health--bank-spent-p b pj))
        (let ((ym (substring (nth 1 b) 0 7)))
          (puthash ym (+ (gethash ym bank 0) (nth 3 b)) bank))))
    (cl-loop for k from n downto 1
             for ym = (substring (fin-health--month-start k) 0 7)
             collect (list ym (gethash ym bank 0) (or (cdr (assoc ym ledger)) 0)))))

(defun fin-health-gaps (n)
  "Months of `fin-health-months' N whose gap passes `fin-health-gap-share'."
  (cl-remove-if-not (lambda (m) (pcase-let ((`(,_ ,bank ,led) m))
                                  (and (> bank 0) (> (abs (- led bank)) (* fin-health-gap-share bank)))))
                    (fin-health-months n)))

(defconst fin-health--bill-regexp "\\`Pagamento d[ae] fatura\\|\\`Pagamento recebido\\'"
  "A card bill payment: paid on the account, received on the card.")

(defun fin-health--bank-cash (year month)
  "Signed cents all bank accounts gained in YEAR-MONTH, card bill payments
left out: they only move money between the account and the card."
  (let ((total 0))
    (dolist (r (fin-db-query "SELECT type, amount, description FROM bank_txn
                               WHERE strftime('%Y-%m', date) = printf('%04d-%02d', ?, ?)"
                             (list year month))
               total)
      (pcase-let ((`(,type ,amount ,desc) r))
        (unless (string-match-p fin-health--bill-regexp desc)
          (cl-incf total (if (equal type "in") amount (- amount))))))))

(defun fin-health-cash (year month)
  "(LEFT CASH) of YEAR-MONTH: ledger income less spending less what the bank
moved into savings, and the cash the bank accounts gained.  They match when
the ledger holds the month's flows."
  (pcase-let ((`(,_ ,in ,out ,_) (nth (1- month) (fin-report--year-months year))))
    (list (- (or in 0) (or out 0) (fin-report--bank-saved (format "%04d-%02d" year month)))
          (fin-health--bank-cash year month))))

(defun fin-health-cash-gaps ()
  "(YM LEFT CASH) of this month and the last whose cash check is off by more
than `fin-health-cash-slack'."
  (delq nil (mapcar (lambda (back)
                      (let* ((ym (substring (fin-health--month-start back) 0 7))
                             (c (fin-health-cash (string-to-number (substring ym 0 4))
                                                 (string-to-number (substring ym 5 7)))))
                        (when (> (abs (- (car c) (cadr c))) fin-health-cash-slack)
                          (cons ym c))))
                    '(0 1))))

(defun fin-health--years-since (iso today)
  "Years from ISO date to TODAY (ISO), as a float."
  (/ (- (time-to-days (date-to-time (concat today "T00:00:00")))
        (time-to-days (date-to-time (concat iso "T00:00:00"))))
     365.25))

(defun fin-health-account-gaps (today)
  "(NAME DATA SHEET) of rebuilt accounts off the sheet: by more than 1% when
their yield is known, else below the data or above it by more than the
yield `fin-health-yield-rate' allows since the anchor."
  (cl-remove-if-not
   (lambda (r)
     (pcase-let ((`(,_ ,data ,sheet ,note ,known ,_) r))
       (when (and data sheet)
         (let ((gap (- sheet data)) (slack (max 10000 (* 0.01 sheet))))
           (cond (known (> (abs gap) slack))
                 ((string-match "\\`since \\([0-9-]+\\)" note)
                  (or (< gap (- slack))
                      (> gap (+ slack (* sheet fin-health-yield-rate
                                         (fin-health--years-since (match-string 1 note) today)))))))))))
   (fin-report--accounts-derived)))

(defun fin-health--inbox-root ()
  (file-name-directory (directory-file-name fin-bank-inbox)))

(defun fin-health--inbox-files ()
  "Paths of every file under the inbox root, relative to it."
  (let ((root (fin-health--inbox-root)))
    (and (file-directory-p root)
         (mapcar (lambda (f) (file-relative-name f root))
                 (directory-files-recursively root "")))))

(defun fin-health-missing-docs ()
  "Labels of `fin-health-expected-docs' no inbox file matches."
  (let ((files (fin-health--inbox-files)))
    (delq nil (mapcar (lambda (d)
                        (cl-assert (and (stringp (car d)) (stringp (cdr d))) nil
                                   "fin-health: bad expected doc %S" d)
                        (unless (cl-some (lambda (f) (string-match-p (cdr d) f)) files)
                          (car d)))
                      fin-health-expected-docs))))

(defun fin-health-key-files ()
  "Inbox files that carry private keys."
  (cl-remove-if-not (lambda (f) (string-match-p fin-health-key-file-regexp f))
                    (fin-health--inbox-files)))

(provide 'health)
;;; health.el ends here
