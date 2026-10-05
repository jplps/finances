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
;;   ignored bank rows, savings moves and PJ tax payments left out; ledger
;;   savings, tax and company rows left out.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'bank)
(require 'bankfix)
(require 'pj)

(defconst fin-health-accounts '("account" "card" "pj" "pj-card")
  "Account kinds whose statements the inbox check follows.")

(defcustom fin-health-gap-share 0.10
  "Share of a month's bank spending past which the ledger gap is flagged."
  :type 'number :group 'fin)

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

(defun fin-health--bank-spent-p (b)
  "Non-nil if bank row B is spending as the ledger books it."
  (and (equal (nth 2 b) "out")
       (not (fin-bank--ignored-p b))
       (not (fin-conv--savings-p b))
       (not (fin-pj-tax-item b))))

(defun fin-health-months (n)
  "(YYYY-MM BANK LEDGER) for the N complete months before this one."
  (cl-assert (and (integerp n) (> n 0)))
  (let* ((from (fin-health--month-start n))
         (to (fin-health--month-start 0))
         (bank (make-hash-table :test #'equal))
         (ledger (fin-bank-fix--ledger-out from)))
    (dolist (b (fin-bankdb-since from))
      (when (and (string< (nth 1 b) to) (fin-health--bank-spent-p b))
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

(provide 'health)
;;; health.el ends here
