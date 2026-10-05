;;; health.el --- Ledger health: how far the ODS trails the bank  -*- lexical-binding: t; -*-

;; Cheap checks for the cockpit, from what the import already stored: no
;; reconciliation runs here (`fin-bank-fix' does that).
;;
;; - Statements: the latest balance date per account kind, i.e. how fresh
;;   the inbox is.
;; - Months: bank spending vs ledger spending per month, counted like
;;   `fin-bank-fix' counts a month's room: bank rows it ignores, savings
;;   moves and PJ tax payments left out; ledger savings, tax and company
;;   rows (`fin-bank-fix-unspent-categories') left out.
;; - Open items: `other' remainders still standing for unitemized spending.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'bank)
(require 'bankfix)
(require 'pj)

(defconst fin-health-accounts '("account" "card" "pj" "pj-card")
  "Account kinds whose statement freshness the cockpit shows.")

(defun fin-health-statements ()
  "(ACCOUNT DATE) of the latest statement balance per account kind; DATE nil
when none was imported."
  (mapcar (lambda (a) (list a (car (fin-bankdb-latest-balance a)))) fin-health-accounts))

(defun fin-health--month-start (months-back)
  "ISO first day of the month MONTHS-BACK before the current one."
  (cl-assert (natnump months-back))
  (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time)))
    (let ((k (- (+ (* y 12) (1- m)) months-back)))
      (format "%04d-%02d-01" (/ k 12) (1+ (% k 12))))))

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

(defun fin-health-remainders ()
  "(COUNT . CENTS) of `other' remainder rows left in the ledger."
  (let ((r (car (fin-db-query "SELECT COUNT(*), COALESCE(SUM(amount), 0) FROM entry
                                WHERE item = 'other' AND type = 'out'"))))
    (cons (car r) (cadr r))))

(provide 'health)
;;; health.el ends here
