;;; pj.el --- Company (PJ) account: income and taxes  -*- lexical-binding: t; -*-

;; Pure planning over row lists, like conventions.el.  Within the months a
;; PJ statement covers fully, the company account is the truth for two
;; kinds of ledger rows:
;;
;; - Income: client payments (`fin-pj-income-payers' names each payer's
;;   income category) match the ledger's item-less income rows of that
;;   category (an income row with an item, a bonus, has another source), the
;;   nearest date within `fin-pj-window' days, an equal amount first.  A
;;   different amount is edited to the bank's; an unmatched payment is
;;   added; a ledger income row the PJ never received is reported, never
;;   deleted (a bonus may come from elsewhere).
;; - Taxes: payments named by `fin-pj-tax-items' are `cnpj / ITEM' rows.
;;   The ledger's item-less `cnpj' rows were monthly estimates of them, so
;;   in covered months they give way: deleted, the real payments added.
;;
;; Bank rows: (id date type amount desc).  Ledger rows: (id date type
;; category item amount k/n).

(require 'cl-lib)
(require 'calendar)
(require 'reconcile)

(defcustom fin-pj-income-payers nil
  "PJ client payments -> ledger income category: list of (REGEXP . CATEGORY)."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defcustom fin-pj-tax-items nil
  "PJ tax and accounting payments -> `cnpj' item: list of (REGEXP . ITEM)."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defcustom fin-pj-window 31
  "Days a client payment may sit from the ledger income row it matches."
  :type 'integer :group 'fin)

(defun fin-pj--lookup (alist desc)
  "Value of the first (REGEXP . VALUE) in ALIST matching DESC, ignoring case."
  (let ((case-fold-search t))
    (cdr (cl-find-if (lambda (a) (string-match-p (car a) desc)) alist))))

(defun fin-pj-income-category (b)
  "Income category of PJ bank row B, or nil."
  (and (equal (nth 2 b) "in") (fin-pj--lookup fin-pj-income-payers (nth 4 b))))

(defun fin-pj-tax-item (b)
  "`cnpj' item of PJ bank row B, or nil."
  (and (equal (nth 2 b) "out") (fin-pj--lookup fin-pj-tax-items (nth 4 b))))

(defun fin-pj-months (first last)
  "YYYY-MM months fully inside ISO dates FIRST .. LAST."
  (cl-assert (not (string< last first)))
  (pcase-let* ((`(,y ,m ,d) (mapcar #'string-to-number (split-string first "-")))
               (`(,y ,m) (if (= d 1) (list y m) (if (= m 12) (list (1+ y) 1) (list y (1+ m)))))
               (`(,ly ,lm ,ld) (mapcar #'string-to-number (split-string last "-")))
               (`(,ly ,lm) (if (= ld (calendar-last-day-of-month lm ly)) (list ly lm)
                             (if (= lm 1) (list (1- ly) 12) (list ly (1- lm)))))
               (out nil))
    (while (or (< y ly) (and (= y ly) (<= m lm)))
      (push (format "%04d-%02d" y m) out)
      (if (= m 12) (setq y (1+ y) m 1) (setq m (1+ m))))
    (nreverse out)))

(defun fin-pj--best (b ledger cat used)
  "Unused ledger row of LEDGER in CAT for bank row B: equal amount first,
then nearest date, within `fin-pj-window'."
  (let ((day (fin-reconcile--day (nth 1 b))) best best-key)
    (dolist (l ledger)
      (when (and (equal (nth 3 l) cat) (not (memq l used)))
        (let ((d (abs (- day (fin-reconcile--day (nth 1 l))))))
          (when (<= d fin-pj-window)
            (let ((key (list (if (= (nth 5 l) (nth 3 b)) 0 1) d)))
              (when (or (null best-key) (or (< (car key) (car best-key))
                                            (and (= (car key) (car best-key)) (< (cadr key) (cadr best-key)))))
                (setq best l best-key key)))))))
    best))

(defun fin-pj-plan-income (rows ledger)
  "Actions fixing ledger income rows LEDGER from PJ client payments ROWS."
  (let (used out)
    (dolist (b (sort (copy-sequence rows) (lambda (x y) (string< (nth 1 x) (nth 1 y)))))
      (let* ((cat (fin-pj-income-category b))
             (l (fin-pj--best b ledger cat used)))
        (cl-assert cat nil "fin-pj: not a client payment: %S" b)
        (cond ((null l) (push (list :add (list (nth 1 b) "in" cat nil (nth 3 b) nil nil) (nth 4 b)) out))
              (t (push l used)
                 (unless (= (nth 5 l) (nth 3 b))
                   (push (list :edit l (nth 3 b) (concat "pj: " (nth 4 b))) out))))))
    (dolist (l ledger)
      (unless (memq l used)
        (push (list :report l "income the PJ account never received") out)))
    (nreverse out)))

(defun fin-pj--tax-row-p (l items)
  "Non-nil if ledger row L is a `cnpj' estimate (no item) or one of ITEMS."
  (and (equal (nth 3 l) "cnpj") (or (null (nth 4 l)) (member (nth 4 l) items))))

(defun fin-pj-plan-taxes (rows ledger months)
  "Actions for PJ tax payments ROWS against `cnpj' rows of LEDGER in MONTHS.
A payment already booked (same date, amount and item) stays; others are
added; item-less estimates are deleted; booked tax rows with no payment
are reported."
  (let* ((items (delete-dups (mapcar #'cdr fin-pj-tax-items)))
         (in-months (lambda (date) (member (substring date 0 7) months)))
         (cands (cl-remove-if-not (lambda (l) (and (funcall in-months (nth 1 l)) (fin-pj--tax-row-p l items)))
                                  ledger))
         used out)
    (dolist (b rows)
      (when (funcall in-months (nth 1 b))
        (let* ((item (fin-pj-tax-item b))
               (l (cl-find-if (lambda (l) (and (not (memq l used)) (equal (nth 1 l) (nth 1 b))
                                               (= (nth 5 l) (nth 3 b)) (equal (nth 4 l) item)))
                              cands)))
          (cl-assert item nil "fin-pj: not a tax payment: %S" b)
          (if l (push l used)
            (push (list :add (list (nth 1 b) "out" "cnpj" item (nth 3 b) nil nil) (nth 4 b)) out)))))
    (dolist (l cands)
      (unless (memq l used)
        (push (if (nth 4 l)
                  (list :report l "tax row with no PJ payment")
                (list :delete l "cnpj estimate replaced by the PJ payments"))
              out)))
    (nreverse out)))

(provide 'pj)
;;; pj.el ends here
