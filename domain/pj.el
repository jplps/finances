;;; pj.el --- Company (PJ) account: income and taxes  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'calendar)
(require 'reconcile)

(defcustom fin-pj-income-payers nil
  "PJ client payments -> ledger income category: list of (REGEXP . CATEGORY)."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defcustom fin-pj-tax-items nil
  "PJ tax and accounting payments -> `cnpj' item: list of (REGEXP . ITEM).
Matched case-sensitively."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defcustom fin-pj-window 31
  "Days a client payment may sit from the ledger income row it matches."
  :type 'integer :group 'fin)

(defun fin-pj--lookup (alist desc fold)
  "Value of the first (REGEXP . VALUE) of ALIST matching DESC.
FOLD non-nil ignores case."
  (let ((case-fold-search fold))
    (cdr (cl-find-if (lambda (a) (string-match-p (car a) desc)) alist))))

(defun fin-pj-income-category (b)
  "Income category of PJ bank row B, or nil.  Payer names ignore case."
  (and (equal (nth 2 b) "in") (fin-pj--lookup fin-pj-income-payers (nth 4 b) t)))

(defun fin-pj-tax-item (b)
  "`cnpj' item of PJ bank row B, or nil.  Case counts: `DAS' is the
tax, `das' a Portuguese word in shop names."
  (and (equal (nth 2 b) "out") (fin-pj--lookup fin-pj-tax-items (nth 4 b) nil)))

(defun fin-pj-cover (first last from)
  "(START . END) ISO dates the PJ statements spanning FIRST .. LAST cover on
or after FROM: START the first day of the first whole month, END the last
statement day.  Nil when empty."
  (cl-assert (not (string< last first)))
  (pcase-let* ((s (if (string< first from) from first))
               (`(,y ,m ,d) (mapcar #'string-to-number (split-string s "-")))
               (start (cond ((= d 1) s)
                            ((= m 12) (format "%04d-01-01" (1+ y)))
                            (t (format "%04d-%02d-01" y (1+ m))))))
    (unless (string< last start) (cons start last))))

(defun fin-pj-month-end (date)
  "Last day of ISO DATE's month, as an ISO date."
  (let ((y (string-to-number (substring date 0 4))) (m (string-to-number (substring date 5 7))))
    (format "%04d-%02d-%02d" y m (calendar-last-day-of-month m y))))

(defun fin-pj-within-p (date start end)
  "Non-nil if ISO DATE lies in START .. END, inclusive."
  (not (or (string< date start) (string< end date))))

(defun fin-pj--best (b ledger cat used)
  "Best unused LEDGER row in CAT for bank row B within `fin-pj-window'.
Ranks same month (payments are booked at month end), equal amount, on or
after B, then nearest date."
  (let ((day (fin-reconcile--day (nth 1 b))) (ym (substring (nth 1 b) 0 7)) best best-key)
    (dolist (l ledger)
      (when (and (equal (nth 3 l) cat) (not (memq l used)))
        (let* ((dd (- (fin-reconcile--day (nth 1 l)) day))
               (key (list (if (string-prefix-p ym (nth 1 l)) 0 1) (if (= (nth 5 l) (nth 3 b)) 0 1)
                          (if (< dd 0) 1 0) (abs dd))))
          (when (and (<= (abs dd) fin-pj-window) (or (null best-key) (fin-pj--key< key best-key)))
            (setq best l best-key key)))))
    best))

(defun fin-pj--key< (a b)
  "Non-nil if number list A sorts before B of the same length."
  (cl-assert (= (length a) (length b)))
  (let ((d (cl-find-if-not #'zerop (cl-mapcar #'- a b))))
    (and d (< d 0))))

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

(defun fin-pj-plan-taxes (rows ledger cover)
  "Actions for PJ tax payments ROWS against `cnpj' LEDGER rows, COVER being
the statements' (START . END).  Booked payments stay, others are added;
up to END, item-less estimates are deleted and unpaid booked rows reported."
  (cl-assert (consp cover))
  (let* ((items (delete-dups (mapcar #'cdr fin-pj-tax-items)))
         (start (car cover)) (end (cdr cover))
         (cands (cl-remove-if-not (lambda (l) (and (fin-pj-within-p (nth 1 l) start (fin-pj-month-end end))
                                                   (fin-pj--tax-row-p l items)))
                                  ledger))
         used out)
    (dolist (b rows)
      (when (fin-pj-within-p (nth 1 b) start end)
        (let* ((item (fin-pj-tax-item b))
               (l (cl-find-if (lambda (l) (and (not (memq l used)) (equal (nth 1 l) (nth 1 b))
                                               (= (nth 5 l) (nth 3 b)) (equal (nth 4 l) item)))
                              cands)))
          (cl-assert item nil "fin-pj: not a tax payment: %S" b)
          (if l (push l used)
            (push (list :add (list (nth 1 b) "out" "cnpj" item (nth 3 b) nil nil) (nth 4 b)) out)))))
    (dolist (l cands)
      (unless (or (memq l used) (string< end (nth 1 l)))
        (push (if (nth 4 l)
                  (list :report l "tax row with no PJ payment")
                (list :delete l "cnpj estimate replaced by the PJ payments"))
              out)))
    (nreverse out)))

(provide 'pj)
;;; pj.el ends here
