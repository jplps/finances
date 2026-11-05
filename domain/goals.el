;;; goals.el --- Plan targets vs actual entries  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'calendar)
(require 'db)
(require 'budget)
(require 'patrimony)

(defconst fin-goals--measure
  '(("investments" save ("investments") ("reserve"))
    ("retirement"  save ("retirement"))
    ("patrimony"   save))
  "Plan row -> (DIRECTION SOURCE...), SOURCE (CATEGORY) or (CATEGORY . ITEM).
A source nets outflows minus inflows.  An unlisted child of a listed saving
row saves into PARENT / CHILD; other unlisted rows are spending in the
same-named category.  `patrimony' also takes each register class's
caixinha; `reserve' rows are emergency reserve withdrawals.")

(defun fin-goals--classes ()
  "Patrimony register classes."
  (mapcar #'car (fin-patrimony--summary)))

(defun fin-goals--spec (category &optional parent)
  "(DIRECTION SOURCES) for plan CATEGORY under PARENT."
  (let ((m (assoc category fin-goals--measure)))
    (cond ((and (null m) (eq 'save (nth 1 (assoc parent fin-goals--measure))))
           (list 'save (list (cons parent category))))
          ((null m) (list 'spend (list (list category))))
          ((equal category "patrimony")
           (list (nth 1 m) (append (nthcdr 2 m)
                                   (mapcar (lambda (c) (cons "investments" c)) (fin-goals--classes)))))
          (t (list (nth 1 m) (nthcdr 2 m))))))

(defun fin-goals--net (sources from to)
  "Cents out minus in for SOURCES between ISO dates FROM and TO, inclusive."
  (cl-assert (and (stringp from) (stringp to) (not (string< to from))))
  (apply #'+
         (mapcar (lambda (src)
                   (or (caar (fin-db-query
                              (concat "SELECT SUM(CASE WHEN type='out' THEN amount ELSE -amount END)
                                         FROM entry WHERE category = ? AND date BETWEEN ? AND ?"
                                      (if (cdr src) " AND item = ?" ""))
                              (append (list (car src) from to) (and (cdr src) (list (cdr src))))))
                       0))
                 sources)))

(defun fin-goals--month-range (y m)
  "(FIRST LAST) ISO dates of month M in year Y."
  (list (format "%04d-%02d-01" y m)
        (format "%04d-%02d-%02d" y m (calendar-last-day-of-month m y))))

(defun fin-goals-status (direction target mtd)
  "`over' or `within' for spending, `reached' or `pending' for saving."
  (cl-assert (memq direction '(spend save)))
  (if (eq direction 'save)
      (if (>= mtd target) 'reached 'pending)
    (if (> mtd target) 'over 'within)))

(defun fin-goals--row (category kind parent target year month)
  (pcase-let ((`(,dir ,sources) (fin-goals--spec category parent)))
    (let ((mtd (apply #'fin-goals--net sources (fin-goals--month-range year month))))
      (list :category category :kind kind :parent parent :depth (if parent 1 0)
            :direction dir :target target :mtd mtd :status (fin-goals-status dir target mtd)))))

(defun fin-goals--patrimony-rows (kind year month)
  "Rows per register class: its caixinha this month vs its amortization."
  (let ((range (fin-goals--month-range year month)))
    (mapcar (lambda (s)
              (let ((saved (apply #'fin-goals--net (list (cons "investments" (nth 0 s))) range))
                    (target (or (nth 3 s) 0)))
                (list :category (nth 0 s) :kind kind :parent "patrimony" :depth 2
                      :direction 'save :target target :mtd saved
                      :status (fin-goals-status 'save target saved))))
            (fin-patrimony--summary))))

(defun fin-goals (year month)
  "Goal plists for MONTH of YEAR, each plan row followed by its children.
Keys :category :kind :parent :depth :direction :target :mtd :status;
`patrimony' is followed by its classes at depth 2."
  (cl-assert (and (integerp year) (<= 1 month 12)))
  (let (out)
    (dolist (b (fin-budget--share year))
      (pcase-let ((`(,cat ,kind ,target ,_pct) b))
        (push (fin-goals--row cat kind nil target year month) out)
        (dolist (k (fin-budget--children cat year))
          (push (fin-goals--row (car k) kind cat (nth 1 k) year month) out)
          (when (equal (car k) "patrimony")
            (dolist (p (fin-goals--patrimony-rows kind year month)) (push p out))))))
    (nreverse out)))

(provide 'goals)
;;; goals.el ends here
