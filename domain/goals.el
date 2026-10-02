;;; goals.el --- Plan targets vs actual entries  -*- lexical-binding: t; -*-

;; Each plan row (and each child of `investments') gets its monthly target
;; from budget.el and the current month so far from entries.  Spending rows
;; go `over' past the target, else stay `within'; saving rows (contributions)
;; are `reached' at the target, else `pending'.
;;
;; The `patrimony' child splits into the register's classes, each against
;; its monthly amortization: `car' entries are class `car'; `patrimony'
;; entries take the class `fin-goals-patrimony-classes' maps their item to,
;; and an `unmapped' row (target 0) reports the rest.

(require 'cl-lib)
(require 'calendar)
(require 'db)
(require 'budget)
(require 'patrimony)

(defcustom fin-goals-patrimony-classes nil
  "Patrimony entry item -> register class: list of (REGEXP . CLASS).
First REGEXP matching the downcased item wins; set in infra/config.el."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defconst fin-goals--measure
  '(("investments" save ("investments") ("reserve") ("family") ("vacations") ("patrimony") ("car"))
    ("retirement"  save ("retirement"))
    ("emergency"   save ("investments" . "emergency") ("reserve"))
    ("vacations"   spend ("vacations"))
    ("family"      spend ("family"))
    ("patrimony"   spend ("patrimony") ("car")))
  "Plan row -> (DIRECTION SOURCE...).  A SOURCE is (CATEGORY) or
\(CATEGORY . ITEM); its net is outflows minus inflows.  Rows not listed are
spending, measured by the entry category of the same name.  `investments'
funds all its children, so its actual is their sum.  Car and goods
spending is weighed against the patrimony amortization that funds it;
`reserve' rows are withdrawals from the emergency reserve.")

(defun fin-goals--spec (category)
  "(DIRECTION SOURCES) for plan CATEGORY."
  (let ((m (assoc category fin-goals--measure)))
    (if m (list (nth 1 m) (nthcdr 2 m)) (list 'spend (list (list category))))))

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
  (pcase-let ((`(,dir ,sources) (fin-goals--spec category)))
    (let ((mtd (apply #'fin-goals--net sources (fin-goals--month-range year month))))
      (list :category category :kind kind :parent parent :depth (if parent 1 0)
            :direction dir :target target :mtd mtd :status (fin-goals-status dir target mtd)))))

(defun fin-goals--patrimony-class (item)
  "Register class for patrimony entry ITEM, or nil when unmapped."
  (let ((it (downcase (or item ""))))
    (cdr (cl-find-if (lambda (m) (string-match-p (car m) it)) fin-goals-patrimony-classes))))

(defun fin-goals--patrimony-mtd (year month)
  "Alist CLASS -> net cents this month; nil CLASS collects unmapped items."
  (let* ((range (fin-goals--month-range year month))
         (out (list (cons "car" (apply #'fin-goals--net '(("car")) range)))))
    (dolist (r (fin-db-query
                "SELECT item, SUM(CASE WHEN type='out' THEN amount ELSE -amount END)
                   FROM entry WHERE category = 'patrimony' AND date BETWEEN ? AND ?
                  GROUP BY item"
                range))
      (let* ((cls (fin-goals--patrimony-class (car r)))
             (cell (assoc cls out)))
        (if cell (setcdr cell (+ (cdr cell) (cadr r)))
          (push (cons cls (cadr r)) out))))
    out))

(defun fin-goals--patrimony-rows (kind year month)
  "Rows per register class under `patrimony', then `unmapped' when nonzero."
  (let ((mtd (fin-goals--patrimony-mtd year month))
        (row (lambda (cat target spent)
               (list :category cat :kind kind :parent "patrimony" :depth 2
                     :direction 'spend :target target :mtd spent
                     :status (fin-goals-status 'spend target spent)))))
    (append
     (mapcar (lambda (s) (funcall row (nth 0 s) (or (nth 3 s) 0) (or (cdr (assoc (nth 0 s) mtd)) 0)))
             (fin-report--patrimony-summary))
     (let ((left (or (cdr (assoc nil mtd)) 0)))
       (unless (zerop left) (list (funcall row "unmapped" 0 left)))))))

(defun fin-goals (year month)
  "Goal rows for MONTH of YEAR: plan rows then their children.
Each is a plist (:category :kind :parent :depth :direction :target :mtd
:status); `patrimony' is followed by its classes at depth 2."
  (cl-assert (and (integerp year) (<= 1 month 12)))
  (let (out)
    (dolist (b (fin-report--budget-share year))
      (pcase-let ((`(,cat ,kind ,target ,_pct) b))
        (push (fin-goals--row cat kind nil target year month) out)
        (dolist (k (fin-report--budget-children cat year))
          (push (fin-goals--row (car k) kind cat (nth 1 k) year month) out)
          (when (equal (car k) "patrimony")
            (dolist (p (fin-goals--patrimony-rows kind year month)) (push p out))))))
    (nreverse out)))

(provide 'goals)
;;; goals.el ends here
