;;; cashflow.el --- Cashflow aggregates  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)

(defcustom fin-report-transfer-categories
  '(("out" "investments" "retirement" "reserve")
    ("in"  "investments" "retirement" "reserve" "vacations"))
  "Entry categories that move money between own accounts, by entry type:
alist (TYPE CATEGORY...).  An `out' row listed here is a contribution to
savings, an `in' row a withdrawal from them.  Flow metrics count neither as
spending nor as income: see `fin-report--flow-query'."
  :type '(alist :key-type string :value-type (repeat string)) :group 'fin)

(defun fin-report--sql-string (s)
  "S as a single-quoted SQL literal."
  (cl-assert (stringp s))
  (concat "'" (string-replace "'" "''" s) "'"))

(defun fin-report--transfer-sql ()
  "SQL condition over an `entry' row: true for a transfer."
  (if (null fin-report-transfer-categories) "0"
    (mapconcat (lambda (e)
                 (cl-assert (and (member (car e) '("in" "out")) (cdr e)))
                 (format "(type=%s AND category IN (%s))" (fin-report--sql-string (car e))
                         (mapconcat #'fin-report--sql-string (cdr e) ",")))
               fin-report-transfer-categories " OR ")))

(defun fin-report--flow-query (sql &optional values)
  "Run SQL (with VALUES) after a `flow' CTE: `entry' with every transfer
\(see `fin-report-transfer-categories') retyped `save', its amount signed:
contributions positive, withdrawals negative.  So in flow, `in' is income,
`out' spending, Σ in − Σ out what was left to save, and Σ save what moved
into savings.  SQL is a SELECT, or `, NAME AS (...) SELECT' for more CTEs."
  (let ((tr (fin-report--transfer-sql)))
    (fin-db-query
     (format "WITH flow AS (
                SELECT id, date, category, item,
                       CASE WHEN %s THEN 'save' ELSE type END AS type,
                       CASE WHEN type='in' AND (%s) THEN -amount ELSE amount END AS amount
                  FROM entry) %s"
             tr tr sql)
     values)))

(defun fin-report--year-now ()
  (string-to-number (format-time-string "%Y")))

(defun fin-report--month-now ()
  (string-to-number (format-time-string "%m")))

(defun fin-report--years ()
  "Distinct years in entry, descending."
  (mapcar #'car
          (fin-db-query
           "SELECT DISTINCT CAST(strftime('%Y', date) AS INTEGER) AS y
              FROM entry WHERE date IS NOT NULL ORDER BY y DESC")))

(defun fin-report--year-months (year)
  "Per-month (m, in, out, liquid) for YEAR  12 rows.
In is income and out spending, transfers to and from savings excluded (see
`fin-report--flow-query'); liquid, in − out, is what the month left to save."
  (let* ((raw (fin-report--flow-query
               "SELECT CAST(strftime('%m', date) AS INTEGER) AS m,
                       SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
                       SUM(CASE WHEN type='out' THEN amount ELSE 0 END),
                       SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                     - SUM(CASE WHEN type='out' THEN amount ELSE 0 END)
                  FROM flow
                 WHERE strftime('%Y', date) = ?
                 GROUP BY m
                 ORDER BY m"
               (list (format "%d" year))))
         (by-m (mapcar (lambda (r) (cons (car r) r)) raw)))
    (cl-loop for m from 1 to 12
             collect (or (cdr (assoc m by-m))
                         (list m nil nil nil)))))

(defun fin-report--month-items (year month)
  "Spending rows for YEAR-MONTH, ordered by date.  Un-itemized lumps show a
NULL item and surface under their category in the drilldown."
  (fin-report--flow-query
   "SELECT COALESCE(item, category), category, amount, date
      FROM flow
     WHERE strftime('%Y-%m', date) = printf('%04d-%02d', ?, ?)
       AND type = 'out'
     ORDER BY date ASC, id ASC"
   (list year month)))

(defun fin-report--annual-sums ()
  "Per-year (year, in, out, liquid, savings %), transfers excluded."
  (fin-report--flow-query
   "SELECT CAST(strftime('%Y', date) AS INTEGER) AS y,
           SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
           SUM(CASE WHEN type='out' THEN amount ELSE 0 END),
           SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
         - SUM(CASE WHEN type='out' THEN amount ELSE 0 END),
           CASE WHEN SUM(CASE WHEN type='in' THEN amount ELSE 0 END) > 0
                THEN ROUND(100.0 * (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                  - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                            / SUM(CASE WHEN type='in' THEN amount ELSE 0 END), 1)
                ELSE NULL END
      FROM flow
     WHERE date IS NOT NULL
     GROUP BY y
     ORDER BY y ASC"))

(defun fin-report--monthly-flow ()
  "All-time per-month (ym, in, out), transfers excluded."
  (fin-report--flow-query
   "SELECT strftime('%Y-%m', date) AS ym,
           SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
           SUM(CASE WHEN type='out' THEN amount ELSE 0 END)
      FROM flow
     WHERE date IS NOT NULL
     GROUP BY ym
     ORDER BY ym"))

(provide 'cashflow)
;;; cashflow.el ends here
