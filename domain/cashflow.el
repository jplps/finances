;;; cashflow.el --- Cashflow aggregates  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)

(defcustom fin-cashflow-transfer-categories
  '(("out" "investments" "retirement" "reserve")
    ("in"  "investments" "retirement" "reserve"))
  "Alist (TYPE CATEGORY...) of transfers between own accounts.
An `out' row is a savings contribution, an `in' row a withdrawal; flow
metrics count neither as spending nor income."
  :type '(alist :key-type string :value-type (repeat string)) :group 'fin)

(defun fin-cashflow--sql-string (s)
  "S as a single-quoted SQL literal."
  (cl-assert (stringp s))
  (concat "'" (string-replace "'" "''" s) "'"))

(defun fin-cashflow--transfer-sql ()
  "SQL condition true for a transfer `entry' row."
  (if (null fin-cashflow-transfer-categories) "0"
    (mapconcat (lambda (e)
                 (cl-assert (and (member (car e) '("in" "out")) (cdr e)))
                 (format "(type=%s AND category IN (%s))" (fin-cashflow--sql-string (car e))
                         (mapconcat #'fin-cashflow--sql-string (cdr e) ",")))
               fin-cashflow-transfer-categories " OR ")))

(defun fin-cashflow--flow-query (sql &optional values)
  "Run SQL with VALUES after a `flow' CTE over `entry'.
Transfers become type `save', contributions positive and withdrawals
negative, so `in' is income and `out' spending.  SQL is a SELECT, or
`, NAME AS (...) SELECT' to add CTEs."
  (let ((tr (fin-cashflow--transfer-sql)))
    (fin-db-query
     (format "WITH flow AS (
                SELECT id, date, category, item,
                       CASE WHEN %s THEN 'save' ELSE type END AS type,
                       CASE WHEN type='in' AND (%s) THEN -amount ELSE amount END AS amount
                  FROM entry) %s"
             tr tr sql)
     values)))

(defun fin-cashflow--year-now ()
  (string-to-number (format-time-string "%Y")))

(defun fin-cashflow--month-now ()
  (string-to-number (format-time-string "%m")))

(defun fin-cashflow--year-months (year)
  "Twelve rows (M IN OUT LIQUID) for YEAR; empty months carry nils."
  (let* ((raw (fin-cashflow--flow-query
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

(defun fin-cashflow--month-items (year month)
  "Spending rows (ITEM CATEGORY AMOUNT DATE) of YEAR-MONTH by date.
ITEM falls back to the category."
  (fin-cashflow--flow-query
   "SELECT COALESCE(item, category), category, amount, date
      FROM flow
     WHERE strftime('%Y-%m', date) = printf('%04d-%02d', ?, ?)
       AND type = 'out'
     ORDER BY date ASC, id ASC"
   (list year month)))

(defun fin-cashflow--annual-sums ()
  "Rows (YEAR IN OUT LIQUID SAVE-%), nil SAVE-% without income."
  (fin-cashflow--flow-query
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

(defun fin-cashflow--monthly-flow ()
  "Rows (YYYY-MM IN OUT) for every month."
  (fin-cashflow--flow-query
   "SELECT strftime('%Y-%m', date) AS ym,
           SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
           SUM(CASE WHEN type='out' THEN amount ELSE 0 END)
      FROM flow
     WHERE date IS NOT NULL
     GROUP BY ym
     ORDER BY ym"))

(provide 'cashflow)
;;; cashflow.el ends here
