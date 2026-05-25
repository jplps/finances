;;; cashflow.el --- Cashflow aggregates  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)

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
  "Per-month (m, in, out, liquid) for YEAR — summary rows only.  12 rows."
  (let* ((raw (fin-db-query
               "SELECT CAST(strftime('%m', date) AS INTEGER) AS m,
                       SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
                       SUM(CASE WHEN type='out' THEN amount ELSE 0 END),
                       SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                     - SUM(CASE WHEN type='out' THEN amount ELSE 0 END)
                  FROM entry
                 WHERE strftime('%Y', date) = ?
                   AND item IS NULL
                 GROUP BY m
                 ORDER BY m"
               (list (format "%d" year))))
         (by-m (mapcar (lambda (r) (cons (car r) r)) raw)))
    (cl-loop for m from 1 to 12
             collect (or (cdr (assoc m by-m))
                         (list m nil nil nil)))))

(defun fin-report--month-items (year month)
  "Itemized rows for YEAR-MONTH (item IS NOT NULL), ordered by date."
  (fin-db-query
   "SELECT item, category, amount
      FROM entry
     WHERE strftime('%Y-%m', date) = printf('%04d-%02d', ?, ?)
       AND item IS NOT NULL
     ORDER BY date ASC, id ASC"
   (list year month)))

(defun fin-report--annual-sums ()
  "Per-year (year, in, out, liquid, savings %) using summary rows."
  (fin-db-query
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
      FROM entry
     WHERE item IS NULL
     GROUP BY y
     ORDER BY y ASC"))

(defun fin-report--monthly-flow ()
  "All-time per-month (ym, in, out) using summary rows."
  (fin-db-query
   "SELECT strftime('%Y-%m', date) AS ym,
           SUM(CASE WHEN type='in'  THEN amount ELSE 0 END),
           SUM(CASE WHEN type='out' THEN amount ELSE 0 END)
      FROM entry
     WHERE item IS NULL AND date IS NOT NULL
     GROUP BY ym
     ORDER BY ym"))

(provide 'cashflow)
;;; cashflow.el ends here
