;;; stats.el --- Headline metrics + breakdowns  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'cashflow)

(defun fin-report--cat-shares ()
  "All-time per-category total out (spending; transfers excluded)."
  (fin-report--flow-query
   "SELECT category, SUM(amount)
      FROM flow
     WHERE type='out'
     GROUP BY category
     ORDER BY SUM(amount) DESC"))

(defun fin-report--income-shares ()
  "All-time per-category total in (income; withdrawals excluded)."
  (fin-report--flow-query
   "SELECT category, SUM(amount)
      FROM flow WHERE type='in'
      GROUP BY category ORDER BY SUM(amount) DESC"))

(defun fin-report--top-recurring (&optional limit min-months sort-by)
  "Items appearing in ≥MIN-MONTHS distinct months.
When SORT-BY is `value', sort by total spend; otherwise by months (default).
Returns (item months_active total_spend avg_per_month)."
  (let* ((order (if (eq sort-by 'value)
                    "SUM(amount)"
                  "COUNT(DISTINCT strftime('%%Y-%%m', date)), SUM(amount)")))
    (fin-db-query
     (format "SELECT item,
                     COUNT(DISTINCT strftime('%%Y-%%m', date)) AS m,
                     SUM(amount),
                     CAST(ROUND(SUM(amount) * 1.0 /
                                COUNT(DISTINCT strftime('%%Y-%%m', date))) AS INTEGER)
                FROM entry
               WHERE type='out' AND item IS NOT NULL
               GROUP BY item
               HAVING m >= %d
               ORDER BY %s DESC
               LIMIT %d"
             (or min-months 6) order (or limit 10)))))

(defun fin-report--top-by-count (&optional limit min-count)
  "Items appearing most often (by raw entry count).
Returns (item count total avg/entry)."
  (fin-db-query
   (format "SELECT item,
                   COUNT(*) AS n,
                   SUM(amount),
                   CAST(ROUND(SUM(amount) * 1.0 / COUNT(*)) AS INTEGER)
              FROM entry
             WHERE type='out' AND item IS NOT NULL
             GROUP BY item
             HAVING n >= %d
             ORDER BY n DESC, SUM(amount) DESC
             LIMIT %d"
           (or min-count 3) (or limit 10))))

(defun fin-report--yearly-saves ()
  "Per-year save %: (in − out) / in, transfers excluded."
  (fin-report--flow-query
   "SELECT CAST(strftime('%Y',date) AS INTEGER) AS y,
           ROUND(100.0 *
                 (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                 / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1)
      FROM flow WHERE date IS NOT NULL
      GROUP BY y ORDER BY y ASC"))

(defun fin-report--cumulative-networth ()
  "Per-month running sum of liquid (in − out), transfers excluded: what
was kept, in savings or on hand."
  (fin-report--flow-query
   "SELECT ym, SUM(net) OVER (ORDER BY ym) AS cum
      FROM (SELECT strftime('%Y-%m', date) AS ym,
                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS net
              FROM flow WHERE date IS NOT NULL
              GROUP BY ym)
     ORDER BY ym"))

(defun fin-report--calendar-flow ()
  "`fin-report--monthly-flow' up to the current month with every calendar
month in between, an empty month as zero in and out."
  (let* ((now  (format-time-string "%Y-%m"))
         (rows (cl-remove-if (lambda (r) (string> (car r) now)) (fin-report--monthly-flow)))
         (y    (and rows (string-to-number (substring (caar rows) 0 4))))
         (m    (and rows (string-to-number (substring (caar rows) 5 7))))
         (end  (caar (last rows)))
         out)
    (cl-loop repeat 1200
             for ym = (and rows (format "%04d-%02d" y m))
             while (and ym (not (string> ym end)))
             do (push (or (assoc ym rows) (list ym 0 0)) out)
                (if (= m 12) (setq y (1+ y) m 1) (setq m (1+ m))))
    (cl-assert (equal (caar out) end))
    (nreverse out)))

(defun fin-report--trailing-12 (months)
  "((YM . WINDOW) ...) for MONTHS (ym in out), WINDOW the month and up to
11 months before it."
  (let ((v (vconcat months)))
    (cl-loop for k below (length v)
             collect (cons (car (aref v k))
                           (append (cl-subseq v (max 0 (- k 11)) (1+ k)) nil)))))

(defun fin-report--column-sum (rows n)
  "Sum of column N over ROWS, nil as zero."
  (apply #'+ (mapcar (lambda (r) (or (nth n r) 0)) rows)))

(defun fin-report--rolling-save-rate ()
  "Trailing 12-calendar-month save % per month."
  (mapcar (lambda (w)
            (let ((i (fin-report--column-sum (cdr w) 1))
                  (o (fin-report--column-sum (cdr w) 2)))
              (list (car w) (and (> i 0) (/ (fround (/ (* 1000.0 (- i o)) i)) 10.0)))))
          (fin-report--trailing-12 (fin-report--calendar-flow))))

(defun fin-report--rolling-flow ()
  "Trailing 12-calendar-month moving average of in and out per month."
  (mapcar (lambda (w)
            (let ((n (float (length (cdr w)))))
              (list (car w)
                    (round (/ (fin-report--column-sum (cdr w) 1) n))
                    (round (/ (fin-report--column-sum (cdr w) 2) n)))))
          (fin-report--trailing-12 (fin-report--calendar-flow))))

(defun fin-report--pareto-spending (&optional limit)
  "Top items by spend with cumulative % of total out (spending, itemized or
not)."
  (fin-report--flow-query
   (format
    "SELECT item, v,
            ROUND(100.0 * SUM(v) OVER (ORDER BY v DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
                  / (SELECT SUM(amount) FROM flow WHERE type='out'), 1)
       FROM (SELECT item, SUM(amount) AS v
               FROM flow WHERE type='out' AND item IS NOT NULL
              GROUP BY item)
      ORDER BY v DESC
      LIMIT %d"
    (or limit 20))))

(defun fin-report--monthly-spend-grid ()
  "(year, month, spend) per month for heatmap."
  (fin-report--flow-query
   "SELECT CAST(strftime('%Y', date) AS INTEGER),
           CAST(strftime('%m', date) AS INTEGER),
           SUM(amount)
      FROM flow WHERE type='out' AND date IS NOT NULL
      GROUP BY 1, 2 ORDER BY 1, 2"))

(defun fin-report--period ()
  "Return (FIRST LAST MONTHS ENTRIES) up to the current month
\(excludes future placeholders)."
  (car (fin-db-query
        "SELECT MIN(date), MAX(date),
                COUNT(DISTINCT strftime('%Y-%m', date)),
                COUNT(*)
           FROM entry
          WHERE strftime('%Y-%m', date) <= strftime('%Y-%m', 'now', 'localtime')")))

(defun fin-report--records ()
  "Headline facts: biggest purchase/income, best/worst month, save rates, etc.
Flow figures exclude transfers (see `fin-report--flow-query'); best and
worst month exclude the current, partial one; the average yearly save is
weighted by income, i.e. Σ (in − out) / Σ in."
  (let* ((biggest    (car (fin-report--flow-query
                           "SELECT item, amount, date
                              FROM flow
                             WHERE type='out' AND item IS NOT NULL
                             ORDER BY amount DESC LIMIT 1")))
         (best-mo    (car (fin-report--flow-query
                           "SELECT strftime('%Y-%m', date) AS ym,
                                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS liq
                              FROM flow
                             WHERE date IS NOT NULL
                               AND strftime('%Y-%m', date) < strftime('%Y-%m', 'now', 'localtime')
                             GROUP BY ym
                             ORDER BY liq DESC LIMIT 1")))
         (worst-mo   (car (fin-report--flow-query
                           "SELECT strftime('%Y-%m', date) AS ym,
                                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS liq
                              FROM flow
                             WHERE date IS NOT NULL
                               AND strftime('%Y-%m', date) < strftime('%Y-%m', 'now', 'localtime')
                             GROUP BY ym
                             ORDER BY liq ASC LIMIT 1")))
         (avg-save   (caar (fin-report--flow-query
                            "SELECT ROUND(100.0 *
                                    (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                   - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                                    / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1)
                               FROM flow
                              WHERE date IS NOT NULL")))
         (months-tracked (caar (fin-db-query
                                "SELECT COUNT(DISTINCT strftime('%Y-%m', date)) FROM entry")))
         (biggest-in (car (fin-report--flow-query
                           "SELECT category, amount, date
                              FROM flow WHERE type='in'
                              ORDER BY amount DESC LIMIT 1")))
         (rec-share  (caar (fin-report--flow-query
                            "SELECT ROUND(100.0 *
                              (SELECT SUM(amount) FROM flow
                                WHERE type='out' AND item IS NOT NULL
                                  AND item IN (SELECT item FROM flow
                                                WHERE type='out' AND item IS NOT NULL
                                                GROUP BY item
                                                HAVING COUNT(DISTINCT strftime('%Y-%m',date)) >= 6))
                              /
                              (SELECT SUM(amount) FROM flow WHERE type='out'), 1)")))
         (best-save  (car (fin-report--flow-query
                           "SELECT CAST(strftime('%Y',date) AS INTEGER) AS y,
                                   ROUND(100.0 *
                                         (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                        - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                                         / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1) AS sv
                              FROM flow
                             WHERE date IS NOT NULL
                              GROUP BY y
                              ORDER BY sv DESC LIMIT 1")))
         (first-entry (caar (fin-db-query "SELECT MIN(date) FROM entry"))))
    (list :biggest biggest :best-mo best-mo :worst-mo worst-mo
          :avg-save avg-save :months-tracked months-tracked
          :biggest-in biggest-in :rec-share rec-share
          :best-save best-save :first-entry first-entry)))

(provide 'stats)
;;; stats.el ends here
