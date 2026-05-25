;;; stats.el --- Headline metrics + breakdowns  -*- lexical-binding: t; -*-

(require 'db)

(defun fin-report--cat-shares ()
  "All-time per-category total out (summary rows)."
  (fin-db-query
   "SELECT category, SUM(amount)
      FROM entry
     WHERE type='out' AND item IS NULL
     GROUP BY category
     ORDER BY SUM(amount) DESC"))

(defun fin-report--income-shares ()
  "All-time per-category total (type='in', summary rows)."
  (fin-db-query
   "SELECT category, SUM(amount)
      FROM entry WHERE type='in' AND item IS NULL
      GROUP BY category ORDER BY SUM(amount) DESC"))

(defun fin-report--top-recurring (&optional limit min-months sort-by)
  "Items appearing in ≥MIN-MONTHS distinct months.
SORT-BY: 'value sorts by total spend; otherwise by months (default).
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
  "Per-year save % (summary rows)."
  (fin-db-query
   "SELECT CAST(strftime('%Y',date) AS INTEGER) AS y,
           ROUND(100.0 *
                 (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                 / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1)
      FROM entry WHERE item IS NULL AND date IS NOT NULL
      GROUP BY y ORDER BY y ASC"))

(defun fin-report--cumulative-networth ()
  "Per-month running sum of liquid (in − out)."
  (fin-db-query
   "SELECT ym, SUM(net) OVER (ORDER BY ym) AS cum
      FROM (SELECT strftime('%Y-%m', date) AS ym,
                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS net
              FROM entry WHERE item IS NULL AND date IS NOT NULL
              GROUP BY ym)
     ORDER BY ym"))

(defun fin-report--rolling-save-rate ()
  "Trailing 12-month save % per month."
  (fin-db-query
   "WITH m AS (
      SELECT strftime('%Y-%m', date) AS ym,
             SUM(CASE WHEN type='in'  THEN amount ELSE 0 END) AS i,
             SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS o
        FROM entry WHERE item IS NULL AND date IS NOT NULL
        GROUP BY ym)
    SELECT ym,
           CASE WHEN SUM(i) OVER w > 0
                THEN ROUND(100.0*(SUM(i) OVER w - SUM(o) OVER w)/SUM(i) OVER w, 1)
                ELSE NULL END
      FROM m
      WINDOW w AS (ORDER BY ym ROWS BETWEEN 11 PRECEDING AND CURRENT ROW)
     ORDER BY ym"))

(defun fin-report--rolling-flow ()
  "Trailing 12-month moving average of in and out per month."
  (fin-db-query
   "WITH m AS (
      SELECT strftime('%Y-%m', date) AS ym,
             SUM(CASE WHEN type='in'  THEN amount ELSE 0 END) AS i,
             SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS o
        FROM entry WHERE item IS NULL AND date IS NOT NULL
        GROUP BY ym)
    SELECT ym,
           CAST(ROUND(AVG(i) OVER w) AS INTEGER),
           CAST(ROUND(AVG(o) OVER w) AS INTEGER)
      FROM m
      WINDOW w AS (ORDER BY ym ROWS BETWEEN 11 PRECEDING AND CURRENT ROW)
     ORDER BY ym"))

(defun fin-report--pareto-spending (&optional limit)
  "Top items by spend with cumulative % of total out."
  (fin-db-query
   (format
    "WITH per_item AS (
       SELECT item, SUM(amount) AS v
         FROM entry WHERE type='out' AND item IS NOT NULL
         GROUP BY item),
     ranked AS (
       SELECT item, v,
              SUM(v) OVER (ORDER BY v DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cum,
              SUM(v) OVER () AS total
         FROM per_item)
     SELECT item, v, ROUND(100.0 * cum / total, 1)
       FROM ranked
       ORDER BY v DESC
       LIMIT %d"
    (or limit 20))))

(defun fin-report--monthly-spend-grid ()
  "(year, month, spend) per month for heatmap."
  (fin-db-query
   "SELECT CAST(strftime('%Y', date) AS INTEGER),
           CAST(strftime('%m', date) AS INTEGER),
           SUM(amount)
      FROM entry WHERE type='out' AND item IS NULL AND date IS NOT NULL
      GROUP BY 1, 2 ORDER BY 1, 2"))

(defun fin-report--period ()
  "Return (FIRST LAST MONTHS ENTRIES) up to the current month (excludes future placeholders)."
  (car (fin-db-query
        "SELECT MIN(date), MAX(date),
                COUNT(DISTINCT strftime('%Y-%m', date)),
                COUNT(*)
           FROM entry
          WHERE strftime('%Y-%m', date) <= strftime('%Y-%m', 'now')")))

(defun fin-report--records ()
  "Headline facts: biggest purchase/income, best/worst month, save rates, etc."
  (let* ((biggest    (car (fin-db-query
                           "SELECT item, amount, date
                              FROM entry
                             WHERE type='out' AND item IS NOT NULL
                             ORDER BY amount DESC LIMIT 1")))
         (best-mo    (car (fin-db-query
                           "SELECT strftime('%Y-%m', date) AS ym,
                                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS liq
                              FROM entry
                             WHERE item IS NULL
                             GROUP BY ym
                             ORDER BY liq DESC LIMIT 1")))
         (worst-mo   (car (fin-db-query
                           "SELECT strftime('%Y-%m', date) AS ym,
                                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS liq
                              FROM entry
                             WHERE item IS NULL
                             GROUP BY ym
                             ORDER BY liq ASC LIMIT 1")))
         (avg-save   (caar (fin-db-query
                            "SELECT ROUND(AVG(rate),1) FROM (
                               SELECT 100.0 *
                                 (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                                 / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0) AS rate
                                 FROM entry WHERE item IS NULL
                                 GROUP BY strftime('%Y', date))")))
         (months-tracked (caar (fin-db-query
                                "SELECT COUNT(DISTINCT strftime('%Y-%m', date)) FROM entry")))
         (biggest-in (car (fin-db-query
                           "SELECT category, amount, date
                              FROM entry WHERE type='in' AND item IS NULL
                              ORDER BY amount DESC LIMIT 1")))
         (rec-share  (caar (fin-db-query
                            "SELECT ROUND(100.0 *
                              (SELECT SUM(amount) FROM entry
                                WHERE type='out' AND item IS NOT NULL
                                  AND item IN (SELECT item FROM entry
                                                WHERE type='out' AND item IS NOT NULL
                                                GROUP BY item
                                                HAVING COUNT(DISTINCT strftime('%Y-%m',date)) >= 6))
                              /
                              (SELECT SUM(amount) FROM entry WHERE type='out'), 1)")))
         (best-save  (car (fin-db-query
                           "SELECT CAST(strftime('%Y',date) AS INTEGER) AS y,
                                   ROUND(100.0 *
                                         (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                        - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                                         / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1) AS sv
                              FROM entry WHERE item IS NULL
                              GROUP BY y
                              ORDER BY sv DESC LIMIT 1")))
         (first-entry (caar (fin-db-query "SELECT MIN(date) FROM entry"))))
    (list :biggest biggest :best-mo best-mo :worst-mo worst-mo
          :avg-save avg-save :months-tracked months-tracked
          :biggest-in biggest-in :rec-share rec-share
          :best-save best-save :first-entry first-entry)))

(provide 'stats)
;;; stats.el ends here
