;;; stats.el --- Headline metrics + breakdowns  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'cashflow)

(defconst fin-report--to-date "date <= date('now', 'localtime')"
  "SQL condition: the row is dated today or earlier.")

(defun fin-report--cat-shares ()
  "Rows (CATEGORY TOTAL) of spending to date."
  (fin-report--flow-query
   (format "SELECT category, SUM(amount)
      FROM flow
     WHERE type='out' AND %s
     GROUP BY category
     ORDER BY SUM(amount) DESC" fin-report--to-date)))

(defun fin-report--income-shares ()
  "Rows (CATEGORY TOTAL) of income to date."
  (fin-report--flow-query
   (format "SELECT category, SUM(amount)
      FROM flow WHERE type='in' AND %s
      GROUP BY category ORDER BY SUM(amount) DESC" fin-report--to-date)))

(defun fin-report--top-recurring (&optional limit min-months sort-by)
  "Rows (ITEM MONTHS TOTAL AVG-PER-MONTH) of items in ≥MIN-MONTHS months.
SORT-BY `value' sorts by total, else by months."
  (let* ((order (if (eq sort-by 'value)
                    "SUM(amount)"
                  "COUNT(DISTINCT strftime('%%Y-%%m', date)), SUM(amount)")))
    (fin-report--flow-query
     (format "SELECT item,
                     COUNT(DISTINCT strftime('%%Y-%%m', date)) AS m,
                     SUM(amount),
                     CAST(ROUND(SUM(amount) * 1.0 /
                                COUNT(DISTINCT strftime('%%Y-%%m', date))) AS INTEGER)
                FROM flow
               WHERE type='out' AND item IS NOT NULL AND %s
               GROUP BY item
               HAVING m >= %d
               ORDER BY %s DESC
               LIMIT %d"
             fin-report--to-date (or min-months 6) order (or limit 10)))))

(defun fin-report--top-by-count (&optional limit min-count)
  "Rows (ITEM COUNT TOTAL AVG-PER-ENTRY) of the most frequent spending items."
  (fin-report--flow-query
   (format "SELECT item,
                   COUNT(*) AS n,
                   SUM(amount),
                   CAST(ROUND(SUM(amount) * 1.0 / COUNT(*)) AS INTEGER)
              FROM flow
             WHERE type='out' AND item IS NOT NULL AND %s
             GROUP BY item
             HAVING n >= %d
             ORDER BY n DESC, SUM(amount) DESC
             LIMIT %d"
           fin-report--to-date (or min-count 3) (or limit 10))))

(defun fin-report--cumulative-networth ()
  "Rows (YYYY-MM CUMULATIVE-LIQUID) to date."
  (fin-report--flow-query
   (format "SELECT ym, SUM(net) OVER (ORDER BY ym) AS cum
      FROM (SELECT strftime('%%Y-%%m', date) AS ym,
                   SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                 - SUM(CASE WHEN type='out' THEN amount ELSE 0 END) AS net
              FROM flow WHERE %s
              GROUP BY ym)
     ORDER BY ym" fin-report--to-date)))

(defun fin-report--calendar-flow ()
  "`fin-report--monthly-flow' to the current month, gaps filled with zeros."
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
  "((YM . WINDOW) ...) for MONTHS, WINDOW the month and up to 11 before."
  (let ((v (vconcat months)))
    (cl-loop for k below (length v)
             collect (cons (car (aref v k))
                           (append (cl-subseq v (max 0 (- k 11)) (1+ k)) nil)))))

(defun fin-report--column-sum (rows n)
  "Sum of column N over ROWS, nil as zero."
  (apply #'+ (mapcar (lambda (r) (or (nth n r) 0)) rows)))

(defun fin-report--rolling-save-rate ()
  "Rows (YYYY-MM SAVE-%) over trailing 12 months; nil SAVE-% without income."
  (mapcar (lambda (w)
            (let ((i (fin-report--column-sum (cdr w) 1))
                  (o (fin-report--column-sum (cdr w) 2)))
              (list (car w) (and (> i 0) (/ (fround (/ (* 1000.0 (- i o)) i)) 10.0)))))
          (fin-report--trailing-12 (fin-report--calendar-flow))))

(defun fin-report--rolling-flow ()
  "Rows (YYYY-MM IN OUT) of trailing 12-month averages."
  (mapcar (lambda (w)
            (let ((n (float (length (cdr w)))))
              (list (car w)
                    (round (/ (fin-report--column-sum (cdr w) 1) n))
                    (round (/ (fin-report--column-sum (cdr w) 2) n)))))
          (fin-report--trailing-12 (fin-report--calendar-flow))))

(defun fin-report--pareto-spending (&optional limit)
  "Rows (ITEM TOTAL CUMULATIVE-%) of top spending items to date.
The percentage is of all spending, itemized or not."
  (fin-report--flow-query
   (format
    "SELECT item, v,
            ROUND(100.0 * SUM(v) OVER (ORDER BY v DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
                  / (SELECT SUM(amount) FROM flow WHERE type='out' AND %s), 1)
       FROM (SELECT item, SUM(amount) AS v
               FROM flow WHERE type='out' AND item IS NOT NULL AND %s
              GROUP BY item)
      ORDER BY v DESC
      LIMIT %d"
    fin-report--to-date fin-report--to-date (or limit 20))))

(defun fin-report--monthly-spend-grid ()
  "Rows (YEAR MONTH SPEND) to date."
  (fin-report--flow-query
   (format "SELECT CAST(strftime('%%Y', date) AS INTEGER),
           CAST(strftime('%%m', date) AS INTEGER),
           SUM(amount)
      FROM flow WHERE type='out' AND %s
      GROUP BY 1, 2 ORDER BY 1, 2" fin-report--to-date)))

(defun fin-report--period ()
  "Return (FIRST LAST MONTHS ENTRIES) up to the current month."
  (car (fin-db-query
        "SELECT MIN(date), MAX(date),
                COUNT(DISTINCT strftime('%Y-%m', date)),
                COUNT(*)
           FROM entry
          WHERE strftime('%Y-%m', date) <= strftime('%Y-%m', 'now', 'localtime')")))

(defun fin-report--records ()
  "Plist of headline facts, transfers excluded.
Best and worst month skip the current one; :avg-save is income-weighted."
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
                            (concat "SELECT ROUND(100.0 *
                                    (SUM(CASE WHEN type='in'  THEN amount ELSE 0 END)
                                   - SUM(CASE WHEN type='out' THEN amount ELSE 0 END))
                                    / NULLIF(SUM(CASE WHEN type='in' THEN amount ELSE 0 END),0), 1)
                               FROM flow
                              WHERE " fin-report--to-date))))
         (months-tracked (caar (fin-db-query
                                (concat "SELECT COUNT(DISTINCT strftime('%Y-%m', date)) FROM entry
                                          WHERE " fin-report--to-date))))
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
