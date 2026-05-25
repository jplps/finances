;;; budget.el --- Budget plan + runway  -*- lexical-binding: t; -*-

(require 'db)

(defconst fin-budget-fix-target 55
  "Target share (%) of monthly liquid earmarked for fix outflows.
Var target is the complement (100 - fix-target).")

(defconst fin-budget-var-target (- 100 fin-budget-fix-target)
  "Target share (%) of monthly liquid for var outflows.
Derived from `fin-budget-fix-target' so the two always sum to 100.")

(defun fin-report--monthly-liquid (year)
  "Average monthly liquid (net in) for YEAR — summary rows only."
  (or (caar
       (fin-db-query
        "SELECT AVG(t) FROM (
           SELECT SUM(amount) AS t
             FROM entry
            WHERE type='in' AND strftime('%Y', date)=? AND item IS NULL
            GROUP BY strftime('%Y-%m', date))"
        (list (format "%d" year))))
      0))

(defun fin-report--budget-share (year)
  "Top-level budget: (category, type, amount, share-of-liquid %) for YEAR.
For fix rows: amount stored; share derived.  For var rows: share stored; amount derived."
  (let ((liquid (fin-report--monthly-liquid year)))
    (fin-db-query
     "SELECT category, type,
             CAST(ROUND(COALESCE(amount, share * ?1)) AS INTEGER) AS amt,
             CASE WHEN ?1 > 0
                  THEN ROUND(100.0 * COALESCE(amount, share * ?1) / ?1, 1)
                  ELSE NULL END
        FROM budget
       WHERE parent IS NULL
       ORDER BY type, amt DESC"
     (list liquid))))

(defun fin-report--var-total (year)
  "Sum of var leaves.  Investments leaves = ODS extras + patrimony amortization."
  (let ((liquid (fin-report--monthly-liquid year)))
    (or (caar (fin-db-query
               "SELECT SUM(v) FROM (
                  -- top-level var rows that are leaves (no children) AND not 'investments'
                  SELECT COALESCE(amount, share * ?1) AS v
                    FROM budget b
                   WHERE parent IS NULL AND type='var'
                     AND category <> 'investments'
                     AND NOT EXISTS (SELECT 1 FROM budget WHERE parent = b.category)
                  UNION ALL
                  -- children of non-investments var parents
                  SELECT COALESCE(c.amount, c.share *
                                  (SELECT COALESCE(amount, share * ?1)
                                     FROM budget WHERE category = c.parent AND parent IS NULL)) AS v
                    FROM budget c
                   WHERE c.parent IN (SELECT category FROM budget
                                       WHERE parent IS NULL AND type='var'
                                         AND category <> 'investments')
                  UNION ALL
                  -- investments: ODS extras
                  SELECT COALESCE(c.amount, c.share *
                                  (SELECT COALESCE(amount, share * ?1)
                                     FROM budget WHERE category = 'investments' AND parent IS NULL)) AS v
                    FROM budget c
                   WHERE c.parent = 'investments'
                  UNION ALL
                  -- investments: patrimony amortization
                  SELECT SUM(amount * 1.0 / NULLIF(lifespan_months,0)) AS v
                    FROM patrimony)"
               (list liquid)))
        0)))

(defun fin-report--planned-month (year)
  "Plan-derived monthly (in, out, liquid) placeholder for forecast months."
  (let* ((liquid  (round (fin-report--monthly-liquid year)))
         (fix     (or (nth 1 (fin-report--runway)) 0))
         (var     (round (fin-report--var-total year)))
         (out     (+ fix var)))
    (list liquid out (- liquid out))))

(defun fin-report--budget-children (parent year)
  "Sub-rows for PARENT budget category for YEAR.
For 'investments': UNION of ODS extras + per-category patrimony amortization.
For other parents: just ODS rows.
Children can be driven by amount or share-of-parent; the other derived.
Percentage column is share of the parent's resolved amount."
  (let* ((liquid (fin-report--monthly-liquid year))
         (parent-amt (or (caar
                          (fin-db-query
                           "SELECT CAST(ROUND(COALESCE(amount, share * ?1)) AS INTEGER)
                              FROM budget WHERE category = ?2 AND parent IS NULL"
                           (list liquid parent)))
                         0)))
    (fin-db-query
     "SELECT category, amt,
             CASE WHEN ?3 > 0 THEN ROUND(100.0 * amt / ?3, 1) ELSE NULL END AS pct
        FROM (
          SELECT c.category,
                 CAST(ROUND(COALESCE(c.amount, c.share *
                                     (SELECT COALESCE(amount, share * ?1)
                                        FROM budget WHERE category = ?2 AND parent IS NULL))) AS INTEGER) AS amt
            FROM budget c
           WHERE c.parent = ?2
          UNION ALL
          SELECT 'patrimony' AS category,
                 CAST(ROUND(SUM(amount * 1.0 / NULLIF(lifespan_months,0))) AS INTEGER) AS amt
            FROM patrimony
           WHERE ?2 = 'investments'
          HAVING SUM(amount) > 0
        )
       ORDER BY amt DESC"
     (list liquid parent parent-amt))))

(defun fin-report--runway ()
  "Return (RESERVE FIX-MONTHLY MONTHS)."
  (let* ((reserve (or (caar (fin-db-query
                             "SELECT SUM(balance)
                                FROM account
                               WHERE parent = 'emergency'"))
                      0))
         (fix     (or (caar (fin-db-query
                             "SELECT SUM(amount)
                                FROM budget
                               WHERE parent IS NULL AND type='fix'"))
                      0)))
    (list reserve fix (if (> fix 0) (/ (float reserve) fix) 0))))

(provide 'budget)
;;; budget.el ends here
