;;; budget.el --- Budget plan + runway  -*- lexical-binding: t; -*-

(require 'db)

(defconst fin-budget-fix-target 55
  "Target share (%) of monthly liquid earmarked for fix outflows.
Var target is the complement (100 - fix-target).")

(defconst fin-budget-var-target (- 100 fin-budget-fix-target)
  "Target share (%) of monthly liquid for var outflows.
Derived from `fin-budget-fix-target' so the two always sum to 100.")

(defun fin-report--realized-income (year)
  "Average monthly (BRUTE CNPJ) realized from entries for YEAR, in cents.
Averages complete months only; falls back to every month carrying rows when
YEAR has no complete month yet."
  (or (car
       (fin-db-query
        "WITH m AS (
           SELECT strftime('%Y-%m', date) AS ym,
                  SUM(CASE WHEN type='in' THEN amount ELSE 0 END) AS brute,
                  SUM(CASE WHEN type<>'in' AND category='cnpj'
                           THEN amount ELSE 0 END) AS cnpj
             FROM entry
            WHERE strftime('%Y', date)=?
              AND (type='in' OR category='cnpj')
            GROUP BY ym)
         SELECT CAST(ROUND(COALESCE(
                  (SELECT AVG(brute) FROM m WHERE ym < strftime('%Y-%m','now')),
                  (SELECT AVG(brute) FROM m), 0)) AS INTEGER),
                CAST(ROUND(COALESCE(
                  (SELECT AVG(cnpj) FROM m WHERE ym < strftime('%Y-%m','now')),
                  (SELECT AVG(cnpj) FROM m), 0)) AS INTEGER)"
        (list (format "%d" year))))
      (list 0 0)))

(defun fin-report--monthly-income (year)
  "Monthly (BRUTE CNPJ LIQUID SOURCE) for YEAR.  Money in cents.
SOURCE is `plan' when the budget declares income rows (type='income',
category `brute'/`cnpj'), else `actual' — averaged from realized entries."
  (let* ((plan  (car (fin-db-query
                      "SELECT COALESCE(SUM(CASE WHEN category='brute' THEN amount END), 0),
                              COALESCE(SUM(CASE WHEN category='cnpj'  THEN amount END), 0)
                         FROM budget
                        WHERE parent IS NULL AND type='income'")))
         (declared (> (or (nth 0 plan) 0) 0))
         (pair  (if declared plan (fin-report--realized-income year)))
         (brute (or (nth 0 pair) 0))
         (cnpj  (or (nth 1 pair) 0)))
    (list brute cnpj (- brute cnpj) (if declared 'plan 'actual))))

(defun fin-report--monthly-liquid (year)
  "Average monthly liquid (net in) for YEAR, in cents.
Declared plan income wins; otherwise realized entries are averaged."
  (nth 2 (fin-report--monthly-income year)))

(defun fin-report--budget-share (year)
  "Top-level budget: (category, type, amount, share-of-liquid %) for YEAR.
For fix rows: amount stored; share derived.  For var rows: share stored;
amount derived."
  (let ((liquid (fin-report--monthly-liquid year)))
    (fin-db-query
     "SELECT category, type,
             CAST(ROUND(COALESCE(amount, share * ?1)) AS INTEGER) AS amt,
             CASE WHEN ?1 > 0
                  THEN 100.0 * COALESCE(amount, share * ?1) / ?1
                  ELSE NULL END
        FROM budget
       WHERE parent IS NULL AND type IN ('fix','var')
       ORDER BY type, amt DESC"
     (list liquid))))

(defun fin-report--budget-base (parent year)
  "Return (PARENT-AMT BASE) for the PARENT budget category in YEAR, in cents.
BASE is what PARENT's share-driven children divide: the parent amount less
what is already committed \u2014 children carrying an explicit amount, plus the
patrimony amortization injected into `investments'.  A negative BASE is
returned as-is so an overrun surfaces instead of silently clamping."
  (or (car (fin-db-query
            "SELECT amt,
                    amt
                    - COALESCE((SELECT SUM(s.amount) FROM budget s
                                 WHERE s.parent = ?2 AND s.amount IS NOT NULL), 0)
                    - COALESCE((SELECT CAST(ROUND(SUM(p.amount * 1.0
                                                      / NULLIF(p.lifespan_months,0))) AS INTEGER)
                                  FROM patrimony p WHERE ?2 = 'investments'), 0)
               FROM (SELECT CAST(ROUND(COALESCE(amount, share * ?1)) AS INTEGER) AS amt
                       FROM budget WHERE category = ?2 AND parent IS NULL)"
            (list (fin-report--monthly-liquid year) parent)))
      (list 0 0)))

(defun fin-report--budget-children (parent year)
  "Sub-rows (category, amount, % of parent) for PARENT budget category in YEAR.
For the investments parent: UNION of ODS extras + per-category patrimony
amortization.  Other parents: just ODS rows.
Children are driven by amount or by share; a share resolves against the
parent's uncommitted base (see `fin-report--budget-base'), so fixed draws are
never double-counted.  Percentage column is share of the parent's amount."
  (let* ((b          (fin-report--budget-base parent year))
         (parent-amt (nth 0 b))
         (base       (nth 1 b)))
    (fin-db-query
     "SELECT category, amt,
             CASE WHEN ?3 > 0 THEN 100.0 * amt / ?3 ELSE NULL END AS pct
        FROM (
          SELECT c.category,
                 CAST(ROUND(COALESCE(c.amount, c.share * ?1)) AS INTEGER) AS amt
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
     (list base parent parent-amt))))

(defun fin-report--var-total (year)
  "Sum of var leaves for YEAR, in cents.
A parent with children contributes those children \u2014 investments' patrimony
amortization among them; a childless parent contributes its own amount."
  (let ((total 0))
    (dolist (row (fin-report--budget-share year) total)
      (when (equal (nth 1 row) "var")
        (let ((kids (fin-report--budget-children (nth 0 row) year)))
          (setq total
                (+ total
                   (if kids
                       (apply #'+ (mapcar (lambda (k) (or (nth 1 k) 0)) kids))
                     (or (nth 2 row) 0)))))))))

(defun fin-report--planned-month (year)
  "Plan-derived monthly (in, out, liquid) placeholder for forecast months."
  (let* ((liquid  (round (fin-report--monthly-liquid year)))
         (fix     (or (nth 1 (fin-report--runway)) 0))
         (var     (round (fin-report--var-total year)))
         (out     (+ fix var)))
    (list liquid out (- liquid out))))

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
