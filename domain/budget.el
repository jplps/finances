;;; budget.el --- Budget plan + runway  -*- lexical-binding: t; -*-

(require 'db)
(require 'cashflow)
(require 'accounts)

(defconst fin-budget-fix-target 55
  "Target share (%) of monthly liquid for fix outflows.")

(defconst fin-budget-var-target (- 100 fin-budget-fix-target)
  "Target share (%) of monthly liquid for var outflows.")

(defun fin-report--realized-income (year)
  "Average monthly (BRUTE CNPJ) cents realized in YEAR.
Complete months only, else every month with rows; savings withdrawals are
not income."
  (or (car
       (fin-report--flow-query
        ", m AS (
           SELECT strftime('%Y-%m', date) AS ym,
                  SUM(CASE WHEN type='in' THEN amount ELSE 0 END) AS brute,
                  SUM(CASE WHEN type<>'in' AND category='cnpj'
                           THEN amount ELSE 0 END) AS cnpj
             FROM flow
            WHERE strftime('%Y', date)=?
              AND (type='in' OR category='cnpj')
            GROUP BY ym)
         SELECT CAST(ROUND(COALESCE(
                  (SELECT AVG(brute) FROM m WHERE ym < strftime('%Y-%m','now','localtime')),
                  (SELECT AVG(brute) FROM m), 0)) AS INTEGER),
                CAST(ROUND(COALESCE(
                  (SELECT AVG(cnpj) FROM m WHERE ym < strftime('%Y-%m','now','localtime')),
                  (SELECT AVG(cnpj) FROM m), 0)) AS INTEGER)"
        (list (format "%d" year))))
      (list 0 0)))

(defun fin-report--monthly-income (year)
  "Monthly (BRUTE CNPJ LIQUID SOURCE) for YEAR, in cents.
SOURCE is `plan' when the budget declares income rows, else `actual'."
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
  "Monthly liquid income for YEAR, in cents."
  (nth 2 (fin-report--monthly-income year)))

(defun fin-report--budget-share (year)
  "Top-level rows (CATEGORY TYPE AMOUNT SHARE-%) for YEAR.
Fix rows store the amount, var rows the share of monthly liquid."
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
  "Return (PARENT-AMT BASE) in cents for budget category PARENT in YEAR.
BASE, split by share-driven children, is PARENT-AMT less children with an
amount and, for `investments', patrimony amortization; it may be negative."
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
  "Rows (CATEGORY AMOUNT PCT-OF-PARENT) under PARENT in YEAR.
Shares resolve against `fin-report--budget-base'; `investments' also gets
a patrimony amortization row."
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

(defun fin-report--var-total (year &optional spend-only)
  "Cents of var leaves for YEAR: a parent's children, else its own amount.
SPEND-ONLY skips `out' transfer rows of `fin-report-transfer-categories'."
  (let ((saving (and spend-only (cdr (assoc "out" fin-report-transfer-categories))))
        (total 0))
    (dolist (row (fin-report--budget-share year) total)
      (when (and (equal (nth 1 row) "var") (not (member (nth 0 row) saving)))
        (let ((kids (fin-report--budget-children (nth 0 row) year)))
          (setq total
                (+ total
                   (if kids
                       (apply #'+ (mapcar (lambda (k) (or (nth 1 k) 0)) kids))
                     (or (nth 2 row) 0)))))))))

(defun fin-report--planned-month (year)
  "Plan-derived monthly (IN OUT LIQUID) in cents for forecast months.
IN is brute income, OUT fix, spending var and cnpj; saving rows stay in
LIQUID."
  (let* ((inc     (fin-report--monthly-income year))
         (in      (round (nth 0 inc)))
         (fix     (or (nth 1 (fin-report--runway)) 0))
         (var     (round (fin-report--var-total year t)))
         (out     (+ fix var (round (nth 1 inc)))))
    (list in out (- in out))))

(defun fin-report--runway ()
  "Return (RESERVE FIX-MONTHLY MONTHS), money in cents."
  (let* ((reserve (fin-report--accounts-total :reserve))
         (fix     (or (caar (fin-db-query
                             "SELECT SUM(amount)
                                FROM budget
                               WHERE parent IS NULL AND type='fix'"))
                      0)))
    (list reserve fix (if (> fix 0) (/ (float reserve) fix) 0))))

(provide 'budget)
;;; budget.el ends here
