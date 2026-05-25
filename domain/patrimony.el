;;; patrimony.el --- Patrimony catalog + amortization  -*- lexical-binding: t; -*-

(require 'db)

(defun fin-report--patrimony-summary ()
  "Per-category (count, total amount, total monthly amortization).
Monthly derived: amount / lifespan_months."
  (fin-db-query
   "SELECT category,
           COUNT(*),
           SUM(amount),
           CAST(ROUND(SUM(amount * 1.0 / NULLIF(lifespan_months,0))) AS INTEGER) AS monthly
      FROM patrimony
     GROUP BY category
     ORDER BY monthly DESC"))

(defun fin-report--patrimony-items ()
  "Per-item rows.  Monthly derived: amount / lifespan_months."
  (fin-db-query
   "SELECT category, item, amount, lifespan_months,
           CAST(ROUND(amount * 1.0 / NULLIF(lifespan_months,0)) AS INTEGER)
      FROM patrimony
     ORDER BY category, amount * 1.0 / NULLIF(lifespan_months,0) DESC"))

(defun fin-report--patrimony-total ()
  "Net worth: sum of top-level account balances."
  (or (caar (fin-db-query
             "SELECT SUM(balance) FROM account WHERE parent IS NULL"))
      0))

(provide 'patrimony)
;;; patrimony.el ends here
