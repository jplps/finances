;;; patrimony.el --- Patrimony catalog + amortization  -*- lexical-binding: t; -*-

(require 'db)
(require 'accounts)

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
  "Net worth: every account rebuilt from the bank."
  (fin-report--accounts-total))

(provide 'patrimony)
;;; patrimony.el ends here
