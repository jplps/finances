;;; patrimony.el --- Patrimony catalog + amortization  -*- lexical-binding: t; -*-

(require 'db)
(require 'accounts)

(defun fin-report--patrimony-summary ()
  "Rows (CATEGORY COUNT AMOUNT MONTHLY), MONTHLY = amount / lifespan_months."
  (fin-db-query
   "SELECT category,
           COUNT(*),
           SUM(amount),
           CAST(ROUND(SUM(amount * 1.0 / NULLIF(lifespan_months,0))) AS INTEGER) AS monthly
      FROM patrimony
     GROUP BY category
     ORDER BY monthly DESC"))

(defun fin-report--patrimony-items ()
  "Rows (CATEGORY ITEM AMOUNT LIFESPAN-MONTHS MONTHLY)."
  (fin-db-query
   "SELECT category, item, amount, lifespan_months,
           CAST(ROUND(amount * 1.0 / NULLIF(lifespan_months,0)) AS INTEGER)
      FROM patrimony
     ORDER BY category, amount * 1.0 / NULLIF(lifespan_months,0) DESC"))

(defun fin-report--patrimony-total ()
  "Net worth in cents."
  (fin-report--accounts-total))

(provide 'patrimony)
;;; patrimony.el ends here
