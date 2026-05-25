;;; accounts.el --- Account balances + share derivation  -*- lexical-binding: t; -*-

(require 'db)

(defun fin-report--accounts ()
  "Top-level accounts.  Share % derived from total top-level balance."
  (fin-db-query
   "SELECT category, balance,
           ROUND(100.0 * balance /
                 NULLIF((SELECT SUM(balance) FROM account WHERE parent IS NULL),0), 1)
      FROM account
     WHERE parent IS NULL
     ORDER BY balance DESC"))

(defun fin-report--account-children (parent)
  "Sub-rows for PARENT.  Share derived from parent's balance."
  (fin-db-query
   "SELECT category, balance,
           ROUND(100.0 * balance /
                 NULLIF((SELECT balance FROM account WHERE category = ?1),0), 1)
      FROM account
     WHERE parent = ?1
     ORDER BY balance DESC"
   (list parent)))

(provide 'accounts)
;;; accounts.el ends here
