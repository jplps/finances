;;; test-db.el --- adapters/db.el schema + CRUD tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'db)

(ert-deftest db/available-p-true-in-emacs29 ()
  (should (fin-db-available-p)))

(ert-deftest db/rebuild-creates-all-tables ()
  (fin-test-with-db
    (let ((tables (mapcar #'car
                          (fin-db-query
                           "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"))))
      (should (member "entry"     tables))
      (should (member "budget"    tables))
      (should (member "patrimony" tables))
      (should (member "account"   tables))
      (should (member "refresh_log" tables)))))

(ert-deftest db/rebuild-is-idempotent ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "in" "salary" 100 nil)
    (should (= 1 (fin-db-count "entry")))
    (fin-db-rebuild)
    (should (= 0 (fin-db-count "entry"))))) ; rebuild wipes

(ert-deftest db/entry-amount-is-integer-cents ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "in" "salary" 12345 nil)
    (let ((row (car (fin-db-query "SELECT amount FROM entry"))))
      (should (integerp (car row)))
      (should (= 12345 (car row))))))

(ert-deftest db/budget-composite-pk-allows-same-cat-different-parent ()
  "Composite PRIMARY KEY (category, parent) lets `house' exist top-level AND under another."
  (fin-test-with-db
    (fin-test-insert-budget "house" nil "fix" 100000 nil)
    (fin-test-insert-budget "house" "investments" "var" 10000 nil)
    (should (= 2 (fin-db-count "budget")))))

(ert-deftest db/budget-duplicate-non-null-parent-errors ()
  "Composite PK rejects duplicates when both (category, parent) are non-NULL.
SQLite treats NULL parents as distinct, so the constraint only fires when both
parts are actual values."
  (fin-test-with-db
    (fin-test-insert-budget "retirement" "investments" "var" 100000 nil)
    (should-error
     (fin-test-insert-budget "retirement" "investments" "var" 999 nil))))

(ert-deftest db/bulk-insert-and-truncate ()
  (fin-test-with-db
    (fin-db-bulk-insert
     "entry" '("date" "type" "category" "amount" "currency")
     '(("2025-01-01" "in"  "salary" 100 "BRL")
       ("2025-01-02" "out" "food"    50 "BRL")
       ("2025-01-03" "out" "food"    30 "BRL")))
    (should (= 3 (fin-db-count "entry")))
    (fin-db-truncate "entry")
    (should (= 0 (fin-db-count "entry")))))

(provide 'test-db)
;;; test-db.el ends here
