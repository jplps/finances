;;; helpers.el --- Shared test fixtures + utilities  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root)
  (dolist (sub '("adapters" "tools" "domain" "view"))
    (add-to-list 'load-path (expand-file-name sub root))))

(require 'db)
(require 'parser)
(require 'budget)
(require 'stats)
(require 'patrimony)
(require 'accounts)
(require 'cashflow)
(require 'fmt)
(require 'charts)
(require 'html)
(require 'panels)
(require 'dashboard)
(require 'sync)
(require 'bankdb)
(require 'ofx)
(require 'odswrite)
(require 'reconcile)
(require 'conventions)
(require 'goals)
(require 'bank)
(require 'bankfix)
(load (expand-file-name "../banks/nubank" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defmacro fin-test-with-db (&rest body)
  "Run BODY on a fresh temp DB with no Receita payment lists, deleted after."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "fin-test-" nil ".db"))
          (fin-db-path tmp)
          (fin-db--conn nil)
          (fin-ir-payments-dir (expand-file-name "fin-test-no-payments/" temporary-file-directory)))
     (unwind-protect
         (progn (fin-db-rebuild) ,@body)
       (fin-db-close)
       (ignore-errors (delete-file tmp)))))

(defun fin-test-insert-entry (date type cat amount &optional item installment installments)
  "Insert an entry row; AMOUNT in cents."
  (fin-db-exec
   "INSERT INTO entry(date,type,category,item,amount,installment,installments)
    VALUES(?,?,?,?,?,?,?)"
   (list date type cat item amount installment installments)))

(defun fin-test-insert-budget (cat parent type amount share)
  (fin-db-exec
   "INSERT INTO budget(category,parent,type,amount,share) VALUES(?,?,?,?,?)"
   (list cat parent type amount share)))

(defun fin-test-insert-patrimony (cat item amount lifespan)
  (fin-db-exec
   "INSERT INTO patrimony(category,item,amount,lifespan_months) VALUES(?,?,?,?)"
   (list cat item amount lifespan)))

(defun fin-test-seed-minimal ()
  "Tiny coherent dataset: 1 income, 1 outflow, 1 budget, 1 patrimony."
  (fin-test-insert-entry "2025-01-31" "in"  "salary" 500000 nil)
  (fin-test-insert-entry "2025-01-31" "out" "food"   100000 nil)
  (fin-test-insert-budget "food" nil "fix" 100000 nil)
  (fin-test-insert-patrimony "car" "wheels" 1000000 60))

(defun fin-test-seed-multi-year ()
  "Multi-year history including a future-month placeholder."
  (fin-test-insert-entry "2023-06-30" "in"  "salary" 400000 nil)
  (fin-test-insert-entry "2023-06-30" "out" "food"   80000 nil)
  (fin-test-insert-entry "2024-06-30" "in"  "salary" 450000 nil)
  (fin-test-insert-entry "2024-06-30" "out" "food"   90000 nil)
  (fin-test-insert-entry "2025-06-30" "in"  "salary" 500000 nil)
  (fin-test-insert-entry "2025-06-30" "out" "food"  100000 nil)
  (fin-test-insert-entry "2099-12-31" "out" "food"  100000 nil))

(defun fin-test-seed-budget-tree ()
  "Budget tree: fix + var, investments parent with patrimony amortization."
  (fin-test-insert-budget "house" nil "fix" 200000 nil)
  (fin-test-insert-budget "food"  nil "fix" 100000 nil)
  (fin-test-insert-budget "investments" nil "var" nil 0.25)
  (fin-test-insert-budget "retirement" "investments" "var" 80000 nil)
  (fin-test-insert-budget "free"       "investments" "var" 60000 nil)
  (fin-test-insert-patrimony "car"   "wheels"  1200000 60)
  (fin-test-insert-patrimony "house" "fridge"   240000 120))

(provide 'helpers)
;;; helpers.el ends here
