;;; test-accounts.el --- domain/accounts.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'accounts)

(ert-deftest accounts/top-level-only ()
  (fin-test-with-db
    (fin-test-insert-account "main"      nil    400000 nil)
    (fin-test-insert-account "emergency" nil    600000 nil)
    (fin-test-insert-account "main-sub"  "main" 100000 nil)  ; child excluded
    (let ((rows (fin-report--accounts)))
      (should (= 2 (length rows)))
      ;; emergency (600k) ranks first → 60%, main → 40%
      (should (equal "emergency" (nth 0 (car rows))))
      (should (= 60.0 (nth 2 (car rows))))
      (should (= 40.0 (nth 2 (cadr rows)))))))

(ert-deftest accounts/empty-no-crash ()
  (fin-test-with-db
    (should (null (fin-report--accounts)))))

(ert-deftest accounts/children-shares-of-parent ()
  (fin-test-with-db
    (fin-test-insert-account "emergency"   nil          1000000 nil)
    (fin-test-insert-account "emergency-a" "emergency"  600000  nil)
    (fin-test-insert-account "emergency-b" "emergency"  400000  nil)
    (let ((rows (fin-report--account-children "emergency")))
      (should (= 2 (length rows)))
      (should (= 60.0 (nth 2 (car rows))))
      (should (= 40.0 (nth 2 (cadr rows)))))))

(ert-deftest accounts/children-zero-parent-balance-no-crash ()
  "NULLIF guards against division by zero when parent balance = 0."
  (fin-test-with-db
    (fin-test-insert-account "emergency"   nil         0      nil)
    (fin-test-insert-account "emergency-a" "emergency" 100000 nil)
    (let ((row (car (fin-report--account-children "emergency"))))
      (should (null (nth 2 row))))))

(ert-deftest accounts/children-share-of-top-level-parent ()
  "A sub-account named like a top-level one must not stand in for it."
  (fin-test-with-db
    (fin-test-insert-account "emergency"   "other"     100     nil)
    (fin-test-insert-account "emergency-a" "emergency" 600000  nil)
    (should (null (nth 2 (car (fin-report--account-children "emergency")))))
    (fin-test-insert-account "emergency"   nil         1000000 nil)
    (should (= 60.0 (nth 2 (car (fin-report--account-children "emergency")))))))

(ert-deftest accounts/derived-from-opening-and-bank-flows ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-test-insert-account "emergency" nil 600000)
    (fin-test-insert-account "nu" "emergency" 520000)
    (cl-flet ((bank (id date type amount desc)
                (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                              VALUES (?,'t','account',?,?,?,?)" (list id date type amount desc))))
      (bank "a" "2025-12-31" "out" 99999 "Aplicação RDB")          ; on the anchor day: already counted
      (bank "b" "2026-01-10" "out" 100000 "Aplicação RDB")
      (bank "c" "2026-02-10" "in" 30000 "Resgate RDB")
      (bank "d" "2026-02-11" "out" 5000 "Compra no débito - Loja"))
    (fin-bankdb-record-balance "account" "2026-02-28" 12345)
    (let ((fin-accounts-derived
           '((:name "nubank" :sheet ("emergency/nu") :since "2025-12-31" :opening 400000
              :into ("\\`Aplicação RDB") :out-of ("\\`Resgate RDB"))
             (:name "checking" :balance "account"))))
      (should (equal '(("nubank" 470000 520000 "since 2025-12-31")
                       ("checking" 12345 nil "2026-02-28"))
                     (fin-report--accounts-derived))))))

(ert-deftest accounts/derived-from-zero-notes-first-flow ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2023-04-13','out',5000,'Aplicação RDB')")
    (let ((fin-accounts-derived '((:name "nu" :sheet () :since "2020-01-01" :opening 0
                                   :into ("\\`Aplicação RDB") :out-of ()))))
      (should (equal '(("nu" 5000 nil "since 2023-04-13")) (fin-report--accounts-derived))))))

(ert-deftest accounts/closed-account-shows-result ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2025-08-05','out',50000,'Compra de criptomoedas'),
                         ('b','t','account','2026-01-17','in',45000,'Venda de criptomoedas')")
    (let ((fin-accounts-derived '((:name "crypto" :sheet () :since "2020-01-01" :opening 0 :closed t
                                   :into ("\\`Compra de cripto") :out-of ("\\`Venda de cripto")))))
      (should (equal '(("crypto" 0 nil "closed, loss 50.00")) (fin-report--accounts-derived))))))

(ert-deftest accounts/derived-spec-fails-loud ()
  (fin-test-with-db
    (let ((fin-accounts-derived '((:name "x" :sheet ("y")))))
      (should-error (fin-report--accounts-derived)))))

(provide 'test-accounts)
;;; test-accounts.el ends here
