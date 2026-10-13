;;; test-accounts.el --- domain/accounts.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'accounts)

(ert-deftest accounts/derived-from-opening-and-bank-flows ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (cl-flet ((bank (id date type amount desc)
                (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                              VALUES (?,'t','account',?,?,?,?)" (list id date type amount desc))))
      (bank "a" "2025-12-31" "out" 99999 "Aplicação RDB")          ; on the anchor day: already counted
      (bank "b" "2026-01-10" "out" 100000 "Aplicação RDB")
      (bank "c" "2026-02-10" "in" 30000 "Resgate RDB")
      (bank "d" "2026-02-11" "out" 5000 "Compra no débito - Loja"))
    (fin-bankdb-record-balance "account" "2026-02-28" 12345)
    (let ((fin-accounts-derived
           '((:name "nubank" :reserve t :since "2025-12-31" :opening 400000
              :into ("\\`Aplicação RDB") :out-of ("\\`Resgate RDB"))
             (:name "checking" :balance "account"))))
      (should (equal '((:name "nubank" :cents 470000 :note "since 2025-12-31" :cash nil :reserve t :closed nil)
                       (:name "checking" :cents 12345 :note "2026-02-28" :cash t :reserve nil :closed nil))
                     (fin-report--accounts-derived)))
      (should (= 482345 (fin-report--accounts-total)))
      (should (= 470000 (fin-report--accounts-total :reserve)))
      (should (= 12345 (fin-report--accounts-total :cash))))))

(ert-deftest accounts/derived-from-zero-notes-first-flow ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2023-04-13','out',5000,'Aplicação RDB')")
    (let ((fin-accounts-derived '((:name "nu" :since "2020-01-01" :opening 0
                                   :into ("\\`Aplicação RDB") :out-of ()))))
      (should (equal "since 2023-04-13" (plist-get (car (fin-report--accounts-derived)) :note)))
      (let ((fin-accounts-derived (list (append (car fin-accounts-derived) '(:yield 700)))))
        (should (equal '(5700 "since 2023-04-13 + yield 7.00")
                       (let ((a (car (fin-report--accounts-derived)))) (list (plist-get a :cents) (plist-get a :note)))))))))

(ert-deftest accounts/closed-account-shows-result ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2025-08-05','out',50000,'Compra de criptomoedas'),
                         ('b','t','account','2026-01-17','in',45000,'Venda de criptomoedas')")
    (let ((fin-accounts-derived '((:name "crypto" :since "2020-01-01" :opening 0 :closed t
                                   :into ("\\`Compra de cripto") :out-of ("\\`Venda de cripto")))))
      (should (equal '(:name "crypto" :cents 0 :note "closed, loss 50.00" :cash nil :reserve nil :closed t)
                     (car (fin-report--accounts-derived)))))))

(ert-deftest accounts/derived-spec-fails-loud ()
  (fin-test-with-db
    (let ((fin-accounts-derived '((:name "x"))))
      (should-error (fin-report--accounts-derived)))))

(ert-deftest accounts/bank-saved-nets-withdrawals ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2026-10-02','out',100000,'Aplicação RDB'),
                         ('b','t','account','2026-10-03','in',30000,'Resgate RDB'),
                         ('c','t','account','2026-10-04','out',20000,'Compra de ETF'),
                         ('d','t','account','2026-10-05','in',1000,'Compra de ETF'),
                         ('e','t','account','2026-10-06','out',5000,'Compra no débito - Loja'),
                         ('f','t','pj','2026-10-07','out',7000,'Aplicação RDB'),
                         ('g','t','account','2026-09-30','out',9000,'Aplicação RDB'),
                         ('h','t','account','2023-05-01','in',4000,'Pix - Conta: 51-6'),
                         ('i','t','account','2024-01-05','out',6000,'Pix - Conta: 51-6')")
    (let ((fin-accounts-derived
           '((:name "nu" :since "2020-01-01" :opening 0 :into ("\\`Aplicação RDB") :out-of ("\\`Resgate RDB"))
             (:name "etf" :since "2020-01-01" :opening 0 :into ("\\`Compra de ETF") :out-of ("\\`Compra de ETF"))
             (:name "bb" :since "2023-12-31" :opening 0 :into ("Conta: 51-6\\'") :out-of ("Conta: 51-6\\'"))
             (:name "checking" :balance "account"))))
      (should (= 89000 (fin-report--bank-saved "2026-10")))
      (should (= 9000 (fin-report--bank-saved "2026-09")))
      (should (= 98000 (fin-report--bank-saved "2026")))
      (should (= 0 (fin-report--bank-saved "2023")))            ; before bb's :since
      (should (= 6000 (fin-report--bank-saved "2024")))
      (should-error (fin-report--bank-saved "2026-1")))))

(ert-deftest accounts/derived-defaults-and-memo ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','t','account','2020-03-01','out',5000,'Aplicação RDB')")
    (let ((fin-accounts-derived '((:name "nu" :into ("\\`Aplicação RDB") :out-of ()))))
      (should (equal '(5000 "since 2020-03-01")
                     (let ((a (car (fin-report--accounts-derived)))) (list (plist-get a :cents) (plist-get a :note)))))
      (let ((fin-report--accounts-memo (list nil)) (calls 0))
        (cl-letf* ((orig (symbol-function 'fin-report--accounts-compute))
                   ((symbol-function 'fin-report--accounts-compute)
                    (lambda () (cl-incf calls) (funcall orig))))
          (fin-report--accounts-total)
          (fin-report--accounts-total :reserve)
          (should (= 1 calls)))))))

(provide 'test-accounts)
;;; test-accounts.el ends here
