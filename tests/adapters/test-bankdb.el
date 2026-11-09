;;; test-bankdb.el --- adapters/bankdb.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'bankdb)

(defun test-bankdb--row (id date &optional type amount)
  (list id "test" "card" date (or type "out") (or amount 1000) "padaria"))

(ert-deftest bankdb/insert-skips-duplicate-ids ()
  (fin-test-with-db
    (should (= 2 (fin-bankdb-insert (list (test-bankdb--row "a" "2025-06-10")
                                          (test-bankdb--row "b" "2025-06-11")))))
    (should (= 1 (fin-bankdb-insert (list (test-bankdb--row "a" "2025-06-10")
                                          (test-bankdb--row "c" "2025-06-12")))))
    (should (= 3 (fin-db-count "bank_txn")))))

(ert-deftest bankdb/survives-rebuild ()
  (fin-test-with-db
    (fin-bankdb-insert (list (test-bankdb--row "a" "2025-06-10")))
    (fin-db-rebuild)
    (should (= 1 (fin-db-count "bank_txn")))))

(ert-deftest bankdb/invalid-row-writes-nothing ()
  (fin-test-with-db
    (should-error (fin-bankdb-insert (list (test-bankdb--row "a" "2025-06-10")
                                           (test-bankdb--row "b" "10/06/2025"))))
    (should-error (fin-bankdb-insert (list (test-bankdb--row "c" "2025-06-10" "out" -5))))
    (should-error (fin-bankdb-insert (list (test-bankdb--row "d" "2025-06-10" "debit"))))
    (should-error (fin-bankdb-insert (list (test-bankdb--row "" "2025-06-10"))))
    (fin-bankdb-ensure)
    (should (= 0 (fin-db-count "bank_txn")))))

(ert-deftest bankdb/account-span-runs-to-the-statement-end ()
  (fin-test-with-db
    (should-not (cdr (fin-bankdb-account-rows "card")))
    (fin-bankdb-insert (list (test-bankdb--row "a" "2025-06-02") (test-bankdb--row "b" "2025-06-26")))
    (should (equal '("2025-06-02" . "2025-06-26") (cdr (fin-bankdb-account-rows "card"))))
    (fin-bankdb-record-balance "card" "2025-06-30" 0)
    (should (equal '("2025-06-02" . "2025-06-30") (cdr (fin-bankdb-account-rows "card"))))
    (should (equal '("a" "b") (sort (car (fin-bankdb-account-rows "card")) #'string<)))))

(ert-deftest bankdb/year-filters-and-orders ()
  (fin-test-with-db
    (fin-bankdb-insert (list (test-bankdb--row "b" "2025-07-01")
                             (test-bankdb--row "a" "2025-01-01")
                             (test-bankdb--row "z" "2024-12-31")))
    (should (equal '("a" "b") (mapcar #'car (fin-bankdb-year 2025))))))

(ert-deftest bankdb/year-excludes-future-rows ()
  (fin-test-with-db
    (fin-bankdb-insert (list (test-bankdb--row "past"   "2025-06-10")
                             (test-bankdb--row "future" "2099-06-10")))
    (should (equal '("past") (mapcar #'car (fin-bankdb-year 2025))))
    (should (null (fin-bankdb-year 2099)))
    (should (= 2 (fin-db-count "bank_txn")))))

(ert-deftest bankdb/since-filters-from-date-to-today ()
  (fin-test-with-db
    (fin-bankdb-insert (list (test-bankdb--row "old" "2023-12-31")
                             (test-bankdb--row "new" "2024-01-01")
                             (test-bankdb--row "future" "2099-01-01")))
    (should (equal '("new") (mapcar #'car (fin-bankdb-since "2024-01-01"))))
    (should-error (fin-bankdb-since "2024/01/01"))))

(ert-deftest bankdb/file-import-recorded ()
  (fin-test-with-db
    (should-not (fin-bankdb-file-imported-p "abc"))
    (fin-bankdb-record-import "abc" "/tmp/x.ofx" 3)
    (should (fin-bankdb-file-imported-p "abc"))
    (should-error (fin-bankdb-record-import "abc" "/tmp/x.ofx" 3))))

(ert-deftest bankdb/latest-balance-per-account ()
  (fin-test-with-db
    (should-not (fin-bankdb-latest-balance "account"))
    (fin-bankdb-record-balance "account" "2026-09-01" 1000)
    (fin-bankdb-record-balance "account" "2026-10-01" 145366)
    (fin-bankdb-record-balance "account" "2026-10-01" 145366)
    (fin-bankdb-record-balance "pj" "2026-10-05" 0)
    (should (equal '("2026-10-01" . 145366) (fin-bankdb-latest-balance "account")))
    (should (equal '("2026-10-05" . 0) (fin-bankdb-latest-balance "pj")))
    (should-error (fin-bankdb-record-balance "account" "1/10/2026" 1))))

(ert-deftest bankdb/superseded-period-per-kind ()
  (fin-test-with-db
    (fin-bankdb-insert
     (list (list "c0" "t" "pj-card" "2025-03-06" "out" 1 "first day")
           (list "c1" "t" "pj-card" "2025-04-06" "out" 1 "next bill's first day")
           (list "a0" "t" "account" "2025-03-01" "out" 1 "first day")
           (list "a1" "t" "account" "2025-03-31" "out" 1 "last day")
           (list "a2" "t" "account" "2025-04-01" "out" 1 "next month")))
    (should (= 1 (fin-bankdb-drop-superseded "pj-card" "2025-03-06" "2025-04-06" nil)))
    (should (= 2 (fin-bankdb-drop-superseded "account" "2025-03-01" "2025-03-31" nil)))
    (should (equal '("a2" "c1") (mapcar #'car (fin-db-query "SELECT id FROM bank_txn ORDER BY id"))))
    (should-error (fin-bankdb-drop-superseded "account" "2025-3-1" "2025-03-31" nil))))

(ert-deftest bankdb/relisted-row-replaces-its-old-date ()
  (fin-test-with-db
    (fin-bankdb-insert
     (list (list "nubank:F1:aaa" "t" "card" "2026-11-05" "out" 54990 "Ebn*Playstation")
           (list "nubank:F2:bbb" "t" "card" "2026-10-06" "out" 3599 "Loja - Parcela 5/10")
           (list "nubank:F1:eee" "t" "account" "2026-11-05" "out" 54990 "Ebn*Playstation")
           (list "nubank:F3:fff" "t" "card" "2026-11-05" "out" 54990 "Ebn*Playstation")))
    (let ((rows (list (list "nubank:F1:ddd" "t" "card" "2026-11-06" "out" 54990 "Ebn*Playstation")
                      (list "nubank:F2:ccc" "t" "card" "2026-11-06" "out" 3599 "Loja - Parcela 6/10"))))
      (should (= 1 (fin-bankdb-drop-relisted rows)))
      (should (= 2 (fin-bankdb-insert rows)))
      (should (= 0 (fin-bankdb-drop-relisted rows))))
    (should (equal '("nubank:F1:ddd" "nubank:F1:eee" "nubank:F2:bbb" "nubank:F2:ccc" "nubank:F3:fff")
                   (mapcar #'car (fin-db-query "SELECT id FROM bank_txn ORDER BY id"))))))

(provide 'test-bankdb)
;;; test-bankdb.el ends here
