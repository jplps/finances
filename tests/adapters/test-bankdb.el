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

(provide 'test-bankdb)
;;; test-bankdb.el ends here
