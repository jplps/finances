;;; test-cashflow.el --- domain/cashflow.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'cashflow)

(ert-deftest cashflow/year-months-pads-missing-with-12-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "in" "salary" 100000 nil)
    (let ((rows (fin-cashflow--year-months 2025)))
      (should (= 12 (length rows)))
      (should (equal 6 (car (nth 5 rows))))
      (should (= 100000 (nth 1 (nth 5 rows))))
      (should (null (nth 1 (nth 0 rows)))))))

(ert-deftest cashflow/month-items-returns-all-out-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-10" "out" "food" 100 nil)
    (fin-test-insert-entry "2025-06-15" "out" "food" 200 "bread")
    (fin-test-insert-entry "2025-06-20" "out" "food" 300 "butter")
    (let ((rows (fin-cashflow--month-items 2025 6)))
      (should (= 3 (length rows)))
      (should (equal "food"   (nth 0 (nth 0 rows))))
      (should (equal "bread"  (nth 0 (nth 1 rows))))
      (should (equal "butter" (nth 0 (nth 2 rows))))
      (should (= 600 (apply #'+ (mapcar (lambda (r) (nth 2 r)) rows)))))))

(ert-deftest cashflow/month-items-join-a-purchase-installments ()
  (fin-test-with-db
    (dolist (k '(7 8 9 10)) (fin-test-insert-entry "2026-11-05" "out" "patrimony" 3599 "oster" k 10))
    (fin-test-insert-entry "2026-11-06" "out" "car" 5090 "bateria" 8 10)
    (fin-test-insert-entry "2026-11-07" "out" "food" 900 "padaria")
    (should (equal '(("oster 7–10/10" "patrimony" 14396 "2026-11-05")
                     ("bateria 8/10" "car" 5090 "2026-11-06")
                     ("padaria" "food" 900 "2026-11-07"))
                   (fin-cashflow--month-items 2026 11)))))

(ert-deftest cashflow/annual-sums-derives-save-pct ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2025-06-30" "out" "food"   400  nil)
    (let* ((row (car (fin-cashflow--annual-sums))))
      (should (equal 2025 (nth 0 row)))
      (should (= 1000 (nth 1 row)))
      (should (= 400  (nth 2 row)))
      (should (= 600  (nth 3 row)))
      (should (= 60.0 (nth 4 row))))))

(ert-deftest cashflow/annual-sums-no-income-yields-nil-save-pct ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "out" "food" 100 nil)
    (let ((row (car (fin-cashflow--annual-sums))))
      (should (null (nth 4 row))))))

(ert-deftest cashflow/monthly-flow-orders-asc-by-ym ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-03-31" "in" "salary" 300 nil)
    (fin-test-insert-entry "2025-01-31" "in" "salary" 100 nil)
    (fin-test-insert-entry "2025-02-28" "in" "salary" 200 nil)
    (let ((yms (mapcar #'car (fin-cashflow--monthly-flow))))
      (should (equal '("2025-01" "2025-02" "2025-03") yms)))))

(defun fin-test--seed-transfers ()
  "Jan 2025: income 1000, spending 400; 300 + 100 saved, 50 withdrawn."
  (fin-test-insert-entry "2025-01-31" "in"  "salary"      1000 nil)
  (fin-test-insert-entry "2025-01-31" "out" "food"        400  nil)
  (fin-test-insert-entry "2025-01-31" "out" "investments" 300  "car")
  (fin-test-insert-entry "2025-01-31" "out" "retirement"  100  nil)
  (fin-test-insert-entry "2025-01-31" "in"  "reserve"     50   nil))

(ert-deftest cashflow/transfers-are-neither-income-nor-spending ()
  (fin-test-with-db
    (fin-test--seed-transfers)
    (should (equal '(1 1000 400 600) (car (fin-cashflow--year-months 2025))))
    (should (equal '(2025 1000 400 600 60.0) (car (fin-cashflow--annual-sums))))
    (should (equal '(("2025-01" 1000 400)) (fin-cashflow--monthly-flow)))
    (should (equal '(("food" "food" 400 "2025-01-31")) (fin-cashflow--month-items 2025 1)))))

(ert-deftest cashflow/flow-signs-saves-contributions-minus-withdrawals ()
  (fin-test-with-db
    (fin-test--seed-transfers)
    (should (= 350 (caar (fin-cashflow--flow-query "SELECT SUM(amount) FROM flow WHERE type='save'"))))))

(ert-deftest cashflow/transfer-categories-are-configurable ()
  (fin-test-with-db
    (fin-test--seed-transfers)
    (fin-test-insert-entry "2025-01-31" "out" "d'água" 7 nil)
    (let ((fin-cashflow-transfer-categories nil))
      (should (equal '(1 1050 807 243) (car (fin-cashflow--year-months 2025)))))
    (let ((fin-cashflow-transfer-categories '(("out" "d'água"))))
      (should (equal '(1 1050 800 250) (car (fin-cashflow--year-months 2025)))))))

(provide 'test-cashflow)
;;; test-cashflow.el ends here
