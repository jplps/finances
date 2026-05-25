;;; test-cashflow.el --- domain/cashflow.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'cashflow)

(ert-deftest cashflow/years-distinct-desc ()
  (fin-test-with-db
    (fin-test-insert-entry "2023-06-30" "in" "salary" 100 nil)
    (fin-test-insert-entry "2024-06-30" "in" "salary" 100 nil)
    (fin-test-insert-entry "2023-12-31" "in" "salary" 100 nil)
    (should (equal '(2024 2023) (fin-report--years)))))

(ert-deftest cashflow/year-months-pads-missing-with-12-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "in" "salary" 100000 nil)
    (let ((rows (fin-report--year-months 2025)))
      (should (= 12 (length rows)))
      ;; June (m=6) has data; rest nil.
      (should (equal 6 (car (nth 5 rows))))
      (should (= 100000 (nth 1 (nth 5 rows))))
      (should (null (nth 1 (nth 0 rows)))))))    ; Jan empty

(ert-deftest cashflow/month-items-only-non-summary-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-10" "out" "food" 100 nil)       ; summary
    (fin-test-insert-entry "2025-06-15" "out" "food" 200 "bread")
    (fin-test-insert-entry "2025-06-20" "out" "food" 300 "butter")
    (let ((rows (fin-report--month-items 2025 6)))
      (should (= 2 (length rows)))
      (should (equal "bread"  (nth 0 (car rows))))
      (should (equal "butter" (nth 0 (cadr rows)))))))

(ert-deftest cashflow/annual-sums-derives-save-pct ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2025-06-30" "out" "food"   400  nil)
    (let* ((row (car (fin-report--annual-sums))))
      (should (equal 2025 (nth 0 row)))
      (should (= 1000 (nth 1 row)))
      (should (= 400  (nth 2 row)))
      (should (= 600  (nth 3 row)))
      (should (= 60.0 (nth 4 row))))))

(ert-deftest cashflow/annual-sums-no-income-yields-nil-save-pct ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "out" "food" 100 nil)
    (let ((row (car (fin-report--annual-sums))))
      (should (null (nth 4 row))))))

(ert-deftest cashflow/monthly-flow-orders-asc-by-ym ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-03-31" "in" "salary" 300 nil)
    (fin-test-insert-entry "2025-01-31" "in" "salary" 100 nil)
    (fin-test-insert-entry "2025-02-28" "in" "salary" 200 nil)
    (let ((yms (mapcar #'car (fin-report--monthly-flow))))
      (should (equal '("2025-01" "2025-02" "2025-03") yms)))))

(provide 'test-cashflow)
;;; test-cashflow.el ends here
