;;; test-stats.el --- domain/stats.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'stats)

(ert-deftest stats/all-queries-no-crash-on-empty-db ()
  (fin-test-with-db
    (should (null (fin-stats--cat-shares)))
    (should (null (fin-stats--income-shares)))
    (should (null (fin-stats--top-recurring 5 6)))
    (should (null (fin-stats--top-by-count 5 3)))
    (should (null (fin-stats--cumulative-networth)))
    (should (null (fin-stats--rolling-save-rate)))
    (should (null (fin-stats--rolling-flow)))
    (should (null (fin-stats--pareto-spending 5)))
    (should (null (fin-stats--monthly-spend-grid)))
    (should (fin-stats--records))))

(ert-deftest stats/cat-shares-sums-all-out-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "out" "food" 100000 nil)
    (fin-test-insert-entry "2025-01-31" "out" "food" 50000 "bread")
    (let ((row (car (fin-stats--cat-shares))))
      (should (equal (nth 0 row) "food"))
      (should (= 150000 (nth 1 row))))))

(ert-deftest stats/period-excludes-future-months ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "in" "salary" 100000 nil)
    (fin-test-insert-entry "2099-12-31" "out" "food"  100000 nil)
    (let ((p (fin-stats--period)))
      (should (string< (nth 1 p) "2099-01-01")))))

(ert-deftest stats/top-by-count-ranks-by-entry-count ()
  (fin-test-with-db
    (dotimes (_ 5) (fin-test-insert-entry "2025-01-01" "out" "food" 1000 "coffee"))
    (dotimes (_ 2) (fin-test-insert-entry "2025-01-02" "out" "food" 5000 "rare"))
    (let ((rows (fin-stats--top-by-count 10 1)))
      (should (equal "coffee" (nth 0 (car rows))))
      (should (= 5 (nth 1 (car rows)))))))

(ert-deftest stats/top-items-skip-savings-and-future-rows ()
  (fin-test-with-db
    (dotimes (i 6)
      (fin-test-insert-entry (format "2025-%02d-01" (1+ i)) "out" "investments" 50000 "crypto")
      (fin-test-insert-entry (format "2025-%02d-02" (1+ i)) "out" "food" 1000 "coffee"))
    (fin-test-insert-entry "2099-01-01" "out" "food" 1000 "future")
    (should (equal '("coffee") (mapcar #'car (fin-stats--top-recurring 10 6))))
    (should (equal '("coffee") (mapcar #'car (fin-stats--top-by-count 10 1))))
    (should (equal '(("food" 6000)) (fin-stats--cat-shares)))))

(ert-deftest stats/cumulative-networth-runs-monotonically ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 100000 nil)
    (fin-test-insert-entry "2025-02-28" "in"  "salary" 100000 nil)
    (fin-test-insert-entry "2025-02-28" "out" "food"    30000 nil)
    (let ((rows (fin-stats--cumulative-networth)))
      (should (= 100000 (nth 1 (nth 0 rows))))
      (should (= 170000 (nth 1 (nth 1 rows)))))))

(ert-deftest stats/pareto-cumulative-pct-reaches-100 ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "out" "food" 80000 "a")
    (fin-test-insert-entry "2025-01-02" "out" "food" 20000 "b")
    (let* ((rows (fin-stats--pareto-spending 10))
           (last (car (last rows))))
      (should (equal "a" (nth 0 (car rows))))
      (should (= 100.0 (nth 2 last))))))

(ert-deftest stats/records-biggest-out-and-in ()
  (fin-test-with-db
    (fin-test-insert-entry "2024-01-01" "in"  "salary" 500000 nil)
    (fin-test-insert-entry "2025-06-15" "out" "house"  300000 "rent")
    (let* ((r (fin-stats--records))
           (biggest    (plist-get r :biggest))
           (biggest-in (plist-get r :biggest-in)))
      (should (equal "rent"   (nth 0 biggest)))
      (should (= 300000       (nth 1 biggest)))
      (should (equal "salary" (nth 0 biggest-in)))
      (should (= 500000       (nth 1 biggest-in))))))

(ert-deftest stats/flow-metrics-exclude-transfers ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary"      1000 nil)
    (fin-test-insert-entry "2025-01-31" "in"  "reserve"     50   nil)
    (fin-test-insert-entry "2025-01-31" "out" "food"        400  nil)
    (fin-test-insert-entry "2025-01-31" "out" "investments" 300  nil)
    (should (equal '(("food" 400)) (fin-stats--cat-shares)))
    (should (equal '(("salary" 1000)) (fin-stats--income-shares)))
    (should (equal '(("2025-01" 600)) (fin-stats--cumulative-networth)))
    (should (equal '((2025 1 400)) (fin-stats--monthly-spend-grid)))))

(ert-deftest stats/rolling-windows-span-calendar-months ()
  "A year without entries still fills the 12-month window."
  (fin-test-with-db
    (fin-test-insert-entry "2024-01-31" "in"  "salary" 1200 nil)
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 1200 nil)
    (fin-test-insert-entry "2025-01-31" "out" "food"   600  nil)
    (let ((flow (fin-stats--rolling-flow))
          (save (fin-stats--rolling-save-rate)))
      (should (= 13 (length flow)))
      (should (equal '("2024-12" 100 0) (nth 11 flow)))
      (should (equal '("2025-01" 100 50) (car (last flow))))
      (should (equal '("2025-01" 50.0) (car (last save)))))))

(ert-deftest stats/rolling-stops-at-current-month ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 100 nil)
    (fin-test-insert-entry "2099-12-31" "in" "salary" 100 nil)
    (should (string< (caar (last (fin-stats--rolling-flow))) "2099"))))

(ert-deftest stats/pareto-pct-is-of-total-out ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "out" "food"        50  "a")
    (fin-test-insert-entry "2025-01-01" "out" "food"        50  nil)
    (fin-test-insert-entry "2025-01-01" "out" "investments" 900 "car")
    (should (equal '(("a" 50 50.0)) (fin-stats--pareto-spending 10)))))

(ert-deftest stats/records-best-month-excludes-current ()
  (fin-test-with-db
    (fin-test-insert-entry (format-time-string "%Y-%m-01") "in" "salary" 900000 nil)
    (fin-test-insert-entry "2020-01-31" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2020-02-29" "out" "food"   1000 nil)
    (let ((r (fin-stats--records)))
      (should (equal '("2020-01" 1000) (plist-get r :best-mo)))
      (should (equal '("2020-02" -1000) (plist-get r :worst-mo))))))

(ert-deftest stats/records-avg-save-weighted-by-income ()
  "100% of 1000 and 0% of 9000 average to 10%, not 50%."
  (fin-test-with-db
    (fin-test-insert-entry "2023-06-30" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2024-06-30" "in"  "salary" 9000 nil)
    (fin-test-insert-entry "2024-06-30" "out" "food"   9000 nil)
    (should (= 10.0 (plist-get (fin-stats--records) :avg-save)))))

(provide 'test-stats)
;;; test-stats.el ends here
