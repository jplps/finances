;;; test-stats.el --- domain/stats.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'stats)

;;; ── empty-db sanity ────────────────────────────────────────

(ert-deftest stats/all-queries-no-crash-on-empty-db ()
  (fin-test-with-db
    (should (null (fin-report--cat-shares)))
    (should (null (fin-report--income-shares)))
    (should (null (fin-report--top-recurring 5 6)))
    (should (null (fin-report--top-by-count 5 3)))
    (should (null (fin-report--yearly-saves)))
    (should (null (fin-report--cumulative-networth)))
    (should (null (fin-report--rolling-save-rate)))
    (should (null (fin-report--rolling-flow)))
    (should (null (fin-report--pareto-spending 5)))
    (should (null (fin-report--monthly-spend-grid)))
    (should (fin-report--records))))      ; plist; values may be nil

;;; ── --cat-shares / --income-shares (summary rows only) ────

(ert-deftest stats/cat-shares-sums-all-out-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "out" "food" 100000 nil)     ; un-itemized lump
    (fin-test-insert-entry "2025-01-31" "out" "food" 50000 "bread")  ; itemized — every row counts
    (let ((row (car (fin-report--cat-shares))))
      (should (equal (nth 0 row) "food"))
      (should (= 150000 (nth 1 row))))))

;;; ── --period (future months excluded) ─────────────────────

(ert-deftest stats/period-excludes-future-months ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "in" "salary" 100000 nil)
    (fin-test-insert-entry "2099-12-31" "out" "food"  100000 nil)
    (let ((p (fin-report--period)))
      ;; last date must NOT include the 2099 placeholder.
      (should (string< (nth 1 p) "2099-01-01")))))

;;; ── --yearly-saves (NULLIF guards /0 when no income) ─────

(ert-deftest stats/yearly-saves-skips-years-with-zero-income ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-30" "out" "food" 50000 nil)
    (let ((rows (fin-report--yearly-saves)))
      ;; year 2025 row exists but save rate is NULL → second col nil
      (should (= 1 (length rows)))
      (should (null (nth 1 (car rows)))))))

;;; ── --top-by-count (raw entry count) ─────────────────────

(ert-deftest stats/top-by-count-ranks-by-entry-count ()
  (fin-test-with-db
    (dotimes (_ 5) (fin-test-insert-entry "2025-01-01" "out" "food" 1000 "coffee"))
    (dotimes (_ 2) (fin-test-insert-entry "2025-01-02" "out" "food" 5000 "rare"))
    (let ((rows (fin-report--top-by-count 10 1)))
      (should (equal "coffee" (nth 0 (car rows))))
      (should (= 5 (nth 1 (car rows)))))))

;;; ── --cumulative-networth (running sum) ───────────────────

(ert-deftest stats/cumulative-networth-runs-monotonically ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 100000 nil)
    (fin-test-insert-entry "2025-02-28" "in"  "salary" 100000 nil)
    (fin-test-insert-entry "2025-02-28" "out" "food"    30000 nil)
    (let ((rows (fin-report--cumulative-networth)))
      (should (= 100000 (nth 1 (nth 0 rows))))   ; Jan
      (should (= 170000 (nth 1 (nth 1 rows))))))) ; Jan + Feb-net (70k)

;;; ── --pareto-spending (cumulative %) ──────────────────────

(ert-deftest stats/pareto-cumulative-pct-reaches-100 ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "out" "food" 80000 "a")
    (fin-test-insert-entry "2025-01-02" "out" "food" 20000 "b")
    (let* ((rows (fin-report--pareto-spending 10))
           (last (car (last rows))))
      (should (equal "a" (nth 0 (car rows))))
      (should (= 100.0 (nth 2 last))))))

;;; ── --records (headline facts) ────────────────────────────

(ert-deftest stats/records-biggest-out-and-in ()
  (fin-test-with-db
    (fin-test-insert-entry "2024-01-01" "in"  "salary" 500000 nil)
    (fin-test-insert-entry "2025-06-15" "out" "house"  300000 "rent")
    (let* ((r (fin-report--records))
           (biggest    (plist-get r :biggest))
           (biggest-in (plist-get r :biggest-in)))
      (should (equal "rent"   (nth 0 biggest)))
      (should (= 300000       (nth 1 biggest)))
      (should (equal "salary" (nth 0 biggest-in)))
      (should (= 500000       (nth 1 biggest-in))))))

;;; ── transfers (savings moves) ───────────────────────────────

(ert-deftest stats/flow-metrics-exclude-transfers ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in"  "salary"      1000 nil)
    (fin-test-insert-entry "2025-01-31" "in"  "reserve"     50   nil)
    (fin-test-insert-entry "2025-01-31" "out" "food"        400  nil)
    (fin-test-insert-entry "2025-01-31" "out" "investments" 300  nil)
    (should (equal '(("food" 400)) (fin-report--cat-shares)))
    (should (equal '(("salary" 1000)) (fin-report--income-shares)))
    (should (equal '((2025 60.0)) (fin-report--yearly-saves)))
    (should (equal '(("2025-01" 600)) (fin-report--cumulative-networth)))
    (should (equal '((2025 1 400)) (fin-report--monthly-spend-grid)))))

;;; ── rolling windows (calendar months) ──────────────────────

(ert-deftest stats/rolling-windows-span-calendar-months ()
  "A year without entries still fills the 12-month window."
  (fin-test-with-db
    (fin-test-insert-entry "2024-01-31" "in"  "salary" 1200 nil)
    (fin-test-insert-entry "2025-01-31" "in"  "salary" 1200 nil)
    (fin-test-insert-entry "2025-01-31" "out" "food"   600  nil)
    (let ((flow (fin-report--rolling-flow))
          (save (fin-report--rolling-save-rate)))
      (should (= 13 (length flow)))
      (should (equal '("2024-12" 100 0) (nth 11 flow)))  ; Jan/24 + 11 empty
      (should (equal '("2025-01" 100 50) (car (last flow)))) ; Feb/24..Jan/25
      (should (equal '("2025-01" 50.0) (car (last save)))))))

(ert-deftest stats/rolling-stops-at-current-month ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 100 nil)
    (fin-test-insert-entry "2099-12-31" "in" "salary" 100 nil)
    (should (string< (caar (last (fin-report--rolling-flow))) "2099"))))

;;; ── --pareto-spending (of total out) ───────────────────────

(ert-deftest stats/pareto-pct-is-of-total-out ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-01" "out" "food"        50  "a")
    (fin-test-insert-entry "2025-01-01" "out" "food"        50  nil)      ; un-itemized
    (fin-test-insert-entry "2025-01-01" "out" "investments" 900 "car")    ; transfer
    (should (equal '(("a" 50 50.0)) (fin-report--pareto-spending 10)))))

;;; ── --records (months, yearly save) ────────────────────────

(ert-deftest stats/records-best-month-excludes-current ()
  (fin-test-with-db
    (fin-test-insert-entry (format-time-string "%Y-%m-01") "in" "salary" 900000 nil)
    (fin-test-insert-entry "2020-01-31" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2020-02-29" "out" "food"   1000 nil)
    (let ((r (fin-report--records)))
      (should (equal '("2020-01" 1000) (plist-get r :best-mo)))
      (should (equal '("2020-02" -1000) (plist-get r :worst-mo))))))

(ert-deftest stats/records-avg-save-weighted-by-income ()
  "100% of 1000 and 0% of 9000 average to 10%, not 50%."
  (fin-test-with-db
    (fin-test-insert-entry "2023-06-30" "in"  "salary" 1000 nil)
    (fin-test-insert-entry "2024-06-30" "in"  "salary" 9000 nil)
    (fin-test-insert-entry "2024-06-30" "out" "food"   9000 nil)
    (should (= 10.0 (plist-get (fin-report--records) :avg-save)))))

(provide 'test-stats)
;;; test-stats.el ends here
