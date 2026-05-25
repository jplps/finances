;;; test-budget.el --- domain/budget.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'budget)

;;; ── --monthly-liquid ────────────────────────────────────────

(ert-deftest budget/monthly-liquid-zero-when-empty ()
  (fin-test-with-db
    (should (= 0 (fin-report--monthly-liquid 2025)))))

(ert-deftest budget/monthly-liquid-averages-summary-in-rows ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 600000 nil)
    (fin-test-insert-entry "2025-02-28" "in" "salary" 400000 nil)
    ;; non-summary (item ≠ NULL) must be ignored
    (fin-test-insert-entry "2025-03-31" "in" "salary" 9000000 "side-gig")
    (should (= 500000 (fin-report--monthly-liquid 2025)))))

;;; ── --budget-share ──────────────────────────────────────────

(ert-deftest budget/share-uses-amount-for-fix-and-derives-share ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "house" nil "fix" 200000 nil)
    (let* ((row (car (fin-report--budget-share 2025))))
      (should (equal (nth 0 row) "house"))
      (should (equal (nth 1 row) "fix"))
      (should (= 200000 (nth 2 row)))
      (should (= 20.0 (nth 3 row))))))

(ert-deftest budget/share-uses-share-for-var-and-derives-amount ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.25)
    (let* ((row (car (fin-report--budget-share 2025))))
      (should (= 250000 (nth 2 row)))
      (should (= 25.0   (nth 3 row))))))

;;; ── --runway ────────────────────────────────────────────────

(ert-deftest budget/runway-zero-when-no-fix-budget ()
  (fin-test-with-db
    (fin-test-insert-account "emergency-savings" "emergency" 500000 nil)
    (should (= 0 (nth 2 (fin-report--runway))))))

(ert-deftest budget/runway-divides-emergency-reserve-by-fix-monthly ()
  (fin-test-with-db
    (fin-test-insert-account "emergency-savings" "emergency" 1200000 nil)
    (fin-test-insert-budget "house" nil "fix" 200000 nil)
    (fin-test-insert-budget "food"  nil "fix" 100000 nil)
    (let ((rw (fin-report--runway)))
      (should (= 1200000 (nth 0 rw)))
      (should (= 300000  (nth 1 rw)))
      (should (= 4.0     (nth 2 rw))))))

;;; ── --budget-children (investments parent unions patrimony) ─

(ert-deftest budget/children-of-non-investments-parent ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.4)
    (fin-test-insert-budget "retirement" "investments" "var" 100000 nil)
    (fin-test-insert-budget "free"       "investments" "var" 50000 nil)
    (let ((kids (fin-report--budget-children "investments" 2025)))
      ;; ODS extras present (no patrimony rows so no `patrimony' synthetic).
      (should (cl-find "retirement" kids :test (lambda (k r) (equal (nth 0 r) k))))
      (should (cl-find "free"       kids :test (lambda (k r) (equal (nth 0 r) k)))))))

(ert-deftest budget/children-of-investments-includes-patrimony-rollup ()
  "Investments children must include ONE synthetic `patrimony' leaf summing
all per-row amortizations (amount / lifespan_months)."
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.4)
    (fin-test-insert-budget "retirement" "investments" "var" 100000 nil)
    (fin-test-insert-patrimony "car"   "wheels"  1200000 60)   ; 20000/mo
    (fin-test-insert-patrimony "house" "fridge"  240000 120)   ; 2000/mo
    (let* ((kids (fin-report--budget-children "investments" 2025))
           (patri (cl-find-if (lambda (r) (equal (nth 0 r) "patrimony")) kids)))
      (should patri)
      (should (= 22000 (nth 1 patri))))))

(ert-deftest budget/children-skip-patrimony-rollup-on-zero-lifespan ()
  "NULLIF(lifespan_months,0) guards against /0."
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.4)
    (fin-test-insert-patrimony "junk" "x" 100000 0)
    (let* ((kids (fin-report--budget-children "investments" 2025))
           (patri (cl-find-if (lambda (r) (equal (nth 0 r) "patrimony")) kids)))
      ;; SUM amortization becomes NULL → row may be missing or amt = nil; must NOT crash.
      (should (or (null patri) (null (nth 1 patri)) (zerop (nth 1 patri)))))))

;;; ── --var-total ─────────────────────────────────────────────

(ert-deftest budget/var-total-empty-is-zero ()
  (fin-test-with-db
    (should (= 0 (fin-report--var-total 2025)))))

(ert-deftest budget/var-total-includes-patrimony-amortization ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.2)
    (fin-test-insert-budget "retirement" "investments" "var" 100000 nil)
    (fin-test-insert-patrimony "car" "wheels" 1200000 60) ; 20000/mo
    (let ((total (fin-report--var-total 2025)))
      ;; expected = 100000 (retirement) + 20000 (car amortization)
      (should (= 120000 (round total))))))

;;; ── --planned-month ─────────────────────────────────────────

(ert-deftest budget/planned-month-derives-from-plan ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-01-31" "in" "salary" 1000000 nil)
    (fin-test-insert-budget "house" nil "fix" 200000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.2)
    (fin-test-insert-budget "retirement" "investments" "var" 100000 nil)
    (let ((pm (fin-report--planned-month 2025)))
      (should (= 1000000 (nth 0 pm)))           ; liquid
      (should (= 300000  (nth 1 pm)))           ; out (fix 200k + var 100k)
      (should (= 700000  (nth 2 pm))))))        ; liquid - out

(provide 'test-budget)
;;; test-budget.el ends here
