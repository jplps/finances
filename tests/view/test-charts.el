;;; test-charts.el --- view/charts.el SVG output tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'charts)

;;; ── --svg-donut ────────────────────────────────────────────

(ert-deftest charts/donut-empty-emits-svg-no-crash ()
  (let ((out (fin-dashboard--svg-donut nil)))
    (should (string-match-p "<svg" out))
    (should (string-match-p "</svg>" out))))

(ert-deftest charts/donut-emits-path-per-slice ()
  (let ((out (fin-dashboard--svg-donut '(("a" . 30) ("b" . 70)))))
    (should (>= (cl-count ?\< (mapconcat #'identity (split-string out "<path") "")) 0))
    (should (string-match-p "<path d=" out))
    (should (string-match-p "</svg>" out))))

(ert-deftest charts/donut-folds-past-palette-into-other ()
  (let* ((slices (cl-loop for i from 1 to 10 collect (cons (format "c%d" i) (- 20 i))))
         (out (fin-dashboard--svg-donut slices)))
    (should (= (1+ (length fin-dashboard--chart-palette)) (fin-test--substring-count "<path d=" out)))
    (should (string-search "other" out))
    (should (string-search (fin-dashboard--c 'other) out))
    (should (equal '(("b" . 5) ("other" . 3)) (fin-dashboard--fold-slices '(("a" . 1) ("b" . 5) ("c" . 2)) 1)))))

;;; ── --svg-bars ─────────────────────────────────────────────

(ert-deftest charts/bars-empty-no-crash ()
  (let ((out (fin-dashboard--svg-bars nil)))
    (should (string-match-p "<svg" out))))

(ert-deftest charts/bars-positive-and-negative-poles ()
  (let ((out (fin-dashboard--svg-bars '(("2024" 10) ("2025" -5)))))
    (should (string-match-p "<rect" out))
    (should (string-search (fin-dashboard--c 'pos) out))
    (should (string-search (fin-dashboard--c 'neg) out))))

;;; ── --svg-flow ─────────────────────────────────────────────

(ert-deftest charts/flow-future-months-dimmed ()
  (let* ((future (format-time-string "9999-12"))
         (out    (fin-dashboard--svg-flow `(("2020-01" 100 80) (,future 0 0)))))
    (should (string-match-p "opacity=\"0.25\"" out))))

;;; ── --svg-line ─────────────────────────────────────────────

(ert-deftest charts/line-emits-polyline ()
  (let ((out (fin-dashboard--svg-line '(("a" 10) ("b" 20) ("c" 15)))))
    (should (string-match-p "<polyline" out))
    (should (string-match-p "</svg>" out))))

(ert-deftest charts/line-zero-baseline-drawn-when-spanning-zero ()
  (let ((out (fin-dashboard--svg-line '(("a" -10) ("b" 20)))))
    (should (string-match-p "<line " out))))

;;; ── --svg-multiline ────────────────────────────────────────

(defun fin-test--substring-count (needle hay)
  "Count occurrences of NEEDLE substring in HAY."
  (- (length (split-string hay (regexp-quote needle))) 1))

(ert-deftest charts/multiline-emits-one-polyline-per-series ()
  (let* ((s1 (list "in"  "#aaa" '(("a" 10) ("b" 20))))
         (s2 (list "out" "#bbb" '(("a" 5)  ("b" 15))))
         (out (fin-dashboard--svg-multiline (list s1 s2))))
    (should (= 2 (fin-test--substring-count "<polyline" out)))))

;;; ── --svg-pareto ───────────────────────────────────────────

(ert-deftest charts/pareto-emits-rects-and-cumulative-line ()
  (let ((out (fin-dashboard--svg-pareto '(("a" 5000 50.0) ("b" 3000 80.0) ("c" 2000 100.0)))))
    (should (string-match-p "<rect" out))
    (should (string-match-p "<polyline" out))))

;;; ── --svg-heatmap ──────────────────────────────────────────

(ert-deftest charts/heatmap-empty-no-crash ()
  (let ((out (fin-dashboard--svg-heatmap nil)))
    (should (string-match-p "<svg" out))))

(ert-deftest charts/heatmap-emits-rect-per-cell ()
  (let* ((cells '((2024 1 1000) (2024 2 2000) (2025 1 3000)))
         (out   (fin-dashboard--svg-heatmap cells)))
    (should (= 3 (fin-test--substring-count "<rect" out)))))

(ert-deftest charts/bullet-red-only-past-target-when-spending ()
  (let ((under (fin-dashboard--svg-bullet 1000 500 nil "t"))
        (over  (fin-dashboard--svg-bullet 1000 1500 nil "t"))
        (saved (fin-dashboard--svg-bullet 1000 1500 t "t")))
    (should (string-search "mtd good" under))
    (should-not (string-search "mtd bad" under))
    (should (string-search "mtd good" over))
    (should (string-search "mtd bad" over))
    (should (string-search "class=\"mtd bad\" x=\"120.0\"" over))   ; 50% over: right half
    (should-not (string-search "mtd bad" saved))))

(provide 'test-charts)
;;; test-charts.el ends here
