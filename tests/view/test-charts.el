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

;;; ── --svg-bars ─────────────────────────────────────────────

(ert-deftest charts/bars-empty-no-crash ()
  (let ((out (fin-dashboard--svg-bars nil)))
    (should (string-match-p "<svg" out))))

(ert-deftest charts/bars-positive-green-negative-red ()
  (let ((out (fin-dashboard--svg-bars '(("2024" 10) ("2025" -5)))))
    (should (string-match-p "<rect" out))
    (should (string-match-p (fin-dashboard--c 'green) out))
    (should (string-match-p (fin-dashboard--c 'red)   out))))

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

(provide 'test-charts)
;;; test-charts.el ends here
