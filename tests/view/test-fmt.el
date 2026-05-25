;;; test-fmt.el --- view/fmt.el unit tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'fmt)

;;; ── --esc ───────────────────────────────────────────────────

(ert-deftest fmt/esc-amp-lt-gt-quote-apos ()
  (should (equal (fin-dashboard--esc "a & b < c > d \" e ' f")
                 "a &amp; b &lt; c &gt; d &quot; e &#39; f"))
  (should (equal (fin-dashboard--esc "") ""))
  (should (equal (fin-dashboard--esc nil) "")))

;;; ── --ym ────────────────────────────────────────────────────

(ert-deftest fmt/ym-strips-day ()
  (should (equal (fin-dashboard--ym "2025-06-30") "2025-06")))

(ert-deftest fmt/ym-short-input-passthrough ()
  (should (equal (fin-dashboard--ym "2025")    "2025"))
  (should (equal (fin-dashboard--ym "2025-06") "2025-06")))

(ert-deftest fmt/ym-nil-returns-empty-string ()
  (should (equal (fin-dashboard--ym nil) "")))

;;; ── --money / --money-str (cents → R$) ─────────────────────

(ert-deftest fmt/money-str-formats-cents ()
  (should (equal (fin-dashboard--money-str 100000)    "1000.00"))
  (should (equal (fin-dashboard--money-str 12345)     "123.45"))
  (should (equal (fin-dashboard--money-str 0)         "0.00")))

;;; ── --k (compact) ──────────────────────────────────────────

(ert-deftest fmt/k-empty-for-nil-and-zero ()
  (should (equal (fin-dashboard--k nil) ""))
  (should (equal (fin-dashboard--k 0)   "")))

(ert-deftest fmt/k-under-1k-uses-money-str ()
  (should (equal (fin-dashboard--k 99999) "999.99")))

(ert-deftest fmt/k-1k-and-above-uses-k-suffix ()
  (should (equal (fin-dashboard--k 100000)  "1.0k"))
  (should (equal (fin-dashboard--k 1234500) "12.3k")))

(ert-deftest fmt/k-negative-wraps-in-neg-span ()
  (let ((out (fin-dashboard--k -100000)))
    (should (string-match-p "class=\"neg\"" out))
    (should (string-match-p "-1.0k" out))))

;;; ── --k-signed (positives marked .pos) ─────────────────────

(ert-deftest fmt/k-signed-positive-wraps-in-pos-span ()
  (let ((out (fin-dashboard--k-signed 100000)))
    (should (string-match-p "class=\"pos\"" out))
    (should (string-match-p "1.0k" out))))

(ert-deftest fmt/k-signed-negative-wraps-in-neg-span ()
  (let ((out (fin-dashboard--k-signed -100000)))
    (should (string-match-p "class=\"neg\"" out))))

;;; ── --pct-signed ────────────────────────────────────────────

(ert-deftest fmt/pct-signed-handles-edges ()
  (should (equal (fin-dashboard--pct-signed nil) ""))
  (should (equal (fin-dashboard--pct-signed 0)   "0.0%"))
  (should (string-match-p "class=\"pos\".*12.3%" (fin-dashboard--pct-signed 12.3)))
  (should (string-match-p "class=\"neg\".*-4.5%" (fin-dashboard--pct-signed -4.5))))

;;; ── --money-cell / --money-signed-cell (table cells) ───────

(ert-deftest fmt/money-cell-returns-raw-plist ()
  (let ((c (fin-dashboard--money-cell 12345)))
    (should (eq (car c) :raw))
    (should (stringp (cadr c)))))

(ert-deftest fmt/money-signed-cell-zero-is-empty-raw ()
  (should (equal (fin-dashboard--money-signed-cell 0)   '(:raw "")))
  (should (equal (fin-dashboard--money-signed-cell nil) '(:raw ""))))

(provide 'test-fmt)
;;; test-fmt.el ends here
