;;; fmt.el --- Palette + string formatters (esc, money, k, fmt)  -*- lexical-binding: t; -*-

(defconst fin-dashboard--palette
  '((page      "#f3f1ea" "#0c0c0c")
    (surface   "#fbfaf5" "#0c0c0c")
    (surface-2 "#ecebe3" "#1a1a1a")
    (border    "#d8d6cc" "#303030")
    (grid      "#c9c7bc" "#3a3a3a")
    (track     "#e4e2d8" "#1e1e1e")
    (text      "#111111" "#cccccc")
    (text-2    "#3d3c38" "#a8a8a8")
    (muted     "#5f5e58" "#8a8a8a")
    (accent    "#2f6f6a" "#8abeb7")
    (accent-2  "#8a6d00" "#f0c674")
    (pos       "#1d7a24" "#a6e3a1")
    (neg       "#b4233c" "#f38ba8")
    (line-2    "#8a6d00" "#d4c290")
    (good      "#1d7a24" "#a6e3a1")
    (warn      "#8a6d00" "#d4c290")
    (bad       "#b4233c" "#f38ba8")
    (good-text "#1d7a24" "#a6e3a1")
    (bad-text  "#b4233c" "#f38ba8")
    (other     "#8a8984" "#4d5566")
    (cat-1     "#eb6834" "#d95926")
    (cat-2     "#1baf7a" "#199e70")
    (cat-3     "#4a3aa7" "#9085e9")
    (cat-4     "#eda100" "#c98500")
    (cat-5     "#e87ba4" "#d55181")
    (cat-6     "#2a78d6" "#3987e5"))
  "Theme roles: (ROLE LIGHT DARK).")

(defconst fin-dashboard--month-names
  '("january" "february" "march" "april" "may" "june"
    "july" "august" "september" "october" "november" "december")
  "Lowercase month names, January first.")

(defun fin-dashboard--month-name (m)
  (nth (1- m) fin-dashboard--month-names))

(defconst fin-dashboard--chart-palette
  (mapcar (lambda (i) (format "var(--cat-%d)" i)) '(1 2 3 4 5 6))
  "Categorical slots; series past these fold into one neutral `other' slice.")

(defconst fin-dashboard--pie-geom
  '(:w 400 :h 260 :cx 200 :cy 130 :r 100 :rl 120
    :lx-right 320 :lx-left 80 :min-label-frac 0.012)
  "Pie chart geometry (SVG units).")

(defun fin-dashboard--c (key)
  "CSS value of theme role KEY, as var(--KEY)."
  (unless (assq key fin-dashboard--palette) (error "fin-dashboard: no color role %S" key))
  (format "var(--%s)" key))
(defun fin-dashboard--g (plist key) (plist-get plist key))

(defun fin-dashboard--esc (s)
  "HTML-escape S; a non-string is printed, nil as empty."
  (if (stringp s)
      (replace-regexp-in-string
       "[&<>\"']"
       (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;")
                          ("\"" "&quot;") ("'" "&#39;")))
       s)
    (format "%s" (or s ""))))

(defun fin-dashboard--group (n)
  "Non-negative integer N with comma thousands: 1234567 → \"1,234,567\"."
  (let ((s (number-to-string n)) (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out) s (substring s 0 -3)))
    (concat s out)))

(defun fin-dashboard--money-str (cents)
  "CENTS as 1,234.56, signed."
  (let* ((neg (< cents 0))
         (a   (abs cents))
         (s   (format "%s.%02d" (fin-dashboard--group (/ a 100)) (mod a 100))))
    (if neg (concat "-" s) s)))

(defun fin-dashboard--money (cents)
  "HTML of CENTS, empty for nil or zero, negatives in .neg."
  (if (or (null cents) (not (numberp cents)) (zerop cents))
      ""
    (let ((s (fin-dashboard--money-str cents)))
      (if (< cents 0)
          (format "<span class=\"neg\">%s</span>" s)
        s))))

(defun fin-dashboard--k (cents)
  "HTML of CENTS, empty for nil or zero, negatives in .neg; else escaped."
  (cond ((null cents) "")
        ((not (numberp cents)) (fin-dashboard--esc cents))
        ((zerop cents) "")
        (t (let* ((neg (< cents 0))
                  (a   (abs cents))
                  (s   (fin-dashboard--money-str a))
                  (txt (if neg (concat "-" s) s)))
             (if neg (format "<span class=\"neg\">%s</span>" txt) txt)))))

(defun fin-dashboard--k-cell (v) (list :raw (fin-dashboard--k v)))

(defun fin-dashboard--k-signed (cents)
  "Like `fin-dashboard--k', positives in .pos."
  (let ((s (fin-dashboard--k cents)))
    (if (and (numberp cents) (> cents 0))
        (format "<span class=\"pos\">%s</span>" s)
      s)))

(defun fin-dashboard--ym (date)
  "mm/yyyy of ISO DATE or YYYY-MM; shorter strings pass through, nil as empty."
  (if (and (stringp date) (>= (length date) 7))
      (format "%s/%s" (substring date 5 7) (substring date 0 4))
    (or date "")))

(defun fin-dashboard--date (iso)
  "ISO date as dd/mm/yyyy."
  (format "%s/%s" (fin-dashboard--dm iso) (substring iso 0 4)))

(defun fin-dashboard--dm (date)
  "ISO DATE as dd/mm, empty when shorter."
  (if (and (stringp date) (>= (length date) 10))
      (concat (substring date 8 10) "/" (substring date 5 7))
    ""))

(defun fin-dashboard--pct-signed (v)
  "Percentage V to one decimal, in .neg or .pos by sign; nil as empty."
  (cond ((null v) "")
        ((not (numberp v)) (fin-dashboard--esc v))
        ((zerop v) (format "%.1f%%" v))
        ((< v 0) (format "<span class=\"neg\">%.1f%%</span>" v))
        (t        (format "<span class=\"pos\">%.1f%%</span>" v))))

(defun fin-dashboard--money-cell (cents)
  (list :raw (fin-dashboard--money cents)))

(defun fin-dashboard--money-signed-cell (cents)
  "Cell of CENTS in .neg or .pos by sign, empty for nil or zero."
  (cond ((or (null cents) (not (numberp cents)) (zerop cents)) (list :raw ""))
        ((< cents 0) (list :raw (format "<span class=\"neg\">%s</span>"
                                        (fin-dashboard--money-str cents))))
        (t (list :raw (format "<span class=\"pos\">%s</span>"
                              (fin-dashboard--money-str cents))))))

(defun fin-dashboard--fmt (v)
  "Cell HTML of V: (:raw HTML) as is, nil, zero and \"\" empty, else escaped."
  (cond ((null v) "")
        ((and (consp v) (eq (car v) :raw)) (cadr v))
        ((and (numberp v) (zerop v)) "")
        ((floatp v)
         (if (< v 0)
             (format "<span class=\"neg\">%.2f</span>" v)
           (format "%.2f" v)))
        ((numberp v)
         (if (< v 0)
             (format "<span class=\"neg\">%d</span>" v)
           (format "%d" v)))
        ((and (stringp v) (string-empty-p v)) "")
        (t (fin-dashboard--esc v))))

(provide 'fmt)
;;; fmt.el ends here
