;;; fmt.el --- Palette + string formatters (esc, money, k, fmt)  -*- lexical-binding: t; -*-

(defconst fin-dashboard--palette
  '((bg          . "#000000")
    (panel       . "#0a0a0a")
    (panel-2     . "#050505")
    (border      . "#161616")
    (border-2    . "#222222")
    (text        . "#d4d4d4")
    (muted       . "#6b6b6b")
    (accent      . "#b3b8c1")
    (green       . "#a6e3a1")
    (red         . "#f38ba8")
    (amber       . "#d4c290")
    (magenta     . "#9aa0a6")
    (cyan        . "#8a8d9f"))
  "Theme colors.  Single source of truth.")

(defconst fin-dashboard--month-names
  '("january" "february" "march" "april" "may" "june"
    "july" "august" "september" "october" "november" "december")
  "Lowercase full month names indexed 0..11.")

(defun fin-dashboard--month-name (m)
  (nth (1- m) fin-dashboard--month-names))

(defconst fin-dashboard--chart-palette
  '("#89b4fa" "#a6e3a1" "#f38ba8" "#f9e2af" "#cba6f7"
    "#94e2d5" "#89dceb" "#fab387" "#f5c2e7" "#b4befe")
  "Category palette for pie slices.")

(defconst fin-dashboard--pie-geom
  '(:w 400 :h 260 :cx 200 :cy 130 :r 100 :rl 120
    :lx-right 320 :lx-left 80 :min-label-frac 0.012)
  "Pie chart geometry (SVG units).")

(defun fin-dashboard--c (key) (cdr (assq key fin-dashboard--palette)))
(defun fin-dashboard--g (plist key) (plist-get plist key))

(defun fin-dashboard--esc (s)
  "HTML-escape S.  Handles & < > \" '."
  (if (stringp s)
      (replace-regexp-in-string
       "[&<>\"']"
       (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;")
                          ("\"" "&quot;") ("'" "&#39;")))
       s)
    (format "%s" (or s ""))))

(defun fin-dashboard--money-str (cents)
  "Plain string from CENTS (signed integer).  E.g., '1234.56' or '-12.00'."
  (let* ((neg (< cents 0))
         (a   (abs cents))
         (int (/ a 100))
         (cs  (mod a 100))
         (s   (format "%d.%02d" int cs)))
    (if neg (concat "-" s) s)))

(defun fin-dashboard--money (cents)
  "HTML from CENTS.  Negatives wrapped in <span class=neg>."
  (if (or (null cents) (not (numberp cents)) (zerop cents))
      ""
    (let ((s (fin-dashboard--money-str cents)))
      (if (< cents 0)
          (format "<span class=\"neg\">%s</span>" s)
        s))))

(defun fin-dashboard--k (cents)
  "Format CENTS as `X.Yk' for |cents| ≥ R$ 1.000, else full BRL.  Negatives in .neg."
  (cond ((null cents) "")
        ((not (numberp cents)) (fin-dashboard--esc cents))
        ((zerop cents) "")
        (t (let* ((neg (< cents 0))
                  (a   (abs cents))
                  (s   (if (>= a 100000)
                           (format "%.1fk" (/ a 100000.0))
                         (fin-dashboard--money-str a)))
                  (txt (if neg (concat "-" s) s)))
             (if neg (format "<span class=\"neg\">%s</span>" txt) txt)))))

(defun fin-dashboard--k-cell (v) (list :raw (fin-dashboard--k v)))

(defun fin-dashboard--k-signed (cents)
  "Like `fin-dashboard--k' but also wraps positives in `.pos'."
  (let ((s (fin-dashboard--k cents)))
    (if (and (numberp cents) (> cents 0))
        (format "<span class=\"pos\">%s</span>" s)
      s)))

(defun fin-dashboard--pct-signed-cell (v)
  "Cell for a signed percentage V: wraps neg in `.neg', pos in `.pos'."
  (cond ((null v) "")
        ((not (numberp v)) (list :raw (fin-dashboard--esc v)))
        ((zerop v) (list :raw (format "%.1f" v)))
        ((< v 0) (list :raw (format "<span class=\"neg\">%.1f</span>" v)))
        (t        (list :raw (format "<span class=\"pos\">%.1f</span>" v)))))

(defun fin-dashboard--ym (date)
  "Strip day from ISO date (`YYYY-MM-DD' → `YYYY-MM').  Pass-through if shorter."
  (if (and (stringp date) (>= (length date) 7)) (substring date 0 7) (or date "")))

(defun fin-dashboard--pct-signed (v)
  "Signed percentage HTML: `<span class=neg/pos>%.1f%%</span>'.  Nil/0 → bare."
  (cond ((null v) "")
        ((not (numberp v)) (fin-dashboard--esc v))
        ((zerop v) (format "%.1f%%" v))
        ((< v 0) (format "<span class=\"neg\">%.1f%%</span>" v))
        (t        (format "<span class=\"pos\">%.1f%%</span>" v))))

(defun fin-dashboard--money-cell (cents)
  "Cell wrapper for monetary CENTS."
  (list :raw (fin-dashboard--money cents)))

(defun fin-dashboard--money-signed-cell (cents)
  "Cell for signed CENTS: wraps neg in `.neg', pos in `.pos'."
  (cond ((or (null cents) (not (numberp cents)) (zerop cents)) (list :raw ""))
        ((< cents 0) (list :raw (format "<span class=\"neg\">%s</span>"
                                        (fin-dashboard--money-str cents))))
        (t (list :raw (format "<span class=\"pos\">%s</span>"
                              (fin-dashboard--money-str cents))))))

(defun fin-dashboard--fmt (v)
  "Generic cell formatter.  Numbers → string; :raw → unwrap; strings → escape."
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
