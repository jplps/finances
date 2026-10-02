;;; charts.el --- SVG charts (donut, bars, flow)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)

(defun fin-dashboard--fold-slices (slices n)
  "SLICES largest first, the N largest kept and the rest summed as \"other\"."
  (let* ((sorted (sort (cl-remove-if-not (lambda (s) (and (cdr s) (> (cdr s) 0))) (copy-sequence slices))
                       (lambda (a b) (> (cdr a) (cdr b)))))
         (rest (nthcdr n sorted)))
    (if (null rest) sorted
      (append (seq-take sorted n) (list (cons "other" (apply #'+ (mapcar #'cdr rest))))))))

(defun fin-dashboard--svg-donut (slices)
  "Pie chart with leader-line labels.  SLICES = ((label . value) ...).
Categorical slots in fixed order; past them one neutral \"other\" slice."
  (let* ((slices   (fin-dashboard--fold-slices slices (length fin-dashboard--chart-palette)))
         (g        fin-dashboard--pie-geom)
         (w        (fin-dashboard--g g :w))
         (h        (fin-dashboard--g g :h))
         (cx       (fin-dashboard--g g :cx))
         (cy       (fin-dashboard--g g :cy))
         (r        (fin-dashboard--g g :r))
         (rl       (fin-dashboard--g g :rl))
         (lx-r     (fin-dashboard--g g :lx-right))
         (lx-l     (fin-dashboard--g g :lx-left))
         (min-lbl  (fin-dashboard--g g :min-label-frac))
         (palette  fin-dashboard--chart-palette)
         (c-text   (fin-dashboard--c 'text))
         (total    (apply #'+ (mapcar (lambda (s) (or (cdr s) 0)) slices)))
         (a        0.0)
         (paths    (list (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart pie\">" w h))))
    (when (> total 0)
      (cl-loop
       for s in slices
       for i from 0
       when (and (cdr s) (> (cdr s) 0))
       do (let* ((frac   (/ (cdr s) (float total)))
                 (a1     a)
                 (a2     (+ a (* 2 float-pi frac)))
                 (mid    (/ (+ a1 a2) 2.0))
                 (large  (if (> frac 0.5) 1 0))
                 (color  (or (nth i palette) (fin-dashboard--c 'other)))
                 (x1 (+ cx (* r (cos a1))))   (y1 (+ cy (* r (sin a1))))
                 (x2 (+ cx (* r (cos a2))))   (y2 (+ cy (* r (sin a2))))
                 (px (+ cx (* r (cos mid))))  (py (+ cy (* r (sin mid))))
                 (pxo (+ cx (* rl (cos mid)))) (pyo (+ cy (* rl (sin mid))))
                 (right? (> (cos mid) 0))
                 (lx     (if right? lx-r lx-l))
                 (anchor (if right? "start" "end"))
                 (tx     (if right? (+ lx 4) (- lx 4))))
            (push (format "<path d=\"M %.2f %.2f L %.2f %.2f A %g %g 0 %d 1 %.2f %.2f Z\" fill=\"%s\"><title>%s · %.1f%%</title></path>"
                          cx cy x1 y1 r r large x2 y2 color
                          (fin-dashboard--esc (car s)) (* 100 frac))
                  paths)
            (when (>= frac min-lbl)
              (push (format "<polyline points=\"%.2f,%.2f %.2f,%.2f %.2f,%.2f\" stroke=\"%s\" stroke-width=\"0.6\" fill=\"none\"/>"
                            px py pxo pyo lx pyo color) paths)
              (push (format "<text x=\"%.2f\" y=\"%.2f\" font-size=\"10.5\" text-anchor=\"%s\" fill=\"%s\" dominant-baseline=\"middle\">%s</text>"
                            tx pyo anchor c-text (fin-dashboard--esc (car s))) paths))
            (setq a a2))))
    (push "</svg>" paths)
    (mapconcat #'identity (nreverse paths) "")))

(defun fin-dashboard--svg-bars (rows)
  "Mini bar chart from baseline.  ROWS = (label value).
Positive: green up; negative: red down."
  (let* ((n      (length rows))
         (vw     200) (vh 80)
         (pad-x  4) (pad-y 8)
         (mid    (/ vh 2))
         (half-h (- mid pad-y))
         (max-v  (max 1.0 (apply #'max 0.0
                                 (mapcar (lambda (r) (abs (or (nth 1 r) 0))) rows))))
         (col-w  (/ (- vw (* 2 pad-x)) (float n)))
         (bar-w  (max 1.0 (* col-w 0.7)))
         (gap    (/ (- col-w bar-w) 2.0))
         (green  (fin-dashboard--c 'pos))
         (red    (fin-dashboard--c 'neg))
         (i      -1))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart bars\" preserveAspectRatio=\"none\">" vw vh)
     (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"0.5\"/>"
             pad-x mid (- vw pad-x) mid (fin-dashboard--c 'grid))
     (mapconcat
      (lambda (r)
        (cl-incf i)
        (let* ((label (nth 0 r))
               (val   (or (nth 1 r) 0))
               (pos   (>= val 0))
               (h     (* half-h (/ (abs val) max-v)))
               (x0    (+ pad-x gap (* i col-w)))
               (y     (if pos (- mid h) mid))
               (color (if pos green red)))
          (format "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\"><title>%s · %.1f%%</title></rect>"
                  x0 y bar-w h color label val)))
      rows "")
     "</svg>")))

(defun fin-dashboard--svg-flow (months)
  "Bar chart of monthly in (green up) + out (red down) from zero baseline.
MONTHS = (ym in out).  Months past current month rendered dimmed."
  (let* ((n      (length months))
         (vw     1200) (vh 120)
         (pad-x  8) (pad-y 4)
         (mid    (/ vh 2))
         (half-h (- mid pad-y))
         (now-ym (format-time-string "%Y-%m"))
         (max-v  (max 1.0
                      (apply #'max 0.0
                             (apply #'append
                                    (mapcar (lambda (r)
                                              (list (or (nth 1 r) 0)
                                                    (or (nth 2 r) 0)))
                                            months)))))
         (col-w  (/ (- vw (* 2 pad-x)) (float n)))
         (bar-w  (max 1.0 (* col-w 0.7)))
         (gap    (/ (- col-w bar-w) 2.0))
         (green  (fin-dashboard--c 'pos))
         (red    (fin-dashboard--c 'neg))
         (i      -1))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart flow\" preserveAspectRatio=\"none\">" vw vh)
     (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"0.5\"/>"
             pad-x mid (- vw pad-x) mid (fin-dashboard--c 'grid))
     (mapconcat
      (lambda (r)
        (cl-incf i)
        (let* ((ym    (nth 0 r))
               (in    (or (nth 1 r) 0))
               (out   (or (nth 2 r) 0))
               (x0    (+ pad-x gap (* i col-w)))
               (h-in  (* half-h (/ (float in)  max-v)))
               (h-out (* half-h (/ (float out) max-v)))
               (op    (if (string> ym now-ym) 0.25 1.0)))
          (concat
           (format "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" opacity=\"%.2f\"><title>%s · in %.2f</title></rect>"
                   x0 (- mid h-in) bar-w h-in green op ym in)
           (format "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" opacity=\"%.2f\"><title>%s · out %.2f</title></rect>"
                   x0 mid bar-w h-out red op ym out))))
      months "")
     "</svg>")))

(defun fin-dashboard--svg-line (points &optional color)
  "Single-series line.  POINTS = (label value)."
  (let* ((vw 600) (vh 120) (pad-x 8) (pad-y 8)
         (n (length points))
         (vals (mapcar (lambda (p) (or (nth 1 p) 0)) points))
         (vmin (apply #'min 0 vals))
         (vmax (apply #'max 0 vals))
         (span (max 1 (- vmax vmin)))
         (col (or color (fin-dashboard--c 'accent)))
         (xstep (/ (- vw (* 2 pad-x)) (float (max 1 (- n 1)))))
         (i -1)
         (coords (mapconcat
                  (lambda (p)
                    (cl-incf i)
                    (let* ((v (or (nth 1 p) 0))
                           (x (+ pad-x (* xstep i)))
                           (y (- vh pad-y (* (- vh (* 2 pad-y))
                                             (/ (- v vmin) (float span))))))
                      (format "%.1f,%.1f" x y)))
                  points " "))
         (zero-y (when (and (< vmin 0) (>= vmax 0))
                   (- vh pad-y (* (- vh (* 2 pad-y))
                                  (/ (- 0 vmin) (float span)))))))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart line\" preserveAspectRatio=\"none\">" vw vh)
     (when zero-y
       (format "<line x1=\"%d\" y1=\"%.1f\" x2=\"%d\" y2=\"%.1f\" stroke=\"%s\" stroke-width=\"0.5\"/>"
               pad-x zero-y (- vw pad-x) zero-y (fin-dashboard--c 'grid)))
     (format "<polyline points=\"%s\" stroke=\"%s\" stroke-width=\"1.5\" fill=\"none\"/>" coords col)
     "</svg>")))

(defun fin-dashboard--svg-multiline (series)
  "Multiple lines.  SERIES = ((name color points) ...) with points = (label value)."
  (let* ((vw 600) (vh 120) (pad-x 8) (pad-y 8)
         (all-vals (apply #'append
                          (mapcar (lambda (s)
                                    (mapcar (lambda (p) (or (nth 1 p) 0)) (nth 2 s)))
                                  series)))
         (vmin (apply #'min 0 all-vals))
         (vmax (apply #'max 1 all-vals))
         (span (max 1 (- vmax vmin)))
         (n-each (length (nth 2 (car series))))
         (xstep (/ (- vw (* 2 pad-x)) (float (max 1 (- n-each 1))))))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart line\" preserveAspectRatio=\"none\">" vw vh)
     (mapconcat
      (lambda (s)
        (let* ((color (nth 1 s))
               (i -1)
               (coords (mapconcat
                        (lambda (p)
                          (cl-incf i)
                          (let* ((v (or (nth 1 p) 0))
                                 (x (+ pad-x (* xstep i)))
                                 (y (- vh pad-y (* (- vh (* 2 pad-y))
                                                   (/ (- v vmin) (float span))))))
                            (format "%.1f,%.1f" x y)))
                        (nth 2 s) " ")))
          (format "<polyline points=\"%s\" stroke=\"%s\" stroke-width=\"1.5\" fill=\"none\"><title>%s</title></polyline>"
                  coords color (fin-dashboard--esc (nth 0 s)))))
      series "")
     "</svg>")))

(defun fin-dashboard--svg-pareto (rows)
  "Bars (per-item spend) + cumulative % line.  ROWS = (item value cum-pct)."
  (let* ((n (length rows))
         (vw 600) (vh 120) (pad-x 8) (pad-y 8)
         (max-v (max 1.0 (apply #'max 0.0
                                (mapcar (lambda (r) (or (nth 1 r) 0)) rows))))
         (col-w (/ (- vw (* 2 pad-x)) (float (max 1 n))))
         (bar-w (max 1.0 (* col-w 0.7)))
         (gap   (/ (- col-w bar-w) 2.0))
         (bar    (fin-dashboard--c 'neg))
         (line   (fin-dashboard--c 'accent))
         (i -1)
         (line-pts
          (mapconcat
           (lambda (r)
             (cl-incf i)
             (let* ((cp (or (nth 2 r) 0))
                    (x  (+ pad-x (* (+ i 0.5) col-w)))
                    (y  (- vh pad-y (* (- vh (* 2 pad-y)) (/ cp 100.0)))))
               (format "%.1f,%.1f" x y)))
           rows " "))
         (i2 -1))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart pareto\" preserveAspectRatio=\"none\">" vw vh)
     (mapconcat
      (lambda (r)
        (cl-incf i2)
        (let* ((item (nth 0 r))
               (v    (or (nth 1 r) 0))
               (cp   (or (nth 2 r) 0))
               (h    (* (- vh (* 2 pad-y)) (/ v max-v)))
               (x0   (+ pad-x gap (* i2 col-w)))
               (y    (- vh pad-y h)))
          (format "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\"><title>%s · R$ %.2f · cum %.1f%%</title></rect>"
                  x0 y bar-w h bar (fin-dashboard--esc item) (/ v 100.0) cp)))
      rows "")
     (format "<polyline points=\"%s\" stroke=\"%s\" stroke-width=\"2\" fill=\"none\"/>" line-pts line)
     "</svg>")))

(defun fin-dashboard--svg-heatmap (cells)
  "Year × month grid colored by spend intensity.  CELLS = (year month value).
Spend magnitude: the out hue, light to dark."
  (let* ((years (sort (delete-dups (mapcar (lambda (c) (nth 0 c)) cells)) #'<))
         (ny (max 1 (length years)))
         (pad 12) (cell-h 14) (cell-gap 2)
         (vw 600)
         (vh (+ (* 2 pad) (* cell-h ny)))
         (cell-w (/ (- vw (* 2 pad)) 12.0))
         (max-v (max 1.0 (apply #'max 0.0
                                (mapcar (lambda (c) (or (nth 2 c) 0)) cells))))
         (log-max (log (+ 1.0 max-v)))
         (yidx (let ((h (make-hash-table)) (i 0))
                 (dolist (y years) (puthash y i h) (cl-incf i)) h))
         (hue (fin-dashboard--c 'neg)))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"chart heatmap\" preserveAspectRatio=\"none\">" vw vh)
     (mapconcat
      (lambda (c)
        (let* ((y (nth 0 c)) (m (nth 1 c)) (v (or (nth 2 c) 0))
               (row (gethash y yidx))
               (x   (+ pad (* (- m 1) cell-w)))
               (yy  (+ pad (* row cell-h)))
               (op  (max 0.08 (/ (log (+ 1.0 v)) log-max))))
          (format "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" opacity=\"%.2f\"><title>%d-%02d · R$ %.2f</title></rect>"
                  x yy (- cell-w cell-gap) (- cell-h cell-gap) hue op y m (/ v 100.0))))
      cells "")
     "</svg>")))

(defconst fin-dashboard--bullet-geom '(:w 240 :h 16 :bar 10)
  "Bullet chart geometry (SVG units).")

(defun fin-dashboard--svg-bullet (target mtd save title)
  "Progress bar: the full width is TARGET; MTD fills it green.  Spending
past TARGET grows red from the right end, by the overrun's share of
TARGET; a saving row (SAVE) never turns red.  TITLE is the hover text.
Amounts in cents."
  (let* ((g (fin-dashboard--g fin-dashboard--bullet-geom :w))
         (h (fin-dashboard--g fin-dashboard--bullet-geom :h))
         (bh (fin-dashboard--g fin-dashboard--bullet-geom :bar))
         (frac (lambda (v) (if (> target 0) (min 1.0 (/ (max 0 v) (float target))) (if (> v 0) 1.0 0.0))))
         (fill (funcall frac mtd))
         (over (if save 0.0 (funcall frac (- mtd (max 0 target)))))
         (rect (lambda (cls x w color)
                 (format "<rect class=\"%s\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%d\" rx=\"1\" fill=\"%s\"/>"
                         cls x (/ (- h bh) 2.0) w bh color))))
    (concat
     (format "<svg viewBox=\"0 0 %d %d\" xmlns=\"http://www.w3.org/2000/svg\" class=\"bullet\" preserveAspectRatio=\"none\" role=\"img\"><title>%s</title>"
             g h (fin-dashboard--esc title))
     (funcall rect "track" 0 g (fin-dashboard--c 'track))
     (when (> fill 0)
       (funcall rect "mtd good" 0 (max 2 (* g fill)) (fin-dashboard--c 'pos)))
     (when (> over 0)
       (let ((w (max 2 (* g over))))
         (funcall rect "mtd bad" (- g w) w (fin-dashboard--c 'neg))))
     "</svg>")))

(provide 'charts)
;;; charts.el ends here
