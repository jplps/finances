;;; dashboard.el --- HTML dashboard render + interactive cmd  -*- lexical-binding: t; -*-

(require 'fmt)
(require 'style)
(require 'panels)
(require 'sync)
(require 'stats)

(defcustom fin-dashboard-output-path
  (expand-file-name "dashboard.html"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Where to write the dashboard."
  :type 'file :group 'fin)

(defun fin-dashboard--ymd-diff (start end)
  "Return calendar (Y M D) diff between two ISO date strings."
  (let* ((s (decode-time (date-to-time (concat start "T00:00:00"))))
         (e (decode-time (date-to-time (concat end   "T00:00:00"))))
         (sy (nth 5 s)) (sm (nth 4 s)) (sd (nth 3 s))
         (ey (nth 5 e)) (em (nth 4 e)) (ed (nth 3 e))
         (y (- ey sy)) (m (- em sm)) (d (- ed sd)))
    (when (< d 0) (cl-decf m) (cl-incf d 30))
    (when (< m 0) (cl-decf y) (cl-incf m 12))
    (list y m d)))

(defun fin-dashboard--relative-time (ts)
  "Human-readable relative time from TS (lisp timestamp) to now."
  (let* ((d (float-time (time-subtract (current-time) ts))))
    (cond ((< d 60)    "<1m")
          ((< d 3600)  (format "%dm" (/ d 60)))
          ((< d 86400) (format "%dh" (/ d 3600)))
          ((< d 604800) (format "%dd" (/ d 86400)))
          (t (format-time-string "%Y-%m-%d %H:%M" ts)))))

(defun fin-dashboard--status-bar ()
  "tmux-like bar: one numbered link per panel (accesskey = its number)."
  (concat "<nav class=\"status\">"
          (mapconcat (lambda (i)
                       (pcase-let ((`(,id . ,label) (nth i fin-dashboard--tabs)))
                         (format "<a href=\"#%s\" title=\"%s\" accesskey=\"%d\">%d:%s</a>"
                                 id id (1+ i) (1+ i) label)))
                     (number-sequence 0 (1- (length fin-dashboard--tabs))) "")
          (format "<span class=\"clock\">%s</span></nav>"
                  (downcase (format-time-string "%H:%M %d-%b-%y")))))

(defun fin-dashboard--render ()
  (let* ((ods-ts  (file-attribute-modification-time
                   (file-attributes (expand-file-name fin-ods-path))))
         (period  (fin-report--period))
         (first-d (nth 0 period))
         (last-d  (nth 1 period))
         (entries (nth 3 period))
         (ymd     (and first-d last-d (fin-dashboard--ymd-diff first-d last-d))))
    (concat
     "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     "<title>finances</title>"
     "<style>" (fin-dashboard--style) "</style></head><body>"
     "<header><h1>finances</h1>"
     (format "<span class=\"meta\">%s / %s (%dy %dm) &middot; %d entries &middot; <span title=\"ODS edited\">↻ %s</span> &middot; λ</span>"
             (or (and first-d (substring first-d 0 7)) "?")
             (or (and last-d  (substring last-d  0 7)) "?")
             (or (nth 0 ymd) 0) (or (nth 1 ymd) 0)
             (or entries 0)
             (fin-dashboard--relative-time ods-ts))
     "</header>"
     (fin-dashboard--records-ticker)
     "<main>"
     (fin-dashboard--panel-cockpit)
     (fin-dashboard--panel-wealth)
     (fin-dashboard--panel-stats)
     "</main>"
     (fin-dashboard--status-bar)
     "</body></html>")))

;;;###autoload
(defun fin-dashboard ()
  "Regenerate the HTML dashboard and open it in the default browser."
  (interactive)
  (let ((out (expand-file-name fin-dashboard-output-path)))
    (with-temp-file out
      (insert (fin-dashboard--render)))
    (browse-url (concat "file://" out))
    (message "fin-dashboard: %s" out)
    out))

(provide 'dashboard)
;;; dashboard.el ends here
