;;; core.el --- Personal finance: ODS + SQLite + dashboard  -*- lexical-binding: t; -*-

(defconst fin--root
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Project root.")

(dolist (sub '("adapters" "tools" "domain" "view"))
  (add-to-list 'load-path (expand-file-name sub fin--root)))

(require 'db)
(require 'parser)
(require 'sync)
(require 'cashflow)
(require 'budget)
(require 'patrimony)
(require 'accounts)
(require 'stats)
(require 'dashboard)

;;;###autoload
(defun fin ()
  "Refresh DB from ODS and regenerate + open the dashboard."
  (interactive)
  (fin-sync-refresh)
  (fin-dashboard))

;;;###autoload
(defun fin-open-ods ()
  "Open the source ODS in the system default application."
  (interactive)
  (let ((path (expand-file-name fin-ods-path)))
    (cond
     ((eq system-type 'darwin)    (start-process "fin-ods" nil "open" path))
     ((eq system-type 'gnu/linux) (start-process "fin-ods" nil "xdg-open" path))
     (t (find-file path)))))

(provide 'core)
;;; core.el ends here
