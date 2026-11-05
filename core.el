;;; core.el --- Personal finance: ODS + SQLite + dashboard  -*- lexical-binding: t; -*-

(defconst fin--root
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Project root.")

(dolist (sub '("adapters" "tools" "domain" "view"))
  (add-to-list 'load-path (expand-file-name sub fin--root)))

(require 'db)
(require 'parser)
(require 'sync)
(require 'bankdb)
(require 'ofx)
(require 'reconcile)
(require 'bank)
(require 'conventions)
(require 'odswrite)
(require 'bankfix)
(require 'cashflow)
(require 'budget)
(require 'patrimony)
(require 'accounts)
(require 'stats)
(require 'goals)
(require 'dashboard)

(defun fin--load-config ()
  "Load infra/config.el when present."
  (let ((config (expand-file-name "infra/config.el" fin--root)))
    (when (file-readable-p config) (load config nil t))))

(fin--load-config)

(defun fin-reload ()
  "Reload every project source file and infra/config.el."
  (interactive)
  (load (expand-file-name "core.el" fin--root) nil t)
  (dolist (sub '("adapters" "tools" "domain" "view"))
    (dolist (f (directory-files (expand-file-name sub fin--root) t "\\.el\\'"))
      (load f nil t)))
  (fin--load-config))

(defun fin (&optional no-reload)
  "Reload, import the inbox, fix the ODS from the bank, refresh the DB and
open the dashboard.  With prefix arg NO-RELOAD, skip the reload."
  (interactive "P")
  (unless no-reload (fin-reload))
  (fin-bank-import)
  (fin-bank-fix-settle #'fin-sync-refresh)
  (fin-dashboard))

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
