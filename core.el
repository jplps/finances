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

;; Personal settings (own accounts, salary payer, payee aliases) stay out of
;; the repo, in the gitignored infra/.
(defun fin--load-config ()
  "Load infra/config.el when present."
  (let ((config (expand-file-name "infra/config.el" fin--root)))
    (when (file-readable-p config) (load config nil t))))

(fin--load-config)

(defun fin-reload ()
  "Reload every project source file and infra/config.el so `fin' picks up
edits without restart."
  (interactive)
  (dolist (sub '("adapters" "tools" "domain" "view"))
    (dolist (f (directory-files (expand-file-name sub fin--root) t "\\.el\\'"))
      (load f nil t)))
  (fin--load-config))

;;;###autoload
(defun fin (&optional no-reload)
  "Reload sources, refresh DB from ODS, then regenerate + open the dashboard.
With prefix arg NO-RELOAD, skip the reload (use the already-loaded code)."
  (interactive "P")
  (unless no-reload (fin-reload))
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
