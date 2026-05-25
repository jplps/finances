;;; all-tests.el --- ERT entrypoint: requires every test file  -*- lexical-binding: t; -*-

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path dir)
  (dolist (sub '("adapters" "domain" "tools" "view"))
    (add-to-list 'load-path (expand-file-name sub dir))))

(require 'helpers)

;; Mirror app layout: tests/<layer>/test-<file>.el provides `test-<file>`.
(require 'test-db)
(require 'test-parser)
(require 'test-budget)
(require 'test-stats)
(require 'test-patrimony)
(require 'test-accounts)
(require 'test-cashflow)
(require 'test-sync)
(require 'test-fmt)
(require 'test-charts)
(require 'test-dashboard)

(provide 'all-tests)
;;; all-tests.el ends here
