;;; bootstrap.el --- Self-locating loader for the finances tool  -*- lexical-binding: t; -*-

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path root)
  (require 'core))

(provide 'finances)
;;; bootstrap.el ends here
