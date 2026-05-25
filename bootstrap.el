;;; bootstrap.el --- Self-locating loader for the finances tool  -*- lexical-binding: t; -*-

;; Drop-in loader.  Put one line in your init.el:
;;   (load "/path/to/finances/bootstrap.el")
;; or, if you symlinked the repo to a stable location:
;;   (load "~/.emacs.d/site-lisp/finances/bootstrap.el")
;;
;; This file figures out the repo root from its own location, wires the
;; load-path, and requires `core'.  Move the repo → only the `load' path
;; above needs to change (and not even that if you symlinked).

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path root)
  (require 'core))

(provide 'finances)
;;; bootstrap.el ends here
