;;; sync.el --- Refresh SQLite from normalized ODS  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'parser)

(defcustom fin-ods-path
  (expand-file-name "../infra/seeds.ods"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Path to the normalized source ODS."
  :type 'file :group 'fin)

(defun fin-sync--sheet-rows (dom name)
  (cdr (fin-ods-rows-trimmed (fin-ods-sheet dom name))))

(defun fin-sync--entries (dom)
  (let (out)
    (dolist (r (fin-sync--sheet-rows dom "entries"))
      (when (car r)
        (push (list (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)
                    (and (numberp (nth 4 r)) (round (nth 4 r)))
                    (or (nth 5 r) "BRL")
                    (nth 6 r))
              out)))
    (nreverse out)))

(defun fin-sync--plan (dom)
  (let (out)
    (dolist (r (fin-sync--sheet-rows dom "plan"))
      (when (car r)
        (push (list (nth 0 r) (nth 1 r) (nth 2 r)
                    (and (numberp (nth 3 r)) (round (nth 3 r)))
                    (and (numberp (nth 4 r)) (float (nth 4 r))))
              out)))
    (nreverse out)))

(defun fin-sync--patrimony (dom)
  (let (out)
    (dolist (r (fin-sync--sheet-rows dom "patrimony"))
      (when (car r)
        (push (list (nth 0 r) (nth 1 r)
                    (and (numberp (nth 2 r)) (round (nth 2 r)))
                    (and (numberp (nth 3 r)) (round (nth 3 r))))
              out)))
    (nreverse out)))

(defun fin-sync--accounts (dom)
  (let (out)
    (dolist (r (fin-sync--sheet-rows dom "accounts"))
      (when (car r)
        (push (list (nth 0 r) (nth 1 r)
                    (or (and (numberp (nth 2 r)) (round (nth 2 r))) 0)
                    (nth 3 r))
              out)))
    (nreverse out)))

(defconst fin-sync--entry-cols
  '("date" "type" "category" "item" "amount" "currency" "note"))

(defconst fin-sync--plan-cols
  '("category" "parent" "type" "amount" "share"))

(defconst fin-sync--patrimony-cols
  '("category" "item" "amount" "lifespan_months"))

(defconst fin-sync--account-cols
  '("category" "parent" "balance" "updated_at"))

(defun fin-sync-refresh ()
  "Mirror `fin-ods-path' into SQLite. Wipes derived tables and re-inserts."
  (interactive)
  (let* ((dom     (fin-ods-parse fin-ods-path))
         (entries (fin-sync--entries dom))
         (plan    (fin-sync--plan dom))
         (patri   (fin-sync--patrimony dom))
         (accts   (fin-sync--accounts dom)))
    (dolist (tbl '("entry" "budget" "patrimony" "account"))
      (fin-db-truncate tbl))
    (fin-db-bulk-insert "entry"     fin-sync--entry-cols     entries)
    (fin-db-bulk-insert "budget"    fin-sync--plan-cols      plan)
    (fin-db-bulk-insert "patrimony" fin-sync--patrimony-cols patri)
    (fin-db-bulk-insert "account"   fin-sync--account-cols   accts)
    (fin-db-exec
     "INSERT INTO refresh_log (ran_at, ods_mtime, entries_in, notes) VALUES (?,?,?,?)"
     (list (format-time-string "%Y-%m-%dT%H:%M:%S")
           (format-time-string "%Y-%m-%dT%H:%M:%S"
                               (file-attribute-modification-time
                                (file-attributes (expand-file-name fin-ods-path))))
           (length entries)
           (format "refresh: entries=%d plan=%d patri=%d accts=%d"
                   (length entries) (length plan) (length patri) (length accts))))
    (message "fin-sync: entries=%d plan=%d patri=%d accts=%d"
             (length entries) (length plan) (length patri) (length accts))
    (length entries)))

(provide 'sync)
;;; sync.el ends here
