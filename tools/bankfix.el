;;; bankfix.el --- Fix the ODS ledger from imported bank rows  -*- lexical-binding: t; -*-

;; `fin-bank-fix' reconciles every bank row since `fin-bank-fix-from'
;; against the ledger, turns the differences into ledger changes per
;; conventions.el, writes them to the ODS (with a backup) and shows what
;; it did.  It never touches the DB: run `fin' afterwards.  Running it
;; again changes nothing, since the fixed ledger then matches the bank.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'reconcile)
(require 'conventions)
(require 'odswrite)
(require 'bank)

(defvar fin-ods-path)

(defcustom fin-bank-fix-from "2024-01-01"
  "First date fixed from the bank: account and card statements both cover
it, and the ledger lists single purchases from there on."
  :type 'string :group 'fin)

(defun fin-bank-fix--history ()
  "(category item count) of every itemized outflow in the ledger."
  (fin-db-query
   "SELECT category, item, COUNT(*) FROM entry
     WHERE type = 'out' AND item IS NOT NULL GROUP BY category, item"))

(defun fin-bank-fix--check-closed (path)
  "Signal if LibreOffice holds PATH open: its save would undo the fix."
  (let ((lock (expand-file-name (concat ".~lock." (file-name-nondirectory path) "#")
                                (file-name-directory (expand-file-name path)))))
    (when (file-exists-p lock)
      (user-error "fin-bank: close %s in LibreOffice first" (file-name-nondirectory path)))))

(defun fin-bank-fix--reconcile (from)
  "Reconcile bank rows since FROM.  Return plist of match buckets and
:notes (normalize removals), :history."
  (let* ((norm   (fin-conv-normalize (fin-bankdb-since from)))
         (bank   (cl-remove-if #'fin-bank--ignored-p (plist-get norm :rows)))
         (span   (fin-bank--date-span bank))
         (raw    (fin-bank--ledger-span span (max fin-reconcile-shift-window
                                                  fin-reconcile-exact-window)))
         ;; Monthly savings nets are compared by month, never row by row.
         (nets   (cl-remove-if-not #'fin-conv--savings-ledger-p raw))
         (ledger (fin-reconcile-net (cl-set-difference raw nets)))
         (hist   (fin-bank-fix--history))
         (res    (fin-reconcile-match bank ledger))
         (saving (cl-remove-if-not #'fin-conv--savings-p (plist-get res :bank-only)))
         (sh     (fin-reconcile-shifted (cl-set-difference (plist-get res :bank-only) saving)
                                        (plist-get res :entry-only)
                                        (fin-conv-related-fn hist))))
    (list :matched (plist-get res :matched) :near (plist-get res :near)
          :shifted (plist-get sh :shifted)
          :bank-only (append (plist-get sh :bank-only) saving)
          :entry-only (append (plist-get sh :entry-only) nets) :notes (plist-get norm :notes)
          :history hist)))

(defun fin-bank-fix--key (l)
  "ODS row key of ledger row L."
  (cl-assert (integerp (car l)) nil "fin-bank: not a ledger row: %S" l)
  (list (nth 1 l) (nth 2 l) (nth 3 l) (or (nth 4 l) "") (nth 5 l)))

(defun fin-bank-fix--changes (actions)
  "ODS changes for ACTIONS.  A deleted row takes no edit; a row takes one edit."
  (let ((deleted (mapcar (lambda (a) (fin-bank-fix--key (cadr a)))
                         (cl-remove-if-not (lambda (a) (eq (car a) :delete)) actions)))
        (edited nil) (out nil))
    (dolist (a actions)
      (pcase a
        (`(:add ,fields ,_)
         (pcase-let ((`(,date ,type ,cat ,item ,amount ,note) fields))
           (push (list :add (list date type cat (or item "") amount note)) out)))
        (`(:edit ,l ,new ,_)
         (let ((k (fin-bank-fix--key l)))
           (unless (or (member k deleted) (member k edited) (= new (nth 4 k)))
             (push k edited) (push (list :edit k new) out))))
        (`(:delete ,l ,_) (push (list :delete (fin-bank-fix--key l)) out))
        (`(,(or :report :skip) . ,_) nil)
        (_ (error "fin-bank: bad action %S" a))))
    (cl-delete-duplicates (nreverse out) :test #'equal :from-end t)))

(defun fin-bank-fix--insert-section (title items fmt)
  (insert (format "\n* %s (%d)\n\n" title (length items)))
  (dolist (i items) (insert "  " (funcall fmt i) "\n")))

(defun fin-bank-fix--report (actions notes backup)
  "Show ACTIONS and NOTES; BACKUP is the saved copy or nil (dry run)."
  (let ((buf (get-buffer-create "*fin-bank-fix*")))
    (cl-flet ((of (kind) (cl-remove-if-not (lambda (a) (eq (car a) kind)) actions))
              (money (n) (fin-bank--cents n)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (if backup (format "ODS fixed; backup %s\nRun M-x fin to refresh.\n" backup)
                    "Dry run: nothing written.\n"))
          (fin-bank-fix--insert-section
           "Added" (of :add)
           (lambda (a) (pcase-let ((`(,d ,ty ,c ,i ,amt ,n) (cadr a)))
                         (format "%s %-3s %10s  %s / %s%s  ← %s" d ty (money amt) c (or i "-")
                                 (if n (format " (%s)" n) "") (caddr a)))))
          (fin-bank-fix--insert-section
           "Edited" (of :edit)
           (lambda (a) (let ((l (cadr a)))
                         (format "%s %s / %s  %s → %s  (%s)" (nth 1 l) (nth 3 l) (or (nth 4 l) "-")
                                 (money (nth 5 l)) (money (caddr a)) (cadddr a)))))
          (fin-bank-fix--insert-section
           "Deleted" (of :delete)
           (lambda (a) (let ((l (cadr a)))
                         (format "%s %s / %s  %s  (%s)" (nth 1 l) (nth 3 l) (or (nth 4 l) "-")
                                 (money (nth 5 l)) (caddr a)))))
          (fin-bank-fix--insert-section
           "Needs a look" (of :report)
           (lambda (a) (let ((r (cadr a)))   ; bank row (5) or ledger row (7)
                         (format "%s %s  %s" (nth 1 r) (money (nth (if (= (length r) 5) 3 5) r))
                                 (caddr a)))))
          (let ((why (make-hash-table :test #'equal))
                (skips (mapcar (lambda (a) (cons (cadr a) (caddr a))) (of :skip))))
            (dolist (n (append notes skips)) (puthash (cdr n) (1+ (gethash (cdr n) why 0)) why))
            (insert (format "\n* Bank rows folded, cancelled or skipped (%d)\n\n"
                            (+ (length notes) (length skips))))
            (maphash (lambda (k v) (insert (format "  %4d  %s\n" v k))) why)))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

;;;###autoload
(defun fin-bank-fix (&optional dry-run)
  "Fix the ODS ledger from bank rows since `fin-bank-fix-from', then report.
With prefix arg DRY-RUN, only report.  Run `fin' afterwards."
  (interactive "P")
  (fin-bank-fix--check-closed fin-ods-path)
  (let* ((r (fin-bank-fix--reconcile fin-bank-fix-from))
         (actions (fin-conv-plan :matched (plist-get r :matched) :near (plist-get r :near)
                                 :shifted (plist-get r :shifted) :bank-only (plist-get r :bank-only)
                                 :entry-only (plist-get r :entry-only)
                                 :history (plist-get r :history)
                                 :today (format-time-string "%Y-%m-%d")))
         (changes (fin-bank-fix--changes actions))
         (backup (when (and changes (not dry-run))
                   (fin-odsw-save fin-ods-path (fin-odsw-apply (fin-odsw-read fin-ods-path) changes)))))
    (fin-bank-fix--report actions (plist-get r :notes) backup)
    (message "fin-bank: %d changes%s" (length changes)
             (cond (dry-run " (dry run)") (changes ", run M-x fin") (t "")))
    changes))

(provide 'bankfix)
;;; bankfix.el ends here
