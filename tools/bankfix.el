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
(require 'pj)
(require 'odswrite)
(require 'bank)

(defvar fin-ods-path)

(defcustom fin-bank-fix-from "2024-01-01"
  "First date fixed from the bank: account and card statements both cover
it, and the ledger lists single purchases from there on."
  :type 'string :group 'fin)

(defcustom fin-bank-fix-itemized-from "2022-07-01"
  "First date the ledger books single purchases; before it, monthly lumps
per category and income by payer, so bank rows are reported, not added."
  :type 'string :group 'fin)

(defcustom fin-bank-fix-unspent-categories '("investments" "retirement" "reserve" "cnpj")
  "Ledger categories that are not Nubank spending: savings goals, withheld
tax.  Left out when comparing a month's ledger with the bank."
  :type '(repeat string) :group 'fin)

(defun fin-bank-fix--ledger-out (from)
  "Alist YYYY-MM -> ledger outflows since FROM, spent categories only."
  (mapcar (lambda (r) (cons (car r) (cadr r)))
          (fin-db-query
           (format "SELECT strftime('%%Y-%%m', date), SUM(amount) FROM entry
                     WHERE type = 'out' AND date >= ? AND date <= date('now', 'localtime')
                       AND category NOT IN (%s)
                     GROUP BY 1"
                   (mapconcat (lambda (_) "?") fin-bank-fix-unspent-categories ","))
           (cons from fin-bank-fix-unspent-categories))))

(defun fin-bank-fix--months (bank raw from)
  "(YYYY-MM bank-out ledger-out) per month of BANK rows, savings excluded.
In a month no card statement covers, the card bill paid from the account
(in RAW, the rows before ignoring) stands for the card purchases."
  (let ((sums (make-hash-table :test #'equal)) (ledger (fin-bank-fix--ledger-out from))
        (card (fin-bankdb-card-months)) out)
    (cl-flet ((add (b) (let ((ym (substring (nth 1 b) 0 7)))
                         (puthash ym (+ (gethash ym sums 0) (nth 3 b)) sums))))
      (dolist (b bank)
        (when (and (equal (nth 2 b) "out") (not (fin-conv--savings-p b))) (add b)))
      (dolist (b raw)
        (when (and (equal (nth 2 b) "out") (string-match-p "\\`Pagamento d[ae] fatura" (nth 4 b))
                   (not (member (substring (nth 1 b) 0 7) card)))
          (add b))))
    (maphash (lambda (ym v) (push (list ym v (or (cdr (assoc ym ledger)) 0)) out)) sums)
    (sort out (lambda (a b) (string< (car a) (car b))))))

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

(defun fin-bank-fix--pj-split (bank raw)
  "Split BANK rows and RAW ledger rows for the PJ account (pj.el).
Return plist :bank and :raw (left to the general match) and :pj (actions).
In the months a PJ statement fully covers, client payments and tax
payments reconcile against income and `cnpj' rows there, transfers from
the company stop counting as salary, and PJ rows outside those months are
dropped (a partial month cannot be judged)."
  (pcase-let* ((`(,ids . ,span) (fin-bankdb-account-rows "pj")))
    (if (null span)
        (list :bank bank :raw raw :pj nil)
      (let* ((months (fin-pj-months (car span) (cdr span)))
             (pj (let ((h (make-hash-table :test #'equal))) (dolist (i ids) (puthash i t h)) h))
             (cov (lambda (r) (member (substring (nth 1 r) 0 7) months)))
             (pj-row (lambda (b) (gethash (car b) pj)))
             (income (cl-remove-if-not (lambda (b) (and (funcall pj-row b) (funcall cov b) (fin-pj-income-category b))) bank))
             (taxes (cl-remove-if-not (lambda (b) (and (funcall pj-row b) (funcall cov b) (fin-pj-tax-item b))) bank))
             (bank (cl-remove-if (lambda (b) (or (memq b income) (memq b taxes)
                                                 (and (funcall pj-row b) (not (funcall cov b)))
                                                 (and (funcall cov b) (fin-conv--salary-p b))))
                                 bank))
             (cats (delete-dups (mapcar #'cdr fin-pj-income-payers)))
             (items (delete-dups (mapcar #'cdr fin-pj-tax-items)))
             ;; An income row with an item (a bonus) has another source.
             (led-in (cl-remove-if-not (lambda (l) (and (funcall cov l) (equal (nth 2 l) "in") (null (nth 4 l))
                                                        (member (nth 3 l) cats)))
                                       raw))
             (led-tax (cl-remove-if-not (lambda (l) (and (funcall cov l) (fin-pj--tax-row-p l items))) raw)))
        (list :bank bank
              :raw (cl-remove-if (lambda (l) (or (memq l led-in) (memq l led-tax))) raw)
              :pj (append (fin-pj-plan-income income led-in)
                          (fin-pj-plan-taxes taxes led-tax months)))))))

(defun fin-bank-fix--reconcile (from)
  "Reconcile bank rows since FROM.  Return plist of match buckets and
:notes (normalize removals), :months, :history, :pj (PJ actions)."
  (let* ((norm   (fin-conv-normalize (fin-bankdb-since from)))
         (bank   (cl-remove-if #'fin-bank--ignored-p (plist-get norm :rows)))
         (span   (fin-bank--date-span bank))
         (split  (fin-bank-fix--pj-split
                  bank (fin-bank--ledger-span span (max fin-reconcile-shift-window
                                                        fin-reconcile-exact-window))))
         (bank   (plist-get split :bank))
         (raw    (plist-get split :raw))
         (ledger (fin-reconcile-net raw))
         (hist   (fin-bank-fix--history))
         (res    (fin-reconcile-match bank ledger))
         (saving (cl-remove-if-not #'fin-conv--savings-p (plist-get res :bank-only)))
         (sh     (fin-reconcile-shifted (cl-set-difference (plist-get res :bank-only) saving)
                                        (plist-get res :entry-only)
                                        (fin-conv-related-fn hist))))
    (list :matched (plist-get res :matched) :near (plist-get res :near)
          :shifted (plist-get sh :shifted)
          :bank-only (append (plist-get sh :bank-only) saving)
          :entry-only (plist-get sh :entry-only) :notes (plist-get norm :notes)
          :months (fin-bank-fix--months bank (plist-get norm :rows) from)
          :history hist :pj (plist-get split :pj))))

(defun fin-bank-fix--lumped (actions itemized-from)
  "ACTIONS with adds dated before ITEMIZED-FROM turned into reports."
  (mapcar (lambda (a)
            (if (and (eq (car a) :add) (string< (car (cadr a)) itemized-from))
                (pcase-let ((`(,date ,type ,_c ,_i ,amount . ,_) (cadr a)))
                  (list :report (list nil date type amount (caddr a))
                        (format "before %s the ledger books monthly lumps" itemized-from)))
              a))
          actions))

(defun fin-bank-fix--lump-card-p (b itemized-from)
  "Non-nil if bank row B comes from a PDF card bill dated before ITEMIZED-FROM."
  (and (string-prefix-p "nu:pdf:" (car b)) (string< (nth 1 b) itemized-from)))

(defun fin-bank-fix--remainders (to)
  "Ledger `other' remainder rows dated before TO, oldest first."
  (fin-db-query
   "SELECT id, date, type, category, item, amount, NULL FROM entry
     WHERE item = 'other' AND type = 'out' AND date < ? ORDER BY date, id"
   (list to)))

(defun fin-bank-fix--next-month (ym)
  "YYYY-MM after YM."
  (pcase-let ((`(,y ,m) (mapcar #'string-to-number (split-string ym "-"))))
    (if (= m 12) (format "%04d-01" (1+ y)) (format "%04d-%02d" y (1+ m)))))

(defun fin-bank-fix--carve-pool (b cat pool)
  "Remainders of POOL that card row B may draw from, in draw order: its
month, then the next (a bill was often booked when paid), category CAT
first.  Each remainder is a cons (LEDGER-ROW . LEFT)."
  (let ((ym (substring (nth 1 b) 0 7)))
    (cl-loop for m in (list ym (fin-bank-fix--next-month ym))
             append (let ((rs (cl-remove-if-not (lambda (r) (equal (substring (nth 1 (car r)) 0 7) m)) pool)))
                      (append (cl-remove-if-not (lambda (r) (equal (nth 3 (car r)) cat)) rs)
                              (cl-remove-if (lambda (r) (equal (nth 3 (car r)) cat)) rs))))))

(defun fin-bank-fix--carve (rows history itemized-from)
  "Actions for card ROWS of lump months, before ITEMIZED-FROM.
There the ledger holds card spending as month-end `other' remainders, so
a charge becomes a ledger row (`fin-conv--add-action' over HISTORY)
only when remainders of its month and the next cover it, and takes that
money from them: totals never grow.  Charges left uncovered and card
credits are reported."
  (let ((pool (mapcar (lambda (r) (cons r (nth 5 r))) (fin-bank-fix--remainders itemized-from)))
        out)
    (dolist (b (sort (copy-sequence rows) (lambda (x y) (string< (nth 1 x) (nth 1 y)))))
      (if (not (equal (nth 2 b) "out"))
          (push (list :report b "card credit in a lump month") out)
        (let* ((add (fin-conv--add-action b history))
               (from (fin-bank-fix--carve-pool b (nth 2 (cadr add)) pool)))
          (if (< (apply #'+ (mapcar #'cdr from)) (nth 3 b))
              (push (list :report b "card charge beyond the month's remainders") out)
            (let ((need (nth 3 b)))
              (dolist (r from)
                (let ((take (min need (cdr r))))
                  (setcdr r (- (cdr r) take))
                  (setq need (- need take))))
              (cl-assert (zerop need)))
            (push add out)))))
    (dolist (r pool)
      (let ((l (car r)))
        (cond ((= (cdr r) (nth 5 l)) nil)
              ((zerop (cdr r)) (push (list :delete l "card rows itemize the remainder") out))
              (t (push (list :edit l (cdr r) "card rows itemize the remainder") out)))))
    (nreverse out)))

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
         (pcase-let ((`(,date ,type ,cat ,item ,amount ,k ,n) fields))
           (push (list :add (list date type cat (or item "") amount k n)) out)))
        (`(:edit ,l ,new ,_)
         (let ((k (fin-bank-fix--key l)))
           (unless (or (member k deleted) (member k edited) (= new (nth 4 k)))
             (push k edited) (push (list :edit k new) out))))
        (`(:delete ,l ,_) (push (list :delete (fin-bank-fix--key l)) out))
        (`(,(or :report :skip) . ,_) nil)
        (_ (error "fin-bank: bad action %S" a))))
    ;; Two identical adds are two charges; only edits and deletes repeat.
    (cl-delete-duplicates (nreverse out) :from-end t
                          :test (lambda (a b) (and (not (eq (car a) :add)) (equal a b))))))

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
           (lambda (a) (pcase-let ((`(,d ,ty ,c ,i ,amt ,k ,n) (cadr a)))
                         (format "%s %-3s %10s  %s / %s%s  ← %s" d ty (money amt) c (or i "-")
                                 (if k (format " (%d/%d)" k n) "") (caddr a)))))
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
         (lump-p (lambda (b) (fin-bank-fix--lump-card-p b fin-bank-fix-itemized-from)))
         (actions (fin-conv-plan :matched (plist-get r :matched) :near (plist-get r :near)
                                 :shifted (plist-get r :shifted)
                                 :bank-only (cl-remove-if lump-p (plist-get r :bank-only))
                                 :entry-only (plist-get r :entry-only)
                                 :history (plist-get r :history)
                                 :months (plist-get r :months)))
         (actions (append (fin-bank-fix--lumped actions fin-bank-fix-itemized-from)
                          (fin-bank-fix--carve (cl-remove-if-not lump-p (plist-get r :bank-only))
                                               (plist-get r :history) fin-bank-fix-itemized-from)
                          (plist-get r :pj)))
         (changes (fin-bank-fix--changes actions))
         (backup (when (and changes (not dry-run))
                   (fin-odsw-save fin-ods-path (fin-odsw-apply (fin-odsw-read fin-ods-path) changes)))))
    (fin-bank-fix--report actions (plist-get r :notes) backup)
    (message "fin-bank: %d changes%s" (length changes)
             (cond (dry-run " (dry run)") (changes ", run M-x fin") (t "")))
    changes))

(provide 'bankfix)
;;; bankfix.el ends here
