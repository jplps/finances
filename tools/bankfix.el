;;; bankfix.el --- Fix the ODS ledger from imported bank rows  -*- lexical-binding: t; -*-

;; `fin-bank-fix' reconciles every bank row since `fin-bank-fix--from'
;; (by default the first day of last month: the month in progress and the
;; one before, whose card bill may still post) against the ledger, turns
;; the differences into ledger changes per conventions.el, writes them to
;; the ODS and shows what it did.  A ledger row it added goes again when a
;; later export of the statement no longer lists its bank row (a dropped
;; card authorization).  It never touches the ledger DB: run `fin'
;; afterwards.  Running it again changes nothing.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'reconcile)
(require 'conventions)
(require 'pj)
(require 'odswrite)
(require 'bank)

(defvar fin-ods-path)

(defcustom fin-bank-fix-from nil
  "First date fixed from the bank, ISO; nil for the first day of last
month, so older, settled history is never touched.  Set a date only to
re-fix older months once."
  :type '(choice (const nil) string) :group 'fin)

(defun fin-bank-fix--from ()
  "`fin-bank-fix-from', or the first day of last month."
  (or fin-bank-fix-from
      (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time)))
        (if (= m 1) (format "%04d-12-01" (1- y)) (format "%04d-%02d-01" y (1- m))))))

(defun fin-bank-fix--vanished ()
  "(BANK-IDS . ACTIONS): adds whose bank row a later export dropped, and a
delete for each whose ledger row is still there unchanged."
  (let (ids acts)
    (dolist (v (fin-bankdb-vanished-adds) (cons ids (nreverse acts)))
      (push (car v) ids)
      (let ((l (car (fin-db-query "SELECT id, date, type, category, item, amount FROM entry
                                    WHERE date = ? AND type = ? AND category = ?
                                      AND COALESCE(item, '') = ? AND amount = ? LIMIT 1"
                                  (cdr v)))))
        (when l (push (list :delete l "the bank no longer lists it") acts))))))

(defun fin-bank-fix--added (actions)
  "(bank-id date type category item amount) of the adds in ACTIONS that
carry a bank id."
  (cl-loop for a in actions
           when (and (eq (car a) :add) (stringp (nth 3 a)))
           collect (pcase-let ((`(,date ,type ,cat ,item ,amount . ,_) (cadr a)))
                     (list (nth 3 a) date type cat (or item "") amount))))

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

(defun fin-bank-fix--pj-months (span from)
  "YYYY-MM months the PJ statement SPAN (FIRST . LAST) fully covers on or
after ISO FROM, where the bank rows start."
  (let ((first (if (string< (car span) from) from (car span))))
    (unless (string< (cdr span) first) (fin-pj-months first (cdr span)))))

(defun fin-bank-fix--pj-split (bank raw from)
  "Split BANK rows since FROM and RAW ledger rows for the PJ account (pj.el).
Return plist :bank and :raw (left to the general match) and :pj (actions).
In the months a PJ statement fully covers, client payments and tax
payments reconcile against income and `cnpj' rows there, transfers from
the company stop counting as salary, and PJ rows outside those months are
dropped (a partial month cannot be judged)."
  (pcase-let* ((`(,ids . ,span) (fin-bankdb-account-rows "pj")))
    (if (null span)
        (list :bank bank :raw raw :pj nil)
      (let* ((months (fin-bank-fix--pj-months span from))
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
                                                        fin-reconcile-exact-window))
                  from))
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

(defcustom fin-bank-fix-flag-days 90
  "Bank rows a run only reports are flagged in the cockpit for this many days."
  :type 'integer :group 'fin)

(defcustom fin-bank-fix-flag-min 30000
  "Smallest reported bank row, in cents, the cockpit flags for classifying."
  :type 'integer :group 'fin)

(defcustom fin-bank-fix-flag-ignore-regexps nil
  "Descriptions of reported bank rows the cockpit should not flag: rows the
ledger books under another name or date, checked once."
  :type '(repeat regexp) :group 'fin)

(defun fin-bank-fix--flags (actions)
  "(id date amount description reason) of recent, large bank rows ACTIONS
only report: the ones a person must classify."
  (let ((since (format-time-string "%Y-%m-%d" (time-subtract nil (days-to-time fin-bank-fix-flag-days)))))
    (cl-loop for a in actions
             for r = (cadr a)
             when (and (eq (car a) :report) (= (length r) 5) (stringp (car r))
                       (not (string< (nth 1 r) since)) (>= (nth 3 r) fin-bank-fix-flag-min)
                       (not (cl-some (lambda (re) (string-match-p re (nth 4 r)))
                                     fin-bank-fix-flag-ignore-regexps)))
             collect (list (car r) (nth 1 r) (nth 3 r) (nth 4 r) (caddr a)))))

(defun fin-bank-fix--merchant-installment-p (date amount k n desc)
  "Non-nil if the bank holds installment K of N of DESC's merchant, of
AMOUNT cents within 1%, up to 45 days before ISO DATE."
  (let ((item (fin-conv-item desc)))
    (cl-some (lambda (r) (and (equal (fin-conv-installment (car r)) (list k n))
                              (equal (fin-conv-item (car r)) item)))
             (fin-db-query
              "SELECT description FROM bank_txn
                WHERE type = 'out' AND julianday(?) - julianday(date) BETWEEN 0 AND 45
                  AND abs(amount - ?) <= max(1, ? / 100) AND description LIKE ?"
              (list date amount amount (format "%%Parcela %d/%d%%" k n))))))

(defun fin-bank-fix--series (date amount k n desc)
  "(CATEGORY ITEM) of the ledger's installment K-1 of N before an
installment of AMOUNT cents on ISO DATE, bank DESC: amount within 1%, 20
to 45 days earlier, and of DESC's payee: its item names the payee, else
it is the only candidate and the bank holds the merchant's installment
K-1.  Nil when K is 1 or none is booked."
  (when (and k n (> k 1))
    (let ((words (fin-conv--bank-words desc))
          (cands (fin-db-query
                  "SELECT category, item FROM entry
                    WHERE type = 'out' AND installments = ? AND installment = ?
                      AND abs(amount - ?) <= max(1, ? / 100)
                      AND julianday(?) - julianday(date) BETWEEN 20 AND 45
                    ORDER BY abs(amount - ?)"
                  (list n (1- k) amount amount date amount))))
      (or (cl-find-if (lambda (c) (and (cadr c) (fin-conv--item-hit-p desc words (cadr c)))) cands)
          (and (= (length cands) 1)
               (fin-bank-fix--merchant-installment-p date amount (1- k) n desc)
               (car cands))))))

(defun fin-bank-fix--continue-series (actions)
  "ACTIONS with each installment add booked like its previous installment."
  (mapcar (lambda (a)
            (pcase a
              (`(:add (,date ,type ,_cat ,_item ,amount ,k ,n) ,src . ,id)
               (let ((s (fin-bank-fix--series date amount k n src)))
                 (if (and s (equal type "out"))
                     (append (list :add (list date type (car s) (cadr s) amount k n) src) id)
                   a)))
              (_ a)))
          actions))

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
        (`(:add ,fields . ,_)
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

(defun fin-bank-fix--report (actions notes written &optional quiet)
  "Show ACTIONS and NOTES; WRITTEN is the ODS path, or nil (dry run or
nothing to change).  QUIET fills the buffer without showing it."
  (let ((buf (get-buffer-create "*fin-bank-fix*")))
    (cl-flet ((of (kind) (cl-remove-if-not (lambda (a) (eq (car a) kind)) actions))
              (money (n) (fin-bank--cents n)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (if written (format "ODS fixed: %s\nRun M-x fin to refresh.\n" written)
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
    (unless quiet (pop-to-buffer buf))))

;;;###autoload
(defun fin-bank-fix (&optional dry-run quiet)
  "Fix the ODS ledger from bank rows since `fin-bank-fix--from', then report.
With prefix arg DRY-RUN, only report.  QUIET shows the report only when
something changed.  Run `fin' afterwards (or let `fin' run this)."
  (interactive "P")
  (fin-bank-fix--check-closed fin-ods-path)
  (let* ((r (fin-bank-fix--reconcile (fin-bank-fix--from)))
         (gone (fin-bank-fix--vanished))
         (actions (fin-conv-plan :matched (plist-get r :matched) :near (plist-get r :near)
                                 :shifted (plist-get r :shifted)
                                 :bank-only (plist-get r :bank-only)
                                 :entry-only (plist-get r :entry-only)
                                 :history (plist-get r :history)
                                 :months (plist-get r :months)))
         (actions (append actions (plist-get r :pj) (cdr gone)))
         (actions (fin-bank-fix--continue-series actions))
         (changes (fin-bank-fix--changes actions))
         (written (when (and changes (not dry-run))
                   (fin-odsw-save fin-ods-path (fin-odsw-apply (fin-odsw-read fin-ods-path) changes)))))
    (fin-bankdb-record-fix-run (length changes) dry-run (fin-bank-fix--flags actions))
    (unless dry-run
      (fin-bankdb-forget-adds (car gone))
      (when written (fin-bankdb-record-fix-adds (fin-bank-fix--added actions))))
    (fin-bank-fix--report actions (plist-get r :notes) written (and quiet (null changes)))
    (message "fin-bank: %d changes%s" (length changes)
             (cond (dry-run " (dry run)") (changes ", run M-x fin") (t "")))
    changes))

(provide 'bankfix)
;;; bankfix.el ends here
