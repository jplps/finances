;;; bankfix.el --- Fix the ODS ledger from imported bank rows  -*- lexical-binding: t; -*-

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
  "First ISO date fixed from the bank; nil for the first day of last month."
  :type '(choice (const nil) string) :group 'fin)

(defun fin-bank-fix--from ()
  "`fin-bank-fix-from', or the first day of last month."
  (or fin-bank-fix-from
      (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time)))
        (if (= m 1) (format "%04d-12-01" (1- y)) (format "%04d-%02d-01" y (1- m))))))

(defun fin-bank-fix--vanished ()
  "(BANK-IDS . ACTIONS): adds whose bank row a later export dropped, and a
delete for each whose ledger row is still there unchanged.  Identical
adds take distinct ledger rows."
  (let (ids acts taken)
    (dolist (v (fin-bankdb-vanished-adds) (cons ids (nreverse acts)))
      (push (car v) ids)
      (let ((l (cl-find-if-not (lambda (r) (memq (car r) taken))
                               (fin-db-query "SELECT id, date, type, category, item, amount FROM entry
                                               WHERE date = ? AND type = ? AND category = ?
                                                 AND COALESCE(item, '') = ? AND amount = ?
                                               ORDER BY id"
                                             (cdr v)))))
        (when l
          (push (car l) taken)
          (push (list :delete l "the bank no longer lists it") acts))))))

(defun fin-bank-fix--added (actions)
  "(bank-id date type category item amount) of the adds in ACTIONS that
carry a bank id."
  (cl-loop for a in actions
           when (and (eq (car a) :add) (stringp (nth 3 a)))
           collect (pcase-let ((`(,date ,type ,cat ,item ,amount . ,_) (cadr a)))
                     (list (nth 3 a) date type cat (or item "") amount))))

(defcustom fin-bank-fix-unspent-categories '("investments" "retirement" "reserve" "cnpj")
  "Ledger categories left out when comparing a month's ledger with the bank."
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
        (when (and (equal (nth 2 b) "out") fin-bank-bill-paid-regexp
                   (string-match-p fin-bank-bill-paid-regexp (nth 4 b))
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

(defun fin-bank-fix--pj-split (bank raw from)
  "Split BANK rows since FROM and RAW ledger rows for the PJ account.
Return plist :bank and :raw (left to the general match) and :pj (actions).
From the first whole month the PJ statements cover to their last day
(`fin-pj-cover'), client and tax payments match income and `cnpj' rows of
those months and company transfers stop counting as salary; ledger rows
after the last statement day are left alone; other PJ rows are dropped."
  (pcase-let* ((`(,ids . ,span) (fin-bankdb-account-rows "pj"))
               (cover (and span (fin-pj-cover (car span) (cdr span) from))))
    (if (null cover)
        (list :bank (cl-remove-if (lambda (b) (member (car b) ids)) bank) :raw raw :pj nil)
      (let* ((start (car cover)) (end (cdr cover))
             (pj (let ((h (make-hash-table :test #'equal))) (dolist (i ids) (puthash i t h)) h))
             (cov (lambda (r) (fin-pj-within-p (nth 1 r) start end)))
             (cov-led (lambda (l) (fin-pj-within-p (nth 1 l) start (fin-pj-month-end end))))
             (pj-row (lambda (b) (gethash (car b) pj)))
             (income (cl-remove-if-not (lambda (b) (and (funcall pj-row b) (funcall cov b) (fin-pj-income-category b))) bank))
             (taxes (cl-remove-if-not (lambda (b) (and (funcall pj-row b) (funcall cov b) (fin-pj-tax-item b))) bank))
             (bank (cl-remove-if (lambda (b) (or (memq b income) (memq b taxes)
                                                 (and (funcall pj-row b) (not (funcall cov b)))
                                                 (and (funcall cov b) (fin-conv--salary-p b))))
                                 bank))
             (cats (delete-dups (mapcar #'cdr fin-pj-income-payers)))
             (items (delete-dups (mapcar #'cdr fin-pj-tax-items)))
             (led-in (cl-remove-if-not (lambda (l) (and (funcall cov-led l) (equal (nth 2 l) "in") (null (nth 4 l))
                                                        (member (nth 3 l) cats)))
                                       raw))
             (led-tax (cl-remove-if-not (lambda (l) (and (funcall cov-led l) (fin-pj--tax-row-p l items))) raw)))
        (list :bank bank
              :raw (cl-remove-if (lambda (l) (or (memq l led-in) (memq l led-tax))) raw)
              :pj (append (cl-remove-if (lambda (a) (and (eq (car a) :report) (string< end (nth 1 (nth 1 a)))))
                                        (fin-pj-plan-income income led-in))
                          (fin-pj-plan-taxes taxes led-tax cover)))))))

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
  "Descriptions of reported bank rows the cockpit does not flag."
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
              (list date amount amount (format "%%%d/%d%%" k n))))))

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

(defun fin-bank-fix--continue-series (actions &optional pairs)
  "ACTIONS with each installment add booked like its previous installment:
the ledger's, else that of the same merchant's N-installment purchase in
PAIRS (bank . ledger) matches or resolved earlier in ACTIONS (prepaid
installments arrive together, discounted)."
  (let ((known (make-hash-table :test #'equal))
        (resolved (make-hash-table :test #'eq))
        (k-of (lambda (a) (or (nth 5 (nth 1 a)) 0))))
    (dolist (p (sort (cl-remove-if-not (lambda (p) (car (fin-conv-installment (nth 4 (car p))))) (copy-sequence pairs))
                     (lambda (x y) (< (car (fin-conv-installment (nth 4 (car x))))
                                      (car (fin-conv-installment (nth 4 (car y))))))))
      (let ((l (cdr p)))
        (when (equal (nth 2 l) "out")
          (puthash (list (fin-conv-item (nth 4 (car p))) (cadr (fin-conv-installment (nth 4 (car p)))))
                   (list (nth 3 l) (nth 4 l)) known))))
    (dolist (a (sort (copy-sequence actions) (lambda (x y) (< (funcall k-of x) (funcall k-of y)))))
      (pcase a
        (`(:add (,date "out" ,_cat ,_item ,amount ,(and k (pred integerp)) ,n) ,src . ,_)
         (let* ((key (list (fin-conv-item src) n))
                (s (or (fin-bank-fix--series date amount k n src) (gethash key known))))
           (when s
             (puthash key s known)
             (puthash a s resolved))))))
    (mapcar (lambda (a)
              (let ((s (gethash a resolved)))
                (if (null s) a
                  (pcase-let ((`(:add (,date ,type ,_ ,_ ,amount ,k ,n) ,src . ,id) a))
                    (append (list :add (list date type (car s) (cadr s) amount k n) src) id)))))
            actions)))

(defun fin-bank-fix--renumber (pairs)
  "(:renumber LEDGER-ROW OLD-K NEW-K SOURCE) for matched PAIRS (bank .
ledger) whose ledger installment numbers differ from the bank's: within
rows of one ledger key, numbers the bank lacks take those the ledger lacks."
  (let ((groups (make-hash-table :test #'equal)) out)
    (dolist (p pairs)
      (pcase-let ((`(,k ,n) (fin-conv-installment (nth 4 (car p))))
                  (lk (nth 6 (cdr p))))
        (when (and k lk (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" lk)
                   (= n (string-to-number (match-string 2 lk))))
          (push (list k (string-to-number (match-string 1 lk)) (cdr p) (nth 4 (car p)))
                (gethash (fin-bank-fix--key (cdr p)) groups)))))
    (maphash
     (lambda (_ g)
       (let ((bank-ks (sort (mapcar #'car g) #'<))
             (led-ks (sort (mapcar #'cadr g) #'<)))
         (cl-loop for old in (sort (cl-loop with rest = (copy-sequence bank-ks)
                                            for k in led-ks
                                            if (memql k rest) do (setq rest (cl-remove k rest :count 1))
                                            else collect k)
                                   #'<)
                  for new in (sort (cl-loop with rest = (copy-sequence led-ks)
                                            for k in bank-ks
                                            if (memql k rest) do (setq rest (cl-remove k rest :count 1))
                                            else collect k)
                                   #'<)
                  for hit = (cl-find old g :key #'cadr)
                  do (push (list :renumber (nth 2 hit) old new (nth 3 hit)) out))))
     groups)
    (nreverse out)))

(defun fin-bank-fix--key (l)
  "ODS row key of ledger row L."
  (cl-assert (integerp (car l)) nil "fin-bank: not a ledger row: %S" l)
  (list (nth 1 l) (nth 2 l) (nth 3 l) (or (nth 4 l) "") (nth 5 l)))

(defun fin-bank-fix--changes (actions)
  "ODS changes for ACTIONS.  A deleted row takes no edit; a row takes one
edit and one delete.  Deletes of distinct identical rows all stay."
  (let ((deleted (mapcar (lambda (a) (fin-bank-fix--key (cadr a)))
                         (cl-remove-if-not (lambda (a) (eq (car a) :delete)) actions)))
        (edited nil) (deleted-ids nil) (out nil))
    (dolist (a actions)
      (pcase a
        (`(:add ,fields . ,_)
         (pcase-let ((`(,date ,type ,cat ,item ,amount ,k ,n) fields))
           (push (list :add (list date type cat (or item "") amount k n)) out)))
        (`(:edit ,l ,new ,_)
         (let ((k (fin-bank-fix--key l)))
           (unless (or (member k deleted) (member k edited) (= new (nth 4 k)))
             (push k edited) (push (list :edit k new) out))))
        (`(:delete ,l ,_)
         (unless (memql (car l) deleted-ids)
           (push (car l) deleted-ids)
           (push (list :delete (fin-bank-fix--key l)) out)))
        (`(:renumber ,l ,old ,new ,_)
         (let ((k (fin-bank-fix--key l)))
           (unless (member k deleted)
             (push (list :renumber k old new) out))))
        (`(,(or :report :skip) . ,_) nil)
        (_ (error "fin-bank: bad action %S" a))))
    (nreverse out)))

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
           "Renumbered" (of :renumber)
           (lambda (a) (pcase-let ((`(:renumber ,l ,old ,new ,src) a))
                         (format "%s %s / %s  %s  installment %d → %d  (%s)" (nth 1 l) (nth 3 l)
                                 (or (nth 4 l) "-") (money (nth 5 l)) old new src))))
          (fin-bank-fix--insert-section
           "Deleted" (of :delete)
           (lambda (a) (let ((l (cadr a)))
                         (format "%s %s / %s  %s  (%s)" (nth 1 l) (nth 3 l) (or (nth 4 l) "-")
                                 (money (nth 5 l)) (caddr a)))))
          (fin-bank-fix--insert-section
           "Needs a look" (of :report)
           (lambda (a) (let ((r (cadr a)))
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

(defun fin-bank-fix (&optional dry-run quiet)
  "Fix the ODS ledger from bank rows since `fin-bank-fix--from', then report.
With prefix arg DRY-RUN, only report.  QUIET shows the report only when
something changed."
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
         (actions (fin-bank-fix--continue-series
                   actions (append (plist-get r :matched) (plist-get r :near) (plist-get r :shifted))))
         (actions (append actions (fin-bank-fix--renumber (plist-get r :matched))))
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

(defconst fin-bank-fix-passes 3
  "Most fix passes `fin-bank-fix-settle' runs before it gives up.")

(defun fin-bank-fix-settle (refresh)
  "Run `fin-bank-fix' quietly, then REFRESH the DB, until a pass changes
nothing.  Fail after `fin-bank-fix-passes' passes that all changed."
  (cl-assert (functionp refresh))
  (cl-loop for pass from 1
           for changes = (fin-bank-fix nil t)
           do (funcall refresh)
           until (null changes)
           when (>= pass fin-bank-fix-passes)
           do (error "fin-bank: ledger still changing after %d fixes" pass)))

(provide 'bankfix)
;;; bankfix.el ends here
