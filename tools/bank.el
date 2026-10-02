;;; bank.el --- Reconcile staged bank rows against the ODS ledger  -*- lexical-binding: t; -*-

;; The review buffer shows differences and `fin-bank-copy-missing' puts
;; paste-ready rows on the kill ring; `fin-bank-fix' (bankfix.el) writes
;; the fixes to the ODS.
;;
;; Statements copied into `fin-bank-inbox' are imported with
;; `fin-bank-import': files are renamed to `nubank-<kind>-<start>_<end>.ofx'
;; first, a re-export of a statement already there is removed, and each
;; file is imported once, keyed by content hash.

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'reconcile)
(require 'ofx)

(defcustom fin-bank-inbox
  (expand-file-name "../infra/inbox/"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory holding bank statement files to import."
  :type 'directory :group 'fin)

(defcustom fin-bank-ignore-regexps
  '("\\`Pagamento de fatura\\'"
    "\\`Pagamento da fatura"
    "\\`Pagamento recebido\\'"
    "\\`Valor adicionado na conta por cartão"
    "\\`Valor enviado como crédito na fatura"
    "\\`Estorno de pagamento"
    "\\`Ajuste a crédito" "\\`Encerramento de dívida")
  "Bank descriptions excluded from reconciliation.
Moves with no ledger counterpart: the card bill payment, seen as paid on
the account and received on the card, would double-count purchases
already reconciled from the card statement; card credit moved to the
account for a Pix, and back; cent-level debt adjustments.  Investment
moves are not listed: bankfix.el skips them as savings.  Rows stay
stored; only reconciliation skips them."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-bank-own-regexps nil
  "Bank descriptions of transfers between your own accounts: your name,
your company.  Ignored like `fin-bank-ignore-regexps', except salary
deposits matching `fin-conv-salary-regexps'."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-bank-name "nubank"
  "Bank prefix of statement file names in the inbox."
  :type 'string :group 'fin)

(defvar fin-conv-salary-regexps)

(defun fin-bank--ignored-p (row)
  "Non-nil if bank ROW is skipped by reconciliation."
  (let ((desc (nth 4 row)))
    (cl-flet ((any (res) (cl-some (lambda (re) (string-match-p re desc)) res)))
      (or (any fin-bank-ignore-regexps)
          (and (any fin-bank-own-regexps)
               (not (any (bound-and-true-p fin-conv-salary-regexps))))))))

(defun fin-bank--txn-id (fitid date cents memo seen)
  "Stable id for one OFX transaction.
Nubank reuses a FITID across installments of one purchase and for a
refund of it, so the id also hashes DATE, CENTS and MEMO.  The same
transaction listed again in an overlapping statement keeps its id.
SEEN counts ids within one file; repeats get an ordinal suffix."
  (let* ((base (format "nu:%s:%s" fitid
                       (substring (secure-hash 'sha1 (format "%s|%d|%s" date cents memo))
                                  0 12)))
         (n    (1+ (gethash base seen 0))))
    (puthash base n seen)
    (if (= n 1) base (format "%s#%d" base n))))

(defun fin-bank--ofx-rows (text)
  "Bank rows from OFX TEXT and the count of zero-amount txns dropped."
  (let* ((parsed  (fin-ofx-parse text))
         (kind    (plist-get parsed :kind))
         (seen    (make-hash-table :test #'equal))
         (dropped 0)
         rows)
    (dolist (tx (plist-get parsed :txns))
      (pcase-let ((`(,fitid ,date ,cents ,memo) tx))
        (if (zerop cents)
            (setq dropped (1+ dropped))
          (push (list (fin-bank--txn-id fitid date cents memo seen)
                      "nubank-ofx" kind date
                      (if (< cents 0) "out" "in") (abs cents) memo)
                rows))))
    (cons (nreverse rows) dropped)))

;;;###autoload
(defun fin-bank-import-file (path)
  "Import the OFX statement at PATH.  Return rows inserted, or nil if
the file was already imported."
  (interactive "fOFX file: ")
  (unless (string-match-p "\\.ofx\\'" (downcase path))
    (user-error "fin-bank: only .ofx is supported: %s" path))
  (let* ((text (fin-ofx-read-file path))
         (sha1 (secure-hash 'sha1 text)))
    (if (fin-bankdb-file-imported-p sha1)
        (progn (message "fin-bank: already imported %s" path) nil)
      (pcase-let* ((`(,rows . ,dropped) (fin-bank--ofx-rows text))
                   (n (fin-bankdb-insert rows)))
        (fin-bankdb-record-import sha1 (expand-file-name path) n)
        (message "fin-bank: %s — %d txns, %d new, %d zero-amount dropped"
                 (file-name-nondirectory path) (length rows) n dropped)
        n))))

(defun fin-bank--canonical-name (text)
  "Inbox file name for OFX TEXT: bank, kind and statement period."
  (pcase-let ((`(,kind ,start ,end) (fin-ofx-period text)))
    (format "%s-%s-%s_%s.ofx" fin-bank-name kind start end)))

(defun fin-bank--same-statement-p (a b)
  "Non-nil if OFX texts A and B differ only in their export timestamp."
  (cl-flet ((strip (s) (replace-regexp-in-string "<DTSERVER>[^<\n]*\\(</DTSERVER>\\)?" "" s)))
    (string= (strip a) (strip b))))

(defun fin-bank--normalize-file (path)
  "Rename PATH inside the inbox to its canonical name.
Delete it when the canonical file holds the same statement; signal when
it holds a different one.  Return the resulting path or nil."
  (let* ((text (fin-ofx-read-file path))
         (dst (expand-file-name (fin-bank--canonical-name text) fin-bank-inbox)))
    (cond ((string= (expand-file-name path) dst) dst)
          ((not (file-exists-p dst)) (rename-file path dst) dst)
          ((fin-bank--same-statement-p text (fin-ofx-read-file dst))
           (delete-file path)
           (message "fin-bank: removed re-export %s" (file-name-nondirectory path))
           nil)
          (t (user-error "fin-bank: %s and %s cover the same period but differ"
                         (file-name-nondirectory path) (file-name-nondirectory dst))))))

;;;###autoload
(defun fin-bank-import ()
  "Name, then import, every .ofx file in `fin-bank-inbox' not imported yet."
  (interactive)
  (unless (file-directory-p fin-bank-inbox)
    (user-error "fin-bank: inbox missing: %s" fin-bank-inbox))
  (dolist (f (directory-files fin-bank-inbox t "\\.[oO][fF][xX]\\'"))
    (fin-bank--normalize-file f))
  (let ((files (directory-files fin-bank-inbox t "\\.[oO][fF][xX]\\'"))
        (new 0))
    (dolist (f files)
      (setq new (+ new (or (fin-bank-import-file f) 0))))
    (message "fin-bank: %d files scanned, %d new txns" (length files) new)
    new))

(defun fin-bank--bank-year (year)
  "Bank rows of YEAR minus those `fin-bank--ignored-p' skips."
  (cl-remove-if #'fin-bank--ignored-p (fin-bankdb-year year)))

(defun fin-bank--date-span (bank)
  "(FIRST . LAST) ISO dates covered by BANK rows, or nil when empty."
  (when bank
    (let ((dates (mapcar (lambda (r) (nth 1 r)) bank)))
      (cons (cl-reduce (lambda (a b) (if (string< b a) b a)) dates)
            (cl-reduce (lambda (a b) (if (string< a b) b a)) dates)))))

(defun fin-bank--ledger-span (span &optional pad-days)
  "Ledger rows within SPAN (FIRST . LAST) widened by PAD-DAYS, default
`fin-reconcile-window'.  Ledger outside the bank's coverage cannot match,
so it is not compared.  Future-dated rows are plan placeholders and never
reach a bank."
  (when span
    (let* ((days (or pad-days fin-reconcile-window))
           (pad (format "%+d days" days)))
      (fin-db-query
       "SELECT id, date, type, category, item, amount,
                CASE WHEN installment IS NOT NULL THEN installment || '/' || installments END
          FROM entry
         WHERE date >= date(?, ?)
           AND date <= date(?, ?)
           AND date <= date('now', 'localtime')
         ORDER BY date, id"
       (list (car span) (format "%+d days" (- days))
             (cdr span) pad)))))

(defun fin-bank--year-rows (year)
  "(BANK LEDGER SPAN) to reconcile for YEAR, ledger netted per
`fin-reconcile-net-rules'."
  (let* ((bank (fin-bank--bank-year year))
         (span (fin-bank--date-span bank)))
    (list bank (fin-reconcile-net (fin-bank--ledger-span span)) span)))

(defun fin-bank--read-year ()
  (list (read-number "Year: " (string-to-number (format-time-string "%Y")))))

(defun fin-bank--check-year (year)
  (unless (and (integerp year) (<= 1900 year 9999))
    (user-error "fin-bank: bad year %S" year)))

(defun fin-bank--cents (n)
  "Integer cents N as a decimal string, sign in front."
  (format "%s%d.%02d" (if (< n 0) "-" "") (/ (abs n) 100) (% (abs n) 100)))

(defun fin-bank--tsv-row (bank-row)
  "BANK-ROW as one TSV line in ODS entries column order.
Category is left empty for the user to fill; installments come from
the bank's \"Parcela k/n\"."
  (pcase-let ((`(,_id ,date ,type ,amount ,desc) bank-row))
    (mapconcat #'identity
               (append (list date type "" (downcase (string-trim desc))
                             (number-to-string amount))
                       (if (string-match "Parcela \\([0-9]+\\)/\\([0-9]+\\)" desc)
                           (list (match-string 1 desc) (match-string 2 desc))
                         (list "" "")))
               "\t")))

(defun fin-bank--insert-month-totals (bank ledger)
  (insert "* Month totals (bank vs ledger)\n\n"
          (format "  %-7s %-3s %12s %12s %12s\n"
                  "month" "typ" "bank" "ledger" "delta"))
  (dolist (r (fin-reconcile-month-totals bank ledger))
    (pcase-let ((`(,ym ,type ,b ,l) r))
      (insert (format "  %-7s %-3s %12s %12s %12s\n"
                      ym type (fin-bank--cents b) (fin-bank--cents l)
                      (fin-bank--cents (- b l)))))))

(defun fin-bank--insert-rows (title rows fmt)
  (insert (format "\n* %s (%d)\n\n" title (length rows)))
  (dolist (r rows) (insert (funcall fmt r) "\n")))

(defun fin-bank--fmt-bank (r)
  (pcase-let ((`(,_id ,date ,type ,amount ,desc) r))
    (format "  %s %-3s %10s  %s" date type (fin-bank--cents amount) desc)))

(defun fin-bank--fmt-near (pair)
  (pcase-let ((`((,_bid ,date ,type ,bamt ,desc) . (,_lid ,_ldate ,_ltype ,cat ,item ,lamt ,_note))
               pair))
    (format "  %s %-3s %10s vs %10s (Δ %s)  %s ↔ %s / %s"
            date type (fin-bank--cents bamt) (fin-bank--cents lamt)
            (fin-bank--cents (- bamt lamt)) desc cat (or item "-"))))

(defun fin-bank--fmt-ledger (r)
  (pcase-let ((`(,_id ,date ,type ,cat ,item ,amount ,_note) r))
    (format "  %s %-3s %10s  %s / %s"
            date type (fin-bank--cents amount) cat (or item "-"))))

;;;###autoload
(defun fin-bank-reconcile (year)
  "Show bank vs ledger differences for YEAR in a review buffer."
  (interactive (fin-bank--read-year))
  (fin-bank--check-year year)
  (pcase-let* ((`(,bank ,ledger ,span) (fin-bank--year-rows year))
               (res (fin-reconcile-match bank ledger))
               (buf (get-buffer-create (format "*fin-reconcile %d*" year))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Reconcile %d — bank %d, ledger %d, matched %d, near %d, window ±%dd\n"
                        year (length bank) (length ledger)
                        (length (plist-get res :matched))
                        (length (plist-get res :near)) fin-reconcile-window)
                (if span
                    (format "Bank coverage %s .. %s\n\n" (car span) (cdr span))
                  "No bank rows for this year — import statements first.\n\n"))
        (fin-bank--insert-month-totals bank ledger)
        (fin-bank--insert-rows "Possible matches — amounts differ (bank vs ledger)"
                               (plist-get res :near) #'fin-bank--fmt-near)
        (fin-bank--insert-rows "Bank only — missing in ODS"
                               (plist-get res :bank-only) #'fin-bank--fmt-bank)
        (fin-bank--insert-rows "Ledger only — not at bank"
                               (plist-get res :entry-only) #'fin-bank--fmt-ledger))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun fin-bank-copy-missing (year)
  "Copy YEAR's bank-only rows to the kill ring as TSV for the ODS entries sheet."
  (interactive (fin-bank--read-year))
  (fin-bank--check-year year)
  (let ((rows (plist-get (apply #'fin-reconcile-match
                                (butlast (fin-bank--year-rows year)))
                         :bank-only)))
    (if (null rows)
        (message "fin-bank: nothing missing in %d" year)
      (kill-new (mapconcat #'fin-bank--tsv-row rows "\n"))
      (message "fin-bank: %d rows copied — paste into entries, fill category"
               (length rows)))))

(provide 'bank)
;;; bank.el ends here
