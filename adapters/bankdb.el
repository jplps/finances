;;; bankdb.el --- Bank transaction staging tables  -*- lexical-binding: t; -*-

;; Bank rows live in the same SQLite file as the ODS mirror, but outside
;; `fin-db--tables', so `fin-db-rebuild' and `fin-sync-refresh' never touch
;; them.  They are re-derivable from the files in the inbox: imports are
;; idempotent by transaction id and by file hash.

(require 'cl-lib)
(require 'db)

(defconst fin-bankdb--ddl
  '("CREATE TABLE IF NOT EXISTS bank_txn (
       id          TEXT PRIMARY KEY,
       source      TEXT NOT NULL,
       account     TEXT NOT NULL,
       date        TEXT NOT NULL,
       type        TEXT NOT NULL CHECK (type IN ('in','out')),
       amount      INTEGER NOT NULL CHECK (amount > 0),
       description TEXT NOT NULL,
       imported_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%S','now')))"
    "CREATE INDEX IF NOT EXISTS bank_txn_date ON bank_txn(date)"

    "CREATE TABLE IF NOT EXISTS bank_balance (
       account     TEXT NOT NULL,
       date        TEXT NOT NULL,
       amount      INTEGER NOT NULL,
       PRIMARY KEY (account, date))"

    "CREATE TABLE IF NOT EXISTS bank_fix_run (
       run_at      TEXT PRIMARY KEY DEFAULT (strftime('%Y-%m-%dT%H:%M:%f','now')),
       changes     INTEGER NOT NULL,
       dry         INTEGER NOT NULL CHECK (dry IN (0,1)))"

    "CREATE TABLE IF NOT EXISTS bank_fix_flag (
       bank_id     TEXT PRIMARY KEY,
       date        TEXT NOT NULL,
       amount      INTEGER NOT NULL,
       description TEXT NOT NULL,
       reason      TEXT NOT NULL)"

    "CREATE TABLE IF NOT EXISTS bank_import (
       file_sha1   TEXT PRIMARY KEY,
       path        TEXT NOT NULL,
       rows_in     INTEGER NOT NULL,
       imported_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%S','now')))"
    "CREATE TABLE IF NOT EXISTS bank_fix_added (
       bank_id     TEXT PRIMARY KEY,
       date        TEXT NOT NULL,
       type        TEXT NOT NULL,
       category    TEXT NOT NULL,
       item        TEXT NOT NULL,
       amount      INTEGER NOT NULL)")
  "Bank staging schema.  Idempotent: safe to run on every open.")

(defconst fin-bankdb--txn-cols
  '("id" "source" "account" "date" "type" "amount" "description"))

(defconst fin-bankdb--date-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
  "ISO date, the only form `bank_txn.date' accepts.")

(defun fin-bankdb-ensure ()
  "Create the bank tables if missing.  Return the connection."
  (let ((db (fin-db-open)))
    (dolist (stmt fin-bankdb--ddl)
      (sqlite-execute db stmt))
    db))

(defun fin-bankdb--check-row (row)
  "Signal an error unless ROW matches `fin-bankdb--txn-cols'."
  (pcase-let ((`(,id ,source ,account ,date ,type ,amount ,desc) row))
    (unless (= (length row) (length fin-bankdb--txn-cols))
      (error "fin-bankdb: row arity %d, want %d: %S"
             (length row) (length fin-bankdb--txn-cols) row))
    (unless (and (stringp id) (> (length id) 0))
      (error "fin-bankdb: empty id: %S" row))
    (unless (and (stringp source) (stringp account) (stringp desc))
      (error "fin-bankdb: source/account/description must be strings: %S" row))
    (unless (and (stringp date) (string-match-p fin-bankdb--date-re date))
      (error "fin-bankdb: bad date %S" date))
    (unless (member type '("in" "out"))
      (error "fin-bankdb: bad type %S" type))
    (unless (and (integerp amount) (> amount 0))
      (error "fin-bankdb: amount must be positive integer cents: %S" amount))))

(defun fin-bankdb-insert (rows)
  "Insert ROWS into bank_txn, skipping ids already present.
Each row follows `fin-bankdb--txn-cols'.  Every row is validated before
any write.  Return the number of rows actually inserted."
  (mapc #'fin-bankdb--check-row rows)
  (let ((db  (fin-bankdb-ensure))
        (sql (format "INSERT OR IGNORE INTO bank_txn (%s) VALUES (%s)"
                     (mapconcat #'identity fin-bankdb--txn-cols ",")
                     (mapconcat (lambda (_) "?") fin-bankdb--txn-cols ",")))
        (n   0))
    (with-sqlite-transaction db
      (dolist (row rows)
        (setq n (+ n (sqlite-execute db sql row)))))
    n))

(defun fin-bankdb-file-imported-p (sha1)
  "Non-nil if a file with SHA1 was already imported."
  (fin-bankdb-ensure)
  (fin-db-query "SELECT 1 FROM bank_import WHERE file_sha1 = ?" (list sha1)))

(defun fin-bankdb-record-import (sha1 path rows-in)
  "Record that file SHA1 at PATH yielded ROWS-IN new rows."
  (fin-bankdb-ensure)
  (fin-db-exec
   "INSERT INTO bank_import (file_sha1, path, rows_in) VALUES (?,?,?)"
   (list sha1 path rows-in)))

(defun fin-bankdb-since (from)
  "Bank rows dated FROM (ISO) up to today, as `fin-bankdb-year' returns them."
  (unless (and (stringp from) (string-match-p fin-bankdb--date-re from))
    (error "fin-bankdb: bad date %S" from))
  (fin-bankdb-ensure)
  (fin-db-query
   "SELECT id, date, type, amount, description
      FROM bank_txn
     WHERE date >= ?
       AND date <= date('now', 'localtime')
     ORDER BY date, id"
   (list from)))

(defun fin-bankdb-card-months ()
  "YYYY-MM months that card statements cover."
  (fin-bankdb-ensure)
  (mapcar #'car (fin-db-query
                 "SELECT DISTINCT strftime('%Y-%m', date) FROM bank_txn WHERE account = 'card'")))

(defun fin-bankdb-record-balance (account date cents)
  "Store ACCOUNT's balance CENTS on ISO DATE; a later import of the same day wins."
  (unless (and (stringp account) (stringp date) (string-match-p fin-bankdb--date-re date) (integerp cents))
    (error "fin-bankdb: bad balance %S %S %S" account date cents))
  (fin-bankdb-ensure)
  (fin-db-exec "INSERT OR REPLACE INTO bank_balance (account, date, amount) VALUES (?,?,?)"
               (list account date cents)))

(defun fin-bankdb-latest-balance (account)
  "(DATE . CENTS) of ACCOUNT's latest recorded balance, or nil."
  (fin-bankdb-ensure)
  (let ((r (car (fin-db-query "SELECT date, amount FROM bank_balance WHERE account = ?
                                ORDER BY date DESC LIMIT 1" (list account)))))
    (and r (cons (car r) (cadr r)))))

(defun fin-bankdb-drop-superseded (account start end keep)
  "Delete ACCOUNT rows of the statement period ISO START .. END whose id is
not in KEEP: a later export of the statement no longer lists them.  A card
bill's period is [START, END), its closing day the next bill's first; an
account statement's is [START, END].  Return the number deleted."
  (unless (and (string-match-p fin-bankdb--date-re start) (string-match-p fin-bankdb--date-re end))
    (error "fin-bankdb: bad period %S %S" start end))
  (let* ((db (fin-bankdb-ensure))
         (sql (format "SELECT id FROM bank_txn WHERE account = ? AND date >= ? AND date %s ?"
                      (if (member account '("card" "pj-card")) "<" "<=")))
         (ids (mapcar #'car (fin-db-query sql (list account start end))))
         (gone (cl-set-difference ids keep :test #'equal)))
    (with-sqlite-transaction db
      (dolist (id gone) (sqlite-execute db "DELETE FROM bank_txn WHERE id = ?" (list id))))
    (length gone)))

(defun fin-bankdb-record-fix-run (changes dry flags)
  "Record a `fin-bank-fix' run: CHANGES count, DRY (non-nil for a dry run),
and FLAGS, the (id date amount description reason) bank rows it only
reported, replacing the previous run's."
  (cl-assert (natnump changes))
  (let ((db (fin-bankdb-ensure)))
    (with-sqlite-transaction db
      (sqlite-execute db "INSERT INTO bank_fix_run (changes, dry) VALUES (?, ?)"
                      (list changes (if dry 1 0)))
      (sqlite-execute db "DELETE FROM bank_fix_flag")
      (dolist (f flags)
        (sqlite-execute db "INSERT OR REPLACE INTO bank_fix_flag (bank_id, date, amount, description, reason)
                            VALUES (?,?,?,?,?)" f)))))

(defun fin-bankdb-record-fix-adds (adds)
  "Remember ADDS, (bank-id date type category item amount) each: the ledger
rows `fin-bank-fix' wrote and the bank row each came from."
  (let ((db (fin-bankdb-ensure)))
    (with-sqlite-transaction db
      (dolist (a adds)
        (cl-assert (and (stringp (car a)) (= (length a) 6)) nil "fin-bankdb: bad add %S" a)
        (sqlite-execute db "INSERT OR REPLACE INTO bank_fix_added (bank_id, date, type, category, item, amount)
                            VALUES (?,?,?,?,?,?)" a)))))

(defun fin-bankdb-vanished-adds ()
  "(bank-id date type category item amount) of ledger rows `fin-bank-fix'
added from bank rows a later export no longer lists."
  (fin-bankdb-ensure)
  (fin-db-query "SELECT bank_id, date, type, category, item, amount FROM bank_fix_added
                  WHERE bank_id NOT IN (SELECT id FROM bank_txn)"))

(defun fin-bankdb-forget-adds (ids)
  "Forget the remembered adds of bank row IDS."
  (let ((db (fin-bankdb-ensure)))
    (with-sqlite-transaction db
      (dolist (id ids) (sqlite-execute db "DELETE FROM bank_fix_added WHERE bank_id = ?" (list id))))))

(defun fin-bankdb-last-fix-run ()
  "(RUN-AT CHANGES DRY) of the latest `fin-bank-fix' run, or nil."
  (fin-bankdb-ensure)
  (car (fin-db-query "SELECT run_at, changes, dry FROM bank_fix_run ORDER BY run_at DESC LIMIT 1")))

(defun fin-bankdb-fix-flags ()
  "(DATE AMOUNT DESCRIPTION REASON) the last run reported, newest first."
  (fin-bankdb-ensure)
  (fin-db-query "SELECT date, amount, description, reason FROM bank_fix_flag ORDER BY date DESC"))

(defun fin-bankdb-imported-since (stamp)
  "Bank rows imported after timestamp STAMP (UTC, as SQLite writes it)."
  (fin-bankdb-ensure)
  (caar (fin-db-query "SELECT COUNT(*) FROM bank_txn WHERE imported_at > ?" (list stamp))))

(defun fin-bankdb-account-rows (account)
  "Ids and (FIRST . LAST) dates of ACCOUNT's rows: (IDS . SPAN), SPAN nil if none."
  (fin-bankdb-ensure)
  (let ((ids (mapcar #'car (fin-db-query "SELECT id FROM bank_txn WHERE account = ?" (list account))))
        (span (car (fin-db-query "SELECT MIN(date), MAX(date) FROM bank_txn WHERE account = ?"
                                 (list account)))))
    (cons ids (and (car span) (cons (car span) (cadr span))))))

(defun fin-bankdb-year (year)
  "Bank rows of YEAR up to today as (id date type amount description), by date.
Open card bills list future installments not charged yet; they are skipped."
  (fin-bankdb-ensure)
  (fin-db-query
   "SELECT id, date, type, amount, description
      FROM bank_txn
     WHERE strftime('%Y', date) = ?
       AND date <= date('now', 'localtime')
     ORDER BY date, id"
   (list (format "%04d" year))))

(provide 'bankdb)
;;; bankdb.el ends here
