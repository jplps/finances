;;; bankdb.el --- Bank transaction staging tables  -*- lexical-binding: t; -*-

;; Bank rows live in the same SQLite file as the ODS mirror, but outside
;; `fin-db--tables', so `fin-db-rebuild' and `fin-sync-refresh' never touch
;; them.  They are re-derivable from the files in the inbox: imports are
;; idempotent by transaction id and by file hash.

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

    "CREATE TABLE IF NOT EXISTS bank_import (
       file_sha1   TEXT PRIMARY KEY,
       path        TEXT NOT NULL,
       rows_in     INTEGER NOT NULL,
       imported_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%S','now')))")
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
