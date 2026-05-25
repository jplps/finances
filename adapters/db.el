;;; db.el --- SQLite mirror of finances ODS  -*- lexical-binding: t; -*-

(require 'sqlite)

(defgroup fin nil
  "Personal finance tool — ODS mirror + read-only dashboard."
  :group 'applications
  :prefix "fin-")

(defcustom fin-db-path
  (expand-file-name "../infra/finances.db"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Path to the SQLite mirror DB."
  :type 'file
  :group 'fin)

(defvar fin-db--conn nil
  "Live SQLite connection, or nil.")

(defconst fin-db--ddl
  '("CREATE TABLE entry (
       id         INTEGER PRIMARY KEY,
       date       TEXT NOT NULL,
       type       TEXT NOT NULL,
       category   TEXT NOT NULL,
       item       TEXT,
       amount     INTEGER NOT NULL,
       currency   TEXT NOT NULL DEFAULT 'BRL',
       note       TEXT,
       created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%S','now')))"
    "CREATE INDEX entry_date ON entry(date)"
    "CREATE INDEX entry_cat  ON entry(category)"
    "CREATE INDEX entry_item ON entry(item)"

    "CREATE TABLE budget (
       category    TEXT NOT NULL,
       parent      TEXT,
       type        TEXT,
       amount      INTEGER,
       share       REAL,
       PRIMARY KEY (category, parent))"

    "CREATE TABLE patrimony (
       id              INTEGER PRIMARY KEY,
       category        TEXT NOT NULL,
       item            TEXT NOT NULL,
       amount          INTEGER NOT NULL,
       lifespan_months INTEGER)"
    "CREATE INDEX patrimony_cat ON patrimony(category)"

    "CREATE TABLE account (
       category    TEXT PRIMARY KEY,
       parent      TEXT,
       balance     INTEGER NOT NULL,
       updated_at  TEXT)"

    "CREATE TABLE refresh_log (
       id         INTEGER PRIMARY KEY,
       ran_at     TEXT NOT NULL,
       ods_mtime  TEXT,
       entries_in INTEGER,
       notes      TEXT)")
  "Full schema as a list of standalone SQL statements.")

(defconst fin-db--tables
  '("entry" "budget" "patrimony" "account" "refresh_log")
  "Logical tables — order irrelevant for DROP IF EXISTS.")

(defun fin-db-available-p ()
  "Non-nil if this Emacs build has SQLite support."
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(defun fin-db-open ()
  "Open (or reuse) the DB connection. Auto-creates parent dir."
  (unless (fin-db-available-p)
    (user-error "Emacs lacks SQLite support — need Emacs 29+ built with sqlite3"))
  (unless fin-db--conn
    (let ((dir (file-name-directory fin-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq fin-db--conn (sqlite-open fin-db-path)))
  fin-db--conn)

(defun fin-db-close ()
  "Close the live connection, if any."
  (interactive)
  (when fin-db--conn
    (sqlite-close fin-db--conn)
    (setq fin-db--conn nil)))

(defun fin-db-rebuild ()
  "Drop every table and recreate from `fin-db--ddl'. Destructive."
  (interactive)
  (let ((db (fin-db-open)))
    (with-sqlite-transaction db
      (dolist (tbl fin-db--tables)
        (sqlite-execute db (format "DROP TABLE IF EXISTS %s" tbl)))
      (dolist (stmt fin-db--ddl)
        (sqlite-execute db stmt)))
    (message "fin-db: rebuilt at %s" fin-db-path)))

(defun fin-db-query (sql &optional values)
  "Run SELECT SQL with optional VALUES (vector or list). Return rows."
  (sqlite-select (fin-db-open) sql values))

(defun fin-db-exec (sql &optional values)
  "Run a single non-SELECT statement."
  (sqlite-execute (fin-db-open) sql values))

(defun fin-db-bulk-insert (table cols rows)
  "Insert ROWS into TABLE (COLS is a list of column names).
ROWS is a list of value lists matching COLS order. Single transaction."
  (when rows
    (let* ((db   (fin-db-open))
           (phs  (mapconcat (lambda (_) "?") cols ","))
           (sql  (format "INSERT INTO %s (%s) VALUES (%s)"
                         table (mapconcat #'identity cols ",") phs)))
      (with-sqlite-transaction db
        (dolist (row rows)
          (sqlite-execute db sql row))))))

(defun fin-db-truncate (table)
  "Wipe all rows of TABLE."
  (fin-db-exec (format "DELETE FROM %s" table)))

(defun fin-db-count (table)
  "Row count of TABLE."
  (caar (fin-db-query (format "SELECT COUNT(*) FROM %s" table))))

(provide 'db)
;;; db.el ends here
