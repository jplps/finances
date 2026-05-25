;;; test-sync.el --- tools/sync.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'sync)

(defun fin-test--sheet-dom (name rows)
  "Synthesize an ODS-shaped <table> DOM for NAME with ROWS (lists of cell strings).
Cells must be strings; numerics expressed as <table-cell value=\"X\">."
  (with-temp-buffer
    (insert (format "<doc><table name=\"%s\">" name))
    (dolist (r rows)
      (insert "<table-row>")
      (dolist (cell r)
        (cond
         ((null cell)
          (insert "<table-cell/>"))
         ((numberp cell)
          (insert (format "<table-cell value=\"%s\"><p>%s</p></table-cell>" cell cell)))
         ((string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" cell)
          (insert (format "<table-cell date-value=\"%s\"><p>%s</p></table-cell>" cell cell)))
         (t
          (insert (format "<table-cell><p>%s</p></table-cell>" cell)))))
      (insert "</table-row>"))
    (insert "</table></doc>")
    (libxml-parse-xml-region (point-min) (point-max))))

;;; ── --entries row builder ──────────────────────────────────

(ert-deftest sync/entries-builder-rounds-amount-defaults-currency ()
  (let* ((dom (fin-test--sheet-dom
               "entries"
               '(("date" "type" "category" "item" "amount" "currency" "note")
                 ("2025-01-31" "in" "salary" nil 5000.51 nil nil))))
         (out (car (fin-sync--entries dom))))
    (should (equal "2025-01-31" (nth 0 out)))
    (should (equal "in"         (nth 1 out)))
    (should (equal "salary"     (nth 2 out)))
    (should (null              (nth 3 out)))
    (should (= 5001            (nth 4 out)))   ; rounded
    (should (equal "BRL"        (nth 5 out))))) ; default

(ert-deftest sync/entries-builder-skips-blank-date-rows ()
  (let* ((dom (fin-test--sheet-dom
               "entries"
               '(("date" "type" "category" "item" "amount" "currency" "note")
                 (nil    "in"   "salary"   nil    1000.0 nil nil)
                 ("2025-01-31" "in" "salary" nil 1000.0 nil nil)))))
    (should (= 1 (length (fin-sync--entries dom))))))

;;; ── --plan row builder ─────────────────────────────────────

(ert-deftest sync/plan-builder-amount-and-share-handled ()
  (let* ((dom (fin-test--sheet-dom
               "plan"
               '(("category" "parent" "type" "amount" "share")
                 ("house" nil "fix" 200000 nil)
                 ("investments" nil "var" nil 0.25))))
         (rows (fin-sync--plan dom)))
    (should (= 2 (length rows)))
    (should (= 200000 (nth 3 (car rows))))
    (should (equal 0.25 (nth 4 (cadr rows))))))

;;; ── --patrimony / --accounts row builders ──────────────────

(ert-deftest sync/patrimony-builder-rounds-amount ()
  (let* ((dom (fin-test--sheet-dom
               "patrimony"
               '(("category" "item" "amount" "lifespan_months")
                 ("car" "wheels" 1234.56 60))))
         (row (car (fin-sync--patrimony dom))))
    (should (= 1235 (nth 2 row)))
    (should (= 60   (nth 3 row)))))

(ert-deftest sync/accounts-builder-defaults-balance-to-zero ()
  (let* ((dom (fin-test--sheet-dom
               "accounts"
               '(("category" "parent" "balance" "updated_at")
                 ("main" nil nil "2025-01-31"))))
         (row (car (fin-sync--accounts dom))))
    (should (= 0 (nth 2 row)))))

;;; ── full refresh: row-builder + bulk-insert + truncate path ─

(ert-deftest sync/refresh-end-to-end-with-stubbed-dom ()
  "Bypass file IO: stub `fin-ods-parse' with an in-memory DOM."
  (fin-test-with-db
    (let* ((dom (with-temp-buffer
                  (insert
                   "<doc>"
                   "<table name=\"entries\"><table-row>"
                   "  <table-cell><p>date</p></table-cell>"
                   "  <table-cell><p>type</p></table-cell>"
                   "  <table-cell><p>category</p></table-cell>"
                   "  <table-cell><p>item</p></table-cell>"
                   "  <table-cell><p>amount</p></table-cell>"
                   "  <table-cell><p>currency</p></table-cell>"
                   "  <table-cell><p>note</p></table-cell>"
                   "</table-row>"
                   "<table-row>"
                   "  <table-cell date-value=\"2025-01-31\"><p>2025-01-31</p></table-cell>"
                   "  <table-cell><p>in</p></table-cell>"
                   "  <table-cell><p>salary</p></table-cell>"
                   "  <table-cell/>"
                   "  <table-cell value=\"1000\"><p>1000</p></table-cell>"
                   "  <table-cell/>"
                   "  <table-cell/>"
                   "</table-row></table>"
                   "<table name=\"plan\"><table-row/></table>"
                   "<table name=\"patrimony\"><table-row/></table>"
                   "<table name=\"accounts\"><table-row/></table>"
                   "</doc>")
                  (libxml-parse-xml-region (point-min) (point-max)))))
      (cl-letf (((symbol-function 'fin-ods-parse) (lambda (_) dom))
                ((symbol-function 'file-attributes) (lambda (_) nil))
                ((symbol-function 'file-attribute-modification-time) (lambda (_) (current-time))))
        (let ((n (fin-sync-refresh)))
          (should (= 1 n))
          (should (= 1 (fin-db-count "entry"))))))))

(provide 'test-sync)
;;; test-sync.el ends here
