;;; test-parser.el --- adapters/parser.el unit tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'parser)

(defun fin-test--parse (xml)
  "Parse XML string and return DOM root."
  (with-temp-buffer
    (insert xml)
    (libxml-parse-xml-region (point-min) (point-max))))

;;; ── --cell-value ────────────────────────────────────────────

(ert-deftest parser/cell-value-text ()
  (let* ((dom (fin-test--parse "<cell><p>hello</p></cell>")))
    (should (equal (fin-ods-cell-value dom) "hello"))))

(ert-deftest parser/cell-value-trims-whitespace ()
  (let* ((dom (fin-test--parse "<cell><p>  spaced  </p></cell>")))
    (should (equal (fin-ods-cell-value dom) "spaced"))))

(ert-deftest parser/cell-value-empty-returns-nil ()
  (let* ((dom (fin-test--parse "<cell><p></p></cell>")))
    (should (null (fin-ods-cell-value dom))))
  (let* ((dom (fin-test--parse "<cell></cell>")))
    (should (null (fin-ods-cell-value dom)))))

(ert-deftest parser/cell-value-numeric-from-value-attr ()
  (let* ((dom (fin-test--parse "<cell value=\"500.50\"><p>R$500.50</p></cell>")))
    (should (equal (fin-ods-cell-value dom) 500.5))))

(ert-deftest parser/cell-value-date-attr-wins-over-text ()
  "`date-value' attr must take precedence over visible text and `value'."
  (let* ((dom (fin-test--parse
               "<cell date-value=\"2025-06-30\"><p>30/06/2025</p></cell>")))
    (should (equal (fin-ods-cell-value dom) "2025-06-30")))
  (let* ((dom (fin-test--parse
               "<cell date-value=\"2025-06-30\" value=\"45838\"><p>30/06/2025</p></cell>")))
    (should (equal (fin-ods-cell-value dom) "2025-06-30"))))

;;; ── --expand-row (handles number-columns-repeated) ─────────

(ert-deftest parser/expand-row-no-repeats ()
  (let* ((dom (fin-test--parse
               "<row><table-cell><p>a</p></table-cell><table-cell><p>b</p></table-cell></row>")))
    (should (equal (fin-ods-expand-row dom) '("a" "b")))))

(ert-deftest parser/expand-row-with-repeats ()
  (let* ((dom (fin-test--parse
               "<row><table-cell><p>a</p></table-cell><table-cell number-columns-repeated=\"3\"><p>b</p></table-cell></row>")))
    (should (equal (fin-ods-expand-row dom) '("a" "b" "b" "b")))))

(ert-deftest parser/expand-row-caps-at-max-col ()
  "Excessive `number-columns-repeated' must be clamped to `fin-ods-max-col'."
  (let* ((fin-ods-max-col 5)
         (dom (fin-test--parse
               "<row><table-cell number-columns-repeated=\"999\"><p>x</p></table-cell></row>")))
    (should (equal (length (fin-ods-expand-row dom)) 5))))

;;; ── --sheet / --sheets ─────────────────────────────────────

(ert-deftest parser/sheet-by-name ()
  (let* ((dom (fin-test--parse
               "<doc><table name=\"a\"/><table name=\"b\"/></doc>")))
    (should (equal (dom-attr (fin-ods-sheet dom "b") 'name) "b"))
    (should (null (fin-ods-sheet dom "missing")))))

;;; ── --rows-trimmed ─────────────────────────────────────────

(ert-deftest parser/rows-trimmed-aligns-to-header-width ()
  (let* ((dom (fin-test--parse
               (concat
                "<table name=\"t\">"
                "  <table-row>"
                "    <table-cell><p>a</p></table-cell>"
                "    <table-cell><p>b</p></table-cell>"
                "  </table-row>"
                "  <table-row>"
                "    <table-cell><p>1</p></table-cell>"
                "    <table-cell><p>2</p></table-cell>"
                "    <table-cell><p>extra</p></table-cell>"
                "  </table-row>"
                "</table>")))
         (rows (fin-ods-rows-trimmed dom)))
    (should (equal (car rows) '("a" "b")))
    (should (equal (cadr rows) '("1" "2")))))

(provide 'test-parser)
;;; test-parser.el ends here
