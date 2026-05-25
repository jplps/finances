;;; parser.el --- Shared ODS XML parse helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dom)
(require 'subr-x)

(defcustom fin-ods-max-col 30
  "Cap on cells per row when expanding `number-columns-repeated'."
  :type 'integer :group 'fin)

(defun fin-ods-parse (path)
  "Return libxml DOM of PATH's content.xml.  Errors if unreadable."
  (let ((p (expand-file-name path)))
    (unless (file-readable-p p)
      (user-error "ODS not readable: %s" p))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (unless (zerop (call-process "unzip" nil t nil "-p" p "content.xml"))
          (user-error "unzip failed on %s" p)))
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun fin-ods-cell-text (cell)
  "Recursively gather text inside CELL."
  (cond ((null cell) "")
        ((stringp cell) cell)
        (t (mapconcat #'fin-ods-cell-text (dom-children cell) ""))))

(defun fin-ods-cell-value (cell)
  "Return ISO string for date cells, float for numeric, trimmed string for text, nil for empty."
  (let ((dv  (dom-attr cell 'date-value))
        (v   (dom-attr cell 'value))
        (txt (string-trim (fin-ods-cell-text cell))))
    (cond (dv  dv)
          (v   (string-to-number v))
          ((string-empty-p txt) nil)
          (t   txt))))

(defun fin-ods-expand-row (row)
  "Expand <table-cell number-columns-repeated=N> with `fin-ods-max-col' cap."
  (let (out)
    (dolist (cell (dom-by-tag row 'table-cell))
      (let* ((rep (string-to-number (or (dom-attr cell 'number-columns-repeated) "1")))
             (rep (min (max rep 1) fin-ods-max-col))
             (val (fin-ods-cell-value cell)))
        (dotimes (_ rep) (push val out))))
    (nreverse out)))

(defun fin-ods-sheets (dom)
  (dom-by-tag dom 'table))

(defun fin-ods-sheet (dom name)
  (cl-find-if (lambda (s) (equal (dom-attr s 'name) name))
              (fin-ods-sheets dom)))

(defun fin-ods-rows (sheet)
  "Return all row-cell lists for SHEET, no trimming."
  (when sheet
    (mapcar #'fin-ods-expand-row (dom-by-tag sheet 'table-row))))

(defun fin-ods-rows-trimmed (sheet)
  "Like `fin-ods-rows' but trims each row to header (first row) width."
  (when sheet
    (let* ((all (fin-ods-rows sheet))
           (hw  (length (car all))))
      (cons (car all)
            (mapcar (lambda (r) (seq-take r hw)) (cdr all))))))

(provide 'parser)
;;; parser.el ends here
