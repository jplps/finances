;;; odswrite.el --- Edit the ODS entries sheet in place  -*- lexical-binding: t; -*-

;; The entries sheet is edited as text inside content.xml, so every row,
;; style and sheet not touched stays byte-identical.  Rows are identified by
;; their values (date type category item amount); each edit or delete must
;; hit exactly one row.  New rows are inserted in the sheet's newest-first
;; order.  Saving re-zips content.xml with zip(1), the counterpart of the
;; unzip(1) the reader uses, after backing up the original.

(require 'cl-lib)
(require 'xml)
(require 'parser)

(defcustom fin-ods-backup-dir "backup"
  "Backup directory, relative to the ODS file's directory."
  :type 'string :group 'fin)

(defconst fin-odsw--sheet-open "<table:table table:name=\"entries\""
  "Start tag of the entries sheet.")

(defun fin-odsw-read (path)
  "Raw content.xml of the ODS at PATH, as a string."
  (let ((p (expand-file-name path)))
    (unless (file-readable-p p) (user-error "ODS not readable: %s" p))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (unless (zerop (call-process "unzip" nil t nil "-p" p "content.xml"))
          (user-error "unzip failed on %s" p)))
      (buffer-string))))

;;; ── Rows and cells ─────────────────────────────────────────

(defun fin-odsw--sheet-bounds (content)
  "(START . END) of the entries sheet in CONTENT."
  (let* ((start (or (string-search fin-odsw--sheet-open content)
                    (error "fin-odsw: no entries sheet")))
         (end (or (string-search "</table:table>" content start)
                  (error "fin-odsw: entries sheet not closed"))))
    (cons start end)))

(defun fin-odsw--rows (content start end)
  "Row strings of CONTENT between START and END, with their bounds.
Return (FIRST LAST ROWS): rows must be contiguous."
  (let ((pos start) first last rows)
    (while (let ((b (string-search "<table:table-row" content pos)))
             (when (and b (< b end))
               (let ((e (or (string-search "</table:table-row>" content b)
                            (error "fin-odsw: row not closed at %d" b))))
                 (setq e (+ e (length "</table:table-row>")))
                 (when last (cl-assert (= b last) nil "fin-odsw: rows not contiguous at %d" b))
                 (unless first (setq first b))
                 (push (substring content b e) rows)
                 (cl-assert (> e pos))
                 (setq pos e last e)))))
    (list first last (nreverse rows))))

(defconst fin-odsw--cell-re
  "<table:table-cell\\([^>]*?\\)\\(?:/>\\|>\\(\\(?:.\\|\n\\)*?\\)</table:table-cell>\\)"
  "One cell: group 1 attributes, group 2 body.")

(defun fin-odsw--attr (name attrs)
  (when (string-match (format "%s=\"\\([^\"]*\\)\"" name) attrs) (match-string 1 attrs)))

(defun fin-odsw--cell-value (attrs body)
  (cond ((fin-odsw--attr "office:date-value" attrs)
         (substring (fin-odsw--attr "office:date-value" attrs) 0 10))
        ((equal (fin-odsw--attr "office:value-type" attrs) "float")
         (round (string-to-number (fin-odsw--attr "office:value" attrs))))
        (body (xml-substitute-special (replace-regexp-in-string "<[^>]+>" "" body)))
        (t "")))

(defun fin-odsw--row-values (row)
  "(date type category item amount installment installments) of ROW;
\"\" for empty."
  (let ((pos 0) vals)
    (while (and (< (length vals) 7) (string-match fin-odsw--cell-re row pos))
      ;; Capture before `fin-odsw--attr' clobbers the match data.
      (let* ((attrs (match-string 1 row)) (body (match-string 2 row)) (end (match-end 0))
             (rep (string-to-number (or (fin-odsw--attr "table:number-columns-repeated" attrs) "1")))
             (v (fin-odsw--cell-value attrs body)))
        (setq pos end)
        (dotimes (_ (min (max rep 1) 7)) (push v vals))))
    (let ((out (nreverse vals)))
      (append out (make-list (max 0 (- 7 (length out))) "")))))

(defun fin-odsw--key (vals)
  "Identity of a row: (date type category item amount)."
  (seq-take vals 5))

(defun fin-odsw--string-cell (s)
  (if (or (null s) (equal s ""))
      "<table:table-cell/>"
    (format "<table:table-cell office:value-type=\"string\" calcext:value-type=\"string\"><text:p>%s</text:p></table:table-cell>"
            (xml-escape-string s))))

(defun fin-odsw--number-cell (n)
  (if (null n)
      "<table:table-cell/>"
    (format "<table:table-cell office:value-type=\"float\" office:value=\"%d\" calcext:value-type=\"float\"><text:p>%d</text:p></table:table-cell>"
            n n)))

(defun fin-odsw-row-xml (date type category item amount installment installments)
  "Entries row in the sheet's own cell format.  AMOUNT in cents;
INSTALLMENT of INSTALLMENTS both nil or 1 <= INSTALLMENT <= INSTALLMENTS."
  (cl-assert (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
  (cl-assert (and (integerp amount) (> amount 0)))
  (cl-assert (member type '("in" "out")))
  (cl-assert (or (and (null installment) (null installments))
                 (and (integerp installment) (integerp installments)
                      (<= 1 installment installments)))
             nil "fin-odsw: bad installment %S/%S" installment installments)
  (concat "<table:table-row table:style-name=\"ro1\">"
          (format "<table:table-cell office:value-type=\"date\" office:date-value=\"%s\" calcext:value-type=\"date\"><text:p>%s/%s/%s 12:00 AM</text:p></table:table-cell>"
                  date (substring date 5 7) (substring date 8 10) (substring date 2 4))
          (fin-odsw--string-cell type) (fin-odsw--string-cell category) (fin-odsw--string-cell item)
          (fin-odsw--number-cell amount)
          (fin-odsw--number-cell installment) (fin-odsw--number-cell installments)
          "</table:table-row>"))

;;; ── Apply ──────────────────────────────────────────────────

(defun fin-odsw--find (keys key)
  "Index of the one row of KEYS equal to KEY (header is index 0)."
  (let ((hits (cl-loop for k in (cdr keys) for i from 1 when (equal k key) collect i)))
    (unless (= (length hits) 1)
      (error "fin-odsw: %d rows match %S" (length hits) key))
    (car hits)))

(defun fin-odsw--set-amount (row old new)
  (let ((pat (format "office:value=\"%d\" calcext:value-type=\"float\"><text:p>%d</text:p>" old old)))
    (let ((n 0) (pos 0))
      (while (setq pos (string-search pat row pos))
        (setq n (1+ n) pos (1+ pos)))
      (unless (= n 1) (error "fin-odsw: amount %d found %d times in row" old n)))
    (string-replace pat (format "office:value=\"%d\" calcext:value-type=\"float\"><text:p>%d</text:p>" new new) row)))

(defun fin-odsw--merge (rows adds)
  "ROWS (date . xml), newest first, with ADDS (date . xml) inserted in order.
Within one date, existing rows come first."
  (let ((pending (sort (copy-sequence adds) (lambda (a b) (string> (car a) (car b)))))
        out)
    (dolist (r rows)
      (while (and pending (string> (caar pending) (car r)))
        (push (cdr (pop pending)) out))
      (push (cdr r) out))
    (dolist (p pending) (push (cdr p) out))
    (nreverse out)))

(defun fin-odsw-apply (content changes)
  "CONTENT with CHANGES applied to the entries sheet.  Each change is
  (:edit (date type category item amount) NEW-AMOUNT)
  (:delete (date type category item amount))
  (:add (date type category item amount installment installments))
Item is \"\" when empty.  Signals unless every edit and delete hits one row."
  (pcase-let* ((`(,start . ,end) (fin-odsw--sheet-bounds content))
               (`(,first ,last ,rows) (fin-odsw--rows content start end))
               (rowv (vconcat rows))
               (keys (mapcar (lambda (r) (fin-odsw--key (fin-odsw--row-values r))) rows))
               (gone (make-hash-table)) (adds nil))
    (cl-assert (equal (seq-take (car keys) 3) '("date" "type" "category")) nil
               "fin-odsw: unexpected header %S" (car keys))
    (dolist (c changes)
      (pcase c
        (`(:edit ,key ,new)
         (let ((i (fin-odsw--find keys key)))
           (aset rowv i (fin-odsw--set-amount (aref rowv i) (nth 4 key) new))
           (setf (nth 4 (nth i keys)) new)))
        (`(:delete ,key) (puthash (fin-odsw--find keys key) t gone))
        (`(:add ,fields)
         (push (cons (car fields) (apply #'fin-odsw-row-xml fields)) adds))
        (_ (error "fin-odsw: bad change %S" c))))
    (let ((data (cl-loop for i from 1 below (length rowv)
                         unless (gethash i gone) collect (cons (car (nth i keys)) (aref rowv i)))))
      (cl-assert (cl-loop for (a b) on (mapcar #'car data) while b always (not (string< a b)))
                 nil "fin-odsw: entries sheet not sorted newest first")
      (concat (substring content 0 first)
              (aref rowv 0)
              (apply #'concat (fin-odsw--merge data adds))
              (substring content last)))))

;;; ── Save ───────────────────────────────────────────────────

(defun fin-odsw--backup (path)
  "Copy PATH into `fin-ods-backup-dir' with a timestamp.  Return the copy."
  (let* ((dir (expand-file-name fin-ods-backup-dir (file-name-directory path)))
         (dst (expand-file-name (format "%s-%s.ods" (file-name-base path)
                                        (format-time-string "%Y%m%dT%H%M%S"))
                                dir)))
    (make-directory dir t)
    (copy-file path dst)
    dst))

(defun fin-odsw-save (path content)
  "Replace content.xml of the ODS at PATH with CONTENT, keeping a backup.
CONTENT must be well-formed; the result must read back.  Return the backup."
  (let ((path (expand-file-name path)))
    (unless (executable-find "zip") (user-error "zip(1) not found"))
    (with-temp-buffer
      (insert content)
      (unless (libxml-parse-xml-region (point-min) (point-max))
        (error "fin-odsw: content.xml not well-formed")))
    (let ((tmp (make-temp-file "fin-ods-" t)))
      (unwind-protect
          (let ((new (expand-file-name "new.ods" tmp)))
            (copy-file path new)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region content nil (expand-file-name "content.xml" tmp) nil 'silent))
            (let ((default-directory (file-name-as-directory tmp)))
              (unless (zerop (call-process "zip" nil nil nil "-q" "new.ods" "content.xml"))
                (error "fin-odsw: zip failed")))
            (unless (fin-ods-sheet (fin-ods-parse new) "entries")
              (error "fin-odsw: rewritten ODS lost its entries sheet"))
            (prog1 (fin-odsw--backup path)
              (copy-file new path t)))
        (delete-directory tmp t)))))

(provide 'odswrite)
;;; odswrite.el ends here
