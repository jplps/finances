;;; odswrite.el --- Edit the ODS entries and patrimony sheets in place  -*- lexical-binding: t; -*-

;; The entries and patrimony sheets are edited as text inside content.xml,
;; so every row, style and sheet not touched stays byte-identical.  Entries
;; are identified by their values (date type category item amount) and new
;; ones inserted in the sheet's newest-first order; patrimony rows by
;; (category item), new ones after the last row of their category.  Each
;; edit or delete must hit exactly one row.  Saving re-zips content.xml with
;; zip(1), the counterpart of the unzip(1) the reader uses.

(require 'cl-lib)
(require 'xml)
(require 'parser)

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

(defun fin-odsw--sheet-bounds (content &optional sheet)
  "(START . END) of SHEET (default \"entries\") in CONTENT."
  (let* ((sheet (or sheet "entries"))
         (start (or (string-search (format "<table:table table:name=\"%s\"" sheet) content)
                    (error "fin-odsw: no %s sheet" sheet)))
         (end (or (string-search "</table:table>" content start)
                  (error "fin-odsw: %s sheet not closed" sheet))))
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
  "(date type category item amount installment installments via) of ROW;
\"\" for empty.  VIA names how a row was paid when not through the bank
statements (cash, bb)."
  (fin-odsw--cells row 8))

(defun fin-odsw--cells (row n)
  "The first N cell values of ROW, \"\" for empty."
  (let ((pos 0) vals)
    (while (and (< (length vals) n) (string-match fin-odsw--cell-re row pos))
      ;; Capture before `fin-odsw--attr' clobbers the match data.
      (let* ((attrs (match-string 1 row)) (body (match-string 2 row)) (end (match-end 0))
             (rep (string-to-number (or (fin-odsw--attr "table:number-columns-repeated" attrs) "1")))
             (v (fin-odsw--cell-value attrs body)))
        (setq pos end)
        (dotimes (_ (min (max rep 1) n)) (push v vals))))
    (let ((out (seq-take (nreverse vals) n)))
      (append out (make-list (max 0 (- n (length out))) "")))))

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

(defun fin-odsw-row-xml (date type category item amount installment installments &optional via)
  "Entries row in the sheet's own cell format.  AMOUNT in cents;
INSTALLMENT of INSTALLMENTS both nil or 1 <= INSTALLMENT <= INSTALLMENTS;
VIA nil, or how it was paid outside the bank statements."
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
          (if via (fin-odsw--string-cell via) "")
          "</table:table-row>"))

(defun fin-odsw--rebuild-row (vals via)
  "Entries row of row values VALS (`fin-odsw--row-values') with VIA set."
  (pcase-let ((`(,date ,type ,cat ,item ,amount ,k ,n . ,_) vals))
    (fin-odsw-row-xml date type cat item amount
                      (and (integerp k) k) (and (integerp n) n)
                      (and via (not (string-empty-p via)) via))))

;;; ── Apply ──────────────────────────────────────────────────

(defun fin-odsw--find-all (keys key n)
  "Indexes of the N rows of KEYS equal to KEY: N deletes of identical rows
(installments differ only outside the key) are unambiguous only together."
  (let ((hits (cl-loop for k in (cdr keys) for i from 1 when (equal k key) collect i)))
    (unless (= (length hits) n)
      (error "fin-odsw: %d rows match %S, %d deletes" (length hits) key n))
    hits))

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
  (:add (date type category item amount installment installments [via]))
  (:via (date type category item amount) VIA)
Item is \"\" when empty; VIA \"\" clears it.  Signals unless every edit or
via finds a row (identical rows are interchangeable) and the deletes of a
key hit exactly as many rows."
  (pcase-let* ((`(,start . ,end) (fin-odsw--sheet-bounds content))
               (`(,first ,last ,rows) (fin-odsw--rows content start end))
               (rowv (vconcat rows))
               (keys (mapcar (lambda (r) (fin-odsw--key (fin-odsw--row-values r))) rows))
               (gone (make-hash-table)) (edited (make-hash-table)) (adds nil))
    (cl-assert (equal (seq-take (car keys) 3) '("date" "type" "category")) nil
               "fin-odsw: unexpected header %S" (car keys))
    (let ((deletes (make-hash-table :test #'equal)))
      (dolist (c changes)
        (when (eq (car c) :delete) (puthash (cadr c) (1+ (gethash (cadr c) deletes 0)) deletes)))
      (maphash (lambda (key n) (dolist (i (fin-odsw--find-all keys key n)) (puthash i t gone)))
               deletes))
    (dolist (c changes)
      (pcase c
        (`(:edit ,key ,new)
         ;; Identical rows (installments aside) are interchangeable for an
         ;; amount edit: take the first one not yet edited.
         (let ((i (or (cl-loop for k in (cdr keys) for i from 1
                               when (and (equal k key) (not (gethash i gone)) (not (gethash i edited)))
                               return i)
                      (error "fin-odsw: no row left matching %S" key))))
           (puthash i t edited)
           (aset rowv i (fin-odsw--set-amount (aref rowv i) (nth 4 key) new))
           (setf (nth 4 (nth i keys)) new)))
        (`(:via ,key ,via)
         (let ((i (or (cl-loop for k in (cdr keys) for i from 1
                               when (and (equal k key) (not (gethash i gone)) (not (gethash i edited)))
                               return i)
                      (error "fin-odsw: no row left matching %S" key))))
           (puthash i t edited)
           (aset rowv i (fin-odsw--rebuild-row (fin-odsw--row-values (aref rowv i)) via))))
        (`(:delete ,_) nil)
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

;;; ── Patrimony ──────────────────────────────────────────────

(defun fin-odsw-patrimony-row-xml (category item amount lifespan)
  "Patrimony row in the sheet's own cell format.  AMOUNT in cents,
LIFESPAN in months."
  (cl-assert (and (stringp category) (not (string-empty-p category))))
  (cl-assert (and (stringp item) (not (string-empty-p item))))
  (cl-assert (and (integerp amount) (> amount 0)))
  (cl-assert (and (integerp lifespan) (> lifespan 0)))
  (concat "<table:table-row table:style-name=\"ro2\">"
          (fin-odsw--string-cell category) (fin-odsw--string-cell item)
          (fin-odsw--number-cell amount) (fin-odsw--number-cell lifespan)
          "</table:table-row>"))

(defun fin-odsw-apply-patrimony (content changes)
  "CONTENT with CHANGES applied to the patrimony sheet.  Each change is
  (:edit (CATEGORY ITEM) NEW-AMOUNT)
  (:delete (CATEGORY ITEM))
  (:add (CATEGORY ITEM AMOUNT LIFESPAN))
A key names exactly one row; an added key must be new.  Adds follow the
last row of their category, or end the sheet."
  (pcase-let* ((`(,start . ,end) (fin-odsw--sheet-bounds content "patrimony"))
               (`(,first ,last ,rows) (fin-odsw--rows content start end))
               (rowv (vconcat rows))
               (vals (mapcar (lambda (r) (fin-odsw--cells r 4)) rows))
               (keys (mapcar (lambda (v) (seq-take v 2)) vals))
               (gone (make-hash-table)) (adds nil))
    (cl-assert (equal (car keys) '("category" "item")) nil
               "fin-odsw: unexpected patrimony header %S" (car keys))
    (cl-flet ((index (key)
                (let ((hits (cl-loop for k in (cdr keys) for i from 1 when (equal k key) collect i)))
                  (unless (= (length hits) 1)
                    (error "fin-odsw: %d patrimony rows match %S" (length hits) key))
                  (car hits))))
      (dolist (c changes)
        (pcase c
          (`(:edit ,key ,new)
           (let ((i (index key)))
             (aset rowv i (fin-odsw--set-amount (aref rowv i) (nth 2 (nth i vals)) new))))
          (`(:delete ,key) (puthash (index key) t gone))
          (`(:add ,fields)
           (when (member (seq-take fields 2) keys)
             (error "fin-odsw: patrimony row %S exists" (seq-take fields 2)))
           (push fields adds))
          (_ (error "fin-odsw: bad patrimony change %S" c)))))
    (let* ((adds (nreverse adds))
           (tail (1- (length rowv)))
           (anchor (lambda (cat) (or (cl-loop for i downfrom (1- (length rowv)) to 1
                                              when (equal (car (nth i keys)) cat) return i)
                                     tail))))
      (concat (substring content 0 first)
              (apply #'concat
                     (cl-loop for i from 0 below (length rowv)
                              unless (gethash i gone) collect (aref rowv i)
                              append (cl-loop for a in adds
                                              when (= (funcall anchor (car a)) i)
                                              collect (apply #'fin-odsw-patrimony-row-xml a))))
              (substring content last)))))

;;; ── Save ───────────────────────────────────────────────────

(defun fin-odsw-save (path content)
  "Replace content.xml of the ODS at PATH with CONTENT; no copy is kept.
CONTENT must be well-formed; the result must read back.  Return PATH."
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
            (copy-file new path t)
            path)
        (delete-directory tmp t)))))

(provide 'odswrite)
;;; odswrite.el ends here
