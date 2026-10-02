;;; test-odswrite.el --- adapters/odswrite.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'odswrite)

(defun test-odsw--content (&rest rows)
  "content.xml with an entries sheet of ROWS (date type cat item amount note)."
  (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<office:document-content xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
          " xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\""
          " xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\""
          " xmlns:calcext=\"urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0\">"
          "<office:body><office:spreadsheet>"
          "<table:table table:name=\"entries\" table:style-name=\"ta1\"><table:table-column/>"
          "<table:table-row table:style-name=\"ro1\">"
          (mapconcat #'fin-odsw--string-cell '("date" "type" "category" "item" "amount" "currency" "note") "")
          "</table:table-row>"
          (mapconcat (lambda (r) (apply #'fin-odsw-row-xml r)) rows "")
          "</table:table>"
          "<table:table table:name=\"plan\"><table:table-row><table:table-cell/></table:table-row></table:table>"
          "</office:spreadsheet></office:body></office:document-content>"))

(defun test-odsw--keys (content)
  (pcase-let* ((`(,s . ,e) (fin-odsw--sheet-bounds content))
               (`(,_f ,_l ,rows) (fin-odsw--rows content s e)))
    (mapcar (lambda (r) (seq-take (fin-odsw--row-values r) 6)) (cdr rows))))

(defconst test-odsw--base
  (test-odsw--content '("2026-09-04" "out" "cnpj" "" 166400 "")
                      '("2026-07-13" "out" "food" "komprão" 42644 "")
                      '("2026-06-01" "out" "patrimony" "escova" 6500 "")
                      '("2026-05-02" "out" "car" "seguro" 15353 "installment 4/12")))

(ert-deftest odsw/row-roundtrip-keeps-values ()
  (should (equal '(("2026-09-04" "out" "cnpj" "" 166400 "BRL")
                   ("2026-07-13" "out" "food" "komprão" 42644 "BRL")
                   ("2026-06-01" "out" "patrimony" "escova" 6500 "BRL")
                   ("2026-05-02" "out" "car" "seguro" 15353 "BRL"))
                 (test-odsw--keys test-odsw--base))))

(ert-deftest odsw/apply-edits-deletes-and-inserts-in-date-order ()
  (let ((out (fin-odsw-apply
              test-odsw--base
              '((:edit ("2026-09-04" "out" "cnpj" "" 166400) 139900)
                (:delete ("2026-06-01" "out" "patrimony" "escova" 6500))
                (:add ("2026-10-01" "out" "free" "ifood & co" 8547 nil))
                (:add ("2026-07-13" "out" "food" "hippo" 3827 nil))
                (:add ("2026-01-02" "in" "extras" "zuleika" 3500 "x"))))))
    (should (equal '(("2026-10-01" "out" "free" "ifood & co" 8547 "BRL")
                     ("2026-09-04" "out" "cnpj" "" 139900 "BRL")
                     ("2026-07-13" "out" "food" "komprão" 42644 "BRL")
                     ("2026-07-13" "out" "food" "hippo" 3827 "BRL")
                     ("2026-05-02" "out" "car" "seguro" 15353 "BRL")
                     ("2026-01-02" "in" "extras" "zuleika" 3500 "BRL"))
                   (test-odsw--keys out)))
    (should (string-search "ifood &amp; co" out))
    (should (string-search "<table:table table:name=\"plan\">" out))))

(ert-deftest odsw/apply-fails-loud-on-missing-or-ambiguous-row ()
  (should-error (fin-odsw-apply test-odsw--base '((:delete ("2026-06-02" "out" "patrimony" "escova" 6500)))))
  (let ((dup (test-odsw--content '("2026-01-01" "out" "food" "x" 100 "")
                                 '("2026-01-01" "out" "food" "x" 100 ""))))
    (should-error (fin-odsw-apply dup '((:delete ("2026-01-01" "out" "food" "x" 100))))))
  (should-error (fin-odsw-apply test-odsw--base '((:add ("2026-01-01" "out" "food" "x" -5 nil))))))

(ert-deftest odsw/save-rezips-reads-back-and-backs-up ()
  (let* ((dir (make-temp-file "fin-odsw-" t))
         (ods (expand-file-name "seeds.ods" dir))
         (fin-ods-backup-dir "backup"))
    (unwind-protect
        (let ((default-directory (file-name-as-directory dir)))
          (with-temp-file "mimetype" (insert "application/vnd.oasis.opendocument.spreadsheet"))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region test-odsw--base nil "content.xml" nil 'silent))
          (should (zerop (call-process "zip" nil nil nil "-q" "-X" "-0" "seeds.ods" "mimetype")))
          (should (zerop (call-process "zip" nil nil nil "-q" "-X" "seeds.ods" "content.xml")))
          (let* ((new (fin-odsw-apply (fin-odsw-read ods)
                                      '((:add ("2026-10-02" "out" "free" "teste" 100 nil)))))
                 (backup (fin-odsw-save ods new)))
            (should (file-exists-p backup))
            (should (string-prefix-p (expand-file-name "backup/" dir) backup))
            (should (equal new (fin-odsw-read ods)))
            (should (equal "2026-10-02" (car (nth 1 (fin-ods-rows (fin-ods-sheet (fin-ods-parse ods) "entries"))))))))
      (delete-directory dir t))))

(provide 'test-odswrite)
;;; test-odswrite.el ends here
