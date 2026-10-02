;;; ofx.el --- OFX 1.x (SGML) / 2.x statement parser  -*- lexical-binding: t; -*-

;; Reads only what reconciliation needs: account kind and the STMTTRN list.
;; Values are taken up to the next tag, so both closed (<X>v</X>) and bare
;; SGML (<X>v) elements parse.  Amounts become integer cents without floats.

(require 'cl-lib)

(defun fin-ofx--decode (raw)
  "Decode unibyte RAW as UTF-8 when valid, else as windows-1252.
Headers are not trusted: Nubank card bills declare CHARSET:1252 but are
UTF-8.  Invalid UTF-8 decodes to eight-bit raw bytes, which flags it."
  (let ((utf8 (decode-coding-string raw 'utf-8)))
    (if (cl-some (lambda (c) (eq (char-charset c) 'eight-bit)) utf8)
        (decode-coding-string raw 'windows-1252)
      utf8)))

(defun fin-ofx-read-file (path)
  "Return the decoded contents of OFX file at PATH."
  (unless (file-readable-p path)
    (error "fin-ofx: cannot read %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (fin-ofx--decode (buffer-string))))

(defun fin-ofx--field (block tag)
  "Trimmed value of TAG inside BLOCK, or nil."
  (when (string-match (format "<%s>\\([^<\r\n]*\\)" tag) block)
    (string-trim (match-string 1 block))))

(defun fin-ofx--date (s)
  "OFX datetime S (YYYYMMDD...) as ISO date."
  (unless (and s (string-match "\\`\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" s))
    (error "fin-ofx: bad date %S" s))
  (format "%s-%s-%s" (match-string 1 s) (match-string 2 s) (match-string 3 s)))

(defun fin-ofx--cents (s)
  "OFX amount S (e.g. \"-126.95\", \"10,5\") as signed integer cents."
  (unless (and s (string-match
                  "\\`\\([-+]\\)?\\([0-9]+\\)\\(?:[.,]\\([0-9]\\{1,2\\}\\)\\)?\\'" s))
    (error "fin-ofx: bad amount %S" s))
  (let* ((neg  (equal (match-string 1 s) "-"))
         (frac (or (match-string 3 s) "0"))
         (abs  (+ (* 100 (string-to-number (match-string 2 s)))
                  (* (string-to-number frac) (if (= 1 (length frac)) 10 1)))))
    (if neg (- abs) abs)))

(defun fin-ofx--kind (text)
  "\"card\" for credit-card statements, \"account\" for bank ones."
  (cond ((string-match-p "<CCACCTFROM>" text) "card")
        ((string-match-p "<BANKACCTFROM>" text) "account")
        (t (error "fin-ofx: no BANKACCTFROM/CCACCTFROM block"))))

(defun fin-ofx--txn (block)
  "STMTTRN BLOCK as (fitid date cents memo)."
  (let ((fitid (fin-ofx--field block "FITID")))
    (unless (and fitid (> (length fitid) 0))
      (error "fin-ofx: STMTTRN without FITID: %s" block))
    (list fitid
          (fin-ofx--date (fin-ofx--field block "DTPOSTED"))
          (fin-ofx--cents (fin-ofx--field block "TRNAMT"))
          (or (fin-ofx--field block "MEMO") (fin-ofx--field block "NAME") ""))))

(defun fin-ofx-parse (text)
  "Parse OFX TEXT.  Return plist (:kind KIND :txns ((fitid date cents memo) ...))."
  (let ((kind (fin-ofx--kind text))
        (pos  0)
        txns)
    (while (string-match "<STMTTRN>\\(\\(?:.\\|\n\\)*?\\)\\(?:</STMTTRN>\\|<STMTTRN>\\|</BANKTRANLIST>\\)"
                         text pos)
      ;; Capture before `fin-ofx--txn' clobbers the match data.  Resume at
      ;; the end of the body, so a bare next <STMTTRN> is seen; that end is
      ;; strictly past the opening tag, so the loop always advances.
      (let ((body (match-string 1 text))
            (end  (match-end 1)))
        (cl-assert (> end pos))
        (push (fin-ofx--txn body) txns)
        (setq pos end)))
    (list :kind kind :txns (nreverse txns))))

(provide 'ofx)
;;; ofx.el ends here
