;;; test-ofx.el --- adapters/ofx.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'ofx)

(defconst test-ofx--closed
  "OFXHEADER:100
DATA:OFXSGML
ENCODING:UTF-8
<OFX><BANKMSGSRSV1><STMTTRNRS><STMTRS>
<BANKACCTFROM><BANKID>0260</BANKID></BANKACCTFROM>
<BANKTRANLIST>
<STMTTRN>
<TRNTYPE>DEBIT</TRNTYPE>
<DTPOSTED>20261001000000[-3:BRT]</DTPOSTED>
<TRNAMT>-126.95</TRNAMT>
<FITID>aaa</FITID>
<MEMO>Compra no débito - Padaria</MEMO>
</STMTTRN>
<STMTTRN>
<TRNTYPE>CREDIT</TRNTYPE>
<DTPOSTED>20260930</DTPOSTED>
<TRNAMT>1279.2</TRNAMT>
<FITID>bbb</FITID>
<MEMO>Resgate RDB</MEMO>
</STMTTRN>
</BANKTRANLIST>
</STMTRS></STMTTRNRS></BANKMSGSRSV1></OFX>
"
  "Nubank-style SGML with closing tags.")

(defconst test-ofx--bare
  "OFXHEADER:100
<OFX><CREDITCARDMSGSRSV1><CCSTMTTRNRS><CCSTMTRS>
<CCACCTFROM><ACCTID>x
<BANKTRANLIST>
<STMTTRN>
<DTPOSTED>20250105
<TRNAMT>-10,5
<FITID>c1
<NAME>LOJA
<STMTTRN>
<DTPOSTED>20250106
<TRNAMT>3
<FITID>c2
</BANKTRANLIST>
</CCSTMTRS></CCSTMTTRNRS></CREDITCARDMSGSRSV1></OFX>
"
  "Pure SGML card statement: no closing tags, NAME instead of MEMO.")

(ert-deftest ofx/cents-without-floats ()
  (should (= -12695 (fin-ofx--cents "-126.95")))
  (should (=  1050  (fin-ofx--cents "10,5")))
  (should (=  300   (fin-ofx--cents "+3")))
  (should (=  1     (fin-ofx--cents "0.01")))
  (should-error (fin-ofx--cents "12.345"))
  (should-error (fin-ofx--cents "abc"))
  (should-error (fin-ofx--cents nil)))

(ert-deftest ofx/date-takes-calendar-part ()
  (should (equal "2026-10-01" (fin-ofx--date "20261001000000[-3:BRT]")))
  (should-error (fin-ofx--date "2026-10-01")))

(ert-deftest ofx/parse-closed-tags ()
  (let ((p (fin-ofx-parse test-ofx--closed)))
    (should (equal "account" (plist-get p :kind)))
    (should (equal '(("aaa" "2026-10-01" -12695 "Compra no débito - Padaria")
                     ("bbb" "2026-09-30" 127920 "Resgate RDB"))
                   (plist-get p :txns)))))

(ert-deftest ofx/parse-bare-sgml-card ()
  (let ((p (fin-ofx-parse test-ofx--bare)))
    (should (equal "card" (plist-get p :kind)))
    (should (equal '(("c1" "2025-01-05" -1050 "LOJA")
                     ("c2" "2025-01-06" 300 ""))
                   (plist-get p :txns)))))

(ert-deftest ofx/period-reads-kind-and-range ()
  (should (equal '("account" "2026-09-01" "2026-09-30")
                 (fin-ofx-period "<BANKACCTFROM><BANKTRANLIST><DTSTART>20260901000000[-3:BRT]</DTSTART><DTEND>20260930</DTEND>")))
  (should (equal '("card" "2026-08-06" "2026-09-06")
                 (fin-ofx-period "<CCACCTFROM><BANKTRANLIST><DTSTART>20260806<DTEND>20260906")))
  (should-error (fin-ofx-period "<BANKACCTFROM><BANKTRANLIST>")))

(ert-deftest ofx/parse-rejects-missing-fitid ()
  (should-error
   (fin-ofx-parse "<BANKACCTFROM><BANKTRANLIST><STMTTRN><DTPOSTED>20250101<TRNAMT>1</STMTTRN>")))

(ert-deftest ofx/parse-rejects-unknown-statement ()
  (should-error (fin-ofx-parse "<OFX></OFX>")))

(ert-deftest ofx/read-file-detects-encoding-ignoring-header ()
  (let ((utf8  (make-temp-file "fin-ofx-" nil ".ofx"))
        (liar  (make-temp-file "fin-ofx-" nil ".ofx"))
        (latin (make-temp-file "fin-ofx-" nil ".ofx")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8))
            (write-region "ENCODING:UTF-8\n<MEMO>débito" nil utf8)
            ;; Nubank card bills: header claims 1252, bytes are UTF-8.
            (write-region "ENCODING:USASCII\nCHARSET:1252\n<MEMO>débito" nil liar))
          (let ((coding-system-for-write 'windows-1252))
            (write-region "ENCODING:USASCII\nCHARSET:1252\n<MEMO>débito" nil latin))
          (should (string-suffix-p "débito" (fin-ofx-read-file utf8)))
          (should (string-suffix-p "débito" (fin-ofx-read-file liar)))
          (should (string-suffix-p "débito" (fin-ofx-read-file latin))))
      (delete-file utf8)
      (delete-file liar)
      (delete-file latin))))

(ert-deftest ofx/balance-reads-ledgerbal ()
  (should (equal '("2026-10-01" . 145366)
                 (fin-ofx-balance "<LEDGERBAL>\n<BALAMT>1453.66\n<DTASOF>20261001000000[-3:BRT]\n</LEDGERBAL>")))
  (should (equal '("2026-10-06" . -26341)
                 (fin-ofx-balance "<LEDGERBAL><BALAMT>-263.41</BALAMT><DTASOF>20261006</DTASOF></LEDGERBAL>")))
  (should-not (fin-ofx-balance "<BANKTRANLIST></BANKTRANLIST>")))

(provide 'test-ofx)
;;; test-ofx.el ends here
