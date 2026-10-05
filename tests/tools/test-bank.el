;;; test-bank.el --- tools/bank.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'bank)

(ert-deftest bank/cents-formats-sign-once ()
  (should (equal "1763.55"  (fin-bank--cents 176355)))
  (should (equal "-1763.55" (fin-bank--cents -176355)))
  (should (equal "-0.92"    (fin-bank--cents -92)))
  (should (equal "0.00"     (fin-bank--cents 0))))

(ert-deftest bank/tsv-row-follows-ods-entry-columns ()
  (should (equal "2025-06-10\tout\t\tpadaria x\t1234\t\t"
                 (fin-bank--tsv-row
                  '("id1" "2025-06-10" "out" 1234 "  Padaria X ")))))

(ert-deftest bank/date-span-min-max ()
  (should (null (fin-bank--date-span nil)))
  (should (equal '("2025-01-02" . "2025-03-04")
                 (fin-bank--date-span '((a "2025-02-01") (b "2025-03-04")
                                        (c "2025-01-02"))))))

(ert-deftest bank/ledger-span-pads-by-window-and-excludes-future ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-06" "out" "food" 100 nil) ; 4d before
    (fin-test-insert-entry "2025-06-07" "out" "food" 100 nil) ; 3d before
    (fin-test-insert-entry "2025-06-23" "out" "food" 100 nil) ; 3d after
    (fin-test-insert-entry "2025-06-24" "out" "food" 100 nil) ; 4d after
    (fin-test-insert-entry "2099-06-10" "out" "food" 100 nil)
    (let ((fin-reconcile-window 3))
      (should (equal '("2025-06-07" "2025-06-23")
                     (mapcar (lambda (r) (nth 1 r))
                             (fin-bank--ledger-span '("2025-06-10" . "2025-06-20")))))
      (should (null (fin-bank--ledger-span '("2099-06-10" . "2099-06-10"))))
      (should (null (fin-bank--ledger-span nil))))))

(ert-deftest bank/copy-missing-puts-bank-only-rows-on-kill-ring ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-10" "out" "food" 1000 "padaria")
    (fin-bankdb-insert '(("a" "test" "card" "2025-06-11" "out" 1000 "Padaria")
                         ("b" "test" "card" "2025-06-12" "out" 2500 "Mercado")))
    (let ((kill-ring nil))
      (fin-bank-copy-missing 2025)
      (should (equal "2025-06-12\tout\t\tmercado\t2500\t\t" (car kill-ring))))))

(ert-deftest bank/tsv-row-carries-installment ()
  (should (equal "2025-06-10\tout\t\tdecathlon - parcela 2/6\t6499\t2\t6"
                 (fin-bank--tsv-row '("id" "2025-06-10" "out" 6499 "Decathlon - Parcela 2/6")))))

(ert-deftest bank/reconcile-rejects-bad-year ()
  (should-error (fin-bank-reconcile 25) :type 'user-error))

(ert-deftest bank/reconcile-buffer-lists-sections ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-10" "out" "food" 1000 "padaria")
    (fin-test-insert-entry "2025-01-10" "out" "food" 1000 "outside coverage")
    (fin-bankdb-insert '(("b" "test" "card" "2025-06-12" "out" 2500 "Mercado")))
    (save-window-excursion
      (fin-bank-reconcile 2025)
      (with-current-buffer "*fin-reconcile 2025*"
        (let ((text (buffer-string)))
          (should (string-match-p "Bank only — missing in ODS (1)" text))
          (should (string-match-p "Ledger only — not at bank (1)" text))
          (should (string-match-p "2025-06 out" text))
          (should (string-match-p "Bank coverage 2025-06-12 .. 2025-06-12" text))
          (should-not (string-match-p "outside coverage" text)))
        (kill-buffer)))))

(ert-deftest bank/near-rows-shown-and-not-copied-as-missing ()
  (fin-test-with-db
    (fin-test-insert-entry "2025-06-13" "out" "food" 42644 "komprão")
    (fin-bankdb-insert '(("k" "test" "account" "2025-06-13" "out" 42664
                          "Compra no débito - KOMPRAO")))
    (let ((kill-ring nil)
          (fin-reconcile-near-ratio 0.05))
      (fin-bank-copy-missing 2025)
      (should (null kill-ring)))
    (save-window-excursion
      (let ((fin-reconcile-near-ratio 0.05))
        (fin-bank-reconcile 2025))
      (with-current-buffer "*fin-reconcile 2025*"
        (should (string-match-p
                 "Possible matches — amounts differ (bank vs ledger) (1)"
                 (buffer-string)))
        (should (string-match-p "426.64 vs     426.44 (Δ 0.20)" (buffer-string)))
        (kill-buffer)))))

;;; ── Import ─────────────────────────────────────────────────

(defmacro test-bank--with-ofx (var text &rest body)
  "Bind VAR to a temp .ofx file holding TEXT while running BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "fin-bank-" nil ".ofx")))
     (unwind-protect
         (progn (let ((coding-system-for-write 'utf-8))
                  (write-region ,text nil ,var))
                ,@body)
       (delete-file ,var))))

(defconst test-bank--ofx
  "ENCODING:UTF-8
<DTSERVER>20261002101930[0:GMT]</DTSERVER>
<BANKACCTFROM><BANKTRANLIST><DTSTART>20250601</DTSTART><DTEND>20250630</DTEND>
<STMTTRN><DTPOSTED>20250610</DTPOSTED><TRNAMT>-10.00</TRNAMT><FITID>a</FITID><MEMO>Padaria</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20250610</DTPOSTED><TRNAMT>25.50</TRNAMT><FITID>b</FITID><MEMO>Pix recebido</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20250610</DTPOSTED><TRNAMT>0.00</TRNAMT><FITID>z</FITID><MEMO>Zero</MEMO></STMTTRN>
</BANKTRANLIST>")

(ert-deftest bank/import-file-maps-sign-to-type-and-drops-zero ()
  (fin-test-with-db
    (test-bank--with-ofx f test-bank--ofx
      (should (= 2 (fin-bank-import-file f)))
      (should (equal '(("account" "2025-06-10" "out" 1000 "Padaria")
                       ("account" "2025-06-10" "in" 2550 "Pix recebido"))
                     (fin-db-query
                      "SELECT account, date, type, amount, description
                         FROM bank_txn ORDER BY amount"))))))

(ert-deftest bank/pdf-rows-stop-where-ofx-card-rows-start ()
  (let* ((text (concat "VENCIMENTO 13 DEZ 2023\n"
                       "Total de compras, 06 NOV a 06 DEZ\t30,00\n"
                       "TRANSAÇÕES\tDE 06 NOV A 06 DEZ\tVALORES EM R$\n"
                       "07 NOV\tPadaria\t10,00\n"
                       "08 NOV\tEstorno de \"Loja\"\t5,00\n"
                       "06 DEZ\tMercado\t20,00\n"))
         (res (fin-bank--pdf-rows text "2023-12-06")))
    (should (equal '(("nubank-pdf" "card" "2023-11-07" "out" 1000 "Padaria")
                     ("nubank-pdf" "card" "2023-11-08" "in" 500 "Estorno de \"Loja\""))
                   (mapcar #'cdr (car res))))
    (should (= 1 (cdr res)))                       ; 06 DEZ left to the OFX
    (should (string-prefix-p "nu:pdf:" (caar (car res))))))

(defconst test-bank--shared-fitid
  "<CCACCTFROM><BANKTRANLIST>
<STMTTRN><DTPOSTED>20260506</DTPOSTED><TRNAMT>-152.46</TRNAMT><FITID>L</FITID><MEMO>Latam - Parcela 5/6</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260605</DTPOSTED><TRNAMT>-152.46</TRNAMT><FITID>L</FITID><MEMO>Latam - Parcela 6/6</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260605</DTPOSTED><TRNAMT>1.27</TRNAMT><FITID>L</FITID><MEMO>Desconto Antecipação Latam</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260511</DTPOSTED><TRNAMT>-42.90</TRNAMT><FITID>E</FITID><MEMO>Loja</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260512</DTPOSTED><TRNAMT>42.90</TRNAMT><FITID>E</FITID><MEMO>Estorno de Loja</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260520</DTPOSTED><TRNAMT>-10.00</TRNAMT><FITID>D</FITID><MEMO>Café</MEMO></STMTTRN>
<STMTTRN><DTPOSTED>20260520</DTPOSTED><TRNAMT>-10.00</TRNAMT><FITID>D</FITID><MEMO>Café</MEMO></STMTTRN>
</BANKTRANLIST>"
  "Nubank card bill: installments, discount and refund reuse one FITID;
the last two rows are a genuine double charge.")

(ert-deftest bank/import-keeps-rows-sharing-fitid ()
  (fin-test-with-db
    (test-bank--with-ofx f test-bank--shared-fitid
      (should (= 7 (fin-bank-import-file f))))))

(ert-deftest bank/overlapping-statement-reuses-ids ()
  ;; Same transactions in a re-export (different file hash) add nothing.
  (fin-test-with-db
    (test-bank--with-ofx f test-bank--shared-fitid
      (fin-bank-import-file f))
    (test-bank--with-ofx f (concat "<DTSERVER>2\n" test-bank--shared-fitid)
      (should (= 0 (fin-bank-import-file f))))
    (should (= 7 (fin-db-count "bank_txn")))))

(ert-deftest bank/import-file-once-per-content ()
  (fin-test-with-db
    (test-bank--with-ofx f test-bank--ofx
      (should (= 2 (fin-bank-import-file f)))
      (should (null (fin-bank-import-file f)))
      (should (= 1 (fin-db-count "bank_import"))))))

(ert-deftest bank/import-file-rejects-non-ofx ()
  (should-error (fin-bank-import-file "/tmp/x.csv") :type 'user-error))

(ert-deftest bank/import-scans-inbox ()
  (fin-test-with-db
    (let ((fin-bank-inbox (make-temp-file "fin-inbox-" t)))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8))
              (write-region test-bank--ofx nil
                            (expand-file-name "a.OFX" fin-bank-inbox))
              (write-region "ignored" nil
                            (expand-file-name "notes.txt" fin-bank-inbox)))
            (should (= 2 (fin-bank-import)))
            (should (equal '("notes.txt" "nubank-account-2025-06-01_2025-06-30.ofx")
                           (directory-files fin-bank-inbox nil "\\`[^.]"))))
        (delete-directory fin-bank-inbox t)))))

(ert-deftest bank/import-removes-re-export-and-refuses-conflict ()
  (fin-test-with-db
    (let ((fin-bank-inbox (make-temp-file "fin-inbox-" t))
          (coding-system-for-write 'utf-8))
      (unwind-protect
          (cl-flet ((put (name text) (write-region text nil (expand-file-name name fin-bank-inbox))))
            (put "a.ofx" test-bank--ofx)
            (put "b.ofx" (replace-regexp-in-string "20261002101930" "20261003000000" test-bank--ofx))
            (fin-bank-import)
            (should (equal '("nubank-account-2025-06-01_2025-06-30.ofx")
                           (directory-files fin-bank-inbox nil "\\.ofx\\'")))
            (put "c.ofx" (replace-regexp-in-string "Padaria" "Mercado" test-bank--ofx))
            (should-error (fin-bank-import) :type 'user-error)
            (should (file-exists-p (expand-file-name "c.ofx" fin-bank-inbox))))
        (delete-directory fin-bank-inbox t)))))

(ert-deftest bank/kind-tells-company-accounts-apart ()
  (let ((fin-bank-pj-accounts '("PJ1" "PJC")))
    (should (equal "account" (fin-bank--kind "<BANKACCTFROM><ACCTID>PF1</ACCTID>")))
    (should (equal "card" (fin-bank--kind "<CCACCTFROM><ACCTID>PFC</ACCTID>")))
    (should (equal "pj" (fin-bank--kind "<BANKACCTFROM><ACCTID>PJ1</ACCTID>")))
    (should (equal "pj-card" (fin-bank--kind "<CCACCTFROM><ACCTID>PJC</ACCTID>")))))

(ert-deftest bank/own-transfers-ignored-except-salary ()
  (let ((fin-bank-own-regexps '("JOAO P"))
        (fin-conv-salary-regexps '("\\`Transferência Recebida - JOAO PEDRO")))
    (should (fin-bank--ignored-p '("a" "2025-06-10" "in" 100 "Transferência recebida pelo Pix - JOAO P LIMA")))
    (should-not (fin-bank--ignored-p '("b" "2025-06-10" "in" 100 "Transferência Recebida - JOAO PEDRO LIMA")))
    (should (fin-bank--ignored-p '("c" "2025-06-10" "in" 100 "Valor adicionado na conta por cartão de crédito")))
    (should-not (fin-bank--ignored-p '("d" "2025-06-10" "out" 100 "Padaria")))))

(ert-deftest bank/ignored-descriptions-skip-reconcile ()
  (fin-test-with-db
    (fin-bankdb-insert '(("a" "t" "account" "2025-06-10" "out" 1000 "Pagamento de fatura")
                         ("b" "t" "account" "2025-06-10" "out" 2000 "Padaria")))
    (should (equal '("b") (mapcar #'car (fin-bank--bank-year 2025))))))

(provide 'test-bank)
;;; test-bank.el ends here
