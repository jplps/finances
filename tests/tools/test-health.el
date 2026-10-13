;;; test-health.el --- tools/health.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'health)

(ert-deftest health/months-count-spending-like-the-fix ()
  (fin-test-with-db
    (let* ((last (fin-health--month-start 1))
           (ym (substring last 0 7))
           (day (concat ym "-10"))
           (fin-bank-own-regexps nil) (fin-conv-salary-regexps nil)
           (fin-pj-tax-items '(("\\bDAS\\b" . "das"))))
      (fin-bankdb-insert
       (list (list "a" "nubank-ofx" "account" day "out" 10000 "Compra no débito - Padaria")
             (list "b" "nubank-ofx" "account" day "out" 50000 "Pagamento da fatura - Cartão Nubank") ; ignored
             (list "c" "nubank-ofx" "account" day "out" 30000 "Aplicação RDB")                       ; savings
             (list "d" "nubank-ofx" "pj" day "out" 20000 "Pagamento de boleto efetuado - DAS")       ; tax
             (list "e" "nubank-ofx" "account" day "in" 90000 "Pix recebido")
             (list "f" "nubank-ofx" "account" day "out" 700 "Compra no débito - Casarao das Ferragens")
             (list "g" "nubank-ofx" "account" day "out" 200 "Pagamento de boleto efetuado - DAS")     ; not the PJ's
             (list "h" "nubank-ofx" "card" day "out" 4000 "Loja")                                    ; refunded
             (list "i" "nubank-ofx" "card" day "in" 4000 "Estorno de \"Loja\"")))
      (fin-test-insert-entry day "out" "food" 12000 "padaria")
      (fin-test-insert-entry day "out" "cnpj" 20000 "das")                                           ; unspent
      (let ((m (car (last (fin-health-months 3)))))
        (should (equal (list ym 10900 12000) m)))
      (should (= 3 (length (fin-health-months 3)))))))

(ert-deftest health/cash-check-matches-ledger-and-bank ()
  (fin-test-with-db
    (let ((fin-accounts-derived '((:name "nu" :since "2020-01-01" :opening 0
                                   :into ("\\`Aplicação RDB") :out-of ("\\`Resgate RDB"))))
          (fin-report-transfer-categories '(("out" "investments"))))
      (fin-bankdb-insert
       (list (list "a" "nubank-ofx" "account" "2026-10-01" "in" 500000 "Pix recebido")
             (list "b" "nubank-ofx" "account" "2026-10-02" "out" 300000 "Aplicação RDB")
             (list "c" "nubank-ofx" "card" "2026-10-03" "out" 80000 "Loja")
             (list "d" "nubank-ofx" "account" "2026-10-04" "out" 50000 "Pagamento de fatura") ; ignored
             (list "e" "nubank-ofx" "card" "2026-10-04" "in" 50000 "Pagamento recebido")))    ; ignored
      (fin-test-insert-entry "2026-10-01" "in" "salary" 500000)
      (fin-test-insert-entry "2026-10-03" "out" "food" 80000)
      (fin-test-insert-entry "2026-10-02" "out" "investments" 100000 "car")   ; allocation, not counted
      (should (equal '(120000 120000) (fin-health-cash 2026 10)))
      (fin-test-insert-entry "2026-10-05" "out" "food" 20000)
      (should (equal '(100000 120000) (fin-health-cash 2026 10))))))

(ert-deftest health/months-leave-out-offbank-rows ()
  (fin-test-with-db
    (let* ((last (fin-health--month-start 1))
           (day (concat (substring last 0 7) "-10"))
           (fin-bank-own-regexps nil) (fin-conv-salary-regexps nil) (fin-pj-tax-items nil))
      (fin-bankdb-insert (list (list "a" "nubank-ofx" "account" day "out" 10000 "Compra no débito - Padaria")))
      (fin-test-insert-entry day "out" "food" 10000 "padaria")
      (fin-test-insert-entry day "out" "free" 5000 "pintura")
      (should (equal 15000 (nth 2 (car (last (fin-health-months 1))))))
      (fin-db-exec "UPDATE entry SET via = 'cash' WHERE item = 'pintura'")
      (should (equal 10000 (nth 2 (car (last (fin-health-months 1)))))))))

(ert-deftest health/inbox-wants-last-month-covered ()
  (fin-test-with-db
    (let ((need (fin-health--last-month-end)))
      (fin-bankdb-record-balance "account" need 100)
      (fin-bankdb-record-balance "card" "2020-01-06" -100)
      (fin-bankdb-record-balance "pj-card" "2099-01-06" 0)      ; open bill, future closing
      (should (equal (list (list "account" need t) (list "card" "2020-01-06" nil)
                           (list "pj-card" "2099-01-06" t))
                     (fin-health-inbox))))))

(ert-deftest health/fix-due-after-import-or-pending-dry-run ()
  (fin-test-with-db
    (should-not (fin-health-fix))
    (fin-bankdb-record-fix-run 0 nil nil)
    (should (plist-get (fin-health-fix) :ok))
    (sleep-for 0.01)
    (fin-bankdb-record-fix-run 3 t nil)
    (should-not (plist-get (fin-health-fix) :ok))              ; dry run with changes pending
    (fin-db-exec "DELETE FROM bank_fix_run")
    (fin-db-exec "INSERT INTO bank_fix_run (run_at, changes, dry) VALUES ('2000-01-01T00:00:00.000', 0, 0)")
    (fin-bankdb-insert (list (list "x" "nubank-ofx" "account" "2026-10-01" "out" 100 "Padaria")))
    (should (= 1 (plist-get (fin-health-fix) :since)))))

(ert-deftest health/flags-keep-recent-large-reported-rows ()
  (let* ((today (format-time-string "%Y-%m-%d"))
         (big (list "a" today "out" 100000 "Pix - Luiz"))
         (small (list "b" today "out" 1000 "Padaria"))
         (old (list "c" "2000-01-01" "out" 100000 "Pix - Old"))
         (muted (list "d" today "out" 100000 "Pagamento de boleto efetuado - ZOOP"))
         (fin-bank-fix-flag-ignore-regexps '("ZOOP"))
         (acts (mapcar (lambda (r) (list :report r "why")) (list big small old muted))))
    (should (equal (list (list "a" today 100000 "Pix - Luiz" "why")) (fin-bank-fix--flags acts)))))

(ert-deftest health/inbox-documents-and-key-files ()
  (let* ((root (make-temp-file "fin-inbox-" t))
         (fin-bank-inbox (file-name-as-directory (expand-file-name "nubank" root))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "ir/defis" root) t)
          (make-directory (expand-file-name "docs" root) t)
          (write-region "" nil (expand-file-name "ir/defis/defis-cnpj-2025.pdf" root))
          (write-region "" nil (expand-file-name "docs/.cert.pfx" root))
          (let ((fin-health-expected-docs '(("defis 2025" . "\\`ir/defis/defis-cnpj-2025\\.pdf\\'")
                                            ("balance sheet" . "balanco"))))
            (should (equal '("balance sheet") (fin-health-missing-docs)))
            (should (equal '("docs/.cert.pfx") (fin-health-key-files)))))
      (delete-directory root t))))

(provide 'test-health)
;;; test-health.el ends here
