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
             (list "e" "nubank-ofx" "account" day "in" 90000 "Pix recebido")))
      (fin-test-insert-entry day "out" "food" 12000 "padaria")
      (fin-test-insert-entry day "out" "cnpj" 20000 "das")                                           ; unspent
      (let ((m (car (last (fin-health-months 3)))))
        (should (equal (list ym 10000 12000) m)))
      (should (= 3 (length (fin-health-months 3)))))))

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

(provide 'test-health)
;;; test-health.el ends here
