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

(ert-deftest health/statements-and-remainders ()
  (fin-test-with-db
    (fin-bankdb-record-balance "account" "2026-10-01" 145366)
    (fin-test-insert-entry "2021-04-30" "out" "food" 71725 "other")
    (should (equal '("account" "2026-10-01") (assoc "account" (fin-health-statements))))
    (should (equal '("card" nil) (assoc "card" (fin-health-statements))))
    (should (equal '(1 . 71725) (fin-health-remainders)))))

(provide 'test-health)
;;; test-health.el ends here
