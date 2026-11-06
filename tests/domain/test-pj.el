;;; test-pj.el --- domain/pj.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'pj)

(defmacro test-pj--with-maps (&rest body)
  `(let ((fin-pj-income-payers '(("Brasil Paralelo" . "bp")))
         (fin-pj-tax-items '(("\\bDAS\\b" . "das") ("DARF" . "darf"))))
     ,@body))

(ert-deftest pj/cover-starts-at-a-whole-month-and-ends-on-the-last-statement-day ()
  (should (equal '("2021-11-01" . "2022-02-03") (fin-pj-cover "2021-10-05" "2022-02-03" "2021-01-01")))
  (should (equal '("2021-10-01" . "2021-10-31") (fin-pj-cover "2021-10-01" "2021-10-31" "2021-01-01")))
  (should (equal '("2024-01-01" . "2024-03-10") (fin-pj-cover "2021-10-05" "2024-03-10" "2024-01-01")))
  (should (equal '("2024-02-01" . "2024-03-10") (fin-pj-cover "2021-10-05" "2024-03-10" "2024-01-15")))
  (should (equal '("2027-01-01" . "2027-01-06") (fin-pj-cover "2026-12-05" "2027-01-06" "2026-01-01")))
  (should-not (fin-pj-cover "2021-10-05" "2023-12-31" "2024-01-01"))
  (should (equal "2026-02-28" (fin-pj-month-end "2026-02-10"))))

(ert-deftest pj/income-edits-matches-adds-missing-reports-unreceived ()
  (test-pj--with-maps
   (let* ((l1 '(1 "2024-03-10" "in" "bp" nil 750000 nil))
          (l2 '(2 "2024-04-10" "in" "bp" nil 750000 nil))
          (l3 '(3 "2024-12-20" "in" "bp" nil 500000 nil))
          (rows '(("a" "2024-03-05" "in" 812500 "Transferência Recebida - Brasil Paralelo")
                  ("b" "2024-04-04" "in" 750000 "Transferência Recebida - Brasil Paralelo")
                  ("c" "2024-05-06" "in" 750000 "Transferência Recebida - Brasil Paralelo")))
          (acts (fin-pj-plan-income rows (list l1 l2 l3))))
     (should (equal (list (list :edit l1 812500 "pj: Transferência Recebida - Brasil Paralelo")
                          '(:add ("2024-05-06" "in" "bp" nil 750000 nil nil) "Transferência Recebida - Brasil Paralelo")
                          (list :report l3 "income the PJ account never received"))
                    acts)))))

(ert-deftest pj/taxes-replace-estimates-and-keep-booked-payments ()
  (test-pj--with-maps
   (let* ((est '(1 "2024-03-31" "out" "cnpj" nil 112000 nil))
          (booked '(2 "2024-03-20" "out" "cnpj" "das" 60000 nil))
          (personal '(3 "2024-03-12" "out" "cnpj" "goedert" 5000 nil))
          (outside '(4 "2024-06-30" "out" "cnpj" nil 112000 nil))
          (rows '(("a" "2024-03-20" "out" 60000 "Pagamento de boleto efetuado - DAS - Simples Nacional")
                  ("b" "2024-03-22" "out" 14520 "Pagamento de boleto efetuado - DARF")))
          (acts (fin-pj-plan-taxes rows (list est booked personal outside) '("2024-03-01" . "2024-03-31"))))
     (should (equal (list '(:add ("2024-03-22" "out" "cnpj" "darf" 14520 nil nil)
                                 "Pagamento de boleto efetuado - DARF")
                          (list :delete est "cnpj estimate replaced by the PJ payments"))
                    acts)))))

(ert-deftest pj/taxes-after-the-last-statement-day-are-left-alone ()
  (test-pj--with-maps
   (let* ((paid '(1 "2026-11-05" "out" "cnpj" "das" 78000 nil))
          (later '(2 "2026-11-20" "out" "cnpj" "darf" 17831 nil))
          (est '(3 "2026-11-30" "out" "cnpj" nil 112000 nil))
          (rows '(("a" "2026-11-05" "out" 78000 "Pagamento de boleto efetuado - DAS")
                  ("b" "2026-11-05" "out" 33000 "Pagamento de boleto efetuado - DARF"))))
     (should (equal '((:add ("2026-11-05" "out" "cnpj" "darf" 33000 nil nil) "Pagamento de boleto efetuado - DARF"))
                    (fin-pj-plan-taxes rows (list paid later est) '("2026-11-01" . "2026-11-06")))))))

(ert-deftest pj/tax-items-match-case-sensitively ()
  (test-pj--with-maps
   (should (equal "das" (fin-pj-tax-item '("a" "2025-03-20" "out" 1 "Pagamento de boleto efetuado - DAS-SIMPLES NACIONAL"))))
   (should-not (fin-pj-tax-item '("b" "2025-03-08" "out" 1 "Compra no débito - Casarao das Ferragens")))
   (should-not (fin-pj-tax-item '("c" "2022-06-05" "out" 1 "Casa Das Essencias")))
   (should (equal "bp" (fin-pj-income-category '("d" "2024-06-05" "in" 1 "Pix - BRASIL PARALELO"))))))

(ert-deftest pj/income-pairs-the-ledger-row-of-the-payment-month ()
  (test-pj--with-maps
   (let* ((jul '(1 "2024-07-30" "in" "bp" nil 750000 nil))
          (aug '(2 "2024-08-31" "in" "bp" nil 750000 nil))
          (sep '(3 "2024-09-30" "in" "bp" nil 1281000 nil))
          (rows '(("a" "2024-07-05" "in" 750000 "Pix - Brasil Paralelo")
                  ("b" "2024-08-05" "in" 750000 "Pix - Brasil Paralelo")
                  ("c" "2024-09-05" "in" 1281000 "Pix - Brasil Paralelo"))))
     (should-not (fin-pj-plan-income rows (list jul aug sep)))
     (let ((before '(4 "2024-07-25" "in" "bp" nil 750000 nil))
           (after '(5 "2024-09-05" "in" "bp" nil 750000 nil))
           (closer '(6 "2024-09-01" "in" "bp" nil 1281000 nil)))
       (should (equal (list (list :report before "income the PJ account never received")
                            (list :report closer "income the PJ account never received"))
                      (fin-pj-plan-income '(("x" "2024-08-15" "in" 750000 "Pix - Brasil Paralelo"))
                                          (list before after closer))))))))

(provide 'test-pj)
;;; test-pj.el ends here
