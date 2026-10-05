;;; test-pj.el --- domain/pj.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'pj)

(defmacro test-pj--with-maps (&rest body)
  `(let ((fin-pj-income-payers '(("Brasil Paralelo" . "bp")))
         (fin-pj-tax-items '(("\\bDAS\\b" . "das") ("DARF" . "darf"))))
     ,@body))

(ert-deftest pj/months-are-the-fully-covered-ones ()
  (should (equal '("2021-11" "2021-12" "2022-01") (fin-pj-months "2021-10-05" "2022-02-03")))
  (should (equal '("2021-10") (fin-pj-months "2021-10-01" "2021-10-31"))))

(ert-deftest pj/income-edits-matches-adds-missing-reports-unreceived ()
  (test-pj--with-maps
   (let* ((l1 '(1 "2024-03-10" "in" "bp" nil 750000 nil))
          (l2 '(2 "2024-04-10" "in" "bp" nil 750000 nil))
          (l3 '(3 "2024-12-20" "in" "bp" nil 500000 nil))     ; bonus paid elsewhere
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
          (personal '(3 "2024-03-12" "out" "cnpj" "goedert" 5000 nil))   ; not a tax item
          (outside '(4 "2024-06-30" "out" "cnpj" nil 112000 nil))
          (rows '(("a" "2024-03-20" "out" 60000 "Pagamento de boleto efetuado - DAS - Simples Nacional")
                  ("b" "2024-03-22" "out" 14520 "Pagamento de boleto efetuado - DARF")))
          (acts (fin-pj-plan-taxes rows (list est booked personal outside) '("2024-03"))))
     (should (equal (list '(:add ("2024-03-22" "out" "cnpj" "darf" 14520 nil nil)
                                 "Pagamento de boleto efetuado - DARF")
                          (list :delete est "cnpj estimate replaced by the PJ payments"))
                    acts)))))

(provide 'test-pj)
;;; test-pj.el ends here
