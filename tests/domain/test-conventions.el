;;; test-conventions.el --- domain/conventions.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'conventions)

(defun test-conv--b (id date type amount desc) (list id date type amount desc))
(defun test-conv--l (id date type cat item amount &optional note)
  (list id date type cat item amount note))

(defconst test-conv--history
  '(("food" "komprão" 40) ("food" "imperatriz" 114) ("free" "youtube premium" 18)
    ("house" "aluguel" 60) ("patrimony" "decathlon" 5)))

;;; ── Words and categories ───────────────────────────────────

(ert-deftest conv/suggest-uses-history-words-prefixes-and-aliases ()
  (should (equal '("food" . "komprão")
                 (fin-conv-suggest "Compra no débito - KOMPRAO KOCH ATACADIST" test-conv--history)))
  (should (equal '("food" . "imperatriz")
                 (fin-conv-suggest "Supermercados Imperatr" test-conv--history)))
  (should (null (fin-conv-suggest "Transferência enviada pelo Pix - Veronica" test-conv--history)))
  (let ((fin-conv-aliases '(("veronica" . "aluguel"))))
    (should (equal '("house" . "aluguel")
                   (fin-conv-suggest "Transferência enviada pelo Pix - Veronica" test-conv--history)))))

(ert-deftest conv/item-strips-bank-boilerplate ()
  (should (equal "academia tutubarao ltda"
                 (fin-conv-item "Transferência enviada pelo Pix - Academia Tutubarao Ltda - 04.871")))
  (should (equal "auvpescola" (fin-conv-item "Mp *Auvpescola - Parcela 3/12")))
  (should (equal "flavia simioli gutierrez"
                 (fin-conv-item "Transferência enviada - Flavia Simioli Gutierrez - •••.567.921-•• - BC")))
  (should (equal "linode . akamai" (fin-conv-item "Linode . Akamai")))
  (should (equal '(3 12) (fin-conv-installment "Mp *Auvpescola - Parcela 3/12")))
  (should (equal '(nil nil) (fin-conv-installment "Padaria"))))

(ert-deftest conv/related-by-word-alias-or-suggested-category ()
  (let ((rel (fin-conv-related-fn test-conv--history))
        (b (test-conv--b "b" "2025-05-06" "out" 100 "Compra no débito - KOMPRAO KOCH")))
    (should (funcall rel b (test-conv--l 1 "2025-05-10" "out" "food" "komprão" 100)))
    (should (funcall rel b (test-conv--l 2 "2025-05-10" "out" "food" "hippo" 100)))
    (should-not (funcall rel b (test-conv--l 3 "2025-05-10" "out" "car" "seguro" 100)))
    (should (funcall rel (test-conv--b "i" "2025-05-06" "out" 100 "Compra no débito via NuPay - iFood")
                     (test-conv--l 4 "2025-05-10" "out" "food" "go pizza" 100)))))

;;; ── Normalize ──────────────────────────────────────────────

(ert-deftest conv/normalize-cancels-refund-pairs ()
  (let ((res (fin-conv-normalize
              (list (test-conv--b "p" "2025-05-11" "out" 4290 "Brasil Paral*Brasilpar")
                    (test-conv--b "r" "2025-05-12" "in" 4290 "Estorno de \"Brasil Paral*Brasilpar\"")
                    (test-conv--b "k" "2025-05-12" "out" 100 "Padaria")))))
    (should (equal '("k") (mapcar #'car (plist-get res :rows))))
    (should (= 2 (length (plist-get res :notes))))))

(ert-deftest conv/normalize-nets-partial-refund-into-payee-purchase ()
  (let ((res (fin-conv-normalize
              (list (test-conv--b "p" "2024-10-10" "out" 20600 "Transferência enviada pelo Pix - Iago Bernardi Winter Mei - 23.380")
                    (test-conv--b "r" "2024-10-10" "in" 3800 "Reembolso recebido pelo Pix - Iago Bernardi Winter Mei - 23.")
                    (test-conv--b "o" "2024-10-09" "out" 9000 "Transferência enviada pelo Pix - Outra Pessoa")))))
    (should (equal '(("p" . 16800) ("o" . 9000))
                   (mapcar (lambda (r) (cons (car r) (nth 3 r))) (plist-get res :rows))))))

(ert-deftest conv/normalize-folds-iof-and-discounts ()
  (let* ((res (fin-conv-normalize
               (list (test-conv--b "l" "2026-07-01" "out" 2701 "Linode . Akamai")
                     (test-conv--b "i" "2026-07-01" "out" 94 "IOF de \"Linode . Akamai\"")
                     (test-conv--b "t" "2026-06-05" "out" 15246 "Latam Air*Asbfwa - Parcela 6/6")
                     (test-conv--b "d" "2026-06-05" "in" 127 "Desconto Antecipação Latam Air*Asbfwa")
                     (test-conv--b "x" "2026-09-04" "in" 162 "Desconto Antecipação Decathlon"))))
         (rows (plist-get res :rows)))
    (should (equal '(("l" . 2795) ("t" . 15119))
                   (mapcar (lambda (r) (cons (car r) (nth 3 r))) rows)))
    (should (equal "discount already netted in ledger"
                   (cdr (assoc "x" (mapcar (lambda (n) (cons (caar n) (cdr n))) (plist-get res :notes))))))))

(ert-deftest conv/normalize-iof-without-merchant-needs-single-candidate ()
  (let ((one (fin-conv-normalize
              (list (test-conv--b "l" "2026-10-02" "out" 2708 "Linode . Akamai")
                    (test-conv--b "i" "2026-10-02" "out" 94 "IOF de compra internacional"))))
        (two (fin-conv-normalize
              (list (test-conv--b "l" "2026-10-02" "out" 2708 "Linode . Akamai")
                    (test-conv--b "p" "2026-10-02" "out" 500 "Padaria")
                    (test-conv--b "i" "2026-10-02" "out" 94 "IOF de compra internacional")))))
    (should (equal '(2802) (mapcar (lambda (r) (nth 3 r)) (plist-get one :rows))))
    (should (= 3 (length (plist-get two :rows))))))

(ert-deftest conv/normalize-drops-pix-funded-by-card ()
  (let ((res (fin-conv-normalize
              (list (test-conv--b "f" "2026-09-30" "in" 1292 "Valor adicionado na conta por cartão de crédito")
                    (test-conv--b "p" "2026-09-30" "out" 1292 "Transferência enviada pelo Pix - UBER")
                    (test-conv--b "c" "2026-09-30" "out" 1314 "Pix no Crédito - UBER")))))
    (should (equal '("f" "c") (mapcar #'car (plist-get res :rows))))))

;;; ── Plan ───────────────────────────────────────────────────

(defun test-conv--plan (&rest args)
  (apply #'fin-conv-plan :history test-conv--history
         :months '(("2025-05" 1000000 0) ("2024-02" 1000000 0) ("2026-09" 1000000 0)
                   ("2026-05" 1000000 0) ("2026-06" 1000000 0) ("2026-07" 1000000 0)
                   ("2026-10" 1000000 0) ("2024-10" 1000000 0))
         args))

(ert-deftest conv/plan-adds-out-and-money-received ()
  (let ((acts (test-conv--plan
               :bank-only (list (test-conv--b "a" "2025-05-06" "out" 36739 "Compra no débito - KOMPRAO KOCH")
                                (test-conv--b "b" "2025-05-07" "out" 1300 "Lucio Joaquim Eller")
                                (test-conv--b "c" "2025-05-08" "in" 3500 "Transferência recebida pelo Pix - ZULEIKA BAJORINAS - •••")))))
    (should (equal '((:add ("2025-05-08" "in" "extras" "zuleika" 3500 nil nil))
                     (:add ("2025-05-06" "out" "food" "komprão" 36739 nil nil))
                     (:add ("2025-05-07" "out" "free" "lucio joaquim eller" 1300 nil nil)))
                   (mapcar (lambda (a) (seq-take a 2)) acts)))))

(ert-deftest conv/plan-adds-only-within-month-room ()
  (let* ((bank (list (test-conv--b "a" "2024-03-02" "out" 4200 "Logbank*Mercearia")
                     (test-conv--b "b" "2024-03-17" "out" 1900 "Logbank*Mercearia")
                     (test-conv--b "c" "2024-04-02" "out" 500 "Padaria")))
         (acts (fin-conv-plan :history test-conv--history :bank-only bank
                              :months '(("2024-03" 800000 795000) ("2024-04" 600000 500000)))))
    (should (equal '(:report :report :add) (mapcar #'car acts)))
    (should (string-match-p "2024-03: ledger 7950.00 \\+ bank-only 61.00 > bank 8000.00" (caddr (car acts))))))

(ert-deftest conv/plan-refund-deletes-matched-purchase ()
  (let* ((purchase (test-conv--b "p" "2025-04-13" "out" 10498 "Compra no débito via NuPay - TUNA*AnotaAi"))
         (ledger (test-conv--l 7 "2025-04-14" "out" "free" "sushi" 10500))
         (acts (test-conv--plan
                :near (list (cons purchase ledger))
                :bank-only (list (test-conv--b "r" "2025-04-13" "in" 10498 "Estorno - Compra no débito via NuPay - TUNA*AnotaAi")))))
    (should (member (list :delete ledger "refunded: Estorno - Compra no débito via NuPay - TUNA*AnotaAi")
                    acts))))

(ert-deftest conv/plan-salary-gap-is-reported-not-fixed ()
  (let* ((fin-conv-salary-regexps '("\\`Transferência Recebida - JOAO"))
         (net (list (list 'net "2026-07" "bp") "2026-07-03" "in" "bp" "net of cnpj" 893670 nil))
         (acts (test-conv--plan
                :bank-only (list (test-conv--b "s" "2026-07-03" "in" 839145 "Transferência Recebida - JOAO"))
                :entry-only (list net))))
    (should (equal (list (list :report net "salary received 8391.45, ledger bp − cnpj 8936.70")) acts))))

(ert-deftest conv/plan-near-salary-is-reported ()
  (let* ((net (list (list 'net "2026-05" "bp") "2026-05-25" "in" "bp" "net of cnpj" 893670 nil))
         (acts (test-conv--plan
                :near (list (cons (test-conv--b "s" "2026-05-25" "in" 893000 "Transferência Recebida") net)))))
    (should (eq :report (caar acts)))))

(ert-deftest conv/plan-near-and-shifted-edits-need-payee-words ()
  (let* ((fin-conv-aliases '(("yelumseg" . "seguro")))
         (l1 (test-conv--l 1 "2026-07-13" "out" "food" "komprão" 42644))
         (l2 (test-conv--l 2 "2026-06-05" "out" "free" "celular" 9990))
         (l3 (test-conv--l 3 "2026-05-02" "out" "car" "seguro" 15353))
         (acts (test-conv--plan
                :near (list (cons (test-conv--b "a" "2026-07-13" "out" 42664 "KOMPRAO KOCH") l1)
                            (cons (test-conv--b "b" "2026-06-05" "out" 10052 "Super Imperatriz Lj") l2))
                :shifted (list (cons (test-conv--b "c" "2026-05-15" "out" 15357 "Yelumseg Parc4") l3)))))
    (should (equal (list (list :edit l1 42664) (list :edit l3 15357))
                   (mapcar (lambda (a) (seq-take a 3)) acts)))))

(ert-deftest conv/plan-shifted-edit-without-payee-words-is-not-made ()
  (let ((acts (test-conv--plan
               :shifted (list (cons (test-conv--b "p" "2024-02-21" "out" 5000 "Compra no débito - Posto da Praca")
                                    (test-conv--l 1 "2024-02-15" "out" "free" "xbox gamepass" 4999))))))
    (should (null acts))))

(ert-deftest conv/plan-salary-pairs-nearest-bp-or-skips ()
  (let* ((fin-conv-salary-regexps '("\\`Transferência Recebida - JOAO"))
         (net (list (list 'net "2024-09" "bp") "2024-09-30" "in" "bp" "net of cnpj" 900000 nil))
         (acts (test-conv--plan
                :bank-only (list (test-conv--b "s" "2024-10-10" "in" 890000 "Transferência Recebida - JOAO")
                                 (test-conv--b "r" "2024-09-29" "in" 9990 "Transferência Recebida - JOAO"))
                :entry-only (list net))))
    (should (member (list :report net "salary received 8900.00, ledger bp − cnpj 9000.00") acts))
    (should (eq :skip (car (cl-find-if (lambda (a) (equal (car (cadr a)) "r")) acts))))))

(ert-deftest conv/plan-combined-ledger-row-books-two-bank-rows ()
  (let ((acts (test-conv--plan
               :bank-only (list (test-conv--b "a" "2026-09-04" "out" 6337 "Decathlon - Parcela 6/6")
                                (test-conv--b "b" "2026-09-04" "out" 6499 "Decathlon - Parcela 5/6")
                                (test-conv--b "c" "2026-09-04" "out" 500 "Padaria"))
               :entry-only (list (test-conv--l 1 "2026-09-04" "out" "patrimony" "decathlon" 12836 nil)))))
    (should (equal '("2026-09-04" "out" "free" "padaria" 500 nil nil) (cadr (car acts))))
    (should (= 1 (length acts)))))

(ert-deftest conv/plan-skips-savings-moves ()
  (let ((acts (test-conv--plan
               :bank-only (list (test-conv--b "a" "2026-05-05" "out" 5000 "Aplicação RDB")
                                (test-conv--b "b" "2026-05-06" "in" 2000 "Resgate RDB")))))
    (should (equal '(:skip :skip) (mapcar #'car acts)))))

(provide 'test-conventions)
;;; test-conventions.el ends here
