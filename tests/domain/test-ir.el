;;; test-ir.el --- domain/ir.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'ir)

(ert-deftest ir/table-tax-by-band ()
  (should (= 0 (fin-ir-table-tax 2025 2846720)))
  (should (= 132097 (fin-ir-table-tax 2025 4000000)))      ; 15% − 4.679,03
  (should (= 274503 (fin-ir-tax 2025 6000000 1200000))))   ; no 2025 reduction

(ert-deftest ir/2026-reduction-zeroes-and-phases-out ()
  (should (= 0 (fin-ir-tax 2026 6000000 (fin-ir-simplified 2026 6000000))))
  ;; 8.429,73 − 0,095575 × 70.000 = 1.739,48 off 27,5% × 56.000 − 10.904,66.
  (should (= 275586 (fin-ir-tax 2026 7000000 1400000)))
  (should (= (fin-ir-table-tax 2026 7000000) (fin-ir-tax 2026 9000000 2000000))))

(ert-deftest ir/simplified-is-capped ()
  (should (= 1000000 (fin-ir-simplified 2025 5000000)))
  (should (= 1675434 (fin-ir-simplified 2025 20000000)))
  (should (= 1764000 (fin-ir-simplified 2026 20000000))))

(ert-deftest ir/unknown-year-fails-loud ()
  (should-error (fin-ir-rule 1999 :table)))

(defun test-ir--bank (id account date type amount desc)
  (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                VALUES (?,'test',?,?,?,?,?)" (list id account date type amount desc)))

;; Positions take their CSV inputs from CTX, so no test reads infra/inbox.
(defun test-ir--ctx (&rest kv)
  "Position context of KV (:payments :simples :invoices), Receita rows matched now."
  (list :receita (fin-ir-receita-matches (plist-get kv :payments))
        :simples (plist-get kv :simples) :invoices (plist-get kv :invoices)))

(defun test-ir--seed-2025 ()
  (test-ir--bank "r1" "pj" "2025-01-05" "in" 1000000 "Pix - BP Educacao")
  (test-ir--bank "r2" "pj" "2025-02-05" "in" 1000000 "Pix - BP Educacao")
  (test-ir--bank "t1" "pj" "2025-01-20" "out" 60000 "DAS - Simples Nacional")
  (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
  (test-ir--bank "w2" "account" "2025-02-10" "in" 900000 "Recebida - OWN CNPJ")
  (test-ir--bank "x1" "account" "2025-02-11" "in" 50000 "Recebida - someone")
  (fin-test-insert-entry "2025-03-01" "out" "health" 30000 "natação")
  (fin-test-insert-entry "2025-03-02" "out" "health" 20000 "fisioterapia")
  (fin-test-insert-entry "2025-03-05" "out" "health" 5000)
  (fin-test-insert-entry "2025-03-03" "out" "retirement" 100000)
  (fin-test-insert-entry "2025-03-04" "in" "extras" 40000 "blue"))

(defmacro test-ir--with-2025 (books &rest body)
  "BODY with the 2025 seed, full books BOOKS and P its position."
  (declare (indent 1))
  `(fin-test-with-db
     (fin-bankdb-ensure)
     (test-ir--seed-2025)
     (let* ((fin-pj-income-payers '(("BP Educacao" . "bp")))
            (fin-pj-tax-items '(("\\bDAS\\b" . "das")))
            (fin-conv-salary-regexps '("OWN CNPJ"))
            (fin-ir-pro-labore '((2025 :monthly 100000)))
            (fin-ir-retirement-plan 'pgbl)
            (fin-ir-individual-income nil)
            (fin-ir-full-books ,books)
            (p (fin-ir-position 2025 (test-ir--ctx))))
       ,@body)))

(ert-deftest ir/position-splits-deposits-and-deductions ()
  (test-ir--with-2025 nil
    (should (= 2000000 (plist-get p :revenue)))
    (should (equal '(("das" . 60000)) (plist-get p :taxes)))
    (should (= 1700000 (plist-get p :drawn)))
    (should (= 900000 (plist-get p :peak-month)))
    (should (= 632000 (plist-get p :profit)))     ; 17.000 − (12 × 1.000 − 1.320)
    (should (= 580000 (plist-get p :presumed)))   ; 32% × 20.000 − 600
    (should (= 52000 (plist-get p :excess)))
    (should (= 580000 (plist-get p :exempt)))
    (should (= 1252000 (plist-get p :taxable)))   ; 12.000 + the excess
    (should (= 132000 (plist-get p :inss)))
    (should (= 20000 (plist-get p :health)))
    (should (= 100000 (plist-get p :pgbl)))
    (should (equal '(("2025-03" . 40000)) (plist-get p :individuals)))
    (should (= 0 (plist-get p :tax)))))

(ert-deftest ir/full-books-make-the-excess-exempt ()
  (dolist (books '(verified yes))
    (test-ir--with-2025 books
      (should (= 632000 (plist-get p :exempt)))
      (should (= 1200000 (plist-get p :taxable))))))

(ert-deftest ir/bad-full-books-fails-loud ()
  (let ((fin-ir-full-books 'maybe))
    (should-error (fin-ir--books-exempt-p))))

(ert-deftest ir/excess-tax-is-the-difference ()
  (let ((fin-ir-retirement-plan nil))
    ;; 50.000 − 20% = 40.000 → 1.320,97; 30.000 − 20% = 24.000 → 0.
    (should (= 132097 (fin-ir--excess-tax 2025 3000000 2000000 0 0 0)))
    (should (= 0 (fin-ir--excess-tax 2025 3000000 0 0 0 0)))))

(ert-deftest ir/monthly-pro-labore-counts-competence-months ()
  (should (= 12 (fin-ir--competence-months 2025 "2026-10-11")))
  (should (= 9 (fin-ir--competence-months 2026 "2026-10-11")))
  (should (= 0 (fin-ir--competence-months 2026 "2026-01-05")))
  (should (= 0 (fin-ir--competence-months 2027 "2026-10-11"))))

(ert-deftest ir/position-adds-other-taxable-income ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
    (let* ((fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2025 . 600000)))
           (fin-ir-other-taxable '((2025 . 1800000) (2026 . 1)))
           (fin-ir-full-books 'verified)
           (p (fin-ir-position 2025 (test-ir--ctx))))
      (should (= 2400000 (plist-get p :taxable)))
      (should (= 266000 (plist-get p :profit))))))   ; 8.000 − (6.000 − 660)

(ert-deftest ir/profit-nets-returns-and-adds-partner-taxes ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
    (test-ir--bank "b1" "pj" "2025-04-29" "in" 100000 "Transferência Recebida - João Pedro Lima")
    (test-ir--bank "b2" "pj" "2025-05-02" "in" 7000 "Transferência Recebida - Stranger")
    (test-ir--bank "i1" "pj" "2025-10-07" "out" 38239 "Pix - RECEITA FEDERAL")
    (let* ((fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-bank-own-regexps '("Jo[aã]o Pedro Lima"))
           (fin-pj-tax-items '(("RECEITA FED" . "darf")))
           (fin-ir-pro-labore '((2025 . 600000)))
           (fin-ir-full-books 'verified)
           (p (fin-ir-position 2025 (test-ir--ctx :payments '(("2025-10-07" "0211" 38239))))))
      (should (= 100000 (plist-get p :returned)))
      (should (= 38239 (plist-get p :partner-taxes)))
      (should-not (plist-get p :taxes))
      (should (= 738239 (plist-get p :drawn)))           ; 8.000 − 1.000 + 382,39
      (should (= 204239 (plist-get p :profit))))))

(ert-deftest ir/annual-pro-labore-and-declared-revenue ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2022-03-10" "in" 5000000 "Recebida - OWN CNPJ")
    (let* ((fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2022 . 1200000)))
           (fin-ir-full-books 'verified)
           (p (fin-ir-position 2022 (test-ir--ctx :simples '(("2021-12" 1 1) ("2022-01" 6000000 360000)
                                                             ("2022-02" 4000000 240000))
                                                  :invoices '(("2022-03" . 11000000))))))
      (should (= 11000000 (plist-get p :invoiced)))
      (should (= 1200000 (plist-get p :taxable)))
      (should (= 100000 (plist-get p :pro-labore)))
      (should (= 132000 (plist-get p :inss)))
      (should (= 3932000 (plist-get p :profit)))            ; 50.000 − (12.000 − 1.320)
      (should (= 10000000 (plist-get p :declared-revenue)))
      (should (equal "2022-02" (plist-get p :declared-through)))
      (should (= 0 (plist-get p :invoiced-through)))         ; the March invoice comes after
      (should (= 2600000 (plist-get p :presumed))))))       ; 32% × 100.000 − 6.000

(ert-deftest ir/pro-labore-from-receita-files ()
  (let* ((dir (make-temp-file "fin-ir-" t))
         (fin-ir-payments-dir dir) (fin-ir-pro-labore nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "fontes-pagadoras" dir))
          (with-temp-file (expand-file-name "fontes-pagadoras/fontes.csv" dir)
            (insert "ano,fonte,rendimento_tributavel,prev_oficial,irrf,isento,processado\n"
                    "2024,A,16300.00,1793.00,0.00,0.00,2025-02-24\n"
                    "2024,B,250.00,0.00,0.00,0.00,2026-04-18\n"))
          (with-temp-file (expand-file-name "dctfweb-x.csv" dir)
            (insert "periodo;transmissao;categoria;origem;tipo;situacao;debito;saldo\n"
                    "02/2026;x;Geral;eSocial;Original;Ativa;330.00;330.00\n"
                    "01/2026;x;Geral;eSocial;Original;Ativa;330.00;330.00\n"
                    "12/2024;x;Geral;eSocial;Original;Ativa;999.00;999.00\n"))
          (should (equal '(1630000 135833 179300) (fin-ir--pro-labore 2024 12)))   ; payer file wins
          (should (equal '(600000 300000 66000) (fin-ir--pro-labore 2026 2)))
          (should-not (fin-ir--pro-labore 2025 12))
          (let ((fin-ir-pro-labore '((2026 :monthly 100000))))
            (should (equal 100000 (nth 1 (fin-ir--pro-labore 2026 2))))))
      (delete-directory dir t))))

(ert-deftest ir/filed-from-receipt-names ()
  (let* ((dir (make-temp-file "fin-ir-" t)) (fin-ir-payments-dir dir) (fin-ir-filed '((2023 . "2024-05-01"))))
    (unwind-protect
        (progn (make-directory (expand-file-name "dirpf" dir))
               (dolist (f '("dirpf-cpf-2025-recibo-2026-10-07.pdf" "dirpf-cpf-2025-declaracao.pdf"))
                 (write-region "" nil (expand-file-name (concat "dirpf/" f) dir)))
               (should (equal '((2025 . "2026-10-07") (2023 . "2024-05-01"))
                              (fin-ir--filed))))
      (delete-directory dir t))))

(ert-deftest ir/duty-statuses ()
  (let ((fin-ir-filed '((2025 . "2026-10-07") (2024 . "2025-04-01"))))
    (should (eq 'missing (plist-get (fin-ir-duty '(:year 2022 :taxable 1439018 :exempt 6052500)) :status)))
    (should (eq 'not-required (plist-get (fin-ir-duty '(:year 2023 :taxable 1584000 :exempt 7355400)) :status)))
    (should (eq 'late (plist-get (fin-ir-duty '(:year 2025 :taxable 4066200 :exempt 0)) :status)))
    (should (eq 'on-time (plist-get (fin-ir-duty '(:year 2024 :taxable 0 :exempt 0)) :status)))
    (should (eq 'unknown (plist-get (fin-ir-duty '(:year 2021 :taxable nil :exempt nil)) :status)))
    (should-error (fin-ir-duty '(:year 2026 :taxable 0 :exempt 0)))))

(ert-deftest ir/duty-counts-exclusive-exempt-income-and-assets ()
  ;; 2022 exempt limit 40.000: 30.000 profit + 6.000 yields + 5.000 from individuals.
  (let ((d (fin-ir-duty '(:year 2022 :taxable 0 :exempt 3000000 :exclusive 600000 :individuals-exempt 500000))))
    (should (eq 'missing (plist-get d :status)))
    (should (= 4100000 (plist-get d :exempt-total))))
  (should (eq 'not-required (plist-get (fin-ir-duty '(:year 2022 :taxable 0 :exempt 3000000 :exclusive 600000)) :status)))
  (should (eq 'missing (plist-get (fin-ir-duty '(:year 2022 :taxable 0 :exempt 0 :assets 30000001)) :status))))

(ert-deftest ir/company-taxes-named-by-receita-payment ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "d1" "pj" "2025-02-13" "out" 60931 "Pix - RECEITA FEDERAL")
    (test-ir--bank "i1" "pj" "2025-02-10" "out" 16698 "Pix - RECEITA FEDERAL")
    (test-ir--bank "u1" "pj" "2025-03-01" "out" 17831 "Pix - RECEITA FEDERAL")
    (test-ir--bank "l1" "pj" "2025-03-27" "out" 33435 "Pix - RECEITA FEDERAL")
    (test-ir--bank "c1" "account" "2025-06-06" "out" 45191 "Pagamento de boleto - RECEITA FED-DARF")
    (test-ir--bank "c2" "account" "2025-07-06" "out" 9900 "Pagamento de boleto - DARF")
    (let* ((fin-pj-tax-items '(("RECEITA FED" . "darf")))
           (receita (fin-ir-receita-matches '(("2025-02-13" "3333" 60931) ("2025-02-11" "1410" 16698)
                                               ("2025-03-27" "4444" 33435) ("2025-06-06" "4444" 45191)
                                               ("2025-07-06" "0211" 9900)))))
      (should (equal '(("das" . 60931) ("inss" . 95324) ("darf" . 17831))
                     (plist-get (fin-ir-company 2025 receita) :taxes)))
      (should (equal '("u1") (mapcar #'car (fin-ir-unmatched receita))))
      (should (equal '("c1") (mapcar #'caar (fin-ir-wrong-account receita)))))))

(ert-deftest ir/receita-payment-matches-once-across-years ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "a" "pj" "2024-12-30" "out" 60931 "Pix - RECEITA FEDERAL")
    (test-ir--bank "b" "pj" "2025-01-03" "out" 60931 "Pix - RECEITA FEDERAL")
    (let ((receita (fin-ir-receita-matches '(("2025-01-02" "3333" 60931)))))
      (should (equal '(("a" . "3333") ("b"))
                     (mapcar (lambda (m) (cons (caar m) (cdr m))) receita))))))

(ert-deftest ir/bad-payment-line-fails-loud ()
  (should-error (fin-ir--payment-row "2025-02-13,3333,1,2025-01-01,609" "f.csv"))
  (should-error (fin-ir--payment-row "x" "f.csv")))

(ert-deftest ir/reads-pgdasd-and-nfse-lists ()
  (let ((ir (make-temp-file "fin-ir-" t)) (nfse (make-temp-file "fin-nfse-" t)))
    (unwind-protect
        (progn
          (write-region "pa,declaracao_seq,receita,das\n2023-07,1,8557.94,342.32\n2023-06,1,1.00,0.06\n2023-06,2,9613.45,576.81\n"
                        nil (expand-file-name "pgdasd-cnpj-2023-resumo.csv" ir))
          (write-region (concat "sistema,numero,emissao,competencia,tomador_doc,tomador,valor,situacao,arquivo\n"
                                "municipal,1,2023-06-30,2023-06,1,A,9613.45,ativa,a.pdf\n"
                                "municipal,2,2023-07-30,2023-07,1,A,100.00,cancelada,b.pdf\n"
                                "municipal,3,2023-07-31,2023-07,1,A,8557.94,ativa,c.pdf\n")
                        nil (expand-file-name "nfse-cnpj-2023-index.csv" nfse))
          (let ((fin-ir-payments-dir ir) (fin-ir-nfse-dir nfse))
            (should (equal '(("2023-06" 961345 57681) ("2023-07" 855794 34232)) (fin-ir-simples)))
            (should (equal '(("2023-06" . 961345) ("2023-07" . 855794)) (fin-ir-invoices))))
          (let ((fin-ir-payments-dir nil) (fin-ir-nfse-dir (expand-file-name "absent/" nfse)))
            (should-not (fin-ir-simples))
            (should-not (fin-ir-invoices))))
      (delete-directory ir t)
      (delete-directory nfse t))))

(ert-deftest ir/bad-pgdasd-and-nfse-lines-fail-loud ()
  (should-error (fin-ir--simples-row "2023-07,1,8557.94" "f.csv"))
  (should-error (fin-ir--simples-row "2023-7,1,8557.94,342.32" "f.csv"))
  (should-error (fin-ir--invoice-row "municipal,1,2023-06-30,2023-06,1,A,9613.45,paga,a.pdf" "f.csv"))
  (should-error (fin-ir--invoice-row "municipal,1,2023-06-30,2023-06,1,A, B,9613.45,ativa,a.pdf" "f.csv")))

(ert-deftest ir/bank-revenue-by-month ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "a" "pj" "2026-01-05" "in" 1015500 "Pix - BP Educacao")
    (test-ir--bank "b" "pj" "2026-01-20" "in" 4000 "Pix - BP Educacao")
    (test-ir--bank "c" "pj" "2026-02-05" "in" 1015500 "Pix - BP Educacao")
    (test-ir--bank "d" "pj" "2026-02-06" "in" 500 "Pix - someone else")
    (test-ir--bank "e" "account" "2026-02-07" "in" 900 "Pix - BP Educacao")
    (let ((fin-pj-income-payers '(("BP Educacao" . "bp"))))
      (should (equal '(("2026-01" . 1019500) ("2026-02" . 1015500)) (fin-ir-bank-revenue))))))

(ert-deftest ir/revenue-gaps-by-month ()
  (let ((simples '(("2024-01" 750000 45000) ("2024-02" 750000 45000) ("2026-08" 1015500 60931)))
        (invoices '(("2023-12" . 1) ("2024-01" . 750000) ("2024-02" . 1931270) ("2026-09" . 1584500))))
    (should (equal '(("2024-02" 750000 1931270 nil) ("2026-08" 1015500 0 1015500))
                   (fin-ir-revenue-gaps simples invoices "2026-10-11" '(("2026-08" . 1015500)))))
    (should (equal '(("2024-02" 750000 1931270 nil) ("2026-08" 1015500 0 nil) ("2026-09" 0 1584500 nil))
                   (fin-ir-revenue-gaps simples invoices "2026-10-21")))
    (should-not (fin-ir-revenue-gaps nil invoices "2026-10-11"))))

(ert-deftest ir/das-rate-outliers-by-year ()
  (let ((odd (fin-ir-rate-outliers '(("2023-06" 961345 57681) ("2023-07" 855794 34232)
                                     ("2023-08" 855921 51355) ("2024-09" 0 0) ("2024-10" 100000 4000)))))
    (should (equal '("2023-07") (mapcar #'car odd)))
    (should (< (abs (- 0.04 (nth 1 (car odd)))) 0.0001))
    (should (< (abs (- 0.06 (nth 2 (car odd)))) 0.0001)))
  (should (= 2.5 (fin-ir--median '(4 1 2 3))))
  (should-error (fin-ir--median nil)))

(ert-deftest ir/bad-pro-labore-fails-loud ()
  (let ((fin-ir-pro-labore '((2025 . "3000"))))
    (should-error (fin-ir--pro-labore 2025 12))))

(ert-deftest ir/position-without-pro-labore-leaves-tax-unknown ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (let* ((fin-ir-pro-labore nil)
           (fin-ir-nfse-dir nil)
           (p (fin-ir-position 2026)))
      (should (= 0 (plist-get p :drawn)))
      (should-not (plist-get p :taxable))
      (should-not (plist-get p :exempt))
      (should-not (plist-get p :tax)))))

(ert-deftest ir/labelled-payment-counts-as-matched ()
  (let ((fin-ir-payment-labels '(("2026-06-08" 17831 "5320"))))
    (should (equal "5320" (fin-ir--labelled-code '("x" "2026-06-08" "pj" 17831 "Pix - RECEITA FEDERAL"))))
    (should-not (fin-ir--labelled-code '("x" "2026-06-08" "pj" 17830 "Pix - RECEITA FEDERAL")))))

(provide 'test-ir)
;;; test-ir.el ends here
