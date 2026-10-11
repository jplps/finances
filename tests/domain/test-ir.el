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

(ert-deftest ir/position-splits-deposits-and-deductions ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "r1" "pj" "2025-01-05" "in" 1000000 "Pix - BP Educacao")
    (test-ir--bank "r2" "pj" "2025-02-05" "in" 1000000 "Pix - BP Educacao")
    (test-ir--bank "t1" "pj" "2025-01-20" "out" 60000 "DAS - Simples Nacional")
    (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
    (test-ir--bank "w2" "account" "2025-02-10" "in" 900000 "Recebida - OWN CNPJ")
    (test-ir--bank "x1" "account" "2025-02-11" "in" 50000 "Recebida - someone")
    (fin-test-insert-entry "2025-03-01" "out" "health" 30000 "natação")
    (fin-test-insert-entry "2025-03-02" "out" "health" 20000 "fisioterapia")
    (fin-test-insert-entry "2025-03-03" "out" "retirement" 100000)
    (fin-test-insert-entry "2025-03-04" "in" "extras" 40000 "blue")
    (let* ((fin-pj-income-payers '(("BP Educacao" . "bp")))
           (fin-pj-tax-items '(("\\bDAS\\b" . "das")))
           (fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2025 :monthly 300000)))
           (fin-ir-retirement-plan 'pgbl)
           (fin-ir-individual-income nil)
           (p (fin-ir-position 2025)))
      (should (= 2000000 (plist-get p :revenue)))
      (should (equal '(("das" . 60000)) (plist-get p :taxes)))
      (should (= 1700000 (plist-get p :drawn)))
      (should (= 900000 (plist-get p :peak-month)))
      (should (= 600000 (plist-get p :taxable)))
      (should (= 1166000 (plist-get p :profit)))   ; 17.000 − (6.000 − 660)
      (should (= 580000 (plist-get p :presumed)))           ; 32% × 20.000 − 600
      (should (= 66000 (plist-get p :inss)))
      (should (= 20000 (plist-get p :health)))
      (should (= 30000 (plist-get p :health-excluded)))
      (should (= 72000 (plist-get p :pgbl)))                ; 12% of 6.000
      (should (equal '(("2025-03" . 40000)) (plist-get p :individuals)))
      (should (= 0 (plist-get p :tax))))))

(ert-deftest ir/position-adds-other-taxable-income ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
    (let* ((fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2025 :monthly 300000)))
           (fin-ir-other-taxable '((2025 . 1800000) (2026 . 1)))
           (p (fin-ir-position 2025)))
      (should (= 2100000 (plist-get p :taxable)))
      (should (= 533000 (plist-get p :profit))))))

(ert-deftest ir/annual-pro-labore-and-declared-revenue ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2022-03-10" "in" 5000000 "Recebida - OWN CNPJ")
    (let* ((fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2022 . 1200000)))
           (fin-ir-simples-declared '((2022 10000000 600000)))
           (fin-ir-invoiced '((2022 . 11000000)))
           (p (fin-ir-position 2022)))
      (should (= 11000000 (plist-get p :invoiced)))
      (should (= 1200000 (plist-get p :taxable)))
      (should (= 100000 (plist-get p :pro-labore)))
      (should (= 132000 (plist-get p :inss)))
      (should (= 3932000 (plist-get p :profit)))            ; 50.000 − (12.000 − 1.320)
      (should (= 10000000 (plist-get p :declared-revenue)))
      (should (= 2600000 (plist-get p :presumed))))))       ; 32% × 100.000 − 6.000

(ert-deftest ir/duty-statuses ()
  (let ((fin-ir-filed '((2025 . "2026-10-07") (2024 . "2025-04-01"))))
    (should (eq 'missing (plist-get (fin-ir-duty '(:year 2022 :taxable 1439018 :profit 6052500)) :status)))
    (should (eq 'not-required (plist-get (fin-ir-duty '(:year 2023 :taxable 1584000 :profit 7355400)) :status)))
    (should (eq 'late (plist-get (fin-ir-duty '(:year 2025 :taxable 4066200 :profit 0)) :status)))
    (should (eq 'on-time (plist-get (fin-ir-duty '(:year 2024 :taxable 0 :profit 0)) :status)))
    (should (eq 'unknown (plist-get (fin-ir-duty '(:year 2021 :taxable nil :profit nil)) :status)))
    (should-error (fin-ir-duty '(:year 2026 :taxable 0 :profit 0)))))

(ert-deftest ir/bank-covers-years-after-the-first-statement ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "p1" "pj" "2021-10-05" "in" 100 "Pix")
    (should-not (fin-ir--pj-covered-p 2021))
    (should (fin-ir--pj-covered-p 2022))))

(ert-deftest ir/company-taxes-named-by-receita-payment ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "d1" "pj" "2025-02-13" "out" 60931 "Pix - RECEITA FEDERAL")
    (test-ir--bank "i1" "pj" "2025-02-10" "out" 16698 "Pix - RECEITA FEDERAL")
    (test-ir--bank "u1" "pj" "2025-03-01" "out" 17831 "Pix - RECEITA FEDERAL")
    (let ((dir (make-temp-file "fin-pay-" t)))
      (unwind-protect
          (progn
            (write-region "arrecadacao,receita,documento,apuracao,total\n2025-02-13,3333,1,2025-01-01,609.31\n2025-02-11,1410,2,2025-01-31,166.98\n"
                          nil (expand-file-name "pagamentos-cnpj-2025.csv" dir))
            (let* ((fin-ir-payments-dir dir)
                   (fin-pj-tax-items '(("RECEITA FED" . "darf"))))
              (should (equal '(("das" . 60931) ("inss" . 16698) ("darf" . 17831))
                             (plist-get (fin-ir-company 2025) :taxes)))))
        (delete-directory dir t)))))

(ert-deftest ir/bad-payment-line-fails-loud ()
  (should-error (fin-ir--payment-row "2025-02-13,3333,1,2025-01-01,609" "f.csv"))
  (should-error (fin-ir--payment-row "x" "f.csv")))

(ert-deftest ir/bad-pro-labore-fails-loud ()
  (let ((fin-ir-pro-labore '((2025 . "3000"))))
    (should-error (fin-ir--pro-labore 2025 12))))

(ert-deftest ir/position-without-pro-labore-leaves-tax-unknown ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (let* ((fin-ir-pro-labore nil)
           (p (fin-ir-position 2026)))
      (should (= 0 (plist-get p :drawn)))
      (should-not (plist-get p :taxable))
      (should-not (plist-get p :tax)))))

(provide 'test-ir)
;;; test-ir.el ends here
