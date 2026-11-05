;;; test-ir.el --- domain/ir.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'ir)

(ert-deftest ir/table-tax-by-band ()
  (should (= 0 (fin-ir-table-tax 2025 2846720)))
  (should (= 132097 (fin-ir-table-tax 2025 4000000)))
  (should (= 274503 (fin-ir-tax 2025 6000000 1200000))))

(ert-deftest ir/2026-reduction-zeroes-and-phases-out ()
  (should (= 0 (fin-ir-tax 2026 6000000 (fin-ir-simplified 2026 6000000))))
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
    (should (= 632000 (plist-get p :profit)))
    (should (= 580000 (plist-get p :presumed)))
    (should (= 52000 (plist-get p :excess)))
    (should (= 580000 (plist-get p :exempt)))
    (should (= 1252000 (plist-get p :taxable)))
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

(ert-deftest ir/book-profit-raises-the-exempt-limit-of-its-year ()
  (let ((fin-ir-book-profit '((2025 . 600000))))
    (test-ir--with-2025 'yes
      (should (= 600000 (plist-get p :book-profit)))
      (should (= 32000 (plist-get p :excess)))
      (should (= 600000 (plist-get p :exempt)))
      (should (= 1232000 (plist-get p :taxable)))))
  (let ((fin-ir-book-profit '((2025 . 100000))))
    (test-ir--with-2025 nil
      (should (= 52000 (plist-get p :excess)))
      (should (= 580000 (plist-get p :exempt))))))

(ert-deftest ir/exempt-gap-checks-declarations-against-the-books ()
  (let ((p '(:year 2025 :exempt 600000 :book-profit 900000
             :defis-exempt 850000 :declared-exempt 1100000)))
    (should (equal '("DIRPF") (car (fin-dashboard--ir-exempt-gap p)))))
  (let ((p '(:year 2022 :exempt 600000 :book-profit 900000 :defis-exempt 0)))
    (should (equal '("DEFIS") (car (fin-dashboard--ir-exempt-gap p)))))
  (let ((p '(:year 2025 :exempt 600000 :defis-exempt 850000)))
    (should (equal '("DEFIS") (car (fin-dashboard--ir-exempt-gap p)))))
  (let ((p '(:year 2021 :exempt 1529163 :defis-exempt 1529163 :declared-exempt 1529163
             :reported-exempt 3240000)))
    (should (equal '("informe") (car (fin-dashboard--ir-exempt-gap p))))
    (should (string-search "informe 2021" (cadr (fin-dashboard--ir-exempt-line (list p)))))))

(ert-deftest ir/profit-line-names-the-limit-each-year-stands-on ()
  (let* ((fin-ir-full-books 'yes)
         (line (fin-dashboard--ir-profit-line
                '((:year 2021 :profit 300000 :presumed 100000 :excess 200000 :excess-tax 2000)
                  (:year 2022 :profit 600000 :presumed 200000 :book-profit 500000
                   :excess 150000 :excess-tax 1000)
                  (:year 2023 :profit 900000 :presumed 950000 :book-profit 950000 :excess 0)))))
    (should-not (car line))
    (should (string-search "books missing or short, else R$ 30.00 more tax" (cadr line)))
    (should (string-search "2021: R$ 3,000.00 over R$ 1,000.00" (cadr line)))
    (should (string-search "2022: R$ 6,000.00 over books R$ 5,000.00" (cadr line)))
    (should-not (string-search "2023" (cadr line))))
  (let ((fin-ir-full-books 'yes))
    (should-not (fin-dashboard--ir-profit-line '((:year 2023 :profit 100 :presumed 0 :excess 99999))))
    (should-not (fin-dashboard--ir-profit-line
                 '((:year 2021 :profit 3307826 :presumed 1529163 :excess 1778663
                    :exempt 1529163 :declared-exempt 1531065))))
    (should (fin-dashboard--ir-profit-line
             '((:year 2021 :profit 3307826 :presumed 1529163 :excess 1778663
                :exempt 1529163 :declared-exempt 3307826 :excess-tax 30462))))
    (should (car (fin-dashboard--ir-profit-line
                  '((:year 2026 :profit 500000 :presumed 100000 :excess 400000)))))))

(ert-deftest ir/tiles-put-each-figure-on-its-line ()
  (let ((html (fin-dashboard--ir-tiles '(:year 2026 :revenue 100 :taxes (("das" . 10) ("inss" . 20))
                                         :drawn 50 :pro-labore 30 :profit 20 :tax 0 :model simplified))))
    (should (string-search "das 0.10<br>inss 0.20" html))
    (should (string-search "pró-labore 0.30/mo<br>profit 0.20" html))
    (should (string-search "simplified model<br>before IRRF withheld" html))))

(ert-deftest ir/return-cell-shows-the-status ()
  (should (equal '(:raw "<span class=\"neg\" title=\"filed 07/10/2026\">late</span>")
                 (fin-dashboard--ir-return-cell '(:status late :filed "2026-10-07"))))
  (should (equal '(:raw "05/05/2025") (fin-dashboard--ir-return-cell '(:status on-time :filed "2025-05-05"))))
  (should (equal '(:raw "<span class=\"neg\">missing</span>")
                 (fin-dashboard--ir-return-cell '(:status missing))))
  (should (equal '(:raw "–") (fin-dashboard--ir-return-cell '(:status not-required))))
  (should (equal '(:raw "") (fin-dashboard--ir-return-cell nil)))
  (should (equal '(:raw "?") (fin-dashboard--ir-return-cell '(:status odd)))))

(ert-deftest ir/bad-full-books-fails-loud ()
  (let ((fin-ir-full-books 'maybe))
    (should-error (fin-ir--books-exempt-p))))

(ert-deftest ir/excess-tax-is-the-difference ()
  (let ((fin-ir-retirement-plan nil))
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
      (should (= 266000 (plist-get p :profit))))))

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
      (should (= 738239 (plist-get p :drawn)))
      (should (= 204239 (plist-get p :profit))))))

(ert-deftest ir/company-spending-for-the-partner-counts-as-drawn ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "w1" "account" "2025-01-10" "in" 800000 "Recebida - OWN CNPJ")
    (test-ir--bank "c1" "pj" "2025-02-19" "out" 1200000 "Pix - GLOBO NISSAN COMERCIO")
    (test-ir--bank "c2" "pj" "2025-02-20" "in" 1200000 "Pix - GLOBO NISSAN estorno")
    (test-ir--bank "u1" "pj" "2025-03-05" "out" 9990 "Pix - UNIFIQUE")
    (let ((fin-conv-salary-regexps '("OWN CNPJ"))
          (fin-ir-pro-labore '((2025 . 600000)))
          (fin-ir-partner-payees '("globo nissan")))
      (should (= 1200000 (fin-ir-partner-paid 2025)))
      (should (= 0 (fin-ir-partner-paid 2024)))
      (let ((p (fin-ir-position 2025 (test-ir--ctx))))
        (should (= 1200000 (plist-get p :partner-paid)))
        (should (= 2000000 (plist-get p :drawn)))))))

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
      (should (= 3932000 (plist-get p :profit)))
      (should (= 10000000 (plist-get p :declared-revenue)))
      (should (equal "2022-02" (plist-get p :declared-through)))
      (should (= 0 (plist-get p :invoiced-through)))
      (should (= 2600000 (plist-get p :presumed))))))

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
          (should (equal '(1630000 135833 179300) (fin-ir--pro-labore 2024 12)))
          (should (equal '(600000 300000 66000) (fin-ir--pro-labore 2026 2)))
          (should-not (fin-ir--pro-labore 2025 12))
          (should (equal '((2024 . 25000)) (fin-ir-other-payers)))
          (let ((fin-ir-pro-labore '((2026 :monthly 100000))))
            (should (equal 100000 (nth 1 (fin-ir--pro-labore 2026 2))))))
      (delete-directory dir t))))

(ert-deftest ir/dctfweb-month-at-inss-cap-leaves-pro-labore-unknown ()
  (let* ((dir (make-temp-file "fin-ir-" t))
         (fin-ir-payments-dir dir) (fin-ir-pro-labore nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "dctfweb-x.csv" dir)
            (insert "periodo;transmissao;categoria;origem;tipo;situacao;debito;saldo\n"
                    "02/2025;x;Geral;eSocial;Original;Ativa;897.32;897.32\n"
                    "01/2025;x;Geral;eSocial;Original;Ativa;330.00;330.00\n"))
          (should-not (fin-ir--pro-labore 2025 2))
          (should (equal '(2025 nil 122732 2) (assq 2025 (fin-ir-pro-labore-files)))))
      (delete-directory dir t))))

(ert-deftest ir/yearly-pro-labore-prorated-to-months-so-far ()
  (let ((fin-ir-pro-labore '((2025 . 1200000))))
    (should (equal '(1200000 100000 132000) (fin-ir--pro-labore 2025 12)))
    (should (equal '(900000 100000 99000) (fin-ir--pro-labore 2025 9)))))

(ert-deftest ir/filed-from-receipt-names ()
  (let* ((dir (make-temp-file "fin-ir-" t)) (fin-ir-payments-dir dir) (fin-ir-filed '((2023 . "2024-05-01"))))
    (unwind-protect
        (progn (make-directory (expand-file-name "dirpf" dir))
               (dolist (f '("dirpf-cpf-2025-recibo-2026-10-07.pdf" "dirpf-cpf-2025-declaracao.pdf"))
                 (write-region "" nil (expand-file-name (concat "dirpf/" f) dir)))
               (should (equal '((2025 . "2026-10-07") (2023 . "2024-05-01"))
                              (fin-ir--filed))))
      (delete-directory dir t))))

(ert-deftest ir/defis-exempt-defaults-to-zero-when-filed ()
  (let* ((dir (make-temp-file "fin-ir-" t)) (fin-ir-payments-dir dir) (fin-ir-defis-exempt '((2022 . 500)))
         (ctx '(:defis ((2022 . 900) (2024 . 700)))))
    (unwind-protect
        (progn (make-directory (expand-file-name "defis" dir))
               (write-region "" nil (expand-file-name "defis/defis-cnpj-2021.pdf" dir))
               (write-region "" nil (expand-file-name "defis/defis-cnpj-2023-retificadora-002.pdf" dir))
               (should (equal 0 (fin-ir--defis-exempt ctx 2021)))
               (should (equal 500 (fin-ir--defis-exempt ctx 2022)))
               (should (equal 0 (fin-ir--defis-exempt ctx 2023)))
               (should (equal 700 (fin-ir--defis-exempt ctx 2024)))
               (should-not (fin-ir--defis-exempt ctx 2025)))
      (delete-directory dir t))))

(ert-deftest ir/yearly-summaries-read-defis-and-books ()
  (let* ((dir (make-temp-file "fin-ir-" t)) (fin-ir-payments-dir dir))
    (unwind-protect
        (progn
          (should-not (fin-ir-defis-files))
          (make-directory (expand-file-name "defis" dir))
          (with-temp-file (expand-file-name "defis/defis-cnpj-2021_2025-resumo.csv" dir)
            (insert "ano,declaracao,isento\n2021,retificadora-002,15291.63\n2025,retificadora-002,85000.00\n"))
          (with-temp-file (expand-file-name "dre-cnpj-2024-resumo.csv" dir)
            (insert "ano,arquivo,lucro\n2024,teves/x.pdf,87277.64\n"))
          (should (equal '((2021 . 1529163) (2025 . 8500000))
                         (sort (fin-ir-defis-files) (lambda (a b) (< (car a) (car b))))))
          (should (equal '((2024 . 8727764)) (fin-ir-book-files)))
          (with-temp-file (expand-file-name "dre-cnpj-2024b-resumo.csv" dir)
            (insert "ano,arquivo,lucro\n2024,y.pdf,1.00\n"))
          (should-error (fin-ir-book-files))
          (with-temp-file (expand-file-name "dre-cnpj-2024b-resumo.csv" dir)
            (insert "ano,arquivo,lucro\nx,y.pdf,1.00\n"))
          (should-error (fin-ir-book-files)))
      (delete-directory dir t))))

(ert-deftest ir/book-summary-sets-the-limit-unless-overridden ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--seed-2025)
    (let* ((fin-pj-income-payers '(("BP Educacao" . "bp")))
           (fin-pj-tax-items '(("\\bDAS\\b" . "das")))
           (fin-conv-salary-regexps '("OWN CNPJ"))
           (fin-ir-pro-labore '((2025 :monthly 100000)))
           (ctx (append (test-ir--ctx) '(:books ((2025 . 600000)) :reported-exempt ((2025 . 1))))))
      (let ((p (fin-ir-position 2025 ctx)))
        (should (= 600000 (plist-get p :book-profit)))
        (should (= 1 (plist-get p :reported-exempt))))
      (let ((fin-ir-book-profit '((2025 . 610000))))
        (should (= 610000 (plist-get (fin-ir-position 2025 ctx) :book-profit)))))))

(ert-deftest ir/reported-exempt-sums-non-zero-payers ()
  (let* ((dir (make-temp-file "fin-ir-" t)) (fin-ir-payments-dir dir))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "fontes-pagadoras" dir))
          (with-temp-file (expand-file-name "fontes-pagadoras/fontes.csv" dir)
            (insert "ano,fonte,rendimento_tributavel,prev_oficial,irrf,isento,processado\n"
                    "2021,A,15600.00,1716.00,0.00,32400.00,2022-02-22\n"
                    "2021,B,250.00,0.00,0.00,0.00,2026-04-18\n"
                    "2022,A,15600.00,1716.00,0.00,0.00,2023-02-27\n"))
          (should (equal '((2021 . 3240000)) (fin-ir-reported-exempt)))
          (with-temp-file (expand-file-name "fontes-pagadoras/fontes.csv" dir)
            (insert "ano,fonte,rendimento_tributavel,prev_oficial,irrf,isento,processado\n"
                    "2021,A,15600.00,1716.00\n"))
          (should-error (fin-ir-reported-exempt)))
      (delete-directory dir t))))

(defun test-ir--dec (exercise fields &optional len exempt)
  "A .DEC text: IR header of EXERCISE, totals record of LEN (744) with
FIELDS, (START . CENTS) each, as 13-digit fields, then an exempt record 84
per EXEMPT (CODE . CENTS)."
  (let ((rec (make-string (or len 744) ?0)))
    (aset rec 0 ?1) (aset rec 1 ?8)
    (dolist (f fields)
      (let ((d (format "%013d" (cdr f))))
        (dotimes (i 13) (aset rec (+ (car f) i) (aref d i)))))
    (concat "IR      " (number-to-string exercise) "2025xx\r\n" rec "\r\n"
            (mapconcat (lambda (e) (format "8400000000000T00000000000%s37554387000138%-60s%013d0000\r\n"
                                           (car e) "PAYER" (cdr e)))
                       exempt "")
            "T9\r\n")))

(ert-deftest ir/dec-totals-read-the-filed-return ()
  (let ((p (fin-ir-dec-totals (test-ir--dec 2026 '((456 . 1800000) (274 . 2864756) (287 . 7343061)
                                                   (300 . 9999999) (326 . 241201))
                                            nil '(("0013" . 9540000) ("0012" . 19020))))))
    (should (= 2026 (plist-get p :exercise)))
    (should (= 9540000 (plist-get p :exempt)))
    (should (= 241201 (plist-get p :exclusive)))
    (should (= 7343061 (plist-get p :assets)))
    (should (= 2864756 (plist-get p :assets-before)))
    (should (= 1800000 (plist-get p :other-taxable))))
  (let ((p (fin-ir-dec-totals (test-ir--dec 2023 '((274 . 148650) (300 . 5542153) (326 . 10932)) 718
                                            '(("0013" . 5540933))))))
    (should (= 2023 (plist-get p :exercise)))
    (should (= 5540933 (plist-get p :exempt)))
    (should (= 10932 (plist-get p :exclusive)))
    (should (= 148650 (plist-get p :assets-before)))
    (should (= 0 (plist-get p :assets))))
  (let ((p (fin-ir-dec-totals (test-ir--dec 2022 '((287 . 148650) (300 . 1531065) (326 . 564)) 718))))
    (should (= 0 (plist-get p :exempt)))
    (should (= 2022 (plist-get p :exercise)))
    (should (= 564 (plist-get p :exclusive)))
    (should (= 148650 (plist-get p :assets))))
  (should-error (fin-ir-dec-totals (test-ir--dec 2023 nil)))
  (should-error (fin-ir-dec-totals
                 (replace-regexp-in-string "0000009540000" "00000095400x0"
                                           (test-ir--dec 2026 nil nil '(("0013" . 9540000))))))
  (should-error (fin-ir-dec-totals (test-ir--dec 2027 nil)))
  (should-error (fin-ir-dec-totals (test-ir--dec 2026 nil 700))))

(ert-deftest ir/declared-field-falls-back-to-next-return-for-assets ()
  (let ((ctx '(:dirpf ((2025 :assets 7343061 :assets-before 2864756 :exempt 9540000)))))
    (should (= 2864756 (fin-ir--declared-field ctx 2024 :assets nil)))
    (should (= 7343061 (fin-ir--declared-field ctx 2025 :assets nil)))
    (should (= 1 (fin-ir--declared-field ctx 2025 :exempt '((2025 . 1)))))
    (should-not (fin-ir--declared-field ctx 2024 :exempt nil))))

(ert-deftest ir/duty-statuses ()
  (let ((fin-ir-payments-dir nil)
        (fin-ir-filed '((2025 . "2026-10-07") (2024 . "2025-04-01"))))
    (should (eq 'missing (plist-get (fin-ir-duty '(:year 2022 :taxable 1439018 :exempt 6052500)) :status)))
    (should (eq 'not-required (plist-get (fin-ir-duty '(:year 2023 :taxable 1584000 :exempt 7355400)) :status)))
    (should (eq 'late (plist-get (fin-ir-duty '(:year 2025 :taxable 4066200 :exempt 0)) :status)))
    (should (eq 'on-time (plist-get (fin-ir-duty '(:year 2024 :taxable 0 :exempt 0)) :status)))
    (should (eq 'unknown (plist-get (fin-ir-duty '(:year 2021 :taxable nil :exempt nil)) :status)))
    (should-error (fin-ir-duty '(:year 2026 :taxable 0 :exempt 0)))))

(ert-deftest ir/duty-counts-exclusive-exempt-income-and-assets ()
  (let* ((fin-ir-payments-dir nil)
         (d (fin-ir-duty '(:year 2022 :taxable 0 :exempt 3000000 :exclusive 600000 :individuals-exempt 500000))))
    (should (eq 'missing (plist-get d :status)))
    (should (= 4100000 (plist-get d :exempt-total)))
    (should (eq 'not-required (plist-get (fin-ir-duty '(:year 2022 :taxable 0 :exempt 3000000 :exclusive 600000)) :status)))
    (should (eq 'missing (plist-get (fin-ir-duty '(:year 2022 :taxable 0 :exempt 0 :assets 30000001)) :status)))))

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
      (should (equal '("c1") (mapcar #'caar (fin-ir-wrong-account receita))))
      (let ((fin-ir-wrong-account-known '(("2025-06-06" 45191))))
        (should-not (fin-ir-wrong-account receita))))))

(ert-deftest ir/receita-rows-are-the-tax-items-paid-to-receita ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "s1" "pj" "2025-02-12" "out" 50 "DAS - Simples Nacional")
    (test-ir--bank "g1" "pj" "2025-02-13" "out" 100 "GPS - INSS")
    (test-ir--bank "c1" "pj" "2025-02-14" "out" 200 "Pix - TEVES ASSESSORIA")
    (test-ir--bank "d1" "account" "2025-02-15" "out" 300 "Pix - das flores")
    (let ((fin-pj-tax-items '(("\\bDAS\\b" . "das") ("\\bGPS\\b" . "gps") ("TEVES ASSESSORIA" . "contabilidade"))))
      (should (equal '("s1") (mapcar #'car (fin-ir--receita-rows)))))))

(ert-deftest ir/receita-codes-name-the-payer ()
  (should (equal '("das" pj) (fin-ir-code "3333")))
  (should (equal '("inss complement" cpf) (fin-ir-code "1872")))
  (should (equal '("irpf fine" cpf) (fin-ir-code "5320")))
  (should (equal '("9999" pj) (fin-ir-code "9999"))))

(ert-deftest ir/third-party-darf-is-no-tax-of-either ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "t1" "pj" "2025-06-08" "out" 17831 "Pix - RECEITA FEDERAL")
    (let* ((fin-pj-tax-items '(("RECEITA FED" . "darf")))
           (fin-ir-payment-labels '(("2025-06-08" 17831 "third-party")))
           (receita (fin-ir-receita-matches nil))
           (company (fin-ir-company 2025 receita)))
      (should (equal "third-party" (cdar receita)))
      (should-not (plist-get company :taxes))
      (should (= 0 (plist-get company :partner-taxes)))
      (should-not (fin-ir-wrong-account receita))
      (should-not (fin-ir-unmatched receita)))))

(ert-deftest ir/receita-payment-matches-once-across-years ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (test-ir--bank "a" "pj" "2024-12-30" "out" 60931 "Pix - RECEITA FEDERAL")
    (test-ir--bank "b" "pj" "2025-01-03" "out" 60931 "Pix - RECEITA FEDERAL")
    (let* ((fin-pj-tax-items '(("RECEITA FED" . "darf")))
           (receita (fin-ir-receita-matches '(("2025-01-02" "3333" 60931)))))
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
