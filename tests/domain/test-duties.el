;;; test-duties.el --- domain/duties.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'duties)
(require 'irpf)

(defmacro test-duties--with-dir (dir &rest body)
  "BODY with DIR a fresh `fin-ir-payments-dir', deleted after."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "fin-duty-" t)) (fin-ir-payments-dir ,dir))
     (unwind-protect (progn ,@body) (delete-directory ,dir t))))

(defun test-duties--write (dir name text)
  (with-temp-file (expand-file-name name dir) (insert text)))

(ert-deftest duty/month-arithmetic-wraps-years ()
  (should (equal "2027-01" (fin-duty--month+ "2026-12" 1)))
  (should (equal "2025-12" (fin-duty--month+ "2026-01" -1)))
  (should (equal "2025-11" (fin-duty--month+ "2026-10" -11)))
  (should (equal "2026-12-20" (fin-duty-due "2026-11")))
  (should (equal "2027-01-20" (fin-duty-due "2026-12")))
  (should-error (fin-duty--month+ "2026-1" 1)))

(ert-deftest duty/window-runs-from-since-to-last-month ()
  (let ((fin-duty-since nil)) (should-not (fin-duty--window "2026-11-05")))
  (let ((fin-duty-since "2025-11"))
    (should (equal '("2025-11" "2025-12" "2026-01") (fin-duty--window "2026-02-05"))))
  (let ((fin-duty-since "2026-10"))
    (should (equal '("2026-10") (fin-duty--window "2026-11-05"))))
  (let ((fin-duty-since "2026-11"))
    (should-not (fin-duty--window "2026-11-05")))
  (let ((fin-duty-since "2026-1"))
    (should-error (fin-duty--window "2026-11-05"))))

(ert-deftest duty/planned-pro-labore-and-its-inss ()
  (let ((fin-duty-pro-labore '(("2025-01" . 151800) ("2025-10" . 300000) ("2026-11" . 162100))))
    (should-not (fin-duty-planned "2024-12"))
    (should (= 151800 (fin-duty-planned "2025-09")))
    (should (= 300000 (fin-duty-planned "2026-10")))
    (should (= 162100 (fin-duty-planned "2027-03")))
    (should (= 33000 (fin-duty-expected-inss "2026-10")))
    (should (= 17831 (fin-duty-expected-inss "2026-11")))
    (should-not (fin-duty-expected-inss "2024-12")))
  (let ((fin-duty-pro-labore '(("2026-01" . 5000000))))
    (should (= (round (* 0.11 (fin-ir-rule 2026 :inss-cap))) (fin-duty-expected-inss "2026-03")))))

(ert-deftest duty/codes-and-payments-by-period ()
  (should (member "3333" (fin-duty--codes "das")))
  (should (member "1099" (fin-duty--codes "inss")))
  (should-not (member "1872" (fin-duty--codes "inss")))
  (let ((payments '(("2026-10-19" "3333" 95069 "2026-09") ("2026-10-07" "1099" 33000 "2026-09")
                    ("2026-10-07" "1099" 100 "2026-08"))))
    (should (= 95069 (fin-duty--paid payments (fin-duty--codes "das") "2026-09")))
    (should (= 33000 (fin-duty--paid payments (fin-duty--codes "inss") "2026-09")))
    (should (= 0 (fin-duty--paid payments (fin-duty--codes "das") "2026-10")))))

(ert-deftest duty/payment-row-keeps-its-period ()
  (should (equal '("2026-10-19" "3333" 95069 "2026-09")
                 (fin-ir--payment-row "2026-10-19,3333,07202629207089659,2026-09-01,950.69" "f.csv")))
  (should-error (fin-ir--payment-row "2026-10-19,3333,1,2026-09,950.69" "f.csv")))

(ert-deftest duty/dctfweb-last-transmission-wins ()
  (should (equal '("2026-09" "2026-10-06 16:01:53" 33000)
                 (fin-duty--dctfweb-row "09/2026;06/10/2026 16:01:53;Geral;eSocial;Original;Ativa;330.00;330.00" "f")))
  (should-error (fin-duty--dctfweb-row "09/2026;x;Geral;eSocial;Original;Ativa;330.00;330.00" "f"))
  (should-error (fin-duty--dctfweb-row "2026-09;x" "f"))
  (test-duties--with-dir dir
    (test-duties--write dir "dctfweb-a.csv"
                        (concat "periodo;transmissao;categoria;origem;tipo;situacao;debito;saldo\n"
                                "09/2026;20/10/2026 14:39:47;Geral;eSocial;Retificadora;Ativa;3342.19;3342.19\n"
                                "09/2026;06/10/2026 16:01:53;Geral;eSocial;Original;Ativa;330.00;330.00\n"
                                "08/2026;04/09/2026 10:14:05;Geral;eSocial;Original;Ativa;330.00;330.00\n"))
    (let ((d (fin-duty-dctfweb)))
      (should (= 334219 (cdr (assoc "2026-09" d))))
      (should (= 33000 (cdr (assoc "2026-08" d)))))))

(ert-deftest duty/open-labels-what-a-month-lacks ()
  (let ((done '(:month "2026-09" :declared 1584500 :das 95069 :das-paid 95069
                :dctfweb 33000 :exported t :inss 33000 :inss-paid 33000)))
    (should-not (fin-duty-open done))
    (should (equal '("PGDAS-D") (fin-duty-open (plist-put (copy-sequence done) :declared nil))))
    (should (equal '("DAS") (fin-duty-open (plist-put (copy-sequence done) :das-paid 90000))))
    (should (equal '("DCTFWeb") (fin-duty-open (plist-put (copy-sequence done) :dctfweb nil))))
    (should-not (fin-duty-open (plist-put (plist-put (copy-sequence done) :dctfweb nil) :exported nil)))
    (should (equal '("DCTFWeb off the pró-labore")
                   (fin-duty-open (plist-put (copy-sequence done) :dctfweb 334219))))
    (should (equal '("INSS") (fin-duty-open (plist-put (copy-sequence done) :inss-paid 0))))
    (should-not (fin-duty-open (plist-put (plist-put (copy-sequence done) :inss nil) :dctfweb nil)))))

(ert-deftest duty/months-read-the-inbox ()
  (test-duties--with-dir dir
    (test-duties--write dir "pagamentos-cnpj-x.csv"
                        (concat "arrecadacao,receita,documento,apuracao,total\n"
                                "2026-10-19,3333,1,2026-09-01,950.69\n"
                                "2026-10-07,1099,2,2026-09-30,330.00\n"))
    (test-duties--write dir "dctfweb-x.csv"
                        (concat "periodo;transmissao;categoria;origem;tipo;situacao;debito;saldo\n"
                                "09/2026;06/10/2026 16:01:53;Geral;eSocial;Original;Ativa;330.00;330.00\n"))
    (let* ((fin-duty-since "2026-09")
           (fin-duty-pro-labore '(("2026-01" . 300000)))
           (ctx '(:simples (("2026-09" 1584500 95069) ("2026-10" 1300000 78000))
                  :invoices (("2026-09" . 1584500) ("2026-10" . 1300000))))
           (months (fin-duty-months "2026-11-05" ctx)))
      (should (equal '("2026-09" "2026-10") (mapcar (lambda (m) (plist-get m :month)) months)))
      (should-not (fin-duty-open (car months)))
      (should (equal '("DAS" "DCTFWeb" "INSS") (fin-duty-open (cadr months))))
      (should (plist-get (cadr months) :exported))
      (should (equal "2026-11-20" (plist-get (cadr months) :due))))))

(ert-deftest duty/mei-months-owe-only-the-das-mei ()
  (let ((ctx '(:mei-until "2020-12"))
        (fin-duty-pro-labore '(("2020-01" . 100000))))
    (should (equal "2020-12" (fin-ir--last-period '(("2020-10-09" "0055" 5825 "2020-10")
                                                     ("2020-12-21" "0055" 5825 "2020-12")
                                                     ("2021-02-19" "3333" 9000 "2021-01"))
                                                   "0055")))
    (should (fin-duty-mei-p "2020-12" ctx))
    (should-not (fin-duty-mei-p "2021-01" ctx))
    (should-not (fin-duty-mei-p "2020-01" nil))
    (let ((m (fin-duty-month "2020-10" ctx '(("2020-10-09" "0055" 5825 "2020-10")) nil)))
      (should (plist-get m :mei))
      (should-not (plist-get m :inss))
      (should-not (fin-duty-open m)))
    (should (equal '("DAS-MEI") (fin-duty-open (fin-duty-month "2020-11" ctx nil nil))))
    (let ((y (fin-duty-year 2020 ctx nil)))
      (should (plist-get y :mei))
      (should-not (fin-duty-year-open y "2026-11-05")))))

(ert-deftest duty/known-gaps-are-not-flagged ()
  (let* ((fin-ir-payments-dir nil)
         (y (fin-duty-year 2021 nil '(:required t :filed "2026-11-03" :deadline "2022-05-31"))))
    (let ((fin-duty-known nil))
      (should (equal '("books" "DEFIS") (fin-duty-year-open y "2026-11-05"))))
    (let ((fin-duty-known '((2021 "books"))))
      (should (equal '("DEFIS") (fin-duty-year-open y "2026-11-05")))))
  (let ((p '(:year 2021 :exempt 1529163 :defis-exempt 1529163 :declared-exempt 1529163
             :reported-exempt 3240000)))
    (let ((fin-duty-known nil)) (should (fin-dashboard--ir-exempt-gap p)))
    (let ((fin-duty-known '((2021 "informe")))) (should-not (fin-dashboard--ir-exempt-gap p)))))

(ert-deftest duty/inss-paid-counts-gps-and-the-partners-complement ()
  (let* ((fin-duty-pro-labore '(("2021-01" . 130000) ("2024-01" . 141200)))
         (payments '(("2026-10-28" "1872" 1015 "2024-07") ("2024-08-06" "1410" 14520 "2024-07"))))
    (should (= 15535 (plist-get (fin-duty-month "2024-07" nil payments nil) :inss-paid)))
    (should-not (member "INSS" (fin-duty-open (fin-duty-month "2024-07" '(:simples (("2024-07" 1 0))) payments nil))))
    (should (= 14300 (plist-get (fin-duty-month "2021-09" nil nil nil '(("2021-09" . 14300))) :inss-paid)))))

(ert-deftest duty/gps-boletos-pay-the-month-before ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('g','test','pj','2021-10-08','out',14300,'Pagamento de boleto efetuado - GPS')")
    (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                  VALUES ('a','test','account','2021-10-08','out',14300,'Pagamento de boleto efetuado - GPS')")
    (let ((fin-pj-tax-items '(("\\bGPS\\b" . "gps"))) (fin-ir-payments-dir nil))
      (should (equal '(("2021-09" . 14300)) (fin-duty-gps))))
    (test-duties--with-dir dir
      (test-duties--write dir "gps-cnpj-x.csv"
                          (concat "\ufeffContribuinte: X\nCNPJ 1\nCompetências de 08/2021 a 09/2021\n"
                                  "Competência;Quantidade de Documentos;Valor Recolhido;Imputação\n"
                                  "08/2021;1;143,00;\n09/2021;2;1.430,00;\n"))
      (let ((fin-pj-tax-items '(("\\bGPS\\b" . "gps"))))
        (should (equal '(("2021-08" . 14300) ("2021-09" . 143000)) (fin-duty-gps)))))))

(ert-deftest duty/yearly-filings-late-after-their-dates ()
  (let* ((ctx '(:books ((2025 . 8621428)) :defis ((2025 . 8500000))))
         (fin-ir-payments-dir nil)
         (y (fin-duty-year 2025 ctx '(:required t :filed "2026-10-07" :deadline "2026-05-29"))))
    (should (equal "2026-03-31" (plist-get y :due)))
    (should-not (fin-duty-year-open y "2026-11-05"))
    (let ((missing (fin-duty-year 2026 nil '(:required t :filed nil :deadline "2027-05-31"))))
      (should-not (fin-duty-year-open missing "2027-03-01"))
      (should (equal '("books" "DEFIS") (fin-duty-year-open missing "2027-04-01")))
      (should (equal '("books" "DEFIS" "DIRPF") (fin-duty-year-open missing "2027-06-01"))))
    (should (eq 'not-required (plist-get (fin-duty-year 2024 ctx '(:required nil)) :dirpf)))
    (should (eq 'unknown (plist-get (fin-duty-year 2026 ctx nil) :dirpf)))))

(ert-deftest duty/room-is-the-profit-kept-in-the-company ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (let ((ins (lambda (id type amount desc)
                 (fin-db-exec "INSERT INTO bank_txn (id, source, account, date, type, amount, description)
                               VALUES (?,'test','pj','2026-03-01',?,?,?)" (list id type amount desc)))))
      (funcall ins "r" "in" 1000000 "Pix - BP Educacao")
      (funcall ins "d" "out" 60000 "DAS")
      (funcall ins "i" "out" 33000 "INSS")
      (funcall ins "c" "out" 27900 "Contabilidade")
      (funcall ins "p" "out" 800000 "Pix - OWN NAME")
      (funcall ins "x" "out" 10000 "Pix - PARTNER RENT"))
    (let* ((fin-bank-own-regexps '("OWN NAME"))
           (pos '(:year 2026 :revenue 1000000 :pro-labore-total 300000 :inss 33000
                  :partner-paid 10000 :partner-taxes 0 :profit 543000))
           (room (fin-duty-room pos)))
      (should (= 612100 (plist-get room :book-profit)))
      (should (= 69100 (plist-get room :room)))
      (should-not (fin-duty-room '(:year 2026))))))

(ert-deftest duty/stale-situacao-fiscal-and-exports ()
  (test-duties--with-dir dir
    (let ((fin-duty-since "2025-11") (fin-duty-stale-days 30)
          (fin-duty-pro-labore nil))
      (should (equal '("situação fiscal CNPJ missing" "situação fiscal CPF missing"
                       "DCTFWeb export missing" "company payments export missing")
                     (fin-duty-stale "2026-11-05")))
      (make-directory (expand-file-name "situacao-fiscal" dir))
      (test-duties--write dir "situacao-fiscal/situacao-fiscal-cnpj-2026-11-05.pdf" "")
      (test-duties--write dir "situacao-fiscal/situacao-fiscal-cpf-2026-09-01.pdf" "")
      (test-duties--write dir "dctfweb-x.csv" "")
      (test-duties--write dir "pagamentos-cnpj-x.csv" "")
      (set-file-times (expand-file-name "dctfweb-x.csv" dir) (date-to-time "2026-10-11 12:00:00"))
      (should (equal '("situação fiscal CPF from 2026-09-01" "DCTFWeb export from 2026-10-11")
                     (fin-duty-stale "2026-11-05"))))))

(defun test-duties--count (needle haystack)
  (let ((n 0) (start 0))
    (while (string-search needle haystack start)
      (setq start (1+ (string-search needle haystack start)) n (1+ n)))
    n))

(ert-deftest duty/view-marks-only-what-needs-action ()
  (let* ((done '(:month "2026-09" :due "2026-10-20" :declared 1 :das 1 :das-paid 1))
         (open '(:month "2026-10" :due "2026-11-20" :declared 1 :das 78000 :das-paid 0
                 :exported t :inss 33000 :inss-paid 0))
         (off '(:month "2026-10" :due "2026-11-20" :declared 1 :das 78000 :das-paid 78000
                :exported t :dctfweb 334219 :inss 33000 :inss-paid 0))
         (months (list done open)))
    (should-not (fin-dashboard--duty-alerts open "2026-11-05"))
    (should-not (fin-dashboard--duty-month-lines months "2026-11-05"))
    (should (equal '("DAS" "DCTFWeb" "INSS") (fin-dashboard--duty-alerts open "2026-11-21")))
    (should (equal '((nil "DAS late: 10/2026") (nil "DCTFWeb late: 10/2026") (nil "INSS late: 10/2026"))
                   (fin-dashboard--duty-month-lines months "2026-11-21")))
    (should (equal '("DCTFWeb off the pró-labore") (fin-dashboard--duty-alerts off "2026-11-05")))
    (should (equal '((nil "fix before 20/11/2026: 10/2026 DCTFWeb off the pró-labore"))
                   (fin-dashboard--duty-month-lines (list off) "2026-11-05")))
    (should (equal '(:raw "–") (fin-dashboard--duty-cell 0 nil '("DAS"))))
    (let ((cell (plist-get (fin-dashboard--duty-cell 0 '("DAS" "INSS") '("DAS")) :raw)))
      (should (string-search "⚠" cell))
      (should (string-search "title=\"DAS\"" cell)))
    (should-not (string-search "⚠" (plist-get (fin-dashboard--duty-cell 60931 '("INSS") '("DAS")) :raw)))
    (let ((table (fin-dashboard--duty-months-table (list done off) "2026-11-05")))
      (should-not (string-search "status" table))
      (should (= 1 (test-duties--count "⚠" table))))))

(ert-deftest duty/runs-join-consecutive-months ()
  (should (equal "01–08/2021, 04/2022"
                 (fin-dashboard--duty-runs '("2021-01" "2021-02" "2021-03" "2021-04" "2021-05"
                                             "2021-06" "2021-07" "2021-08" "2022-04"))))
  (should (equal "11/2021–02/2022" (fin-dashboard--duty-runs '("2021-11" "2021-12" "2022-01" "2022-02"))))
  (should (equal "" (fin-dashboard--duty-runs nil))))

(defconst test-duties--2022
  '((:month "2022-03" :due "2022-04-20" :declared 729375 :das 44341 :das-paid 44341
     :accounting 22000 :pro-labore 130000 :inss 14300 :inss-paid 14300)
    (:month "2022-04" :due "2022-05-20" :declared 680625 :das 40837 :das-paid 40837
     :accounting 22000 :pro-labore 130000 :exported t :dctfweb 14300 :inss 14300 :inss-paid 0)))

(ert-deftest duty/months-table-has-no-totals ()
  (let ((html (fin-dashboard--duty-months-table test-duties--2022 "2026-11-05")))
    (should (string-search "<th>accountant</th><th>pró-labore</th>" html))
    (should-not (string-search "total" html))
    (should (= 1 (test-duties--count "⚠" html)))))

(ert-deftest duty/month-plans-pro-labore-and-books-accounting ()
  (let ((fin-duty-pro-labore '(("2021-01" . 130000))))
    (let ((m (fin-duty-month "2022-04" nil nil nil nil '(("2022-04" . 22000)))))
      (should (= 130000 (plist-get m :pro-labore)))
      (should (= 22000 (plist-get m :accounting))))
    (should-not (plist-get (fin-duty-month "2020-10" '(:mei-until "2020-12") nil nil) :pro-labore))))

(ert-deftest duty/years-block-rows-total-their-months ()
  (let* ((fin-duty-since "2022-03")
         (positions '((:year 2022 :taxable 2107489 :exempt 5540933 :presumed 2237918 :book-profit 5540933
                       :declared-revenue 1410000 :declared-through "2022-04" :revenue-through 1400000
                       :invoiced-through 1410000 :tax 0 :profit 868600 :model simplified :simplified 421498)))
         (closed (list (cons (car positions) '(:status late :filed "2026-10-30"))))
         (html (cl-letf (((symbol-function 'fin-duty-months) (lambda (&rest _) test-duties--2022))
                         ((symbol-function 'fin-dashboard--duty-year-rows) (lambda (&rest _) nil))
                         ((symbol-function 'fin-duty-year) (lambda (&rest _) nil))
                         ((symbol-function 'format-time-string) (lambda (&rest _) "2026-11-05")))
                 (fin-dashboard--years-block positions closed '(:x 1)))))
    (should (string-search "<th>revenue</th><th>DAS</th><th>accountant</th><th>pró-labore</th><th>INSS</th><th>tax</th><th>DIRPF</th>" html))
    (should (string-search "title=\"bank received 14,000.00\">14,100.00</span>" html))
    (should (string-search "<td>851.78</td><td>440.00</td><td>2,600.00</td><td>143.00</td>" html))
    (should (string-search "taxable 21,074.89 · exempt 55,409.33, up to books 55,409.33 · deductions 4,214.98 · profit 10,208.22, paid to you 8,686.00, kept 1,522.22" html))
    (should (string-search "title=\"filed 30/10/2026\">late</span>" html))
    (should-not (string-search "<td>total</td>" html))
    (should-not (string-search "= profit" html))))

(ert-deftest duty/years-alerts-is-a-titled-mark ()
  (let ((fin-duty-known nil))
    (should (equal "<span class=\"neg\" title=\"1 month\">⚠</span>"
                   (fin-dashboard--years-alerts (cdr test-duties--2022) nil "2026-11-05")))
    (should (equal "" (fin-dashboard--years-alerts nil nil "2026-11-05")))
    (should (string-search "title=\"books late, DEFIS late\""
                           (fin-dashboard--years-alerts nil '(:year 2026 :due "2027-03-31") "2027-04-02"))))
  (let ((y '(:books 8621428 :defis 8500000)))
    (should (equal '(("books" 8621428) ("DEFIS" 8500000)) (fin-dashboard--years-filings y 8182208)))
    (should-not (fin-dashboard--years-filings '(:books 8182300 :defis 8182208) 8182208))
    (should-not (fin-dashboard--years-filings nil 8182208))))

(provide 'test-duties)
;;; test-duties.el ends here
