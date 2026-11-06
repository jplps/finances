;;; test-bankfix.el --- tools/bankfix.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'bankfix)

(ert-deftest bankfix/changes-skip-edits-on-deleted-and-repeated-rows ()
  (let* ((l1 '(1 "2025-04-14" "out" "free" "sushi" 10500 nil))
         (l2 '(2 "2025-05-02" "out" "car" "seguro" 15353 nil))
         (changes (fin-bank-fix--changes
                   (list (list :edit l1 10498 "near")
                         (list :delete l1 "refunded")
                         (list :edit l2 15357 "a") (list :edit l2 15358 "b")
                         (list :add '("2025-05-06" "out" "food" nil 100 nil nil) "src")
                         (list :report '("r" "2025-05-06" "in" 5 "x") "look")))))
    (should (equal '((:add ("2025-05-06" "out" "food" "" 100 nil nil))
                     (:delete ("2025-04-14" "out" "free" "sushi" 10500))
                     (:edit ("2025-05-02" "out" "car" "seguro" 15353) 15357))
                   (sort (copy-sequence changes) (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))))

(ert-deftest bankfix/changes-keep-identical-adds ()
  (let ((add (list :add '("2023-05-19" "out" "free" "porks" 1800 nil nil) "Porks")))
    (should (= 2 (length (fin-bank-fix--changes (list add add)))))))

(ert-deftest bankfix/changes-refuse-synthetic-rows ()
  (should-error (fin-bank-fix--changes
                 (list (list :edit (list (list 'net "2026-07" "bp") "2026-07-03" "in" "bp" "net" 1 nil) 2 "x")))))

(ert-deftest bankfix/report-renders-every-kind ()
  (save-window-excursion
    (fin-bank-fix--report
     (list (list :add '("2025-05-06" "out" "food" "komprão" 100 1 2) "KOMPRAO")
           (list :edit '(1 "2025-05-02" "out" "car" "seguro" 15353 nil) 15357 "bank")
           (list :delete '(2 "2025-04-14" "out" "free" "sushi" 10500 nil) "refunded")
           (list :report '("r" "2025-04-13" "in" 3800 "Reembolso") "refund with no purchase found")
           (list :report (list (list 'net "2026-05" "bp") "2026-05-25" "in" "bp" "net of cnpj" 893670 nil) "salary"))
     '((("x" "2025-05-01" "out" 1 "IOF") . "folded into purchase"))
     nil)
    (with-current-buffer "*fin-bank-fix*"
      (let ((text (buffer-string)))
        (dolist (s '("Dry run" "Added (1)" "Edited (1)" "Deleted (1)" "Needs a look (2)"
                     "8936.70" "folded into purchase"))
          (should (string-search s text))))
      (kill-buffer))))

(ert-deftest bankfix/refuses-while-libreoffice-holds-ods ()
  (let* ((dir (make-temp-file "fin-fix-" t))
         (ods (expand-file-name "seeds.ods" dir)))
    (unwind-protect
        (progn
          (write-region "" nil ods)
          (should-not (fin-bank-fix--check-closed ods))
          (write-region "" nil (expand-file-name ".~lock.seeds.ods#" dir))
          (should-error (fin-bank-fix--check-closed ods) :type 'user-error))
      (delete-directory dir t))))

(ert-deftest bankfix/months-count-card-bill-only-without-card-statement ()
  (fin-test-with-db
    (fin-test-insert-entry "2022-05-10" "out" "food" 30000 "x")
    (fin-test-insert-entry "2022-05-11" "out" "investments" 99900 "house")
    (fin-bankdb-insert '(("c" "t" "card" "2023-12-10" "out" 5000 "Padaria")))
    (let ((raw '(("a" "2022-05-03" "out" 20000 "Pix")
                 ("f" "2022-05-08" "out" 40000 "Pagamento de fatura")
                 ("g" "2023-12-08" "out" 9000 "Pagamento de fatura")
                 ("s" "2022-05-09" "out" 7000 "Aplicação RDB"))))
      (should (equal '(("2022-05" 60000 30000) ("2023-12" 5000 0))
                     (fin-bank-fix--months (list (nth 0 raw) (nth 3 raw) '("c" "2023-12-10" "out" 5000 "Padaria"))
                                           raw "2022-01-01"))))))

(ert-deftest bankfix/from-defaults-to-last-month ()
  (let ((fin-bank-fix-from nil))
    (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time)))
      (should (equal (if (= m 1) (format "%04d-12-01" (1- y)) (format "%04d-%02d-01" y (1- m)))
                     (fin-bank-fix--from)))))
  (let ((fin-bank-fix-from "2024-01-01"))
    (should (equal "2024-01-01" (fin-bank-fix--from)))))

(ert-deftest bankfix/vanished-bank-rows-delete-their-adds ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-bankdb-insert (list (list "nu:keep" "nubank-ofx" "card" "2026-10-02" "out" 1000 "Padaria")))
    (fin-test-insert-entry "2026-10-02" "out" "food" 1000 "padaria")
    (fin-test-insert-entry "2026-10-03" "out" "free" 2500 "loja")
    (fin-bankdb-record-fix-adds '(("nu:keep" "2026-10-02" "out" "food" "padaria" 1000)
                                  ("nu:gone" "2026-10-03" "out" "free" "loja" 2500)
                                  ("nu:moved" "2026-10-04" "out" "free" "x" 900)))
    (let ((g (fin-bank-fix--vanished)))
      (should (equal '("nu:gone" "nu:moved") (sort (copy-sequence (car g)) #'string<)))
      (should (equal '((:delete ("2026-10-03" "out" "free" "loja" 2500)))
                     (fin-bank-fix--changes (cdr g))))
      (fin-bankdb-forget-adds (car g))
      (should-not (fin-bankdb-vanished-adds)))
    (should (equal '(("nu:a" "2026-10-05" "out" "food" "" 300))
                   (fin-bank-fix--added
                    '((:add ("2026-10-05" "out" "food" nil 300 nil nil) "X" "nu:a")
                      (:add ("2026-10-06" "out" "food" "y" 400 nil nil) "Y")
                      (:report ("nu:b") "z")))))))

(ert-deftest bankfix/identical-vanished-adds-delete-both-rows ()
  (fin-test-with-db
    (fin-bankdb-ensure)
    (fin-test-insert-entry "2026-10-03" "out" "food" 1250 "padaria")
    (fin-test-insert-entry "2026-10-03" "out" "food" 1250 "padaria")
    (fin-bankdb-record-fix-adds '(("nu:g1" "2026-10-03" "out" "food" "padaria" 1250)
                                  ("nu:g2" "2026-10-03" "out" "food" "padaria" 1250)))
    (let ((g (fin-bank-fix--vanished)))
      (should (= 2 (length (delete-dups (mapcar (lambda (a) (car (cadr a))) (cdr g))))))
      (should (equal '((:delete ("2026-10-03" "out" "food" "padaria" 1250))
                       (:delete ("2026-10-03" "out" "food" "padaria" 1250)))
                     (fin-bank-fix--changes (cdr g)))))))

(ert-deftest bankfix/installment-continues-its-series ()
  (fin-test-with-db
    (fin-test-insert-entry "2026-09-06" "out" "car" 5090 "bateria" 6 10)
    (fin-test-insert-entry "2026-09-06" "out" "food" 5090 "other thing")
    (fin-test-insert-entry "2026-09-07" "out" "health" 2990 "otica sol" 4 10)
    (fin-test-insert-entry "2026-09-06" "out" "house" 1500 "sofa" 2 3)
    (fin-bankdb-insert '(("p6" "t" "card" "2026-09-06" "out" 5090 "Propig *Center Car Com - Parcela 6/10")
                         ("s2" "t" "card" "2026-09-06" "out" 1500 "Loja Movel - Parcela 2/3")))
    (let ((acts (fin-bank-fix--continue-series
                 '((:add ("2026-10-06" "out" "free" "propig" 5090 7 10) "Propig *Center Car Com - Parcela 7/10" "p7")
                   (:add ("2026-10-06" "out" "free" "otica" 2990 5 10) "Otica Sol - Parcela 5/10")
                   (:add ("2026-10-06" "out" "free" "farmacia" 1500 3 3) "Farmacia Lua - Parcela 3/3")
                   (:add ("2026-10-06" "out" "free" "ifood" 5952 nil nil) "iFood")))))
      (should (equal '(:add ("2026-10-06" "out" "car" "bateria" 5090 7 10)
                            "Propig *Center Car Com - Parcela 7/10" "p7")
                     (nth 0 acts)))
      (should (equal "health" (nth 2 (cadr (nth 1 acts)))))
      (should (equal "free" (nth 2 (cadr (nth 2 acts)))))
      (should (equal "ifood" (nth 3 (cadr (nth 3 acts))))))))

(ert-deftest bankfix/prepaid-installments-follow-the-first-resolved-one ()
  (fin-test-with-db
    (fin-test-insert-entry "2026-10-06" "out" "patrimony" 3599 "processador oster" 5 10)
    (fin-bankdb-insert '(("n5" "t" "card" "2026-10-06" "out" 3599 "Newell Brands Brasil - Parcela 5/10")))
    (let ((acts (fin-bank-fix--continue-series
                 '((:add ("2026-11-05" "out" "free" "newell brands brasil" 3599 8 10) "Newell Brands Brasil - Parcela 8/10")
                   (:add ("2026-11-05" "out" "free" "newell brands brasil" 3599 6 10) "Newell Brands Brasil - Parcela 6/10")
                   (:add ("2026-11-05" "out" "free" "newell brands brasil" 3153 10 10) "Newell Brands Brasil - Parcela 10/10")
                   (:add ("2026-11-05" "out" "free" "loja" 3599 7 10) "Loja - Parcela 7/10")))))
      (should (equal '("patrimony" "patrimony" "patrimony" "free")
                     (mapcar (lambda (a) (nth 2 (cadr a))) acts)))
      (should (equal "processador oster" (nth 3 (cadr (nth 2 acts))))))))

(ert-deftest bankfix/installments-follow-a-matched-installment-of-the-purchase ()
  (fin-test-with-db
    (let* ((pairs (list (cons '("b8" "2026-11-05" "out" 5090 "Propig *Center Car Com - Parcela 8/10")
                              '(13 "2026-11-05" "out" "car" "bateria" 5090 "8/10"))
                        (cons '("b0" "2026-11-05" "out" 900 "Padaria")
                              '(14 "2026-11-05" "out" "food" "padaria" 900 nil))))
           (acts (fin-bank-fix--continue-series
                  '((:add ("2026-11-05" "out" "free" "propig" 5090 9 10) "Propig *Center Car Com - Parcela 9/10")
                    (:add ("2026-11-05" "out" "free" "propig" 4837 10 10) "Propig *Center Car Com - Parcela 10/10"))
                  pairs)))
      (should (equal '(("car" "bateria") ("car" "bateria"))
                     (mapcar (lambda (a) (list (nth 2 (cadr a)) (nth 3 (cadr a)))) acts))))))

(ert-deftest bankfix/changes-take-adds-carrying-a-bank-id ()
  (should (equal '((:add ("2025-05-06" "out" "food" "x" 100 nil nil)))
                 (fin-bank-fix--changes (list (list :add '("2025-05-06" "out" "food" "x" 100 nil nil) "X" "id1"))))))

(ert-deftest bankfix/room-reports-keep-the-bank-id-for-flags ()
  (let* ((fin-bank-fix-flag-min 100) (fin-bank-fix-flag-ignore-regexps nil)
         (day (format-time-string "%Y-%m-%d"))
         (acts (fin-conv--within-room
                (list (list :add (list day "out" "free" "loja" 500 1 2) "Loja - Parcela 1/2" "nu:a:1"))
                (list (list (substring day 0 7) 100 0)))))
    (should (equal (list "nu:a:1" day "out" 500 "Loja - Parcela 1/2") (cadr (car acts))))
    (should (equal "nu:a:1" (caar (fin-bank-fix--flags acts))))))

(ert-deftest bankfix/pj-split-books-up-to-the-last-statement-day ()
  (fin-test-with-db
    (let ((fin-pj-income-payers '(("Brasil Paralelo" . "bp")))
          (fin-pj-tax-items '(("\\bDAS\\b" . "das")))
          (fin-conv-salary-regexps '("\\`Salario"))
          (bank (list (list "pj:a" "2026-11-05" "in" 1300000 "Pix - Brasil Paralelo")
                      (list "pj:b" "2026-11-05" "out" 78000 "Pagamento de boleto efetuado - DAS")
                      (list "nu:c" "2026-11-05" "in" 910000 "Salario"))))
      (fin-bankdb-insert (list (list "pj:a" "nubank-ofx" "pj" "2026-11-05" "in" 1300000 "Pix - Brasil Paralelo")
                               (list "pj:b" "nubank-ofx" "pj" "2026-11-05" "out" 78000 "Pagamento de boleto efetuado - DAS")
                               (list "pj:z" "nubank-ofx" "pj" "2026-10-23" "out" 100 "Tarifa")))
      (fin-bankdb-record-balance "pj" "2026-11-06" 0)
      (let* ((later '(9 "2026-11-20" "out" "cnpj" "das" 45000 nil))
             (split (fin-bank-fix--pj-split bank (list later) "2026-10-01")))
        (should-not (plist-get split :bank))
        (should-not (plist-get split :raw))
        (should (equal '((:add ("2026-11-05" "in" "bp" nil 1300000 nil nil) "Pix - Brasil Paralelo")
                         (:add ("2026-11-05" "out" "cnpj" "das" 78000 nil nil) "Pagamento de boleto efetuado - DAS"))
                       (plist-get split :pj)))))))

(ert-deftest bankfix/settle-refreshes-until-a-pass-changes-nothing ()
  (let* ((results (list '(a b) '(c) nil))
         (fixes 0) (refreshes 0))
    (cl-letf (((symbol-function 'fin-bank-fix)
               (lambda (&rest _) (cl-incf fixes) (pop results))))
      (fin-bank-fix-settle (lambda () (cl-incf refreshes))))
    (should (= 3 fixes))
    (should (= 3 refreshes))))

(ert-deftest bankfix/settle-fails-loud-when-the-ledger-keeps-changing ()
  (let ((refreshes 0))
    (cl-letf (((symbol-function 'fin-bank-fix) (lambda (&rest _) '(a))))
      (should-error (fin-bank-fix-settle (lambda () (cl-incf refreshes)))))
    (should (= fin-bank-fix-passes refreshes)))
  (should-error (fin-bank-fix-settle nil)))

(provide 'test-bankfix)
;;; test-bankfix.el ends here
