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

(ert-deftest bankfix/installment-continues-its-series ()
  (fin-test-with-db
    (fin-test-insert-entry "2026-09-06" "out" "car" 5090 "bateria" 6 10)
    (fin-test-insert-entry "2026-09-06" "out" "food" 5090 "other thing")       ; no installment
    (fin-test-insert-entry "2026-09-07" "out" "health" 2990 "otica sol" 4 10)
    (fin-test-insert-entry "2026-09-06" "out" "house" 1500 "sofa" 2 3)       ; another merchant's
    (fin-bankdb-insert '(("p6" "t" "card" "2026-09-06" "out" 5090 "Propig *Center Car Com - Parcela 6/10")
                         ("s2" "t" "card" "2026-09-06" "out" 1500 "Loja Movel - Parcela 2/3")))
    (let ((acts (fin-bank-fix--continue-series
                 '((:add ("2026-10-06" "out" "free" "propig" 5090 7 10) "Propig *Center Car Com - Parcela 7/10" "p7")
                   (:add ("2026-10-06" "out" "free" "otica" 2990 5 10) "Otica Sol - Parcela 5/10")
                   (:add ("2026-10-06" "out" "free" "farmacia" 1500 3 3) "Farmacia Lua - Parcela 3/3")
                   (:add ("2026-10-06" "out" "free" "ifood" 5952 nil nil) "iFood")))))
      ;; Booked under another name: the bank's 6/10 of the merchant beside it tells.
      (should (equal '(:add ("2026-10-06" "out" "car" "bateria" 5090 7 10)
                            "Propig *Center Car Com - Parcela 7/10" "p7")
                     (nth 0 acts)))
      (should (equal "health" (nth 2 (cadr (nth 1 acts)))))      ; item names the payee
      (should (equal "free" (nth 2 (cadr (nth 2 acts)))))        ; 2/3 is another merchant's
      (should (equal "ifood" (nth 3 (cadr (nth 3 acts))))))))

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

(ert-deftest bankfix/pj-months-start-at-the-fix-start ()
  (should (equal '("2024-01" "2024-02") (fin-bank-fix--pj-months '("2021-10-05" . "2024-03-10") "2024-01-01")))
  (should (equal '("2024-02") (fin-bank-fix--pj-months '("2021-10-05" . "2024-03-10") "2024-01-15")))
  (should (equal '("2022-11") (fin-bank-fix--pj-months '("2022-10-05" . "2022-12-10") "2022-01-01")))
  (should-not (fin-bank-fix--pj-months '("2021-10-05" . "2023-12-31") "2024-01-01")))

(provide 'test-bankfix)
;;; test-bankfix.el ends here
