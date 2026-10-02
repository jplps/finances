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

(ert-deftest bankfix/adds-before-itemized-ledger-become-reports ()
  (let ((out (fin-bank-fix--lumped
              (list (list :add '("2021-05-03" "in" "extras" "nkey" 712500 nil nil) "Transferência Recebida - 4 Nkey")
                    (list :add '("2022-07-03" "out" "food" "hippo" 100 nil nil) "HIPPO")
                    (list :edit '(1 "2021-05-01" "out" "food" nil 100 nil) 90 "bank"))
              "2022-07-01")))
    (should (equal '(:report :add :edit) (mapcar #'car out)))
    (should (equal '(nil "2021-05-03" "in" 712500 "Transferência Recebida - 4 Nkey") (cadr (car out))))))

(provide 'test-bankfix)
;;; test-bankfix.el ends here
