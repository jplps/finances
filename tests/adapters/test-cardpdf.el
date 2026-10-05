;;; test-cardpdf.el --- adapters/cardpdf.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'cardpdf)

(defun test-cardpdf--text (&optional total)
  "Visual rows of a small bill crossing a year, its `Total de compras' TOTAL.
The balanced total is 1.574,45 = 64,90 + 1.509,55: the payment is skipped,
and the credit, the top-up and the IOF sit outside it."
  (concat
   "FATURA 13 JAN 2021 EMISSÃO E ENVIO 06 JAN 2021\n"
   "Total de compras, 06 DEZ a 06 JAN\t" (or total "1.574,45") "\n"
   "TRANSAÇÕES\tDE 06 DEZ A 06 JAN\tVALORES EM R$\n"
   "07 DEZ\tPizzaria Don Dani\t64,90\n"
   "10 DEZ\tPagamento em 10 DEZ\t96,53\n"
   "14 DEZ\tReferencia Comercio - 2/10\t1.509,55\n"
   "15 DEZ\tEstorno de \"Pizzaria Don Dani\"\t37,85\n"
   "20 DEZ\tRecarga de celular\t12,00\n"
   "02 JAN\tIOF de \"Im* Keychron.Com\"\t3,00\n"
   "1 de 6\n"))

(ert-deftest cardpdf/parses-rows-across-the-year ()
  (let ((p (fin-cardpdf-parse (test-cardpdf--text))))
    (should (equal "2021-01-13" (plist-get p :due)))
    (should (= 157445 (plist-get p :total)))
    (should (equal '(("2020-12-07" -6490 "Pizzaria Don Dani")
                     ("2020-12-14" -150955 "Referencia Comercio - Parcela 2/10")
                     ("2020-12-15" 3785 "Estorno de \"Pizzaria Don Dani\"")
                     ("2020-12-20" -1200 "Recarga de celular")
                     ("2021-01-02" -300 "IOF de \"Im* Keychron.Com\""))
                   (plist-get p :txns)))
    (should-not (plist-get p :financing))))

(ert-deftest cardpdf/fails-loud-when-unbalanced ()
  (should-error (fin-cardpdf-parse (test-cardpdf--text "1.500,00"))))

(ert-deftest cardpdf/keeps-bill-financing-apart ()
  (let* ((text (concat "VENCIMENTO 13 DEZ 2020\n"
                       "Total de compras, 06 NOV a 06 DEZ\t676,92\n"
                       "TRANSAÇÕES\tDE 06 NOV A 06 DEZ\tVALORES EM R$\n"
                       "13 NOV\tCrédito de parcelamento\t650,65\n"
                       "13 NOV\tParcelamento de Fatura\t666,92\n"
                       "20 NOV\tPadaria\t10,00\n"))
         (p (fin-cardpdf-parse text)))
    (should (equal '(("2020-11-20" -1000 "Padaria")) (plist-get p :txns)))
    (should (= 2 (length (plist-get p :financing))))))

(ert-deftest cardpdf/amounts ()
  (should (= 150955 (fin-cardpdf--cents "1.509,55")))
  (should (= -500 (fin-cardpdf--cents "-5,00")))
  (should-error (fin-cardpdf--cents "1509.55")))

(provide 'test-cardpdf)
;;; test-cardpdf.el ends here
