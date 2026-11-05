;;; config.el --- Personal settings, copy to infra/config.el  -*- lexical-binding: t; -*-

(setq fin-bank-own-regexps '("YOUR NAME" "Your Company"))

(setq fin-bank-ignore-regexps
      '("\\`Pagamento de fatura\\'"
        "\\`Pagamento recebido\\'"))

(setq fin-conv-salary-regexps '("\\`Transferência Recebida - YOUR NAME"))

(setq fin-conv-aliases '(("landlord" . "rent")))

(setq fin-accounts-derived
      '((:name "savings" :reserve t
         :into ("\\`Aplicação") :out-of ("\\`Resgate"))
        (:name "checking" :balance "account")))

(setq fin-health-todo nil)

;;; config.el ends here
