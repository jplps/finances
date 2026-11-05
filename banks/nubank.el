;;; nubank.el --- Nubank statement wording  -*- lexical-binding: t; -*-

(setq fin-reconcile-installment-regexp "Parcela \\([0-9]+\\)/\\([0-9]+\\)")

(setq fin-bank-ignore-regexps
      '("\\`Pagamento de fatura\\'"
        "\\`Pagamento da fatura"
        "\\`Pagamento recebido\\'"
        "\\`Valor adicionado na conta por cartão"
        "\\`Valor enviado como crédito na fatura"
        "\\`Estorno de pagamento"
        "\\`Ajuste a crédito" "\\`Encerramento de dívida"
        "\\`Depósito de Confiança" "\\`Reversão do Crédito de Confiança"))

(setq fin-bank-bill-paid-regexp "\\`Pagamento d[ae] fatura")
(setq fin-bank-bill-received-regexp "\\`Pagamento recebido\\'")

(setq fin-conv-refund-re
      "\\`\\(Estorno\\|Reembolso recebido\\|Devolução\\|Crédito de Confiança\\|Transferência devolvida\\)")

(setq fin-conv-savings-regexps
      '("\\`Aplicação RDB" "\\`Resgate RDB" "\\`Aplicação em investimento"
        "\\`Compra de criptomoedas" "\\`Venda de criptomoedas"
        "\\`Compra de ETF" "\\`Venda de ETF" "\\`Devolução - Aplicação"
        "\\`Dinheiro guardado" "\\`Dinheiro resgatado"))

(setq fin-conv-stopwords
      '("compra" "debito" "via" "nupay" "transferencia" "enviada" "recebida" "pelo"
        "pix" "credito" "conta" "ltda" "brasil" "com" "supermercados" "pagamento"
        "boleto" "efetuado" "sao" "jose" "comercio" "servicos" "produtos" "loja"
        "store" "instituicao" "pagamentos" "tecnologia" "super" "mercado" "parcela"
        "dos" "das"))

(setq fin-conv-prefix-regexps
      '("Compra no débito via NuPay" "Compra no débito"
        "Transferência enviada pelo Pix" "Transferência enviada" "Pix no Crédito"
        "Pagamento de boleto efetuado" "Débito em conta" "Antecipada"))

(setq fin-conv-reversal-regexp "\\`Reversão do Crédito de Confiança")
(setq fin-conv-dispute-credit-regexp "\\`\\(Crédito\\|Depósito\\) de Confiança")

(setq fin-conv-fee-rules
      '(("\\`IOF de \"\\([^\"]+\\)\"" nil)
        ("\\`IOF de compra internacional" t)
        ("\\`Desconto Antecipação \\(.+\\)" t)))

(setq fin-conv-pass-through '("\\`Valor adicionado na conta por cartão" . "Pix"))

(setq fin-conv-transfer-regexp "Transferência")

;;; nubank.el ends here
