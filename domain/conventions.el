;;; conventions.el --- How bank rows become ledger fixes  -*- lexical-binding: t; -*-

;; Pure functions over row lists; no DB or ODS access.  They encode how the
;; ledger books bank activity:
;;
;; - A refund cancels the purchase it refunds; a refunded purchase already
;;   in the ledger is deleted.
;; - IOF and installment discounts fold into their purchase.
;; - A Pix funded by "Pix no Crédito" counts once, on the card.
;; - Salary is booked gross under `bp' with the tax under `cnpj'; a deposit
;;   that differs from `bp' − `cnpj' is reported, never fixed: the tax
;;   booked is a fact the bank cannot tell.
;; - Savings moves are transfers between own accounts: the ledger books them
;;   per goal (`investments', `retirement', `reserve'), never from the bank.
;; - Money received from people is booked as income under
;;   `fin-conv-inflow-category'.
;; - A month never gets more outflows added than the bank spent beyond what
;;   the ledger already holds; otherwise its bank-only rows are reported.
;; - Anything else is added under the category its words suggest from the
;;   ledger's history, else `fin-conv-default-category'.
;;
;; Bank row:   (id date type amount description)
;; Ledger row: (id date type category item amount "k/n"-or-nil)

(require 'cl-lib)
(require 'reconcile)

(defcustom fin-conv-refund-re
  "\\`\\(Estorno\\|Reembolso recebido\\|Devolução\\|Crédito de Confiança\\|Transferência devolvida\\)"
  "Bank descriptions of money returned for a purchase."
  :type 'regexp :group 'fin)

(defcustom fin-conv-refund-days 45
  "Max days between a purchase and its refund."
  :type 'natnum :group 'fin)

(defcustom fin-conv-savings-regexps
  '("\\`Aplicação RDB" "\\`Resgate RDB" "\\`Aplicação em investimento"
    "\\`Compra de criptomoedas" "\\`Venda de criptomoedas"
    "\\`Compra de ETF" "\\`Venda de ETF" "\\`Devolução - Aplicação"
    "\\`Dinheiro guardado" "\\`Dinheiro resgatado")
  "Bank descriptions of moves between the account and savings."
  :type '(repeat regexp) :group 'fin)

(defcustom fin-conv-salary-regexps nil
  "Bank descriptions of salary deposits, checked against `bp' − `cnpj'."
  :type '(repeat regexp) :group 'fin)



(defcustom fin-conv-aliases nil
  "Alist (BANK-WORD . LEDGER-WORD) relating payee names to ledger items,
e.g. (\"veronica\" . \"aluguel\").  Words are lowercase without accents."
  :type '(alist :key-type string :value-type string) :group 'fin)

(defcustom fin-conv-item-aliases '(("\\`Uber" . "uber"))
  "Bank descriptions -> ledger item: list of (REGEXP . ITEM), first match
wins.  For payees whose memos vary (`Uberbr Uber * Pending', `Uber *Trip')."
  :type '(alist :key-type regexp :value-type string) :group 'fin)

(defcustom fin-conv-default-category "free"
  "Category for purchases the history does not suggest."
  :type 'string :group 'fin)

(defcustom fin-conv-inflow-category "extras"
  "Category for money received from people."
  :type 'string :group 'fin)

(defconst fin-conv--stopwords
  '("compra" "debito" "via" "nupay" "transferencia" "enviada" "recebida" "pelo"
    "pix" "credito" "conta" "ltda" "brasil" "com" "supermercados" "pagamento"
    "boleto" "efetuado" "sao" "jose" "comercio" "servicos" "produtos" "loja"
    "store" "instituicao" "pagamentos" "tecnologia" "super" "mercado" "parcela")
  "Words too common in bank descriptions to relate rows.")

(defconst fin-conv--prefix-re
  (concat "\\`\\(Compra no débito via NuPay\\|Compra no débito"
          "\\|Transferência enviada pelo Pix\\|Transferência enviada\\|Pix no Crédito"
          "\\|Pagamento de boleto efetuado\\|Débito em conta\\|Antecipada\\)\\s-*-?\\s-*")
  "Bank boilerplate in front of the payee name.")

;;; ── Words and categories ───────────────────────────────────

(defun fin-conv--money (cents)
  "CENTS as a decimal string."
  (format "%s%d.%02d" (if (< cents 0) "-" "") (/ (abs cents) 100) (% (abs cents) 100)))

(defun fin-conv--bank-words (desc)
  "Meaningful words of the payee in DESC, mapped through `fin-conv-aliases'.
Only the payee counts: the rest names banks and agencies."
  (mapcar (lambda (w) (or (cdr (assoc w fin-conv-aliases)) w))
          (cl-set-difference (fin-reconcile--words (fin-conv-item desc)) fin-conv--stopwords
                             :test #'string=)))

(defun fin-conv--word-hit-p (bank-words item-words)
  "Non-nil if a bank word equals an item word, or one prefixes the other
with at least 5 chars."
  (cl-some (lambda (a)
             (cl-some (lambda (b)
                        (or (string= a b)
                            (and (>= (min (length a) (length b)) 5)
                                 (or (string-prefix-p a b) (string-prefix-p b a)))))
                      item-words))
           bank-words))

(defun fin-conv--item-hit-p (desc bank-words item)
  "Non-nil if ledger ITEM names the payee of DESC."
  (let ((iw (cl-set-difference (fin-reconcile--words item) fin-conv--stopwords
                               :test #'string=)))
    (and iw
         (or (fin-conv--word-hit-p bank-words iw)
             (let ((joined (apply #'concat iw)))
               (and (>= (length joined) 6)
                    (string-match-p (regexp-quote joined)
                                    (apply #'concat (fin-reconcile--words (fin-conv-item desc))))))))))

(defun fin-conv-suggest (desc history)
  "(CATEGORY . ITEM) most often booked for the payee of DESC, or nil.
HISTORY is a list of (category item count)."
  (let ((bw (fin-conv--bank-words desc)) best best-n)
    (dolist (h history)
      (pcase-let ((`(,cat ,item ,n) h))
        (when (and item (fin-conv--item-hit-p desc bw item)
                   (or (null best-n) (> n best-n)))
          (setq best (cons cat item) best-n n))))
    best))

(defun fin-conv-related-fn (history)
  "Predicate on (bank-row ledger-row): the ledger row books that payee.
Shared word, alias, or the category the history suggests for the payee."
  (lambda (b l)
    (let ((bw (fin-conv--bank-words (nth 4 b))))
      (or (and (nth 4 l) (fin-conv--item-hit-p (nth 4 b) bw (nth 4 l)))
          (and (member "ifood" bw) (equal (nth 3 l) "food"))
          (equal (car (fin-conv-suggest (nth 4 b) history)) (nth 3 l))))))

(defun fin-conv-item (desc)
  "Ledger item for bank DESC: its `fin-conv-item-aliases' item, else the
payee name, lowercase."
  (or (cdr (cl-find-if (lambda (a) (let ((case-fold-search nil)) (string-match-p (car a) desc)))
                       fin-conv-item-aliases))
      (let* ((d (replace-regexp-in-string "\\s-*-\\s-*Parcela [0-9]+/[0-9]+" "" desc))
             (d (replace-regexp-in-string fin-conv--prefix-re "" d))
             (d (car (split-string d " - ")))
             (d (replace-regexp-in-string "\\`[A-Za-z]\\{1,4\\}\\s-*\\*\\s-*" "" d))
             (d (replace-regexp-in-string "\\*.*\\'" "" d)))
        (downcase (string-trim d "[ \".]+" "[ \".]+")))))

(defun fin-conv--payer (desc)
  "First name of the person in a received-transfer DESC."
  (let ((who (cadr (split-string desc " - "))))
    (downcase (car (split-string (or who (fin-conv-item desc)))))))

(defun fin-conv-installment (desc)
  "(K N) of the installment named in bank DESC, or (nil nil)."
  (if (string-match "Parcela \\([0-9]+\\)/\\([0-9]+\\)" desc)
      (list (string-to-number (match-string 1 desc)) (string-to-number (match-string 2 desc)))
    (list nil nil)))

;;; ── Normalize: before matching ─────────────────────────────

(defun fin-conv--match-p (re row) (string-match-p re (nth 4 row)))

(defun fin-conv--savings-p (row)
  (cl-some (lambda (re) (fin-conv--match-p re row)) fin-conv-savings-regexps))

(defun fin-conv--days (a b)
  "Days from bank row A to bank row B."
  (- (fin-reconcile--day (nth 1 b)) (fin-reconcile--day (nth 1 a))))

(defun fin-conv--refund-p (row)
  (and (equal (nth 2 row) "in") (fin-conv--match-p fin-conv-refund-re row)
       (not (fin-conv--savings-p row))))

(defun fin-conv--refunded-purchase (refund rows gone)
  "Out row of ROWS, not in GONE, with REFUND's amount up to
`fin-conv-refund-days' before it."
  (cl-find-if (lambda (r)
                (and (not (gethash (car r) gone)) (equal (nth 2 r) "out")
                     (= (nth 3 r) (nth 3 refund)) (not (fin-conv--savings-p r))
                     (<= 0 (fin-conv--days r refund) fin-conv-refund-days)))
              rows))

(defun fin-conv--fee-p (row)
  (string-match-p "\\`\\(IOF de \\|Desconto Antecipação \\)" (nth 4 row)))

(defun fin-conv--merchant-key (desc)
  "First 6 lowercase chars of the merchant in DESC."
  (let ((m (downcase (fin-conv-item desc))))
    (substring m 0 (min 6 (length m)))))

(defun fin-conv--host (fee rows gone same-day name)
  "Out row of ROWS that FEE (IOF or discount) belongs to: same day, or same
month when SAME-DAY is nil, and merchant NAME.  Without NAME the host must
be the only candidate that day."
  (let ((hits (cl-remove-if-not
               (lambda (r)
                 (and (not (gethash (car r) gone)) (not (eq r fee))
                      (equal (nth 2 r) "out")
                      (not (fin-conv--fee-p r))
                      (if same-day (equal (nth 1 r) (nth 1 fee))
                        (equal (substring (nth 1 r) 0 7) (substring (nth 1 fee) 0 7)))
                      (or (null name) (string-prefix-p name (fin-conv--merchant-key (nth 4 r))))))
               rows)))
    (if name (car hits) (and (= (length hits) 1) (car hits)))))

(defun fin-conv--fitid (row)
  "FITID inside bank ROW's id (`nu:FITID:hash'), or nil."
  (let ((parts (split-string (car row) ":")))
    (and (= (length parts) 3) (equal (car parts) "nu") (nth 1 parts))))

(defun fin-conv--same-fitid-host (fee rows gone)
  "The one other out row of ROWS sharing FEE's FITID: Nubank gives a
purchase's IOF the purchase's FITID."
  (let* ((id (fin-conv--fitid fee))
         (hits (and id (cl-remove-if-not
                        (lambda (r) (and (not (eq r fee)) (not (gethash (car r) gone))
                                         (equal (nth 2 r) "out") (not (fin-conv--fee-p r))
                                         (equal (fin-conv--fitid r) id)))
                        rows))))
    (and (= (length hits) 1) (car hits))))

(defun fin-conv--fee-host (fee rows gone)
  "Purchase of ROWS that FEE folds into, or nil."
  (let ((desc (nth 4 fee)))
    (cond
     ((string-match "\\`IOF de \"\\([^\"]+\\)\"" desc)
      (fin-conv--host fee rows gone nil (fin-conv--merchant-key (match-string 1 desc))))
     ((string-prefix-p "IOF de compra internacional" desc)
      (or (fin-conv--same-fitid-host fee rows gone)
          (fin-conv--host fee rows gone t nil)))
     ((string-match "\\`Desconto Antecipação \\(.+\\)" desc)
      (fin-conv--host fee rows gone t (fin-conv--merchant-key (match-string 1 desc)))))))

(defun fin-conv-normalize (bank)
  "Fold and cancel BANK rows the ledger never books on their own.
Return plist :rows (remaining rows, folded amounts applied) and :notes,
a list of (row . reason) for every row removed."
  (let ((gone (make-hash-table :test #'equal))
        (delta (make-hash-table :test #'equal))
        notes)
    (cl-flet ((drop (r why) (puthash (car r) t gone) (push (cons r why) notes)))
      ;; Pix paid with "Pix no Crédito": the card side counts, the account pass-through not.
      (dolist (f bank)
        (when (fin-conv--match-p "\\`Valor adicionado na conta por cartão" f)
          (let ((p (cl-find-if (lambda (r) (and (not (gethash (car r) gone))
                                                (equal (nth 2 r) "out") (equal (nth 1 r) (nth 1 f))
                                                (= (nth 3 r) (nth 3 f))
                                                (fin-conv--match-p "Pix" r)))
                               bank)))
            (when p (drop p "pass-through: paid via Pix no Crédito")))))
      ;; Refund and refunded purchase both in the statement: cancel out.
      (dolist (r bank)
        (when (and (fin-conv--refund-p r) (not (gethash (car r) gone)))
          (let ((p (fin-conv--refunded-purchase r bank gone)))
            (when p (drop r "refund") (drop p "refunded purchase")))))
      ;; Partial refund: reduces the payee's larger purchase before it.
      (dolist (r bank)
        (when (and (fin-conv--refund-p r) (not (gethash (car r) gone)))
          (let* ((payee (cl-set-difference (fin-reconcile--words (nth 4 r)) fin-conv--stopwords
                                           :test #'string=))
                 (p (cl-find-if (lambda (x)
                                  (and (not (gethash (car x) gone)) (equal (nth 2 x) "out")
                                       (> (+ (nth 3 x) (gethash (car x) delta 0)) (nth 3 r))
                                       (<= 0 (fin-conv--days x r) fin-conv-refund-days)
                                       (fin-conv--word-hit-p payee (fin-conv--bank-words (nth 4 x)))))
                                bank)))
            (when p
              (puthash (car p) (- (gethash (car p) delta 0) (nth 3 r)) delta)
              (drop r "partial refund, netted into purchase")))))
      ;; IOF adds to, discounts subtract from, their purchase.
      (dolist (f bank)
        (when (and (fin-conv--fee-p f) (not (gethash (car f) gone)))
          (let ((host (fin-conv--fee-host f bank gone))
                (sign (if (equal (nth 2 f) "in") -1 1)))
            (cond (host (puthash (car host) (+ (gethash (car host) delta 0) (* sign (nth 3 f))) delta)
                        (drop f "folded into purchase"))
                  ((equal (nth 2 f) "in") (drop f "discount already netted in ledger")))))))
    (list :rows (delq nil (mapcar (lambda (r)
                                    (unless (gethash (car r) gone)
                                      (let ((d (gethash (car r) delta 0)))
                                        (if (zerop d) r
                                          (append (seq-take r 3) (list (+ (nth 3 r) d)) (nthcdr 4 r))))))
                                  bank))
          :notes (nreverse notes))))

;;; ── Plan: after matching ───────────────────────────────────

(defun fin-conv--combined (bank-only entry-only)
  "Pairs of BANK-ONLY rows booked as one ENTRY-ONLY row: same type, within
`fin-reconcile-window' days, amounts summing to it, payee named by its item.
Return (BOOKED-BANK-ROWS . USED-LEDGER-ROWS)."
  (let (booked used)
    (dolist (l entry-only)
      (when (and (integerp (car l)) (nth 4 l))
        (let ((cands (cl-remove-if-not
                      (lambda (b) (and (not (memq b booked)) (equal (nth 2 b) (nth 2 l))
                                       (<= (abs (- (fin-reconcile--day (nth 1 b)) (fin-reconcile--day (nth 1 l))))
                                           fin-reconcile-window)
                                       (fin-conv--item-hit-p (nth 4 b) (fin-conv--bank-words (nth 4 b)) (nth 4 l))))
                      bank-only)))
          (cl-block found
            (cl-loop for (a . rest) on cands do
                     (dolist (c rest)
                       (when (= (+ (nth 3 a) (nth 3 c)) (nth 5 l))
                         (push a booked) (push c booked) (push l used)
                         (cl-return-from found))))))))
    (cons booked used)))


(defun fin-conv--salary-p (row)
  (cl-some (lambda (re) (fin-conv--match-p re row)) fin-conv-salary-regexps))

(defun fin-conv--net-row-p (l)
  "Non-nil for the synthetic `bp' − `cnpj' row of `fin-reconcile-net'."
  (and (consp (car l)) (eq (caar l) 'net)))

(defun fin-conv--nearest-net (b nets)
  "Net row of NETS closest to deposit B within `fin-reconcile-shift-window'."
  (let ((day (fin-reconcile--day (nth 1 b))) best best-d)
    (dolist (n nets)
      (let ((d (abs (- day (fin-reconcile--day (nth 1 n))))))
        (when (and (<= d fin-reconcile-shift-window) (or (null best-d) (< d best-d)))
          (setq best n best-d d))))
    best))

(defun fin-conv--salary-report (net bank-amount)
  "Report that the deposit BANK-AMOUNT differs from the ledger NET row."
  (list :report net (format "salary received %s, ledger bp − cnpj %s"
                            (fin-conv--money bank-amount) (fin-conv--money (nth 5 net)))))

(defun fin-conv--names-payee-p (b l)
  "Non-nil if ledger row L's item names the payee of bank row B."
  (and (nth 4 l) (fin-conv--item-hit-p (nth 4 b) (fin-conv--bank-words (nth 4 b)) (nth 4 l))))

(defun fin-conv--pair-actions (near shifted)
  "Edits setting the bank amount on NEAR and SHIFTED pairs whose ledger item
names the payee; salary gaps are reported."
  (let (acts)
    (dolist (p near)
      (let ((b (car p)) (l (cdr p)))
        (cond ((fin-conv--net-row-p l) (push (fin-conv--salary-report l (nth 3 b)) acts))
              ((fin-conv--names-payee-p b l)
               (push (list :edit l (nth 3 b) (concat "bank: " (nth 4 b))) acts)))))
    (dolist (p shifted)
      (let ((b (car p)) (l (cdr p)))
        (when (and (/= (nth 3 b) (nth 5 l)) (not (fin-conv--net-row-p l)) (fin-conv--names-payee-p b l))
          (push (list :edit l (nth 3 b) (concat "bank: " (nth 4 b))) acts))))
    (nreverse acts)))

(defun fin-conv--refund-action (refund pairs)
  "Delete the ledger row of the purchase REFUND returns, found in PAIRS."
  (let ((p (cl-find-if (lambda (p)
                         (let ((b (car p)))
                           (and (equal (nth 2 b) "out") (= (nth 3 b) (nth 3 refund))
                                (integerp (car (cdr p)))
                                (<= 0 (fin-conv--days b refund) fin-conv-refund-days))))
                       pairs)))
    (if p (list :delete (cdr p) (concat "refunded: " (nth 4 refund)))
      (list :report refund "refund with no purchase found"))))

;;; ── Plan: adds within the month's room ─────────────────────

(defun fin-conv--same-payee-p (desc item)
  "Non-nil if every word of ledger ITEM names the payee of bank DESC: equal
to a bank word, or the bank word, truncated, prefixes it (5+ chars).
A single shared word (`uber' in `uber sk8') only suggests a category."
  (let ((bw (fin-conv--bank-words desc))
        (iw (cl-set-difference (fin-reconcile--words item) fin-conv--stopwords :test #'string=)))
    (and iw (cl-every (lambda (w)
                        (cl-some (lambda (b) (or (string= b w)
                                                 (and (>= (length b) 5) (string-prefix-p b w))))
                                 bw))
                      iw))))

(defun fin-conv--add-action (b history)
  "Add out row B under its suggested category, else the default.  The item
is the suggested one when it names the same payee, else the bank payee."
  (let* ((desc (nth 4 b))
         (s (fin-conv-suggest desc history)))
    (list :add (append (list (nth 1 b) "out" (or (car s) fin-conv-default-category)
                             (if (and s (fin-conv--same-payee-p desc (cdr s))) (cdr s) (fin-conv-item desc))
                             (nth 3 b))
                       (fin-conv-installment desc))
          desc)))

(defun fin-conv--inflow-action (b)
  "Add money received B as income from its payer."
  (list :add (list (nth 1 b) "in" fin-conv-inflow-category
                   (if (string-match-p "Transferência" (nth 4 b))
                       (fin-conv--payer (nth 4 b))
                     (fin-conv-item (nth 4 b)))
                   (nth 3 b) nil nil)
        (nth 4 b)))

(defun fin-conv--within-room (adds months)
  "ADDS kept for months whose room holds them all; the others reported.
MONTHS is a list of (YYYY-MM bank-out ledger-out): room is their gap."
  (let ((sums (make-hash-table :test #'equal)))
    (dolist (a adds)
      (let ((ym (substring (car (cadr a)) 0 7)))
        (puthash ym (+ (gethash ym sums 0) (nth 4 (cadr a))) sums)))
    (mapcar
     (lambda (a)
       (pcase-let* ((fields (cadr a))
                    (`(,ym ,bank ,ledger) (or (assoc (substring (car fields) 0 7) months)
                                              (list (substring (car fields) 0 7) 0 0))))
         (if (<= (+ ledger (gethash ym sums)) bank)
             a
           (list :report (list nil (car fields) "out" (nth 4 fields) (caddr a))
                 (format "%s: ledger %s + bank-only %s > bank %s"
                         ym (fin-conv--money ledger) (fin-conv--money (gethash ym sums))
                         (fin-conv--money bank))))))
     adds)))

(cl-defun fin-conv-plan (&key matched near shifted bank-only entry-only history months)
  "Actions fixing the ledger from a reconcile result.
MATCHED, NEAR, SHIFTED are (bank . ledger) pairs; BANK-ONLY and ENTRY-ONLY
leftovers; HISTORY (category item count) rows; MONTHS (YYYY-MM bank-out
ledger-out) totals bounding what a month may get added.
Each action is one of
  (:add (date type category item amount installment installments) source)
  (:edit ledger-row new-amount reason)
  (:delete ledger-row reason)
  (:report row reason)   needs a look
  (:skip row reason)     left out on purpose"
  (let* ((pairs (append matched shifted near))
         (nets (cl-remove-if-not #'fin-conv--net-row-p entry-only))
         (comb (fin-conv--combined bank-only entry-only))
         (bank-only (cl-set-difference bank-only (car comb)))
         (adds nil) (acts nil))
    (dolist (b bank-only)
      (cond
       ((fin-conv--salary-p b)
        (let ((n (fin-conv--nearest-net b nets)))
          (if (or (null n) (< (* 2 (nth 3 b)) (nth 5 n)))
              (push (list :skip b "transfer from own company, not salary") acts)
            (setq nets (delq n nets))
            (push (fin-conv--salary-report n (nth 3 b)) acts))))
       ((fin-conv--refund-p b) (push (fin-conv--refund-action b pairs) acts))
       ((fin-conv--savings-p b) (push (list :skip b "savings move, booked per goal") acts))
       ((equal (nth 2 b) "in") (push (fin-conv--inflow-action b) acts))
       (t (push (fin-conv--add-action b history) adds))))
    (append (fin-conv--pair-actions near shifted)
            (nreverse acts)
            (fin-conv--within-room (nreverse adds) months))))

(provide 'conventions)
;;; conventions.el ends here
