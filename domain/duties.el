;;; duties.el --- Tax obligations of the company and the partner  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ir)

(defvar fin-bank-own-regexps)

(defcustom fin-duty-pro-labore nil
  "Pró-labore declared in eSocial: list of (YYYY-MM . CENTS), oldest first.
Each holds until the next; DCTFWeb charges `fin-ir-inss-rate' of it,
capped at the year's INSS ceiling."
  :type '(alist :key-type string :value-type integer) :group 'fin)

(defcustom fin-duty-since nil
  "First month (YYYY-MM) whose obligations are tracked; nil: none."
  :type '(choice string (const nil)) :group 'fin)

(defcustom fin-duty-known nil
  "Accepted gaps, no longer flagged: list of (YEAR LABEL).
LABEL is a filing (\"books\" \"DEFIS\" \"DIRPF\") or a declaration whose exempt
profit is off the bank's (\"DEFIS\" \"DIRPF\" \"informe\")."
  :type '(repeat (list integer string)) :group 'fin)

(defun fin-duty-known-p (year label)
  "Non-nil when (YEAR LABEL) is in `fin-duty-known'."
  (member (list year label) fin-duty-known))

(defcustom fin-duty-stale-days 30
  "Days after which the latest situação fiscal in the inbox is stale."
  :type 'integer :group 'fin)

(defconst fin-duty--month-re "\\`[0-9]\\{4\\}-[0-9]\\{2\\}\\'")

(defun fin-duty--month+ (month n)
  "YYYY-MM N months after MONTH (N may be negative)."
  (cl-assert (string-match-p fin-duty--month-re month))
  (pcase-let* ((`(,y ,m) (mapcar #'string-to-number (split-string month "-")))
               (i (+ (* 12 y) (1- m) n)))
    (format "%04d-%02d" (/ i 12) (1+ (% i 12)))))

(defun fin-duty-due (month)
  "ISO date MONTH's monthly obligations fall due: the 20th of the next month."
  (concat (fin-duty--month+ month 1) "-20"))

(defun fin-duty--window (today)
  "Months from `fin-duty-since' to the one before TODAY's."
  (when fin-duty-since
    (cl-assert (string-match-p fin-duty--month-re fin-duty-since))
    (let ((last (fin-duty--month+ (substring today 0 7) -1)))
      (cl-loop for m = fin-duty-since then (fin-duty--month+ m 1)
               while (not (string< last m)) collect m))))

(defun fin-duty-planned (month)
  "Cents of the pró-labore `fin-duty-pro-labore' plans for MONTH, or nil."
  (cdr (car (last (cl-remove-if (lambda (p) (string< month (car p))) fin-duty-pro-labore)))))

(defun fin-duty-expected-inss (month)
  "Cents of the INSS MONTH's DCTFWeb must charge, or nil when unplanned."
  (let ((pl (fin-duty-planned month)))
    (and pl (round (* fin-ir-inss-rate
                      (min pl (fin-ir-rule (string-to-number (substring month 0 4)) :inss-cap)))))))

(defun fin-duty--codes (item &optional payer)
  "Receita codes `fin-ir-receita-codes' names ITEM for PAYER (pj default)."
  (mapcar #'car (cl-remove-if-not (lambda (c) (and (equal (nth 1 c) item) (eq (nth 2 c) (or payer 'pj))))
                                  fin-ir-receita-codes)))

(defun fin-duty-mei-p (month ctx)
  "Non-nil when MONTH is up to CTX's :mei-until, the last DAS-MEI período.
A MEI owes only the DAS-MEI: no PGDAS-D, pró-labore, books or DEFIS."
  (let ((until (plist-get ctx :mei-until)))
    (and until (not (string< until month)))))

(defun fin-duty--item-paid (item shift)
  "Alist MONTH -> cents the company account paid to `fin-pj-tax-items' ITEM,
each counted SHIFT months from its payment."
  (let ((case-fold-search nil)
        (res (mapcar #'car (cl-remove-if-not (lambda (i) (equal (cdr i) item)) fin-pj-tax-items)))
        out)
    (dolist (b (fin-db-query "SELECT date, amount, description FROM bank_txn
                               WHERE account = 'pj' AND type = 'out' ORDER BY date")
               out)
      (when (cl-some (lambda (re) (string-match-p re (nth 2 b))) res)
        (setq out (fin-ir--bump out (fin-duty--month+ (substring (car b) 0 7) shift) (nth 1 b)))))))

(defun fin-duty--gps-bank ()
  "Alist MONTH -> cents of GPS the company paid, for the month before."
  (fin-duty--item-paid "gps" -1))

(defun fin-duty--gps-export ()
  "Alist MONTH -> cents of GPS in e-CAC Extrato de Contribuições exports.
Rows \"MM/YYYY;documentos;valor;imputação\"."
  (let (out)
    (dolist (f (and fin-ir-payments-dir (file-directory-p fin-ir-payments-dir)
                    (directory-files fin-ir-payments-dir t "\\`gps-.*\\.csv\\'"))
               out)
      (with-temp-buffer
        (insert-file-contents f)
        (dolist (line (split-string (buffer-string) "\r?\n" t))
          (when (string-match "\\`\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\);[0-9]+;\\([0-9.]+,[0-9]\\{2\\}\\);" line)
            (let ((month (format "%s-%s" (match-string 2 line) (match-string 1 line)))
                  (cents (fin-ir--cents (replace-regexp-in-string
                                         "," "." (replace-regexp-in-string "\\." "" (match-string 3 line))))))
              (setq out (fin-ir--bump out month cents)))))))))

(defun fin-duty-gps ()
  "Alist MONTH -> cents of INSS paid by GPS, absent from payment exports.
The e-CAC statement wins for the months it covers, else bank boletos."
  (let ((export (fin-duty--gps-export)))
    (append export (cl-remove-if (lambda (c) (assoc (car c) export)) (fin-duty--gps-bank)))))

(defun fin-duty--paid (payments codes month)
  "Cents of PAYMENTS (`fin-ir-payments') with a code in CODES for MONTH."
  (cl-loop for p in payments
           when (and (member (nth 1 p) codes) (equal (nth 3 p) month))
           sum (nth 2 p)))

(defun fin-duty--dctfweb-row (line file)
  "(MONTH SENT CENTS) of DCTFWeb CSV LINE from FILE.
SENT is the transmissão as a sortable stamp, CENTS the débito apurado."
  (pcase (split-string line ";")
    ((and `(,period ,sent ,_ ,_ ,_ ,_ ,debit . ,_)
          (guard (and (string-match "\\`\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\)\\'" period)
                      (string-match-p fin-ir--amount-re debit))))
     (let ((month (format "%s-%s" (match-string 2 period) (match-string 1 period))))
       (unless (string-match "\\`\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\)\\(.*\\)\\'" sent)
         (error "fin-duty: bad DCTFWeb transmission in %s: %S" file line))
       (list month (format "%s-%s-%s%s" (match-string 3 sent) (match-string 2 sent)
                           (match-string 1 sent) (match-string 4 sent))
             (fin-ir--cents debit))))
    (_ (error "fin-duty: bad DCTFWeb line in %s: %S" file line))))

(defun fin-duty-dctfweb ()
  "Alist MONTH -> cents of its last transmitted DCTFWeb."
  (let (out)
    (dolist (r (fin-ir--csv fin-ir-payments-dir "\\`dctfweb-.*\\.csv\\'" #'fin-duty--dctfweb-row) out)
      (let ((cell (assoc (car r) out)))
        (cond ((null cell) (push (list (car r) (nth 1 r) (nth 2 r)) out))
              ((string< (nth 1 cell) (nth 1 r)) (setcdr cell (cdr r))))))
    (mapcar (lambda (c) (cons (car c) (nth 2 c))) out)))

(defun fin-duty-month (month ctx payments dctfweb &optional gps accounting)
  "Plist of MONTH's obligations from CTX, PAYMENTS, DCTFWEB, GPS and
ACCOUNTING (alist MONTH -> cents).  Amounts are nil when absent; :exported
is nil when MONTH precedes every DCTFWeb export.  :inss-paid sums company
DARFs, GPS and partner complements (1872); :pro-labore is the planned one."
  (let ((simples (assoc month (plist-get ctx :simples))))
    (list :month month :due (fin-duty-due month) :mei (fin-duty-mei-p month ctx)
          :invoiced (cdr (assoc month (plist-get ctx :invoices)))
          :declared (nth 1 simples) :das (nth 2 simples)
          :das-paid (fin-duty--paid payments (fin-duty--codes "das") month)
          :dctfweb (cdr (assoc month dctfweb))
          :exported (cl-some (lambda (c) (not (string< month (car c)))) dctfweb)
          :pro-labore (and (not (fin-duty-mei-p month ctx)) (fin-duty-planned month))
          :accounting (cdr (assoc month accounting))
          :inss (and (not (fin-duty-mei-p month ctx)) (fin-duty-expected-inss month))
          :inss-paid (+ (fin-duty--paid payments (fin-duty--codes "inss") month)
                        (fin-duty--paid payments (fin-duty--codes "inss complement" 'cpf) month)
                        (or (cdr (assoc month gps)) 0)))))

(defun fin-duty-open (m)
  "Labels of what month plist M still lacks."
  (let ((das (plist-get m :das)) (inss (plist-get m :inss)) (sent (plist-get m :dctfweb))
        (mei (plist-get m :mei)))
    (delq nil (list (unless (or mei (plist-get m :declared)) "PGDAS-D")
                    (and das (< (plist-get m :das-paid) das) "DAS")
                    (and mei (= (plist-get m :das-paid) 0) "DAS-MEI")
                    (and inss (null sent) (plist-get m :exported) "DCTFWeb")
                    (and inss sent (/= sent inss) "DCTFWeb off the pró-labore")
                    (and inss (< (plist-get m :inss-paid) inss) "INSS")))))

(defun fin-duty-months (today &optional ctx)
  "Month plists (`fin-duty-month') from `fin-duty-since' to before TODAY."
  (let ((ctx (or ctx (fin-ir-context))) (payments (fin-ir-payments)) (dctfweb (fin-duty-dctfweb))
        (gps (fin-duty-gps)) (accounting (fin-duty--item-paid "contabilidade" 0)))
    (mapcar (lambda (m) (fin-duty-month m ctx payments dctfweb gps accounting)) (fin-duty--window today))))

(defun fin-duty-year (year ctx duty)
  "Plist of YEAR's obligations from CTX and DUTY (`fin-ir-duty').
:books and :defis are nil when missing; :dirpf is the filing date, nil,
`not-required', or `unknown' when DUTY is nil."
  (list :year year :due (format "%04d-03-31" (1+ year))
        :mei (fin-duty-mei-p (format "%04d-12" year) ctx)
        :books (cdr (assq year (plist-get ctx :books)))
        :defis (fin-ir--defis-exempt ctx year)
        :dirpf-due (plist-get duty :deadline)
        :dirpf (cond ((null duty) 'unknown)
                     ((eq (plist-get duty :required) t) (plist-get duty :filed))
                     (t 'not-required))))

(defun fin-duty-year-open (y today)
  "Labels of what year plist Y lacks past its due dates as of TODAY."
  (let ((late (and (not (plist-get y :mei)) (string< (plist-get y :due) today))))
    (cl-remove-if (lambda (label) (fin-duty-known-p (plist-get y :year) label))
                  (delq nil (list (and late (null (plist-get y :books)) "books")
                                  (and late (null (plist-get y :defis)) "DEFIS")
                                  (and (plist-get y :dirpf-due) (string< (plist-get y :dirpf-due) today)
                                       (null (plist-get y :dirpf)) "DIRPF"))))))

(defun fin-duty--own-out (year)
  "Cents the company account sent in YEAR to the partner's own accounts."
  (let ((case-fold-search t))
    (cl-loop for b in (fin-ir--pj-rows year)
             when (and (equal (nth 2 b) "out")
                       (cl-some (lambda (re) (string-match-p re (nth 4 b))) fin-bank-own-regexps))
             sum (nth 3 b))))

(defun fin-duty-room (pos)
  "Plist :book-profit and :room (exempt draw left) of POS, or nil.
Book profit is revenue less costs and pró-labore; nil without pró-labore."
  (let ((pl (plist-get pos :pro-labore-total)))
    (when pl
      (let* ((year (plist-get pos :year))
             (out (cl-loop for b in (fin-ir--pj-rows year)
                           when (equal (nth 2 b) "out") sum (nth 3 b)))
             (costs (- out (fin-duty--own-out year) (plist-get pos :partner-paid)
                       (plist-get pos :partner-taxes)))
             (book (- (plist-get pos :revenue) costs (- pl (plist-get pos :inss)))))
        (list :book-profit book :room (- book (or (plist-get pos :profit) 0)))))))

(defun fin-duty--latest (dir regexp)
  "Greatest first group of REGEXP over the file names in DIR, or nil."
  (let (best)
    (dolist (f (and dir (file-directory-p dir) (directory-files dir nil regexp)) best)
      (when (and (string-match regexp f) (or (null best) (string< best (match-string 1 f))))
        (setq best (match-string 1 f))))))

(defun fin-duty--days (from to)
  "Days from ISO date FROM to TO."
  (round (/ (float-time (time-subtract (date-to-time (concat to " 00:00:00"))
                                       (date-to-time (concat from " 00:00:00"))))
            86400)))

(defun fin-duty--exported (regexp)
  "Save date of the newest `fin-ir-payments-dir' file matching REGEXP."
  (let (best)
    (dolist (f (and fin-ir-payments-dir (file-directory-p fin-ir-payments-dir)
                    (directory-files fin-ir-payments-dir t regexp))
               best)
      (let ((d (format-time-string "%F" (file-attribute-modification-time (file-attributes f)))))
        (when (or (null best) (string< best d)) (setq best d))))))

(defun fin-duty-stale (today)
  "Labels of Receita inputs stale as of TODAY.
Situação fiscal older than `fin-duty-stale-days'; DCTFWeb and payment
exports saved before the last monthly due date."
  (let* ((dir (and fin-ir-payments-dir (expand-file-name "situacao-fiscal" fin-ir-payments-dir)))
         (due (car (last (cl-remove-if-not (lambda (m) (string< (fin-duty-due m) today))
                                           (fin-duty--window today)))))
         (out nil))
    (dolist (who '("cnpj" "cpf"))
      (let ((d (fin-duty--latest dir (format "\\`situacao-fiscal-%s-\\([0-9-]\\{10\\}\\)\\.pdf\\'" who))))
        (when (or (null d) (> (fin-duty--days d today) fin-duty-stale-days))
          (push (format "situação fiscal %s %s" (upcase who) (if d (concat "from " d) "missing")) out))))
    (when due
      (dolist (e '(("DCTFWeb" . "\\`dctfweb-.*\\.csv\\'") ("company payments" . "\\`pagamentos-cnpj-.*\\.csv\\'")))
        (let ((d (fin-duty--exported (cdr e))))
          (when (or (null d) (string< d (fin-duty-due due)))
            (push (format "%s export %s" (car e) (if d (concat "from " d) "missing")) out)))))
    (nreverse out)))

(provide 'duties)
;;; duties.el ends here
