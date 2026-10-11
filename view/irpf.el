;;; irpf.el --- IR panel: income tax position  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)
(require 'html)
(require 'panels)
(require 'cashflow)
(require 'ir)

(defun fin-dashboard--ir-years ()
  "Years up to now with rules in `fin-ir-rules' and a `fin-ir-pro-labore'."
  (let ((now (fin-report--year-now)))
    (sort (cl-remove-if-not (lambda (y) (and (<= y now) (assq y fin-ir-rules)))
                            (mapcar #'car fin-ir-pro-labore))
          #'<)))

(defun fin-dashboard--ir-money (cents)
  "R$ CENTS grouped, or ? when unknown."
  (if cents (concat "R$ " (fin-dashboard--grouped cents)) "?"))

(defun fin-dashboard--ir-tiles (pos)
  (let ((taxes (plist-get pos :taxes)))
    (concat
     "<div class=\"kpis\">"
     (fin-dashboard--kpi (format "revenue %d" (plist-get pos :year))
                         (fin-dashboard--ir-money (plist-get pos :revenue))
                         (mapconcat (lambda (tx) (format "%s %s" (car tx) (fin-dashboard--grouped (cdr tx))))
                                    taxes " · "))
     (fin-dashboard--kpi "to cpf" (fin-dashboard--ir-money (plist-get pos :drawn))
                         (if (plist-get pos :pro-labore)
                             (format "pró-labore %s/mo · profit %s" (fin-dashboard--grouped (plist-get pos :pro-labore))
                                     (fin-dashboard--grouped (plist-get pos :profit)))
                           "pró-labore unknown"))
     (fin-dashboard--kpi "income tax" (fin-dashboard--ir-money (plist-get pos :tax))
                         (if (plist-get pos :model)
                             (format "%s model · before IRRF withheld" (plist-get pos :model))
                           "needs the pró-labore"))
     "</div>")))

(defun fin-dashboard--ir-closed (positions)
  "(POS . DUTY) of each closed year of POSITIONS (its rules name a deadline)."
  (mapcar (lambda (p) (cons p (fin-ir-duty p)))
          (cl-remove-if-not (lambda (p) (fin-ir-rule (plist-get p :year) :deadline)) positions)))

(defun fin-dashboard--ir-return-cell (duty)
  "Return column of a year's DUTY plist (nil for the current year)."
  (list :raw (pcase (plist-get duty :status)
               ('on-time (fin-dashboard--dm (plist-get duty :filed)))
               ('late (format "<span class=\"neg\">%s late</span>" (fin-dashboard--dm (plist-get duty :filed))))
               ('missing "<span class=\"neg\">missing</span>")
               ('not-required "–")
               ('nil "–")
               (_ "?"))))

(defun fin-dashboard--ir-date (iso)
  "ISO date as dd/mm/yyyy."
  (format "%s/%s" (fin-dashboard--dm iso) (substring iso 0 4)))

(defun fin-dashboard--ir-span (years)
  "YEARS (ascending) as 2021–2025 when consecutive, else 2021, 2023."
  (if (equal years (number-sequence (car years) (car (last years))))
      (if (cdr years) (format "%d–%d" (car years) (car (last years))) (format "%d" (car years)))
    (mapconcat #'number-to-string years ", ")))

(defun fin-dashboard--ir-detail-span (text detail)
  "TEXT (HTML) with DETAIL (plain) as its hover title."
  (format "<span title=\"%s\">%s</span>" (fin-dashboard--esc detail) text))

(defun fin-dashboard--ir-debts-line ()
  "One line for every open debt Receita notified: (OK HTML), nil when none."
  (when fin-ir-open-debts
    (list nil (fin-dashboard--ir-detail-span
               (format "pay %s overdue: %s"
                       (fin-dashboard--ir-money (apply #'+ (mapcar #'caddr fin-ir-open-debts)))
                       (mapconcat (lambda (d) (format "%s %s" (fin-dashboard--esc (nth 3 d))
                                                      (fin-dashboard--ir-money (nth 2 d))))
                                  fin-ir-open-debts " · "))
               (mapconcat (lambda (d) (format "code %s due %s" (nth 1 d) (fin-dashboard--ir-date (nth 0 d)))) fin-ir-open-debts "; ")))))

(defun fin-dashboard--ir-return-risks (closed)
  "One line per closed year whose return is missing, late or unknown."
  (delq nil
        (mapcar (lambda (c)
                  (let* ((p (car c)) (d (cdr c)) (y (plist-get p :year)))
                    (pcase (plist-get d :status)
                      ('missing
                       (list nil (fin-dashboard--ir-detail-span
                                  (format "file DIRPF %d (ano %d), overdue since %s · fine ≥ %s"
                                          (1+ y) y (fin-dashboard--ir-date (plist-get d :deadline))
                                          (fin-dashboard--ir-money fin-ir-late-fine-min))
                                  (format "taxable %s (limit %s), exempt %s (limit %s)"
                                          (fin-dashboard--ir-money (plist-get p :taxable))
                                          (fin-dashboard--ir-money (fin-ir-rule y :filing-taxable))
                                          (fin-dashboard--ir-money (plist-get d :exempt-total))
                                          (fin-dashboard--ir-money (fin-ir-rule y :filing-exempt))))))
                      ('late (list t (format "DIRPF %d filed late on %s" (1+ y) (fin-dashboard--ir-date (plist-get d :filed)))))
                      ('unknown (list nil (format "ano %d: set <b>fin-ir-pro-labore</b>" y))))))
                closed)))

(defun fin-dashboard--ir-exempt-gap (p)
  "\"YEAR: bank X, DEFIS Y, DIRPF Z\" when P's declared exempt profit is
more than R$ 1.000 off the bank's, else nil; also the declarations off."
  (let* ((bank (plist-get p :exempt))
         (off (and bank
                   (cl-remove-if-not
                    (lambda (d) (and (cdr d) (> (abs (- bank (cdr d))) 100000)))
                    (list (cons "DEFIS" (plist-get p :defis-exempt))
                          (cons "DIRPF" (plist-get p :declared-exempt)))))))
    (when off
      (cons (mapcar #'car off)
            (format "%d: bank %s%s" (plist-get p :year) (fin-dashboard--ir-money bank)
                    (mapconcat (lambda (d) (format ", %s %s" (car d) (fin-dashboard--ir-money (cdr d))))
                               off ""))))))

(defun fin-dashboard--ir-exempt-line (positions)
  "One line for the years whose DEFIS or DIRPF exempt profit is off the bank's."
  (let* ((gaps (delq nil (mapcar (lambda (p) (let ((g (fin-dashboard--ir-exempt-gap p)))
                                                (and g (cons (plist-get p :year) g))))
                                 positions)))
         (by (lambda (doc) (mapcar #'car (cl-remove-if-not (lambda (g) (member doc (cadr g))) gaps)))))
    (when gaps
      (list nil (fin-dashboard--ir-detail-span
                 (format "rectify exempt profit, off the bank: %s"
                         (mapconcat (lambda (doc) (format "%s %s" doc (fin-dashboard--ir-span (funcall by doc))))
                                    (cl-remove-if-not by '("DEFIS" "DIRPF")) " · "))
                 (mapconcat #'cddr gaps "; "))))))

(defun fin-dashboard--ir-profit-line (positions)
  "One line for the years whose profit exceeds the presumed one, with the
tax the excess adds over the closed years when full books are not verified."
  (let* ((over (cl-remove-if-not (lambda (p) (and (plist-get p :excess) (> (plist-get p :excess) 0)))
                                 positions))
         (closed (cl-remove-if-not (lambda (p) (fin-ir-rule (plist-get p :year) :deadline)) over))
         (extra (fin-dashboard--ir-money (apply #'+ (mapcar (lambda (p) (plist-get p :excess-tax)) closed)))))
    (when over
      (list (eq fin-ir-full-books 'verified)
            (fin-dashboard--ir-detail-span
             (format "profit over the presumed %s: %s"
                     (fin-dashboard--ir-span (mapcar (lambda (p) (plist-get p :year)) over))
                     (pcase fin-ir-full-books
                       ('verified "exempt, balance sheet on file")
                       ('yes (format "full books claimed, not verified: keep the balance sheet (else %s more tax)" extra))
                       (_ (format "taxable without full books: %s more tax" extra))))
             (mapconcat (lambda (p) (format "%d: %s over %s%s" (plist-get p :year)
                                            (fin-dashboard--ir-money (plist-get p :profit))
                                            (fin-dashboard--ir-money (plist-get p :presumed))
                                            (if (memq p closed)
                                                (format ", %s more tax if taxed" (fin-dashboard--ir-money (plist-get p :excess-tax)))
                                              "")))
                        over "; "))))))

(defun fin-dashboard--ir-month (month)
  "YYYY-MM MONTH as mm/yyyy."
  (format "%s/%s" (substring month 5 7) (substring month 0 4)))

(defun fin-dashboard--ir-revenue-line (ctx)
  "One line for the months whose PGDAS-D revenue is off the NFS-e."
  (let ((gaps (fin-ir-revenue-gaps (plist-get ctx :simples) (plist-get ctx :invoices)
                                   (format-time-string "%F"))))
    (when gaps
      (list nil (fin-dashboard--ir-detail-span
                 (format "rectify PGDAS-D %s: revenue off the NFS-e"
                         (mapconcat (lambda (g) (fin-dashboard--ir-month (car g))) gaps ", "))
                 (mapconcat (lambda (g) (format "%s: declared %s, invoiced %s" (fin-dashboard--ir-month (car g))
                                                (fin-dashboard--ir-money (nth 1 g)) (fin-dashboard--ir-money (nth 2 g))))
                            gaps "; "))))))

(defun fin-dashboard--ir-rate-line (ctx)
  "One line for the PGDAS-D months whose DAS rate is off the year's."
  (let ((odd (fin-ir-rate-outliers (plist-get ctx :simples))))
    (when odd
      (list nil (fin-dashboard--ir-detail-span
                 (format "check PGDAS-D %s: DAS rate off the year's"
                         (mapconcat (lambda (o) (fin-dashboard--ir-month (car o))) odd ", "))
                 (mapconcat (lambda (o) (format "%s: %.2f%% (%s on %s), year median %.2f%%"
                                                (fin-dashboard--ir-month (car o)) (* 100 (nth 1 o))
                                                (fin-dashboard--ir-money (nth 4 o)) (fin-dashboard--ir-money (nth 3 o))
                                                (* 100 (nth 2 o))))
                            odd "; "))))))

(defun fin-dashboard--ir-unmatched-line (ctx)
  "One line for the company's bank payments to Receita no Receita record matches."
  (let ((rows (fin-ir-unmatched (plist-get ctx :receita))))
    (when rows
      (list nil (fin-dashboard--ir-detail-span
                 (format "%s in the bank %s no Receita record"
                         (fin-dashboard--count (length rows) "Receita payment")
                         (if (cdr rows) "match" "matches"))
                 (mapconcat (lambda (r) (format "%s %s" (fin-dashboard--ir-date (nth 1 r)) (fin-dashboard--ir-money (nth 3 r))))
                            rows "; "))))))

(defun fin-dashboard--ir-wrong-account-line (ctx)
  "One line for the taxes paid from the other payer's account."
  (let ((wrong (fin-ir-wrong-account (plist-get ctx :receita))))
    (when wrong
      (list nil (fin-dashboard--ir-detail-span
                 (format "taxes paid from the wrong account: %s in %s"
                         (fin-dashboard--ir-money (apply #'+ (mapcar (lambda (m) (nth 3 (car m))) wrong)))
                         (fin-dashboard--count (length wrong) "payment"))
                 (mapconcat (lambda (m) (format "%s %s %s from the %s" (fin-dashboard--ir-date (nth 1 (car m)))
                                                (car (fin-ir-code (cdr m))) (fin-dashboard--ir-money (nth 3 (car m)))
                                                (if (equal (nth 2 (car m)) "pj") "pj" "cpf")))
                            wrong "; "))))))

(defun fin-dashboard--ir-watch-lines (pos)
  "Lei 15.270 lines for a year from 2026: (OK HTML) each."
  (let ((peak (plist-get pos :peak-month))
        (total (+ (plist-get pos :drawn) (apply #'+ (mapcar #'cdr (plist-get pos :individuals)))))
        (monthly (plist-get pos :pro-labore)))
    (delq nil
          (list
           (list (<= peak fin-ir-dividend-month-limit)
                 (format "largest month to cpf %s, 10%% withheld past %s"
                         (fin-dashboard--ir-money peak) (fin-dashboard--ir-money fin-ir-dividend-month-limit)))
           (list (<= total fin-ir-minimum-tax-floor)
                 (format "income so far %s, minimum tax past %s a year"
                         (fin-dashboard--ir-money total) (fin-dashboard--ir-money fin-ir-minimum-tax-floor)))
           (and monthly
                (list t (format "pró-labore %s/mo %s %s: %s" (fin-dashboard--ir-money monthly)
                                (if (<= monthly fin-ir-monthly-exempt) "≤" "&gt;")
                                (fin-dashboard--ir-money fin-ir-monthly-exempt)
                                (if (<= monthly fin-ir-monthly-exempt) "no monthly tax" "partly taxed"))))))))

(defun fin-dashboard--ir-individual-line (positions)
  "Income from individuals across POSITIONS: (OK HTML), nil when none."
  (let ((months (apply #'append (mapcar (lambda (p) (plist-get p :individuals)) positions))))
    (when months
      (let ((sum (apply #'+ (mapcar #'cdr months))))
        (pcase fin-ir-individual-income
          ('exempt (list t (format "%s from individuals: exempt" (fin-dashboard--ir-money sum))))
          ('taxable (list nil (format "carnê-leão (DARF 0190) due the month after each of %s: %s"
                                      (fin-dashboard--count (length months) "month")
                                      (mapconcat (lambda (m) (format "%s %s" (car m) (fin-dashboard--grouped (cdr m))))
                                                 months ", "))))
          (_ (list nil (format "%s from individuals in %s: set <b>fin-ir-individual-income</b> (work or rent = carnê-leão)"
                               (fin-dashboard--ir-money sum) (fin-dashboard--count (length months) "month")))))))))

(defun fin-dashboard--ir-input-lines ()
  "Inputs the estimate still needs: (OK HTML) each."
  (delq nil
        (list (unless fin-ir-retirement-plan
                (list nil "retirement plan unknown: set <b>fin-ir-retirement-plan</b> (only pgbl deducts)"))
              (unless fin-ir-full-books
                (list nil "full books unknown: set <b>fin-ir-full-books</b> (verified once the balance sheet is on file)")))))

(defun fin-dashboard--ir-lines (name title lines)
  "Block NAME of to-do LINES, (OK HTML) each: only those not ok are listed;
nil when every line is ok."
  (let ((open (cl-remove-if #'car lines)))
    (when open
      (fin-dashboard--block
       (format "%s <span class=\"neg\">⚠ %d</span>" name (length open))
       title
       (concat "<div class=\"health\">"
               (mapconcat (lambda (l) (fin-dashboard--health-line nil (cadr l))) open "")
               "</div>")))))

(defun fin-dashboard--ir-group (label value rows)
  "Accordion row LABEL showing VALUE, ROWS ((NAME CENTS) ...) as its body."
  (list label
        (list (if value (fin-dashboard--money-cell value) "?"))
        (and rows (fin-dashboard--table
                   '("" "amount")
                   (mapcar (lambda (r) (list (car r) (if (cadr r) (fin-dashboard--money-cell (cadr r)) "?")))
                           rows)))))

(defun fin-dashboard--ir-revenue (pos)
  "Revenue group of POS: PGDAS-D and NFS-e over the months PGDAS-D declares,
the bank's when it differs, and the bank revenue of the months after."
  (let ((through (plist-get pos :declared-through)))
    (if (null through)
        (fin-dashboard--ir-group "revenue" (plist-get pos :revenue)
                                 (list (list "bank" (plist-get pos :revenue))
                                       (list "NFS-e" (plist-get pos :invoiced))))
      (let ((after (- (plist-get pos :revenue) (plist-get pos :revenue-through))))
        (fin-dashboard--ir-group "revenue" (plist-get pos :declared-revenue)
                                 (delq nil (list (list "PGDAS-D" (plist-get pos :declared-revenue))
                                                 (list "NFS-e" (plist-get pos :invoiced-through))
                                                 (unless (eql (plist-get pos :revenue-through)
                                                              (plist-get pos :declared-revenue))
                                                   (list "bank" (plist-get pos :revenue-through)))
                                                 (and (> after 0) (list "not declared yet" after)))))))))

(defun fin-dashboard--ir-detail (pos)
  "One year's revenue, company taxes, money to the CPF, deductions and tax,
each group folded."
  (let ((taxes (plist-get pos :taxes))
        (model (plist-get pos :model)))
    (fin-dashboard--alist
     '("" "amount")
     (list (fin-dashboard--ir-revenue pos)
           (fin-dashboard--ir-group "company taxes" (apply #'+ (mapcar #'cdr taxes))
                                    (mapcar (lambda (tx) (list (car tx) (cdr tx))) taxes))
           (fin-dashboard--ir-group "to cpf" (plist-get pos :drawn)
                                    (delq nil
                                          (list (list "pró-labore" (plist-get pos :taxable))
                                                (list "inss on pró-labore" (plist-get pos :inss))
                                                (unless (eql (plist-get pos :profit) (plist-get pos :exempt))
                                                  (list "profit" (plist-get pos :profit)))
                                                (list "presumed profit" (plist-get pos :presumed))
                                                (list "exempt" (plist-get pos :exempt)))))
           (fin-dashboard--ir-group "deductions"
                                    (and model (plist-get pos (if (eq model 'complete) :complete :simplified)))
                                    (delq nil
                                          (list (list "health" (plist-get pos :health))
                                                (list "health, unclassified" (plist-get pos :health-unknown))
                                                (and (plist-get pos :pgbl) (> (plist-get pos :pgbl) 0)
                                                     (list "pgbl" (plist-get pos :pgbl))))))
           (fin-dashboard--ir-group "tax" (plist-get pos :tax) nil)))))

(defun fin-dashboard--ir-years-table (positions closed)
  "Every year, newest first: taxable, exempt, filing duty and return; a
year expands into its detail."
  (fin-dashboard--alist
     '("year" "taxable" "exempt" "due" "return")
     (mapcar (lambda (p)
               (let ((d (cdr (cl-find p closed :key #'car :test #'eq))))
                 (list (number-to-string (plist-get p :year))
                       (list (if (plist-get p :taxable) (fin-dashboard--money-cell (plist-get p :taxable)) "?")
                             (if (plist-get p :exempt) (fin-dashboard--money-cell (plist-get p :exempt)) "?")
                             (if d (pcase (plist-get d :required) ('t "yes") ('nil "no") (_ "?")) "–")
                             (fin-dashboard--ir-return-cell d))
                       (fin-dashboard--ir-detail p))))
             (reverse positions))))

(defun fin-dashboard--ir-risk-lines (positions closed ctx)
  "Risk lines, (OK HTML) each."
  (delq nil (append (list (fin-dashboard--ir-debts-line))
                    (fin-dashboard--ir-return-risks closed)
                    (list (fin-dashboard--ir-exempt-line positions)
                          (fin-dashboard--ir-profit-line positions)
                          (and ctx (fin-dashboard--ir-revenue-line ctx))
                          (and ctx (fin-dashboard--ir-rate-line ctx))
                          (and ctx (fin-dashboard--ir-unmatched-line ctx))
                          (and ctx (fin-dashboard--ir-wrong-account-line ctx))))))

(defun fin-dashboard--ir-todo ()
  "The open IR risks as cockpit lines, (nil HTML) each."
  (let ((years (fin-dashboard--ir-years)))
    (when years
      (let* ((ctx (fin-ir-context))
             (positions (mapcar (lambda (y) (fin-ir-position y ctx)) years)))
        (mapcar (lambda (l) (list nil (fin-dashboard--health-line nil (cadr l))))
                (cl-remove-if #'car (fin-dashboard--ir-risk-lines
                                     positions (fin-dashboard--ir-closed positions) ctx)))))))

(defun fin-dashboard--panel-ir ()
  (let* ((years (fin-dashboard--ir-years))
         (ctx (and years (fin-ir-context)))
         (positions (mapcar (lambda (y) (fin-ir-position y ctx)) years))
         (closed (fin-dashboard--ir-closed positions))
         (current (car (last positions))))
    (fin-dashboard--panel
     "IR" "ir"
     "Imposto de renda per year: taxable and exempt income, filing duty and return, revenue, company taxes, money to the CPF, deductions and tax; risks are in the cockpit's to do"
     (when current (fin-dashboard--ir-tiles current))
     (when (and current (>= (plist-get current :year) 2026))
       (fin-dashboard--ir-lines
        "Watch" "This year's Lei 15.270/2025 limits, income from individuals, inputs missing"
        (delq nil (append (fin-dashboard--ir-watch-lines current)
                          (list (fin-dashboard--ir-individual-line (list current)))
                          (fin-dashboard--ir-input-lines)))))
     (when positions
       (fin-dashboard--block "Years" "Filing duty per year (each exercício's IN RFB), the return delivered; expand a year for its detail"
                             (fin-dashboard--ir-years-table positions closed))))))

(provide 'irpf)
;;; irpf.el ends here
