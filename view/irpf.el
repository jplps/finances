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
     "<div class=\"kpis three\">"
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

(defun fin-dashboard--ir-returns (closed)
  "Table of each closed year's filing duty and return."
  (fin-dashboard--table
   '("year" "taxable" "exempt" "due" "return")
   (mapcar (lambda (c)
             (let ((p (car c)) (d (cdr c)))
               (list (format "%d" (plist-get p :year))
                     (if (plist-get p :taxable) (fin-dashboard--money-cell (plist-get p :taxable)) "?")
                     (if (plist-get p :profit) (fin-dashboard--money-cell (plist-get p :profit)) "?")
                     (pcase (plist-get d :required) ('t "yes") ('nil "no") (_ "?"))
                     (list :raw (pcase (plist-get d :status)
                                  ('on-time (fin-dashboard--dm (plist-get d :filed)))
                                  ('late (format "<span class=\"neg\">%s late</span>"
                                                 (fin-dashboard--dm (plist-get d :filed))))
                                  ('missing "<span class=\"neg\">missing</span>")
                                  ('not-required "–")
                                  (_ "?"))))))
           closed)))

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
                                          (fin-dashboard--ir-money (plist-get p :profit))
                                          (fin-dashboard--ir-money (fin-ir-rule y :filing-exempt))))))
                      ('late (list t (format "DIRPF %d filed late on %s" (1+ y) (fin-dashboard--ir-date (plist-get d :filed)))))
                      ('unknown (list nil (format "ano %d: set <b>fin-ir-pro-labore</b>" y))))))
                closed)))

(defun fin-dashboard--ir-defis-line (positions)
  "One line for the years whose DEFIS exempt profit is below what reached the CPF."
  (let ((off (cl-remove-if-not (lambda (p) (and (plist-get p :defis-exempt) (plist-get p :profit)
                                                (> (- (plist-get p :profit) (plist-get p :defis-exempt)) 100000)))
                               positions)))
    (when off
      (list nil (fin-dashboard--ir-detail-span
                 (format "rectify DEFIS %s: %s exempt profit declared, %s reached the CPF"
                         (fin-dashboard--ir-span (mapcar (lambda (p) (plist-get p :year)) off))
                         (fin-dashboard--ir-money (apply #'+ (mapcar (lambda (p) (plist-get p :defis-exempt)) off)))
                         (fin-dashboard--ir-money (apply #'+ (mapcar (lambda (p) (plist-get p :profit)) off))))
                 (mapconcat (lambda (p) (format "%d: %s" (plist-get p :year) (fin-dashboard--ir-money (plist-get p :profit))))
                            off ", "))))))

(defun fin-dashboard--ir-profit-line (positions)
  "One line for the years whose profit exceeds the presumed one."
  (let ((over (cl-remove-if-not (lambda (p) (and (plist-get p :profit)
                                                 (> (plist-get p :profit) (plist-get p :presumed))))
                                positions)))
    (when over
      (list (eq fin-ir-full-books 'yes)
            (fin-dashboard--ir-detail-span
             (format "profit over the presumed %s: %s"
                     (fin-dashboard--ir-span (mapcar (lambda (p) (plist-get p :year)) over))
                     (if (eq fin-ir-full-books 'yes) "keep full books" "exempt only with full books"))
             (mapconcat (lambda (p) (format "%d: %s over %s" (plist-get p :year)
                                            (fin-dashboard--ir-money (plist-get p :profit))
                                            (fin-dashboard--ir-money (plist-get p :presumed))))
                        over ", "))))))

(defun fin-dashboard--ir-revenue-gap (p)
  "\"YEAR R$ X invoiced not declared\" (or the reverse) for position P, or nil.
Invoices are the reference; without them, client payments in the bank."
  (let* ((declared (plist-get p :declared-revenue))
         (invoiced (plist-get p :invoiced))
         (bank (and (plist-get p :bank-covered) (plist-get p :revenue)))
         (ref (or invoiced bank))
         (what (if invoiced "invoiced" "received")))
    (when (and declared ref (if invoiced (> (abs (- ref declared)) 100) (> (abs (- ref declared)) (* 0.05 declared))))
      (if (> ref declared)
          (format "%d %s %s not declared" (plist-get p :year) (fin-dashboard--ir-money (- ref declared)) what)
        (format "%d %s declared not %s" (plist-get p :year) (fin-dashboard--ir-money (- declared ref)) what)))))

(defun fin-dashboard--ir-revenue-line (positions)
  "One line for the years whose PGDAS-D revenue is off the invoices."
  (let ((gaps (delq nil (mapcar #'fin-dashboard--ir-revenue-gap positions))))
    (when gaps
      (list nil (format "rectify PGDAS-D: %s" (mapconcat #'identity gaps " · "))))))

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
                (list nil "full books unknown: set <b>fin-ir-full-books</b>")))))

(defun fin-dashboard--ir-lines (name title lines)
  "Block NAME of to-do LINES, (OK HTML) each, counting those not ok."
  (let ((todo (cl-count-if-not #'car lines)))
    (fin-dashboard--block
     (format "%s <span class=\"%s\">%s</span>" name (if (zerop todo) "pos" "neg")
             (if (zerop todo) "✓" (format "⚠ %d" todo)))
     title
     (concat "<div class=\"health\">"
             (mapconcat (lambda (l) (fin-dashboard--health-line (car l) (cadr l))) lines "")
             "</div>"))))

(defun fin-dashboard--ir-detail (pos)
  "One year's income and deductions."
  (let ((taxes (plist-get pos :taxes)))
    (fin-dashboard--table
     (list "" (number-to-string (plist-get pos :year)))
     (mapcar (lambda (r) (list (car r) (if (cadr r) (fin-dashboard--money-cell (cadr r)) "?")))
             (append
              (list (list "revenue, bank" (plist-get pos :revenue))
                    (list "revenue, PGDAS-D" (plist-get pos :declared-revenue))
                    (list "revenue, NFS-e" (plist-get pos :invoiced)))
              (mapcar (lambda (tx) (list (car tx) (cdr tx))) taxes)
              (list (list "to cpf" (plist-get pos :drawn))
                    (list "taxable" (plist-get pos :taxable))
                    (list "inss" (plist-get pos :inss))
                    (list "profit" (plist-get pos :profit))
                    (list "presumed profit" (plist-get pos :presumed))
                    (list "health" (plist-get pos :health))
                    (list "health, not deductible" (plist-get pos :health-excluded))
                    (list "retirement" (plist-get pos :retirement))
                    (list "complete" (plist-get pos :complete))
                    (list "simplified" (plist-get pos :simplified))
                    (list "tax" (plist-get pos :tax))))))))

(defun fin-dashboard--ir-years-switch (positions)
  "CSS-only year switch over each position's detail, the last year checked."
  (let ((last (plist-get (car (last positions)) :year)))
    (concat
     "<style>section#ir .iry{display:none;}"
     ".iry-pick{display:flex;flex-wrap:wrap;gap:12px;margin:0 0 8px;}"
     ".iry-pick input{position:absolute;opacity:0;}"
     ".iry-pick label{cursor:pointer;color:var(--muted);}"
     ".iry-pick input:checked+label{color:var(--accent-2);}"
     (mapconcat (lambda (p) (format "section#ir:has(#iry-%d:checked) .iry-%d{display:block;}"
                                    (plist-get p :year) (plist-get p :year)))
                positions "")
     "</style><div class=\"iry-pick\">"
     (mapconcat (lambda (p)
                  (let ((y (plist-get p :year)))
                    (format "<input type=\"radio\" name=\"iry\" id=\"iry-%d\"%s><label for=\"iry-%d\">%d</label>"
                            y (if (= y last) " checked" "") y y)))
                positions "")
     "</div>"
     (mapconcat (lambda (p) (format "<div class=\"iry iry-%d\">%s</div>"
                                    (plist-get p :year) (fin-dashboard--ir-detail p)))
                positions ""))))

(defun fin-dashboard--panel-ir ()
  (let* ((positions (mapcar #'fin-ir-position (fin-dashboard--ir-years)))
         (closed (fin-dashboard--ir-closed positions))
         (current (car (last positions))))
    (fin-dashboard--panel
     "IR" "ir"
     "Imposto de renda: risks with Receita, returns per year, the 2026 rules, income and deductions"
     (when current (fin-dashboard--ir-tiles current))
     (fin-dashboard--ir-lines
      "Risks" "What Receita can charge or question: open debts, returns due, DEFIS and PGDAS-D against the bank"
      (delq nil (append (list (fin-dashboard--ir-debts-line))
                        (fin-dashboard--ir-return-risks closed)
                        (list (fin-dashboard--ir-defis-line positions)
                              (fin-dashboard--ir-revenue-line positions)
                              (fin-dashboard--ir-profit-line positions)))))
     (when closed
       (fin-dashboard--block "Returns" "Filing duty per year (each exercício's IN RFB) and the return delivered"
                             (fin-dashboard--ir-returns closed)))
     (when (and current (>= (plist-get current :year) 2026))
       (fin-dashboard--ir-lines
        "Watch" "This year's Lei 15.270/2025 limits, income from individuals, inputs missing"
        (delq nil (append (fin-dashboard--ir-watch-lines current)
                          (list (fin-dashboard--ir-individual-line (list current)))
                          (fin-dashboard--ir-input-lines)))))
     (when positions
       (fin-dashboard--block "Year" "Company revenue and taxes, deposits to the CPF, deductions and tax"
                             (fin-dashboard--ir-years-switch positions))))))

(provide 'irpf)
;;; irpf.el ends here
