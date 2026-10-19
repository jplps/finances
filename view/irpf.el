;;; irpf.el --- IR panel: income tax position  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)
(require 'html)
(require 'panels)
(require 'cashflow)
(require 'ir)

(defun fin-dashboard--ir-years ()
  "Years up to now with rules in `fin-ir-rules' and a known pró-labore
\(`fin-ir-pro-labore' or `fin-ir-pro-labore-files')."
  (let ((now (fin-report--year-now)))
    (sort (cl-remove-if-not (lambda (y) (and (<= y now) (assq y fin-ir-rules)))
                            (delete-dups (append (mapcar #'car fin-ir-pro-labore)
                                                 (mapcar #'car (fin-ir-pro-labore-files)))))
          #'<)))

(defun fin-dashboard--ir-money (cents)
  "R$ CENTS, or ? when unknown."
  (if cents (concat "R$ " (fin-dashboard--money-str cents)) "?"))

(defun fin-dashboard--ir-tiles (pos)
  (let ((taxes (plist-get pos :taxes)))
    (concat
     "<div class=\"kpis\">"
     (fin-dashboard--kpi (format "revenue %d" (plist-get pos :year))
                         (fin-dashboard--ir-money (plist-get pos :revenue))
                         (mapconcat (lambda (tx) (format "%s %s" (car tx) (fin-dashboard--money-str (cdr tx))))
                                    taxes " · "))
     (fin-dashboard--kpi "to cpf" (fin-dashboard--ir-money (plist-get pos :drawn))
                         (if (plist-get pos :pro-labore)
                             (format "pró-labore %s/mo · profit %s" (fin-dashboard--money-str (plist-get pos :pro-labore))
                                     (fin-dashboard--money-str (plist-get pos :profit)))
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
                                  (format "file the %d return, overdue since %s · fine ≥ %s"
                                          y (fin-dashboard--ir-date (plist-get d :deadline))
                                          (fin-dashboard--ir-money fin-ir-late-fine-min))
                                  (format "taxable %s (limit %s), exempt %s (limit %s)"
                                          (fin-dashboard--ir-money (plist-get p :taxable))
                                          (fin-dashboard--ir-money (fin-ir-rule y :filing-taxable))
                                          (fin-dashboard--ir-money (plist-get d :exempt-total))
                                          (fin-dashboard--ir-money (fin-ir-rule y :filing-exempt))))))
                      ('late (list t (format "%d return filed late on %s" y (fin-dashboard--ir-date (plist-get d :filed)))))
                      ('unknown (list nil (format "%d: add its payer-income or DCTFWeb file to the ir inbox" y))))))
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
                 (format "rectify exempt profit: %s"
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
                       ('verified "covered by the balance sheet")
                       ('yes (format "balance sheet missing, else %s more tax" extra))
                       (_ (format "%s more tax" extra))))
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

(defun fin-dashboard--ir-gap-line (text gaps)
  "One line TEXT for revenue GAPS (`fin-ir-revenue-gaps'), nil when none."
  (when gaps
    (list nil (fin-dashboard--ir-detail-span
               (format text (mapconcat (lambda (g) (fin-dashboard--ir-month (car g))) gaps ", "))
               (mapconcat (lambda (g) (format "%s: declared %s, invoiced %s, received %s"
                                              (fin-dashboard--ir-month (car g))
                                              (fin-dashboard--ir-money (nth 1 g)) (fin-dashboard--ir-money (nth 2 g))
                                              (if (nth 3 g) (fin-dashboard--ir-money (nth 3 g)) "?")))
                          gaps "; ")))))

(defun fin-dashboard--ir-revenue-lines (ctx)
  "Lines for the months whose PGDAS-D revenue is off the NFS-e: the NFS-e
to fix when the PGDAS-D matches what the bank received, else the PGDAS-D."
  (let* ((gaps (fin-ir-revenue-gaps (plist-get ctx :simples) (plist-get ctx :invoices)
                                    (format-time-string "%F") (fin-ir-bank-revenue)))
         (invoice-side (cl-remove-if-not (lambda (g) (and (nth 3 g) (<= (abs (- (nth 1 g) (nth 3 g))) 100)))
                                         gaps)))
    (delq nil (list (fin-dashboard--ir-gap-line "rectify PGDAS-D %s: off the NFS-e and the bank"
                                                (cl-set-difference gaps invoice-side))
                    (fin-dashboard--ir-gap-line "fix NFS-e %s: PGDAS-D matches the bank"
                                                invoice-side)))))

(defun fin-dashboard--ir-rate-line (ctx)
  "One line for the PGDAS-D months whose DAS rate is off the year's."
  (let ((odd (fin-ir-rate-outliers (plist-get ctx :simples))))
    (when odd
      (list nil (fin-dashboard--ir-detail-span
                 (format "check PGDAS-D %s: rate off the year's"
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
                 (format "%s with no Receita record"
                         (fin-dashboard--count (length rows) "payment"))
                 (mapconcat (lambda (r) (format "%s %s" (fin-dashboard--ir-date (nth 1 r)) (fin-dashboard--ir-money (nth 3 r))))
                            rows "; "))))))

(defun fin-dashboard--ir-wrong-account-line (ctx)
  "One line for the taxes paid from the other payer's account."
  (let ((wrong (fin-ir-wrong-account (plist-get ctx :receita))))
    (when wrong
      (list nil (fin-dashboard--ir-detail-span
                 (format "taxes from the wrong account: %s in %s"
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
                 (format "peak month %s, limit %s"
                         (fin-dashboard--ir-money peak) (fin-dashboard--ir-money fin-ir-dividend-month-limit)))
           (list (<= total fin-ir-minimum-tax-floor)
                 (format "income %s, limit %s"
                         (fin-dashboard--ir-money total) (fin-dashboard--ir-money fin-ir-minimum-tax-floor)))
           (and monthly
                (list (<= monthly fin-ir-monthly-exempt)
                      (format "pró-labore %s/mo, limit %s" (fin-dashboard--ir-money monthly)
                              (fin-dashboard--ir-money fin-ir-monthly-exempt))))))))

(defun fin-dashboard--ir-individual-line (positions)
  "Income from individuals across POSITIONS: (OK HTML), nil when none."
  (let ((months (apply #'append (mapcar (lambda (p) (plist-get p :individuals)) positions))))
    (when months
      (let ((sum (apply #'+ (mapcar #'cdr months))))
        (pcase fin-ir-individual-income
          ('exempt (list t (format "%s from individuals: exempt" (fin-dashboard--ir-money sum))))
          ('taxable (list nil (format "carnê-leão (DARF 0190) due for %s: %s"
                                      (fin-dashboard--count (length months) "month")
                                      (mapconcat (lambda (m) (format "%s %s" (car m) (fin-dashboard--money-str (cdr m))))
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
                   '("" "")
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
     '("" "")
     (list (fin-dashboard--ir-revenue pos)
           (fin-dashboard--ir-group "company taxes" (apply #'+ (mapcar #'cdr taxes))
                                    (mapcar (lambda (tx) (list (car tx) (cdr tx))) taxes))
           (fin-dashboard--ir-group "to cpf" (plist-get pos :drawn)
                                    (delq nil
                                          (list (list "pró-labore" (plist-get pos :pro-labore-total))
                                                (and (plist-get pos :other-taxable) (> (plist-get pos :other-taxable) 0)
                                                     (list "other taxable" (plist-get pos :other-taxable)))
                                                (list "inss on pró-labore" (plist-get pos :inss))
                                                (unless (eql (plist-get pos :profit) (plist-get pos :exempt))
                                                  (list "profit" (plist-get pos :profit)))
                                                (list "presumed profit" (plist-get pos :presumed))
                                                (list "exempt" (plist-get pos :exempt)))))
           (fin-dashboard--ir-group "deductions"
                                    (and model (plist-get pos (if (eq model 'complete) :complete :simplified)))
                                    ;; The simplified discount stands alone; the
                                    ;; complete model is the sum of its rows.
                                    (and (eq model 'complete)
                                         (cl-remove-if-not (lambda (r) (and (cadr r) (> (cadr r) 0)))
                                                           (list (list "inss" (plist-get pos :inss))
                                                                 (list "health" (plist-get pos :health))
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
                          (fin-dashboard--ir-profit-line positions))
                    (and ctx (fin-dashboard--ir-revenue-lines ctx))
                    (list (and ctx (fin-dashboard--ir-rate-line ctx))
                          (and ctx (fin-dashboard--ir-unmatched-line ctx))
                          (and ctx (fin-dashboard--ir-wrong-account-line ctx))))))

(defun fin-dashboard--ir-rule-row (rule source lines ok)
  "Row RULE, SOURCE its tooltip, and its status from LINES, (OK HTML)
each: ⚠ when a line is not ok, its text as the tooltip, else ✓ with the
lines' text, OK when there are none."
  (let* ((open (cl-remove-if #'car lines))
         (text (if open (mapconcat #'cadr open "; ")
                 (if lines (mapconcat #'cadr lines " · ") ok))))
    (list (list :raw (fin-dashboard--ir-detail-span (fin-dashboard--esc rule) source))
          (list :raw (format "<span title=\"%s\">%s</span>"
                             (fin-dashboard--esc (fin-dashboard--ir-plain text))
                             (fin-dashboard--health-mark (null open)))))))

(defun fin-dashboard--ir-plain (html)
  "HTML as plain text: tags dropped, entities decoded."
  (let ((s (replace-regexp-in-string "<[^>]*>" "" html)))
    (dolist (e '(("&gt;" . ">") ("&lt;" . "<") ("&quot;" . "\"") ("&amp;" . "&")) s)
      (setq s (string-replace (car e) (cdr e) s)))))

(defun fin-dashboard--ir-inss-line (pos)
  "INSS withheld on POS's pró-labore: (OK HTML), nil when no pró-labore."
  (when (plist-get pos :pro-labore-total)
    (list (> (or (plist-get pos :inss) 0) 0)
          (format "%s withheld on %s" (fin-dashboard--ir-money (plist-get pos :inss))
                  (fin-dashboard--ir-money (plist-get pos :pro-labore-total))))))

(defun fin-dashboard--ir-rules (positions closed ctx)
  "Every rule the owner must comply with, its official source and status,
the current year's from 2026 on (Lei 15.270)."
  (let* ((current (car (last positions)))
         (watch (and (>= (plist-get current :year) 2026) (fin-dashboard--ir-watch-lines current))))
    (fin-dashboard--table
     '("rule" "status")
     (append
      (list (fin-dashboard--ir-rule-row "file DIRPF when required, by the deadline" "IN RFB of each exercício"
                                        (fin-dashboard--ir-return-risks closed) "every return due is filed")
            (fin-dashboard--ir-rule-row "pay what Receita notified" "Receita notices"
                                        (delq nil (list (fin-dashboard--ir-debts-line))) "no open debt")
            (fin-dashboard--ir-rule-row "exempt profit up to the presumed one, unless full books show more"
                                        "LC 123 art. 14 §1–2"
                                        (delq nil (list (fin-dashboard--ir-profit-line positions)))
                                        "profit within the presumed")
            (fin-dashboard--ir-rule-row "DEFIS and DIRPF declare the profit paid" "LC 123 art. 14, 25 and 38"
                                        (delq nil (list (fin-dashboard--ir-exempt-line positions)))
                                        "declarations match the bank")
            (fin-dashboard--ir-rule-row "PGDAS-D revenue equals the NFS-e issued" "LC 123 art. 38-A"
                                        (and ctx (fin-dashboard--ir-revenue-lines ctx)) "every month matches")
            (fin-dashboard--ir-rule-row "DAS at the Anexo III rate" "LC 123 Anexo III"
                                        (delq nil (list (and ctx (fin-dashboard--ir-rate-line ctx))))
                                        "every month at the year's rate")
            (fin-dashboard--ir-rule-row "every tax paid has its Receita record" "Receita payments"
                                        (delq nil (list (and ctx (fin-dashboard--ir-unmatched-line ctx))))
                                        "all matched")
            (fin-dashboard--ir-rule-row "company taxes from the company account" "Receita payments"
                                        (delq nil (list (and ctx (fin-dashboard--ir-wrong-account-line ctx))))
                                        "all from the right account")
            (fin-dashboard--ir-rule-row "11% INSS withheld on pró-labore" "Lei 8.212 art. 21, 30 §4; Lei 10.666 art. 4"
                                        (delq nil (list (fin-dashboard--ir-inss-line current)))
                                        "no pró-labore")
            (fin-dashboard--ir-rule-row "income from individuals: carnê-leão monthly" "Lei 7.713 art. 8"
                                        (delq nil (list (fin-dashboard--ir-individual-line (list current))))
                                        "none"))
      (and watch
           (list (fin-dashboard--ir-rule-row "profit to one person ≤ R$ 50,000 a month, else 10% withheld"
                                             "Lei 15.270 (Lei 9.250 art. 6-A)" (list (nth 0 watch)) nil)
                 (fin-dashboard--ir-rule-row "income ≤ R$ 600,000 a year, else minimum tax"
                                             "Lei 15.270 (Lei 9.250 art. 16-A)" (list (nth 1 watch)) nil)
                 (fin-dashboard--ir-rule-row "taxable ≤ R$ 5,000 a month pays no tax"
                                             "Lei 15.270 (Lei 9.250 art. 3-A)"
                                             (or (cddr watch) (list (list nil "pró-labore unknown"))) nil))))
     "rules")))

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
     (when current
       (fin-dashboard--block "Rules" "Hover a status for its detail"
                             (fin-dashboard--ir-rules positions closed ctx)))
     (fin-dashboard--ir-lines "Inputs" "Inputs the estimate still needs" (fin-dashboard--ir-input-lines))
     (when positions
       (fin-dashboard--block "Years" "Filing duty and return per year"
                             (fin-dashboard--ir-years-table positions closed))))))

(provide 'irpf)
;;; irpf.el ends here
