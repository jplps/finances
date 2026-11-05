;;; irpf.el --- IR panel: income tax position  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)
(require 'html)
(require 'panels)
(require 'cashflow)
(require 'ir)
(require 'duties)

(defun fin-dashboard--ir-years ()
  "Years up to now with `fin-ir-rules' and a known pró-labore, ascending."
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
                                    taxes "<br>"))
     (fin-dashboard--kpi "to cpf" (fin-dashboard--ir-money (plist-get pos :drawn))
                         (if (plist-get pos :pro-labore)
                             (format "pró-labore %s/mo<br>profit %s" (fin-dashboard--money-str (plist-get pos :pro-labore))
                                     (fin-dashboard--money-str (plist-get pos :profit)))
                           "pró-labore unknown"))
     (fin-dashboard--kpi "income tax" (fin-dashboard--ir-money (plist-get pos :tax))
                         (if (plist-get pos :model)
                             (format "%s model<br>before IRRF withheld" (plist-get pos :model))
                           "needs the pró-labore"))
     "</div>")))

(defun fin-dashboard--ir-closed (positions)
  "(POS . DUTY) of each year of POSITIONS whose rules name a deadline."
  (mapcar (lambda (p) (cons p (fin-ir-duty p)))
          (cl-remove-if-not (lambda (p) (fin-ir-rule (plist-get p :year) :deadline)) positions)))

(defun fin-dashboard--ir-return-cell (duty)
  "DIRPF cell of DUTY: filing date, late (filed date on hover), missing or –
when not required; nil empty."
  (list :raw (pcase (plist-get duty :status)
               ('on-time (fin-dashboard--ir-date (plist-get duty :filed)))
               ('late (format "<span class=\"neg\" title=\"filed %s\">late</span>"
                              (fin-dashboard--ir-date (plist-get duty :filed))))
               ('missing "<span class=\"neg\">missing</span>")
               ('not-required "–")
               ('nil "")
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
  "(OK HTML) of the open debts Receita notified, nil when none."
  (when fin-ir-open-debts
    (list nil (fin-dashboard--ir-detail-span
               (format "pay %s overdue: %s"
                       (fin-dashboard--ir-money (apply #'+ (mapcar #'caddr fin-ir-open-debts)))
                       (mapconcat (lambda (d) (format "%s %s" (fin-dashboard--esc (nth 3 d))
                                                      (fin-dashboard--ir-money (nth 2 d))))
                                  fin-ir-open-debts " · "))
               (mapconcat (lambda (d) (format "code %s due %s" (nth 1 d) (fin-dashboard--ir-date (nth 0 d)))) fin-ir-open-debts "; ")))))

(defun fin-dashboard--ir-return-risks (closed)
  "(OK HTML) per CLOSED year whose return is missing, late or unknown."
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
  "(DOCS . TEXT) of P's declarations whose exempt profit is off, else nil.
Off by over R$ 1.000: with books, above them or below the lesser of bank and
books; without, from the bank.  Known gaps (`fin-duty-known-p') are skipped."
  (let* ((bank (plist-get p :exempt))
         (book (plist-get p :book-profit))
         (off (and bank
                   (cl-remove-if-not
                    (lambda (d) (and (cdr d) (if book
                                                 (or (> (cdr d) (+ book 100000))
                                                     (< (cdr d) (- (min bank book) 100000)))
                                               (> (abs (- bank (cdr d))) 100000))))
                    (cl-remove-if (lambda (d) (fin-duty-known-p (plist-get p :year) (car d)))
                                  (list (cons "DEFIS" (plist-get p :defis-exempt))
                                        (cons "DIRPF" (plist-get p :declared-exempt))
                                        (cons "informe" (plist-get p :reported-exempt))))))))
    (when off
      (cons (mapcar #'car off)
            (format "%d: %s %s%s" (plist-get p :year) (if book "books" "bank")
                    (fin-dashboard--ir-money (or book bank))
                    (mapconcat (lambda (d) (format ", %s %s" (car d) (fin-dashboard--ir-money (cdr d))))
                               off ""))))))

(defun fin-dashboard--ir-exempt-line (positions)
  "(OK HTML) for years whose DEFIS, DIRPF or informe exempt profit is off."
  (let* ((gaps (delq nil (mapcar (lambda (p) (let ((g (fin-dashboard--ir-exempt-gap p)))
                                                (and g (cons (plist-get p :year) g))))
                                 positions)))
         (by (lambda (doc) (mapcar #'car (cl-remove-if-not (lambda (g) (member doc (cadr g))) gaps)))))
    (when gaps
      (list nil (fin-dashboard--ir-detail-span
                 (format "rectify exempt profit: %s"
                         (mapconcat (lambda (doc) (format "%s %s" doc (fin-dashboard--ir-span (funcall by doc))))
                                    (cl-remove-if-not by '("DEFIS" "DIRPF" "informe")) " · "))
                 (mapconcat #'cddr gaps "; "))))))

(defun fin-dashboard--ir-profit-line (positions)
  "(OK HTML) for years whose profit exceeds the presumed by over R$ 1.000.
A return declaring no more exempt than allowed settles its year.  Ok when full
books are verified or no closed year is over."
  (let* ((over (cl-remove-if-not
                (lambda (p) (and (plist-get p :excess) (> (plist-get p :excess) 100000)
                                 (not (and (plist-get p :declared-exempt)
                                           (<= (plist-get p :declared-exempt)
                                               (+ (plist-get p :exempt) 100000))))))
                positions))
         (closed (cl-remove-if-not (lambda (p) (fin-ir-rule (plist-get p :year) :deadline)) over))
         (extra (fin-dashboard--ir-money (apply #'+ (mapcar (lambda (p) (plist-get p :excess-tax)) closed)))))
    (when over
      (list (or (eq fin-ir-full-books 'verified) (null closed))
            (fin-dashboard--ir-detail-span
             (format "profit over the presumed %s: %s"
                     (fin-dashboard--ir-span (mapcar (lambda (p) (plist-get p :year)) over))
                     (pcase fin-ir-full-books
                       ('verified "covered by the balance sheet")
                       ('yes (format "books missing or short, else %s more tax" extra))
                       (_ (format "%s more tax" extra))))
             (mapconcat (lambda (p) (format "%d: %s over %s%s" (plist-get p :year)
                                            (fin-dashboard--ir-money (plist-get p :profit))
                                            (if (> (or (plist-get p :book-profit) 0) (plist-get p :presumed))
                                                (concat "books " (fin-dashboard--ir-money (plist-get p :book-profit)))
                                              (fin-dashboard--ir-money (plist-get p :presumed)))
                                            (if (memq p closed)
                                                (format ", %s more tax if taxed" (fin-dashboard--ir-money (plist-get p :excess-tax)))
                                              "")))
                        over "; "))))))

(defun fin-dashboard--ir-month (month)
  "YYYY-MM MONTH as mm/yyyy."
  (format "%s/%s" (substring month 5 7) (substring month 0 4)))

(defun fin-dashboard--ir-gap-line (text gaps)
  "(OK HTML) of format TEXT over the GAPS months, nil when none."
  (when gaps
    (list nil (fin-dashboard--ir-detail-span
               (format text (mapconcat (lambda (g) (fin-dashboard--ir-month (car g))) gaps ", "))
               (mapconcat (lambda (g) (format "%s: declared %s, invoiced %s, received %s"
                                              (fin-dashboard--ir-month (car g))
                                              (fin-dashboard--ir-money (nth 1 g)) (fin-dashboard--ir-money (nth 2 g))
                                              (if (nth 3 g) (fin-dashboard--ir-money (nth 3 g)) "?")))
                          gaps "; ")))))

(defun fin-dashboard--ir-revenue-lines (ctx)
  "(OK HTML) for months whose PGDAS-D is off the NFS-e: the NFS-e to fix when
the PGDAS-D is within R$ 1 of the bank, else the PGDAS-D."
  (let* ((gaps (fin-ir-revenue-gaps (plist-get ctx :simples) (plist-get ctx :invoices)
                                    (format-time-string "%F") (fin-ir-bank-revenue)))
         (invoice-side (cl-remove-if-not (lambda (g) (and (nth 3 g) (<= (abs (- (nth 1 g) (nth 3 g))) 100)))
                                         gaps)))
    (delq nil (list (fin-dashboard--ir-gap-line "rectify PGDAS-D %s: off the NFS-e and the bank"
                                                (cl-set-difference gaps invoice-side))
                    (fin-dashboard--ir-gap-line "fix NFS-e %s: PGDAS-D matches the bank"
                                                invoice-side)))))

(defun fin-dashboard--ir-rate-line (ctx)
  "(OK HTML) for PGDAS-D months whose DAS rate is off the year's median."
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
  "(OK HTML) for company payments to Receita with no Receita record."
  (let ((rows (fin-ir-unmatched (plist-get ctx :receita))))
    (when rows
      (list nil (fin-dashboard--ir-detail-span
                 (format "%s with no Receita record"
                         (fin-dashboard--count (length rows) "payment"))
                 (mapconcat (lambda (r) (format "%s %s" (fin-dashboard--ir-date (nth 1 r)) (fin-dashboard--ir-money (nth 3 r))))
                            rows "; "))))))

(defun fin-dashboard--ir-wrong-account-line (ctx)
  "(OK HTML) for taxes paid from the other payer's account."
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
  "(OK HTML) per input the estimate still needs."
  (delq nil
        (list (unless fin-ir-retirement-plan
                (list nil "retirement plan unknown: set <b>fin-ir-retirement-plan</b> (only pgbl deducts)"))
              (unless fin-ir-full-books
                (list nil "full books unknown: set <b>fin-ir-full-books</b> (verified once the balance sheet is on file)")))))

(defun fin-dashboard--ir-lines (name title lines)
  "Block NAME of the LINES (OK HTML) not ok, nil when all are ok."
  (let ((open (cl-remove-if #'car lines)))
    (when open
      (fin-dashboard--block
       (format "%s <span class=\"neg\">⚠ %d</span>" name (length open))
       title
       (concat "<div class=\"health\">"
               (mapconcat (lambda (l) (fin-dashboard--health-line nil (cadr l))) open "")
               "</div>")))))

(defun fin-dashboard--duty-month-mm (month)
  "YYYY-MM as MM/YYYY."
  (format "%s/%s" (substring month 5 7) (substring month 0 4)))

(defun fin-dashboard--duty-alerts (m today)
  "Open items of month plist M as of TODAY: all once due, before that only a
DCTFWeb off the pró-labore."
  (let ((late (string< (plist-get m :due) today)))
    (cl-remove-if-not (lambda (o) (or late (string-search " off " o))) (fin-duty-open m))))

(defun fin-dashboard--duty-runs (months)
  "MONTHS (YYYY-MM, ascending) as runs: 01–08/2021, 04/2022."
  (let (runs)
    (dolist (m months)
      (if (and runs (equal (fin-duty--month+ (cdar runs) 1) m))
          (setcdr (car runs) m)
        (push (cons m m) runs)))
    (mapconcat (lambda (r)
                 (if (equal (car r) (cdr r))
                     (fin-dashboard--duty-month-mm (car r))
                   (format "%s–%s" (if (equal (substring (car r) 0 4) (substring (cdr r) 0 4))
                                       (substring (car r) 5 7)
                                     (fin-dashboard--duty-month-mm (car r)))
                           (fin-dashboard--duty-month-mm (cdr r)))))
               (nreverse runs) ", ")))

(defun fin-dashboard--duty-month-lines (months today)
  "(OK HTML) as of TODAY: per late item naming its MONTHS, then per month
whose DCTFWeb is off before due."
  (let (late errors)
    (dolist (m months)
      (let ((due (plist-get m :due)))
        (dolist (a (fin-dashboard--duty-alerts m today))
          (if (string< due today)
              (let ((cell (assoc a late)))
                (if cell (setcdr cell (append (cdr cell) (list (plist-get m :month))))
                  (setq late (append late (list (list a (plist-get m :month)))))))
            (push (list nil (format "fix before %s: %s %s" (fin-dashboard--ir-date due)
                                    (fin-dashboard--duty-month-mm (plist-get m :month))
                                    (fin-dashboard--esc a)))
                  errors)))))
    (append (mapcar (lambda (l) (list nil (format "%s late: %s" (fin-dashboard--esc (car l))
                                                  (fin-dashboard--duty-runs (cdr l)))))
                    late)
            (nreverse errors))))

(defun fin-dashboard--duty-year-rows (positions closed ctx)
  "`fin-duty-year' plists of the CLOSED POSITIONS from `fin-duty-since' on."
  (when fin-duty-since
    (let ((since (string-to-number (substring fin-duty-since 0 4))))
      (delq nil (mapcar (lambda (p)
                          (let ((d (cdr (cl-find p closed :key #'car :test #'eq))))
                            (and d (>= (plist-get p :year) since)
                                 (fin-duty-year (plist-get p :year) ctx d))))
                        positions)))))

(defun fin-dashboard--duty-room-line (positions)
  "(OK HTML) when this year's withdrawals exceed the profit by over R$ 1.000."
  (let* ((pos (car (last positions)))
         (room (and pos (fin-duty-room pos))))
    (when (and room (< (plist-get room :room) -100000))
      (list nil (format "%d: drew %s more than the company's profit so far; it is taxable unless the books show more"
                        (plist-get pos :year) (fin-dashboard--ir-money (- (plist-get room :room))))))))

(defun fin-dashboard--duty-lines (positions closed ctx)
  "(OK HTML) per late monthly or yearly obligation."
  (let ((today (format-time-string "%F")))
    (append (fin-dashboard--duty-month-lines (fin-duty-months today ctx) today)
            (mapcar (lambda (y) (list nil (format "%d: %s late" (plist-get y :year)
                                                  (string-join (fin-duty-year-open y today) ", "))))
                    (cl-remove-if-not (lambda (y) (fin-duty-year-open y today))
                                      (fin-dashboard--duty-year-rows positions closed ctx)))
            (mapcar (lambda (s) (list nil (concat "refresh " (fin-dashboard--esc s))))
                    (fin-duty-stale today))
            (list (fin-dashboard--duty-room-line positions)))))

(defun fin-dashboard--duty-cell (cents alerts prefixes)
  "CENTS (– when none), ⚠ titled by the ALERTS starting with any of PREFIXES."
  (let ((mine (cl-remove-if-not (lambda (a) (cl-some (lambda (p) (string-prefix-p p a)) prefixes)) alerts)))
    (list :raw (concat (if (and cents (> cents 0)) (fin-dashboard--money-str cents) "–")
                       (if mine
                           (format " <span class=\"neg\" title=\"%s\">⚠</span>"
                                   (fin-dashboard--esc (string-join mine ", ")))
                         "")))))

(defun fin-dashboard--duty-months-table (months today)
  "MONTHS: revenue declared, DAS paid (with DAS-MEI), accountant, planned
pró-labore and INSS paid, ⚠ where action is due."
  (fin-dashboard--table
   '("month" "revenue" "DAS" "accountant" "pró-labore" "INSS")
   (mapcar (lambda (m)
             (let ((alerts (fin-dashboard--duty-alerts m today)))
               (list (fin-dashboard--duty-month-mm (plist-get m :month))
                     (fin-dashboard--duty-cell (plist-get m :declared) alerts '("PGDAS-D"))
                     (fin-dashboard--duty-cell (plist-get m :das-paid) alerts '("DAS"))
                     (fin-dashboard--duty-cell (plist-get m :accounting) nil nil)
                     (fin-dashboard--duty-cell (plist-get m :pro-labore) alerts '("DCTFWeb"))
                     (fin-dashboard--duty-cell (plist-get m :inss-paid) alerts '("INSS")))))
           months)))

(defun fin-dashboard--years-alerts (months y today)
  "What a year needs acted on as of TODAY: its MONTHS with alerts and the
filings of Y (`fin-duty-year') late; empty when none."
  (let* ((n (cl-count-if (lambda (m) (fin-dashboard--duty-alerts m today)) months))
         (labels (append (and (> n 0) (list (format "%d month%s" n (if (= n 1) "" "s"))))
                         (mapcar (lambda (l) (concat l " late")) (and y (fin-duty-year-open y today))))))
    (if labels
        (format "<span class=\"neg\" title=\"%s\">⚠</span>" (fin-dashboard--esc (string-join labels ", ")))
      "")))

(defun fin-dashboard--years-filings (y exempt)
  "Books and DEFIS of Y (`fin-duty-year') as (LABEL CENTS), only those more
than R$ 1.000 off EXEMPT."
  (cl-loop for (key label) in '((:books "books") (:defis "DEFIS"))
           for cents = (and y (plist-get y key))
           when (and cents exempt (> (abs (- cents exempt)) 100000))
           collect (list label cents)))

(defun fin-dashboard--ir-limit (pos)
  "Exempt limit of POS in words: the presumed profit, or the books' when larger."
  (let ((presumed (plist-get pos :presumed)) (book (plist-get pos :book-profit)))
    (if (and book (> book (or presumed 0)))
        (format "books %s" (fin-dashboard--money book))
      (format "presumed %s" (fin-dashboard--money (or presumed 0))))))

(defun fin-dashboard--hover (text html)
  "HTML with TEXT as its hover title, or HTML alone when TEXT is nil."
  (if text (format "<span title=\"%s\">%s</span>" (fin-dashboard--esc text) html) html))

(defun fin-dashboard--years-revenue-hover (pos)
  "Revenue the bank received and NFS-e issued when off the PGDAS-D of POS, or nil."
  (let ((declared (plist-get pos :declared-revenue))
        (through (plist-get pos :declared-through)))
    (and through
         (let ((parts (delq nil (list (and (not (eql (plist-get pos :revenue-through) declared))
                                           (format "bank received %s" (fin-dashboard--money-str (plist-get pos :revenue-through))))
                                      (and (not (eql (plist-get pos :invoiced-through) declared))
                                           (format "NFS-e issued %s" (fin-dashboard--money-str (plist-get pos :invoiced-through))))))))
           (and parts (string-join parts " · "))))))

(defun fin-dashboard--years-tax-hover (pos profit filings)
  "Taxable, exempt and its limit, FILINGS ((LABEL CENTS) ...), deductions,
and PROFIT against the profit paid out, for POS."
  (let ((money #'fin-dashboard--money-str)
        (paid (or (plist-get pos :profit) 0))
        (model (plist-get pos :model)))
    (string-join
     (delq nil (append (list (format "taxable %s" (funcall money (or (plist-get pos :taxable) 0)))
                             (format "exempt %s, up to %s" (funcall money (or (plist-get pos :exempt) 0))
                                     (fin-dashboard--ir-limit pos)))
                       (mapcar (lambda (f) (format "%s %s" (car f) (funcall money (cadr f))))
                               (cl-remove-if (lambda (f) (eql (cadr f) (plist-get pos :book-profit))) filings))
                       (list (and model (format "deductions %s"
                                                (funcall money (or (plist-get pos (if (eq model 'complete) :complete :simplified)) 0))))
                             (and profit (format "profit %s, paid to you %s, kept %s"
                                                 (funcall money profit) (funcall money paid) (funcall money (- profit paid)))))))
     " · ")))

(defun fin-dashboard--years-block (positions closed ctx)
  "Years block, newest first, from `fin-duty-since' or the first position:
each year the totals of its months and its tax, expanding into the months."
  (let* ((today (format-time-string "%F"))
         (months (and ctx fin-duty-since (fin-duty-months today ctx)))
         (filed (and ctx (fin-dashboard--duty-year-rows positions closed ctx)))
         (now (string-to-number (substring today 0 4)))
         (first (apply #'min now (append (and fin-duty-since (list (string-to-number (substring fin-duty-since 0 4))))
                                         (mapcar (lambda (p) (plist-get p :year)) positions)))))
    (fin-dashboard--block
     "Years"
     "Company months totalled per year (revenue, DAS, accountant, pró-labore, INSS; due the 20th), the tax and the DIRPF; hover revenue and tax for their detail, ⚠ for what needs action"
     (fin-dashboard--alist
      '("year" "revenue" "DAS" "accountant" "pró-labore" "INSS" "tax" "DIRPF" "")
      (mapcar (lambda (yr)
                (let* ((p (cl-find yr positions :key (lambda (x) (plist-get x :year))))
                       (d (and p (cdr (cl-find p closed :key #'car :test #'eq))))
                       (ms (cl-remove-if-not (lambda (m) (string-prefix-p (format "%d-" yr) (plist-get m :month)))
                                             months))
                       (y (and ctx fin-duty-since
                               (or (cl-find yr filed :key (lambda (r) (plist-get r :year)))
                                   (fin-duty-year yr ctx nil))))
                       (sum (lambda (key) (apply #'+ (mapcar (lambda (m) (or (plist-get m key) 0)) ms))))
                       (cell (lambda (cents) (if (and cents (> cents 0)) (fin-dashboard--money-str cents) "")))
                       (profit (and p ms (- (funcall sum :declared) (funcall sum :das-paid)
                                            (funcall sum :accounting) (funcall sum :pro-labore)))))
                  (list (number-to-string yr)
                        (list (list :raw (fin-dashboard--hover (and p (fin-dashboard--years-revenue-hover p))
                                                               (funcall cell (funcall sum :declared))))
                              (funcall cell (funcall sum :das-paid))
                              (funcall cell (funcall sum :accounting))
                              (if (and y (plist-get y :mei)) "MEI" (funcall cell (funcall sum :pro-labore)))
                              (funcall cell (funcall sum :inss-paid))
                              (list :raw (if (and p (plist-get p :tax))
                                             (fin-dashboard--hover
                                              (fin-dashboard--years-tax-hover
                                               p profit (fin-dashboard--years-filings y (plist-get p :exempt)))
                                              (fin-dashboard--money-str (plist-get p :tax)))
                                           ""))
                              (fin-dashboard--ir-return-cell d)
                              (list :raw (fin-dashboard--years-alerts ms y today)))
                        (and ms (fin-dashboard--duty-months-table ms today)))))
              (number-sequence now first -1))))))

(defun fin-dashboard--ir-risk-lines (positions closed ctx)
  "Risk lines, (OK HTML) each."
  (delq nil (append (list (fin-dashboard--ir-debts-line))
                    (and ctx (fin-dashboard--duty-lines positions closed ctx))
                    (fin-dashboard--ir-return-risks closed)
                    (list (fin-dashboard--ir-exempt-line positions)
                          (fin-dashboard--ir-profit-line positions))
                    (and ctx (fin-dashboard--ir-revenue-lines ctx))
                    (list (and ctx (fin-dashboard--ir-rate-line ctx))
                          (and ctx (fin-dashboard--ir-unmatched-line ctx))
                          (and ctx (fin-dashboard--ir-wrong-account-line ctx))))))

(defun fin-dashboard--ir-rule-row (rule source lines ok)
  "Row RULE (SOURCE its tooltip): ⚠ when any of LINES (OK HTML) is not ok,
else ✓; the tooltip is the open lines, else all lines, else OK."
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
  "Rules table: each rule, its official source and status; Lei 15.270 rows
for the current year from 2026 on."
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
     (when (or positions fin-duty-since)
       (fin-dashboard--years-block positions closed ctx)))))

(provide 'irpf)
;;; irpf.el ends here
