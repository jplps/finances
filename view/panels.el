;;; panels.el --- Per-panel renderers + drilldown helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)
(require 'html)
(require 'charts)
;; domain
(require 'cashflow)
(require 'budget)
(require 'patrimony)
(require 'accounts)
(require 'stats)

(defun fin-dashboard--group-by (rows key-idx)
  "Return alist (KEY . ROWS) preserving first-seen order."
  (let (out)
    (dolist (r rows)
      (let* ((k (nth key-idx r))
             (cell (assoc k out)))
        (if cell (setcdr cell (append (cdr cell) (list r)))
          (push (cons k (list r)) out))))
    (nreverse out)))

(defun fin-dashboard--records-dl (recs nw)
  (let ((biggest    (plist-get recs :biggest))
        (biggest-in (plist-get recs :biggest-in))
        (best       (plist-get recs :best-mo))
        (worst      (plist-get recs :worst-mo))
        (avg-save   (plist-get recs :avg-save))
        (best-save  (plist-get recs :best-save))
        (rec-share  (plist-get recs :rec-share))
        (first-e    (plist-get recs :first-entry))
        (months     (plist-get recs :months-tracked)))
    (concat
     "<dl class=\"kv runway\">"
     (when first-e    (fin-dashboard--kv "First entry"     (fin-dashboard--ym first-e)))
     (when months     (fin-dashboard--kv "Months tracked"  (format "%d" months)))
     (when best       (fin-dashboard--kv "Best month"
                                         (format "%s — R$ %s" (nth 0 best)
                                                 (fin-dashboard--k-signed (nth 1 best)))))
     (when worst      (fin-dashboard--kv "Worst month"
                                         (format "%s — R$ %s" (nth 0 worst)
                                                 (fin-dashboard--k-signed (nth 1 worst)))))
     (when biggest-in (fin-dashboard--kv "Biggest income"
                                         (format "%s — R$ %s"
                                                 (fin-dashboard--ym (nth 2 biggest-in))
                                                 (fin-dashboard--k-signed (nth 1 biggest-in)))))
     (when biggest    (fin-dashboard--kv "Biggest purchase"
                                         (format "%s — R$ %s"
                                                 (fin-dashboard--ym (nth 2 biggest))
                                                 (fin-dashboard--k-signed (nth 1 biggest)))))
     (when best-save  (fin-dashboard--kv "Best yearly save"
                                         (format "%d — %s" (nth 0 best-save)
                                                 (fin-dashboard--pct-signed (or (nth 1 best-save) 0)))))
     (when avg-save   (fin-dashboard--kv "Avg yearly save" (fin-dashboard--pct-signed avg-save)))
     (when rec-share  (fin-dashboard--kv "Recurring share" (format "%.1f%%" rec-share)))
     (fin-dashboard--kv "Net worth" (format "R$ %s" (fin-dashboard--money-str (round nw))))
     "</dl>")))

(defun fin-dashboard--panel-stats ()
  (let* ((cats    (fin-report--cat-shares))
         (inc     (fin-report--income-shares))
         (rec     (fin-report--top-recurring 10 6))
         (rec-cnt (fin-report--top-by-count 10 3))
         (saves   (fin-report--yearly-saves))
         (netw    (fin-report--cumulative-networth))
         (rsave   (fin-report--rolling-save-rate))
         (rflow   (fin-report--rolling-flow))
         (pareto  (fin-report--pareto-spending 20))
         (heat    (fin-report--monthly-spend-grid))
         (cat-slices (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r))) cats))
         (inc-slices (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r))) inc))
         (save-rows  (mapcar (lambda (r) (list (format "%d" (nth 0 r))
                                               (or (nth 1 r) 0)))
                             saves))
         (rec-rows   (mapcar (lambda (r) (list (nth 0 r) (nth 1 r)
                                               (fin-dashboard--k-cell (nth 2 r))
                                               (fin-dashboard--k-cell (nth 3 r)))) rec))
         (rec-cnt-rows (mapcar (lambda (r) (list (nth 0 r) (nth 1 r)
                                                  (fin-dashboard--k-cell (nth 2 r))
                                                  (fin-dashboard--k-cell (nth 3 r)))) rec-cnt))
         (recs (fin-report--records))
         (nw   (fin-report--patrimony-total)))
    (fin-dashboard--panel
     "Stats" "stats" "Headline metrics across all tracked entries"
     (fin-dashboard--block "All time flow"
                           "Per-month in (green up) and out (red down) from zero baseline. Future months dimmed."
                           (fin-dashboard--svg-flow (fin-report--monthly-flow))
                           "flow")
     "<div class=\"stats-body\">"
     "<div class=\"stats-col\">"
     (fin-dashboard--block "Net worth"
                           "Cumulative liquid (Σ in − Σ out) per month — cash-only net worth proxy."
                           (fin-dashboard--svg-line netw (fin-dashboard--c 'green))
                           "networth")
     (fin-dashboard--block "Rolling save %"
                           "Trailing 12-month save rate per month — smooths year boundaries."
                           (fin-dashboard--svg-line
                            (mapcar (lambda (r) (list (nth 0 r) (or (nth 1 r) 0))) rsave)
                            (fin-dashboard--c 'amber))
                           "rsave")
     (fin-dashboard--block "Income vs expense (12mo MA)"
                           "12-month moving average of in (green) and out (red) — reveals lifestyle creep."
                           (fin-dashboard--svg-multiline
                            (list (list "in"  (fin-dashboard--c 'green)
                                        (mapcar (lambda (r) (list (nth 0 r) (or (nth 1 r) 0))) rflow))
                                  (list "out" (fin-dashboard--c 'red)
                                        (mapcar (lambda (r) (list (nth 0 r) (or (nth 2 r) 0))) rflow))))
                           "rflow")
     (fin-dashboard--block "Pareto"
                           "Top 20 items by spend with cumulative % of total out."
                           (fin-dashboard--svg-pareto pareto)
                           "pareto")
     (fin-dashboard--block "Yearly save %"
                           "Per-year save rate: (in - out) / in × 100. Green = positive, red = negative."
                           (if save-rows (fin-dashboard--svg-bars save-rows) "")
                           "yearly")
     (fin-dashboard--block "Spend heatmap"
                           "Year × month spending intensity — reveals seasonality."
                           (fin-dashboard--svg-heatmap heat)
                           "heatmap")
     "</div>"
     "<div class=\"stats-col\">"
     (fin-dashboard--block "Category share"
                           "All-time outflow share by category (summary rows only)"
                           (fin-dashboard--svg-donut cat-slices)
                           "cat")
     (fin-dashboard--block "Income share"
                           "All-time inflow share by source (CNPJ tax stored as negative shows net of taxes)"
                           (fin-dashboard--svg-donut inc-slices)
                           "inc")
     "</div>"
     "<div class=\"stats-col\">"
     (fin-dashboard--block "Records"
                           "Headline numbers across all tracked time"
                           (fin-dashboard--records-dl recs nw)
                           "records")
     (fin-dashboard--block "Recurring by months"
                           "Recurring items ranked by distinct months active (persistence)"
                           (fin-dashboard--table '("item" "months" "total" "avg") rec-rows)
                           "recurring")
     (fin-dashboard--block "Recurring all time"
                           "Items by raw entry count — most-frequent purchases."
                           (fin-dashboard--table '("item" "count" "total" "avg") rec-cnt-rows)
                           "recurring-count")
     "</div>"
     "</div>")))

(defun fin-dashboard--panel-objectives ()
  (let* ((year    (fin-report--year-now))
         (liquid  (fin-report--monthly-liquid year))
         (budg    (fin-report--budget-share year))
         (rw      (fin-report--runway))
         (fix-pct (if (> liquid 0) (* 100.0 (/ (nth 1 rw) liquid)) 0))
         (var-pct (if (> liquid 0) (* 100.0 (/ (fin-report--var-total year) liquid)) 0))
         (fix-cls (if (<= fix-pct fin-budget-fix-target) "pos" "neg"))
         (var-cls (if (<= var-pct fin-budget-var-target) "pos" "neg"))
         (fix     (cl-remove-if-not (lambda (b) (equal (nth 1 b) "fix")) budg))
         (var     (cl-remove-if-not (lambda (b) (equal (nth 1 b) "var")) budg))
         (mkrow   (lambda (b)
                    (let* ((cat  (nth 0 b))
                           (kids (fin-report--budget-children cat year)))
                      (list cat
                            (list (fin-dashboard--money-cell (nth 2 b))
                                  (nth 3 b))
                            (when kids
                              (fin-dashboard--table
                               '("category" "amount" "%")
                               (mapcar (lambda (k) (list (nth 0 k)
                                                         (fin-dashboard--money-cell (nth 1 k))
                                                         (nth 2 k)))
                                       kids))))))))
    (fin-dashboard--panel
     "Objectives" "objectives"
     "Budget plan as share of monthly liquid, fix pressure, and emergency runway"
     (format "<p class=\"sub\">Plan · liquid <b>R$ %s</b> · runway <b>%.1f months</b></p>"
             (fin-dashboard--money-str (round liquid)) (nth 2 rw))
     (fin-dashboard--pair
      (fin-dashboard--block
       (format "Fix · <span class=\"%s\">%.1f%%</span> <span class=\"dim\">/ %d%%</span>"
               fix-cls fix-pct fin-budget-fix-target)
       "Fixed monthly outflows (driven by amount)"
       (fin-dashboard--alist '("category" "amount" "%") (mapcar mkrow fix)))
      (fin-dashboard--block
       (format "Var · <span class=\"%s\">%.1f%%</span> <span class=\"dim\">/ %d%%</span>"
               var-cls var-pct fin-budget-var-target)
       "Variable allocations (driven by share of liquid); expand to see sub-allocations"
       (fin-dashboard--alist '("category" "amount" "%") (mapcar mkrow var)))))))

(defun fin-dashboard--panel-accounts ()
  (let* ((accts (fin-report--accounts))
         (rows  (mapcar
                 (lambda (r)
                   (let* ((cat  (nth 0 r))
                          (kids (fin-report--account-children cat)))
                     (list cat
                           (list (fin-dashboard--money-cell (nth 1 r))
                                 (nth 2 r))
                           (when kids
                             (fin-dashboard--table
                              '("category" "balance" "%")
                              (mapcar (lambda (k) (list (nth 0 k)
                                                        (fin-dashboard--money-cell (nth 1 k))
                                                        (nth 2 k)))
                                      kids))))))
                 accts)))
    (fin-dashboard--panel
     "Accounts" "accounts"
     "Account balances; expand a row to see sub-accounts"
     (fin-dashboard--alist '("category" "balance" "%") rows))))

(defun fin-dashboard--panel-patrimony ()
  (let* ((summary       (fin-report--patrimony-summary))
         (items         (fin-report--patrimony-items))
         (by-cat        (fin-dashboard--group-by items 0))
         (total-monthly (apply #'+ (mapcar (lambda (r) (or (nth 3 r) 0)) summary)))
         (idx-by-cat    (let ((h (make-hash-table :test 'equal)))
                          (dolist (g by-cat) (puthash (car g) (cdr g) h))
                          h))
         (rows (mapcar
                (lambda (r)
                  (let* ((cat     (nth 0 r))
                         (cat-mo  (or (nth 3 r) 0))
                         (kids    (gethash cat idx-by-cat)))
                    (list cat
                          (list (nth 1 r)
                                (fin-dashboard--money-cell cat-mo)
                                (format "%.1f" (if (> total-monthly 0)
                                                   (* 100.0 (/ cat-mo (float total-monthly))) 0)))
                          (when kids
                            (fin-dashboard--table
                             '("item" "amount" "lifespan" "monthly" "%")
                             (mapcar (lambda (k)
                                       (let ((mo (or (nth 4 k) 0)))
                                         (list (nth 1 k)
                                               (fin-dashboard--money-cell (nth 2 k))
                                               (nth 3 k)
                                               (fin-dashboard--money-cell mo)
                                               (format "%.1f" (if (> cat-mo 0)
                                                                  (* 100.0 (/ mo (float cat-mo))) 0)))))
                                     kids))))))
                summary)))
    (fin-dashboard--panel
     "Patrimony" "patrimony"
     "Owned items: cost, lifespan, monthly amortization (cost / lifespan); % share of total monthly"
     (format "<p class=\"sub\">Total monthly amortization: <b>R$ %s</b></p>"
             (fin-dashboard--money-str (round total-monthly)))
     (fin-dashboard--alist '("category" "items" "total" "%") rows))))

(defun fin-dashboard--month-body (items)
  "Category drilldown for a month's items."
  (let* ((month-sum (apply #'+ (mapcar (lambda (r) (or (nth 2 r) 0)) items)))
         (groups    (fin-dashboard--group-by items 1))
         (ranked    (mapcar
                     (lambda (g)
                       (let* ((cat (car g))
                              (rs  (sort (copy-sequence (cdr g))
                                         (lambda (a b) (> (or (nth 2 a) 0)
                                                          (or (nth 2 b) 0)))))
                              (tot (apply #'+ (mapcar (lambda (r) (or (nth 2 r) 0)) rs))))
                         (list cat rs tot)))
                     groups))
         (sorted    (sort ranked (lambda (a b) (> (nth 2 a) (nth 2 b)))))
         (rows (mapcar
                (lambda (g)
                  (let* ((cat (nth 0 g)) (rs (nth 1 g)) (tot (nth 2 g))
                         (cnt (length rs)))
                    (list cat
                          (list (number-to-string cnt)
                                (fin-dashboard--money-cell (round tot))
                                (format "%.1f" (if (> month-sum 0)
                                                   (* 100.0 (/ tot (float month-sum))) 0)))
                          (fin-dashboard--table
                           '("item" "amount")
                           (mapcar (lambda (r) (list (nth 0 r)
                                                     (fin-dashboard--money-cell (nth 2 r))))
                                   rs)))))
                sorted)))
    (fin-dashboard--alist '("category" "items" "total" "%") rows)))

(defun fin-dashboard--monthly-block (year months now-y now-m)
  "Year's monthly data as accordion <table>.  Future months use plan-derived placeholders."
  (let* ((planned (and (= year now-y) (fin-report--planned-month year)))
         (rows (mapcar
                (lambda (r)
                  (let* ((m       (car r))
                         (future? (and (= year now-y) (numberp m) (> m now-m)))
                         (in      (if (and future? planned) (nth 0 planned) (nth 1 r)))
                         (out     (if (and future? planned) (nth 1 planned) (nth 2 r)))
                         (liq     (if (and future? planned) (nth 2 planned) (nth 3 r)))
                         (items   (and (not future?) (fin-report--month-items year m))))
                    (list (fin-dashboard--month-name m)
                          (list (fin-dashboard--money-cell in)
                                (fin-dashboard--money-cell out)
                                (fin-dashboard--money-signed-cell liq))
                          (when items (fin-dashboard--month-body items))
                          (when future? "future")
                          (and (= year now-y) (numberp m) (= m now-m)))))
                months)))
    (fin-dashboard--alist '("month" "in" "out" "liquid") rows)))

(defun fin-dashboard--year-content (year now-y now-m)
  (fin-dashboard--monthly-block year (fin-report--year-months year) now-y now-m))

(defun fin-dashboard--panel-cashflow ()
  (let* ((now-y (fin-report--year-now))
         (now-m (fin-report--month-now))
         (rows  (mapcar
                 (lambda (r)
                   (let ((y (nth 0 r)))
                     (list (number-to-string y)
                           (list (fin-dashboard--money-cell (nth 1 r))
                                 (fin-dashboard--money-cell (nth 2 r))
                                 (fin-dashboard--money-signed-cell (nth 3 r))
                                 (nth 4 r))
                           (fin-dashboard--year-content y now-y now-m)
                           nil
                           (= y now-y))))
                 (fin-report--annual-sums))))
    (fin-dashboard--panel
     "Cashflow" "cashflow"
     "Year → month → category drilldown of in/out/liquid"
     (fin-dashboard--alist '("year" "in" "out" "liquid" "%") rows))))

(provide 'panels)
;;; panels.el ends here
