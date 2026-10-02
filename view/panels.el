;;; panels.el --- Per-panel renderers + drilldown helpers  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)
(require 'html)
(require 'charts)
;; domain
(require 'cashflow)
(require 'budget)
(require 'goals)
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

(defun fin-dashboard--tick (label value &optional n)
  "One ticker item: LABEL, VALUE (HTML), and an up/down triangle when N is a
signed number (drawn in CSS so it centers on the text)."
  (format "<span class=\"tick\"><b>%s</b> %s%s</span>"
          label
          (cond ((not (numberp n)) "")
                ((> n 0) "<span class=\"arrow up pos\" role=\"img\" aria-label=\"up\"></span> ")
                ((< n 0) "<span class=\"arrow down neg\" role=\"img\" aria-label=\"down\"></span> ")
                (t ""))
          value))

(defun fin-dashboard--records-items (recs nw)
  "Ticker items for the headline RECS and net worth NW."
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
     (fin-dashboard--tick "net worth" (format "R$ %s" (fin-dashboard--money-str (round nw))) nw)
     (when best       (fin-dashboard--tick "best month"
                                           (format "%s R$ %s" (nth 0 best) (fin-dashboard--k-signed (nth 1 best)))
                                           (nth 1 best)))
     (when worst      (fin-dashboard--tick "worst month"
                                           (format "%s R$ %s" (nth 0 worst) (fin-dashboard--k-signed (nth 1 worst)))
                                           (nth 1 worst)))
     (when biggest-in (fin-dashboard--tick "biggest income"
                                           (format "%s R$ %s" (fin-dashboard--ym (nth 2 biggest-in))
                                                   (fin-dashboard--k-signed (nth 1 biggest-in)))))
     (when biggest    (fin-dashboard--tick "biggest purchase"
                                           (format "%s R$ %s" (fin-dashboard--ym (nth 2 biggest))
                                                   (fin-dashboard--k (nth 1 biggest)))))
     (when best-save  (fin-dashboard--tick "best yearly save"
                                           (format "%d %s" (nth 0 best-save)
                                                   (fin-dashboard--pct-signed (or (nth 1 best-save) 0)))
                                           (nth 1 best-save)))
     (when avg-save   (fin-dashboard--tick "avg yearly save" (fin-dashboard--pct-signed avg-save) avg-save))
     (when rec-share  (fin-dashboard--tick "recurring share" (format "%.1f%%" rec-share)))
     (when first-e    (fin-dashboard--tick "since" (fin-dashboard--ym first-e)))
     (when months     (fin-dashboard--tick "months" (format "%d" months))))))

(defun fin-dashboard--records-ticker ()
  "LED ticker tape of the records: the items twice, so the loop is seamless."
  (let ((items (fin-dashboard--records-items (fin-report--records) (fin-report--patrimony-total))))
    (concat "<div class=\"ticker\" role=\"marquee\" aria-label=\"Records\" title=\"Records: headline numbers across all tracked time\">"
            "<div class=\"window\"><div class=\"tape\"><span>" items "</span><span aria-hidden=\"true\">" items "</span></div></div></div>")))

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
                                                  (fin-dashboard--k-cell (nth 3 r)))) rec-cnt)))
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
                           (fin-dashboard--svg-line netw (fin-dashboard--c 'pos))
                           "networth")
     (fin-dashboard--block "Rolling save %"
                           "Trailing 12-month save rate per month — smooths year boundaries."
                           (fin-dashboard--svg-line
                            (mapcar (lambda (r) (list (nth 0 r) (or (nth 1 r) 0))) rsave)
                            (fin-dashboard--c 'line-2))
                           "rsave")
     (fin-dashboard--block "Income vs expense (12mo MA)"
                           "12-month moving average of in (green) and out (red) — reveals lifestyle creep."
                           (fin-dashboard--svg-multiline
                            (list (list "in"  (fin-dashboard--c 'pos)
                                        (mapcar (lambda (r) (list (nth 0 r) (or (nth 1 r) 0))) rflow))
                                  (list "out" (fin-dashboard--c 'neg)
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

;;; ── objectives: plan targets vs this month ─────────────────

(defun fin-dashboard--goal-row (r)
  (let* ((cat (plist-get r :category)) (target (plist-get r :target)) (mtd (plist-get r :mtd))
         (title (format "%s · this month R$ %s of R$ %s target"
                        cat (fin-dashboard--grouped mtd) (fin-dashboard--grouped target))))
    (format "<tr class=\"goal%s\"><td class=\"cat\">%s</td><td class=\"viz\">%s</td><td class=\"num\">%s</td><td class=\"num dim\">%s</td></tr>"
            (pcase (plist-get r :depth) (1 " child") (2 " child deep") (_ ""))
            (fin-dashboard--esc cat)
            (fin-dashboard--svg-bullet target mtd (eq (plist-get r :direction) 'save) title)
            (fin-dashboard--grouped mtd)
            (fin-dashboard--grouped target))))

(defun fin-dashboard--goals-table (rows)
  (concat "<table class=\"goals\"><thead><tr><th>category</th><th class=\"viz\">progress</th>"
          "<th class=\"num\">month</th><th class=\"num\">target</th></tr></thead><tbody>"
          (mapconcat #'fin-dashboard--goal-row rows "")
          "</tbody></table>"))

(defun fin-dashboard--kpi (label value note &optional cls)
  (format "<div class=\"kpi%s\"><div class=\"kpi-label\">%s</div><div class=\"kpi-value\">%s</div><div class=\"kpi-note\">%s</div></div>"
          (if cls (concat " " cls) "") label value note))

(defun fin-dashboard--panel-objectives ()
  (let* ((year   (fin-report--year-now))
         (month  (fin-report--month-now))
         (liquid (nth 2 (fin-report--monthly-income year)))
         (budg   (fin-report--budget-share year))
         (rw     (fin-report--runway))
         (goals  (fin-goals year month))
         (pct    (lambda (kind) (cl-reduce #'+ (mapcar (lambda (b) (if (equal (nth 1 b) kind) (or (nth 3 b) 0) 0)) budg))))
         (fix-pct (funcall pct "fix"))
         (var-pct (funcall pct "var"))
         (unalloc (- 100.0 fix-pct var-pct))
         (kind-rows (lambda (kind) (cl-remove-if-not (lambda (r) (equal (plist-get r :kind) kind)) goals)))
         (over (cl-count 'over goals :key (lambda (r) (plist-get r :status)))))
    (fin-dashboard--panel
     "Objectives" "objectives"
     "Plan targets per month against the current month so far"
     "<div class=\"kpis\">"
     (fin-dashboard--kpi "Monthly liquid" (concat "R$ " (fin-dashboard--grouped liquid))
                         (if (< (abs unalloc) 0.05) "fully allocated"
                           (format "%.1f%% (R$ %s) unallocated" unalloc
                                   (fin-dashboard--grouped (round (* liquid unalloc 0.01))))))
     (fin-dashboard--kpi "Fixed share" (format "%.1f%%" fix-pct)
                         (format "target ≤ %d%%" fin-budget-fix-target)
                         (if (<= fix-pct fin-budget-fix-target) "good" "bad"))
     (fin-dashboard--kpi "Runway" (format "%.1f mo" (nth 2 rw)) "emergency reserve / fixed costs")
     (fin-dashboard--kpi "Over target" (format "%d of %d" over (length goals))
                         (format "in %s" (fin-dashboard--month-name month))
                         (if (zerop over) "good" "bad"))
     "</div>"
     (fin-dashboard--block (format "Fixed <span class=\"dim\">%.1f%% / %d%%</span>" fix-pct fin-budget-fix-target)
                           "Fixed monthly outflows, driven by amount"
                           (fin-dashboard--goals-table (funcall kind-rows "fix")))
     (fin-dashboard--block (format "Variable <span class=\"dim\">%.1f%% / %d%%</span>" var-pct fin-budget-var-target)
                           "Variable allocations, driven by share of liquid; investments funds the indented rows"
                           (fin-dashboard--goals-table (funcall kind-rows "var"))))))

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
     (fin-dashboard--alist '("category" "balance" "%") rows
                           (list "total"
                                 (fin-dashboard--money-cell
                                  (apply #'+ (mapcar (lambda (r) (or (nth 1 r) 0)) accts)))
                                 "")))))

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
     (fin-dashboard--alist '("category" "items" "amount" "%") rows
                           (list "total" "" (fin-dashboard--money-cell (round total-monthly)) "")))))

(defun fin-dashboard--month-body (items)
  "Category drilldown for a month's items."
  (let* ((month-sum (apply #'+ (mapcar (lambda (r) (or (nth 2 r) 0)) items)))
         (groups    (fin-dashboard--group-by items 1))
         (ranked    (mapcar
                     (lambda (g)
                       (let* ((cat (car g))
                              (rs  (cdr g))   ; query already date-ordered; group-by preserves it
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
                           '("item" "date" "amount")
                           (mapcar (lambda (r) (list (nth 0 r)
                                                     (fin-dashboard--dm (nth 3 r))
                                                     (fin-dashboard--money-cell (nth 2 r))))
                                   rs)))))
                sorted)))
    (fin-dashboard--alist '("category" "items" "out" "%") rows)))

(defun fin-dashboard--year-effective-months (year now-y now-m)
  "(M IN OUT LIQ FUTURE?) per month for YEAR.
Future months of the current year use the objectives forecast
\(`fin-report--planned-month'); all other months are actuals."
  (let ((planned (and (= year now-y) (fin-report--planned-month year))))
    (mapcar
     (lambda (r)
       (let* ((m       (car r))
              (future? (and (= year now-y) (numberp m) (> m now-m))))
         (if (and future? planned)
             (list m (nth 0 planned) (nth 1 planned) (nth 2 planned) t)
           (list m (nth 1 r) (nth 2 r) (nth 3 r) nil))))
     (fin-report--year-months year))))

(defun fin-dashboard--monthly-block (year eff now-y now-m)
  "Accordion <table> of YEAR's effective months EFF (see
`fin-dashboard--year-effective-months')."
  (let ((rows (mapcar
               (lambda (e)
                 (let* ((m       (nth 0 e))
                        (future? (nth 4 e))
                        (items   (and (not future?) (fin-report--month-items year m))))
                   (list (fin-dashboard--month-name m)
                         (list (fin-dashboard--money-cell (nth 1 e))
                               (fin-dashboard--money-cell (nth 2 e))
                               (fin-dashboard--money-signed-cell (nth 3 e)))
                         (when items (fin-dashboard--month-body items))
                         (when future? "future")
                         (and (= year now-y) (numberp m) (= m now-m)))))
               eff)))
    (fin-dashboard--alist '("month" "in" "out" "liquid") rows)))

(defun fin-dashboard--panel-cashflow ()
  (let* ((now-y (fin-report--year-now))
         (now-m (fin-report--month-now))
         (rows  (mapcar
                 (lambda (r)
                   (let* ((y    (nth 0 r))
                          (cur  (= y now-y))
                          (eff  (fin-dashboard--year-effective-months y now-y now-m))
                          ;; Current year reconciles with its month rows by
                          ;; summing actuals + forecast; past years are actuals.
                          (in   (if cur (apply #'+ (mapcar (lambda (e) (or (nth 1 e) 0)) eff)) (nth 1 r)))
                          (out  (if cur (apply #'+ (mapcar (lambda (e) (or (nth 2 e) 0)) eff)) (nth 2 r)))
                          (liq  (if cur (- in out) (nth 3 r)))
                          (sav  (if cur
                                    (and (> in 0)
                                         (/ (fround (/ (* 1000.0 (- in out)) in)) 10.0))
                                  (nth 4 r))))
                     (list (number-to-string y)
                           (list (fin-dashboard--money-cell in)
                                 (fin-dashboard--money-cell out)
                                 (fin-dashboard--money-signed-cell liq)
                                 sav)
                           (fin-dashboard--monthly-block y eff now-y now-m)
                           nil
                           cur)))
                 (reverse (fin-report--annual-sums)))))   ; newest year first
    (fin-dashboard--panel
     "Cashflow" "cashflow"
     "Year → month → category drilldown of in/out/liquid"
     (fin-dashboard--alist '("year" "in" "out" "liquid" "%") rows))))

(provide 'panels)
;;; panels.el ends here
