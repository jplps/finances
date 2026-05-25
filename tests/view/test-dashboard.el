;;; test-dashboard.el --- view/dashboard.el integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'dashboard)

(defun fin-test--render-html ()
  (cl-letf (((symbol-function 'file-attributes)
             (lambda (_) nil))
            ((symbol-function 'file-attribute-modification-time)
             (lambda (_) (current-time))))
    (fin-dashboard--render)))

(ert-deftest dashboard/renders-on-empty-db ()
  (fin-test-with-db
    (let ((html (fin-test--render-html)))
      (should (string-match-p "<!doctype html>" html))
      (should (string-match-p "</html>" html))
      (should (string-match-p "STATS\\|Stats\\|stats"      html))
      (should (string-match-p "OBJECTIVES\\|Objectives"    html))
      (should (string-match-p "ACCOUNTS\\|Accounts"        html))
      (should (string-match-p "PATRIMONY\\|Patrimony"      html))
      (should (string-match-p "CASHFLOW\\|Cashflow"        html)))))

(ert-deftest dashboard/parses-as-html-via-libxml ()
  (fin-test-with-db
    (let ((html (fin-test--render-html)))
      (with-temp-buffer
        (insert html)
        (should (libxml-parse-html-region (point-min) (point-max)))))))

(ert-deftest dashboard/renders-with-real-data ()
  (fin-test-with-db
    (fin-test-seed-multi-year)
    (let ((html (fin-test--render-html)))
      ;; All panel titles emitted
      (dolist (panel '("Records" "Pareto" "Net worth" "Spend heatmap"))
        (should (string-match-p panel html))))))

(ert-deftest dashboard/renders-with-budget-tree ()
  (fin-test-with-db
    (fin-test-seed-multi-year)
    (fin-test-seed-budget-tree)
    (let ((html (fin-test--render-html)))
      ;; Synthetic `patrimony' leaf must appear inside Var investments drilldown.
      (should (string-match-p "patrimony" html)))))

(provide 'test-dashboard)
;;; test-dashboard.el ends here
