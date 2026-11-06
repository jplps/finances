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
      (should (string-match-p "Cockpit"                   html))
      (should (string-match-p "To do"                     html))
      (should (string-search "no entries yet"             html))
      (should (string-search "<section id=\"wealth\""    html))
      (should (string-search "<section id=\"spending\""  html))
      (should (string-search "No entries yet"             html)))))

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
      (dolist (panel '("Records" "Pareto" "Net worth" "Heatmap"))
        (should (string-match-p panel html))))))

(ert-deftest dashboard/renders-with-budget-tree ()
  (fin-test-with-db
    (fin-test-seed-multi-year)
    (fin-test-seed-budget-tree)
    (let ((html (fin-test--render-html)))
      (should (string-match-p "patrimony" html)))))

(ert-deftest dashboard/status-bar-links-every-panel ()
  (fin-test-with-db
    (let ((html (fin-test--render-html)))
      (dolist (n (mapcar #'car (fin-dashboard--shown-tabs)))
        (should (string-search (format "<section id=\"%s\"" n) html))
        (should (string-search (format "href=\"#%s\"" n) html))))))

(ert-deftest dashboard/no-ir-panel-without-tax-data ()
  (fin-test-with-db
    (cl-letf (((symbol-function 'fin-dashboard--ir-years) #'ignore))
      (let* ((fin-duty-since nil)
             (html (fin-test--render-html)))
        (should-not (string-search "<section id=\"ir\"" html))
        (should-not (string-search "<a href=\"#ir\"" html))))))

(ert-deftest dashboard/goal-row-shows-month-and-plan-red-when-over ()
  (let ((none (make-hash-table :test #'equal)))
    (let ((row (fin-dashboard--goal-row '(:category "food" :target 300000 :mtd 160300) none)))
      (should (string-search "num pos\">1,603.00<" row))
      (should (string-search ">3,000.00<" row))
      (should-not (string-search "1,397.00" row)))
    (should (string-search "num neg\">162.00<"
                           (fin-dashboard--goal-row '(:category "wear" :target 16186 :mtd 16200) none)))))

(ert-deftest dashboard/saving-goal-row-shows-only-the-plan ()
  (let ((row (fin-dashboard--goal-row '(:category "retirement" :target 116000 :mtd 50000 :direction save)
                                      (make-hash-table :test #'equal))))
    (should (string-search ">1,160.00<" row))
    (should-not (string-search "500.00" row))))

(ert-deftest dashboard/cockpit-tiles-month-savings-runway ()
  (fin-test-with-db
    (let ((html (fin-dashboard--panel-cockpit)))
      (should (string-search "saved" html))
      (should (string-match-p "in [0-9.,]+<br>out [0-9.,]+<br>saved " html))
      (should (string-search "runway" html))
      (should-not (string-search "kpi-label\">plan<" html))
      (should-not (string-search "Over target" html)))))

(ert-deftest dashboard/patrimony-header-names-monthly-column ()
  (fin-test-with-db
    (fin-test-insert-patrimony "car" "wheels" 1200000 60)
    (should (string-search "<th>items</th><th>monthly</th>" (fin-dashboard--block-patrimony)))))

(ert-deftest dashboard/ir-rule-row-status ()
  (let ((status (lambda (lines ok) (cadr (nth 1 (fin-dashboard--ir-rule-row "r" "s" lines ok))))))
    (should (equal "<span title=\"none\"><span class=\"pos\">✓</span></span>" (funcall status nil "none")))
    (should (string-search "title=\"a · b\"><span class=\"pos\">" (funcall status '((t "a") (t "b")) "none")))
    (should (string-search "title=\"b; 1 &gt; 0\"><span class=\"neg\">"
                           (funcall status '((t "a") (nil "<b>b</b>") (nil "1 &gt; 0")) "none"))))
  (should (equal "<span title=\"s\">a &amp; b</span>"
                 (cadr (car (fin-dashboard--ir-rule-row "a & b" "s" nil "none"))))))

(provide 'test-dashboard)
;;; test-dashboard.el ends here
