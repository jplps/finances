;;; test-goals.el --- domain/goals.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'goals)

(ert-deftest goals/status-by-direction ()
  (should (eq 'within  (fin-goals-status 'spend 1000 1000)))
  (should (eq 'over    (fin-goals-status 'spend 1000 1001)))
  (should (eq 'over    (fin-goals-status 'spend 0 10)))
  (should (eq 'pending (fin-goals-status 'save 1000 999)))
  (should (eq 'reached (fin-goals-status 'save 1000 1000))))

(ert-deftest goals/measures-the-month-so-far ()
  (fin-test-with-db
    (fin-test-insert-budget "brute" nil "income" 1000000 nil)
    (fin-test-insert-budget "food" nil "fix" 300000 nil)
    (fin-test-insert-budget "retirement" nil "var" nil 0.1)
    (fin-test-insert-entry "2026-02-10" "out" "food" 400000 "x")
    (fin-test-insert-entry "2026-03-05" "out" "food" 50000 "x")
    (fin-test-insert-entry "2026-03-04" "out" "retirement" 100000)
    (let* ((rows (fin-goals 2026 3))
           (get (lambda (c) (cl-find c rows :key (lambda (r) (plist-get r :category)) :test #'equal)))
           (food (funcall get "food"))
           (ret  (funcall get "retirement")))
      (should (= 300000 (plist-get food :target)))
      (should (= 50000  (plist-get food :mtd)))     ; February ignored
      (should (eq 'within (plist-get food :status)))
      (should (= 100000 (plist-get ret :target)))   ; 10% of liquid 10000
      (should (eq 'reached (plist-get ret :status)))
      (should (eq 'save (plist-get ret :direction))))))

(ert-deftest goals/children-measure-their-sources ()
  (fin-test-with-db
    (fin-test-insert-budget "brute" nil "income" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.2)
    (fin-test-insert-budget "emergency" "investments" "var" nil 0.5)
    (fin-test-insert-budget "family" "investments" "var" nil 0.5)
    (fin-test-insert-entry "2026-01-04" "out" "investments" 80000 "emergency")
    (fin-test-insert-entry "2026-01-20" "in"  "reserve" 30000)
    (fin-test-insert-entry "2026-01-08" "out" "family" 25000 "latam")
    (let* ((rows (fin-goals 2026 1))
           (get (lambda (c) (cl-find c rows :key (lambda (r) (plist-get r :category)) :test #'equal))))
      (should (equal "investments" (plist-get (funcall get "emergency") :parent)))
      (should (= 50000 (plist-get (funcall get "emergency") :mtd)))   ; 800 in, 300 back
      (should (= 25000 (plist-get (funcall get "family") :mtd)))
      ;; Funds its children: 800 contributed - 300 withdrawn + 250 family.
      (should (= 75000 (plist-get (funcall get "investments") :mtd))))))

(ert-deftest goals/patrimony-splits-into-register-classes ()
  (fin-test-with-db
    (fin-test-insert-budget "brute" nil "income" 1000000 nil)
    (fin-test-insert-budget "investments" nil "var" nil 0.2)
    (fin-test-insert-patrimony "car" "tires" 120000 12)        ; 10000/mo
    (fin-test-insert-patrimony "accessory" "skateboard" 120000 24) ; 5000/mo
    (fin-test-insert-entry "2026-03-02" "out" "car" 3000 "gasolina")
    (fin-test-insert-entry "2026-03-03" "out" "patrimony" 4000 "skate")
    (fin-test-insert-entry "2026-03-04" "out" "patrimony" 500 "mystery")
    (let* ((fin-goals-patrimony-classes '(("skate" . "accessory")))
           (rows (fin-goals 2026 3))
           (get (lambda (c) (cl-find c rows :key (lambda (r) (plist-get r :category)) :test #'equal))))
      (should (= 3000 (plist-get (funcall get "car") :mtd)))
      (should (= 10000 (plist-get (funcall get "car") :target)))
      (should (= 4000 (plist-get (funcall get "accessory") :mtd)))
      (should (= 2 (plist-get (funcall get "accessory") :depth)))
      (should (= 500 (plist-get (funcall get "unmapped") :mtd)))
      (should (eq 'over (plist-get (funcall get "unmapped") :status)))
      ;; Classes sit right after their parent row.
      (should (equal '("patrimony" "car" "accessory" "unmapped")
                     (seq-take (member "patrimony" (mapcar (lambda (r) (plist-get r :category)) rows)) 4))))))

(provide 'test-goals)
;;; test-goals.el ends here
