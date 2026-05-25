;;; test-patrimony.el --- domain/patrimony.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'patrimony)

(ert-deftest patrimony/empty-db ()
  (fin-test-with-db
    (should (null (fin-report--patrimony-summary)))
    (should (null (fin-report--patrimony-items)))
    (should (= 0 (fin-report--patrimony-total)))))

(ert-deftest patrimony/summary-groups-by-category ()
  (fin-test-with-db
    (fin-test-insert-patrimony "car"  "wheels"  1200000 60)  ; 20000/mo
    (fin-test-insert-patrimony "car"  "stereo"  600000  60)  ; 10000/mo
    (fin-test-insert-patrimony "house" "fridge" 240000 120)  ; 2000/mo
    (let ((rows (fin-report--patrimony-summary)))
      (should (= 2 (length rows)))
      ;; car ranks first by total monthly (30000 > 2000)
      (let ((car (car rows)))
        (should (equal "car" (nth 0 car)))
        (should (= 2 (nth 1 car)))
        (should (= 1800000 (nth 2 car)))
        (should (= 30000 (nth 3 car)))))))

(ert-deftest patrimony/items-sorted-by-monthly-desc-per-category ()
  (fin-test-with-db
    (fin-test-insert-patrimony "car" "stereo"   600000 60)   ; 10000/mo
    (fin-test-insert-patrimony "car" "wheels"  1200000 60)   ; 20000/mo
    (let* ((rows (fin-report--patrimony-items))
           (first  (car rows))
           (second (cadr rows)))
      ;; wheels (20000/mo) before stereo (10000/mo)
      (should (equal "wheels" (nth 1 first)))
      (should (equal "stereo" (nth 1 second))))))

(ert-deftest patrimony/items-lifespan-zero-monthly-nil ()
  "NULLIF(lifespan,0) → monthly cell becomes nil, no /0 crash."
  (fin-test-with-db
    (fin-test-insert-patrimony "junk" "x" 100000 0)
    (let ((row (car (fin-report--patrimony-items))))
      (should (null (nth 4 row))))))

(ert-deftest patrimony/total-sums-top-level-accounts-only ()
  (fin-test-with-db
    (fin-test-insert-account "main"     nil          1000000 nil)
    (fin-test-insert-account "emergency" nil         500000  nil)
    (fin-test-insert-account "sub"      "main"       999999  nil) ; child, excluded
    (should (= 1500000 (fin-report--patrimony-total)))))

(provide 'test-patrimony)
;;; test-patrimony.el ends here
