;;; test-accounts.el --- domain/accounts.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'accounts)

(ert-deftest accounts/top-level-only ()
  (fin-test-with-db
    (fin-test-insert-account "main"      nil    400000 nil)
    (fin-test-insert-account "emergency" nil    600000 nil)
    (fin-test-insert-account "main-sub"  "main" 100000 nil)  ; child excluded
    (let ((rows (fin-report--accounts)))
      (should (= 2 (length rows)))
      ;; emergency (600k) ranks first → 60%, main → 40%
      (should (equal "emergency" (nth 0 (car rows))))
      (should (= 60.0 (nth 2 (car rows))))
      (should (= 40.0 (nth 2 (cadr rows)))))))

(ert-deftest accounts/empty-no-crash ()
  (fin-test-with-db
    (should (null (fin-report--accounts)))))

(ert-deftest accounts/children-shares-of-parent ()
  (fin-test-with-db
    (fin-test-insert-account "emergency"   nil          1000000 nil)
    (fin-test-insert-account "emergency-a" "emergency"  600000  nil)
    (fin-test-insert-account "emergency-b" "emergency"  400000  nil)
    (let ((rows (fin-report--account-children "emergency")))
      (should (= 2 (length rows)))
      (should (= 60.0 (nth 2 (car rows))))
      (should (= 40.0 (nth 2 (cadr rows)))))))

(ert-deftest accounts/children-zero-parent-balance-no-crash ()
  "NULLIF guards against division by zero when parent balance = 0."
  (fin-test-with-db
    (fin-test-insert-account "emergency"   nil         0      nil)
    (fin-test-insert-account "emergency-a" "emergency" 100000 nil)
    (let ((row (car (fin-report--account-children "emergency"))))
      (should (null (nth 2 row))))))

(provide 'test-accounts)
;;; test-accounts.el ends here
