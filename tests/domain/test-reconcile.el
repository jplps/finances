;;; test-reconcile.el --- domain/reconcile.el tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'helpers)
(require 'reconcile)

(defun test-reconcile--bank (id date type amount &optional desc)
  (list id date type amount (or desc "x")))

(defun test-reconcile--ledger (id date type amount &optional cat item)
  (list id date type (or cat "food") item amount nil))

(ert-deftest reconcile/day-rejects-non-iso ()
  (should-error (fin-reconcile--day "30/06/2025"))
  (should-error (fin-reconcile--day nil)))

(ert-deftest reconcile/day-spans-month-boundary ()
  (should (= 1 (- (fin-reconcile--day "2025-07-01")
                  (fin-reconcile--day "2025-06-30")))))

(ert-deftest reconcile/match-same-amount-within-window ()
  (let ((res (fin-reconcile-match
              (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
              (list (test-reconcile--ledger 1 "2025-06-12" "out" 1000))
              3)))
    (should (= 1 (length (plist-get res :matched))))
    (should (null (plist-get res :bank-only)))
    (should (null (plist-get res :entry-only)))))

(ert-deftest reconcile/outside-window-is-unmatched ()
  (let ((res (fin-reconcile-match
              (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
              (list (test-reconcile--ledger 1 "2025-06-14" "out" 1000))
              3)))
    (should (null (plist-get res :matched)))
    (should (= 1 (length (plist-get res :bank-only))))
    (should (= 1 (length (plist-get res :entry-only))))))

(ert-deftest reconcile/type-and-amount-must-agree ()
  (let* ((fin-reconcile-near-ratio 0)   ; exact pass only
         (fin-reconcile-near-close-ratio 0)
         (res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000)
                     (test-reconcile--bank "b2" "2025-06-10" "in"  2000))
               (list (test-reconcile--ledger 1 "2025-06-10" "out" 1001)
                     (test-reconcile--ledger 2 "2025-06-10" "out" 2000))
               3)))
    (should (null (plist-get res :matched)))
    (should (= 2 (length (plist-get res :bank-only))))
    (should (= 2 (length (plist-get res :entry-only))))))

(ert-deftest reconcile/ledger-row-matches-once ()
  ;; Two identical bank charges, one ledger row: second charge is missing.
  (let ((res (fin-reconcile-match
              (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000)
                    (test-reconcile--bank "b2" "2025-06-10" "out" 1000))
              (list (test-reconcile--ledger 1 "2025-06-10" "out" 1000))
              3)))
    (should (= 1 (length (plist-get res :matched))))
    (should (equal "b2" (car (car (plist-get res :bank-only)))))))

(ert-deftest reconcile/closest-date-wins ()
  (let* ((res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
               (list (test-reconcile--ledger 1 "2025-06-08" "out" 1000)
                     (test-reconcile--ledger 2 "2025-06-11" "out" 1000))
               3))
         (pair (car (plist-get res :matched))))
    (should (= 2 (car (cdr pair))))
    (should (equal '(1) (mapcar #'car (plist-get res :entry-only))))))

(ert-deftest reconcile/default-window-from-custom ()
  (let ((fin-reconcile-window 0))
    (should (null (plist-get
                   (fin-reconcile-match
                    (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
                    (list (test-reconcile--ledger 1 "2025-06-11" "out" 1000)))
                   :matched)))))

(ert-deftest reconcile/month-totals-per-month-and-type ()
  (let ((rows (fin-reconcile-month-totals
               (list (test-reconcile--bank "b1" "2020-03-05" "out" 1000)
                     (test-reconcile--bank "b2" "2020-03-20" "out" 500)
                     (test-reconcile--bank "b3" "2020-04-01" "in"  700))
               (list (test-reconcile--ledger 1 "2020-03-31" "out" 1400)))))
    (should (equal '(("2020-03" "out" 1500 1400)
                     ("2020-04" "in"   700    0))
                   rows))))

(ert-deftest reconcile/near-pairs-within-ratio-on-shared-word ()
  (let* ((fin-reconcile-near-ratio 0.2)
         (fin-reconcile-near-close-ratio 0.01)
         (res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2026-06-06" "out" 2665 "Linode . Akamai"))
               (list (test-reconcile--ledger 1 "2026-06-06" "out" 2741 "house" "linode"))
               3)))
    (should (null (plist-get res :matched)))
    (should (equal '("b1" . 1) (let ((p (car (plist-get res :near))))
                                 (cons (car (car p)) (car (cdr p))))))
    (should (null (plist-get res :bank-only)))
    (should (null (plist-get res :entry-only)))))

(ert-deftest reconcile/near-rejects-beyond-ratio-window-or-type ()
  (let* ((fin-reconcile-near-ratio 0.1)
         (res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
               (list (test-reconcile--ledger 1 "2025-06-10" "out" 1200)  ; 16.7% gap
                     (test-reconcile--ledger 2 "2025-06-20" "out" 1010)  ; too far
                     (test-reconcile--ledger 3 "2025-06-10" "in"  1010)) ; other type
               3)))
    (should (null (plist-get res :near)))
    (should (= 1 (length (plist-get res :bank-only))))
    (should (= 3 (length (plist-get res :entry-only))))))

(ert-deftest reconcile/exact-beats-near-and-closest-amount-wins ()
  (let* ((fin-reconcile-near-ratio 0.2)
         (fin-reconcile-near-close-ratio 0.2)
         (res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000)
                     (test-reconcile--bank "b2" "2025-06-10" "out" 2000))
               (list (test-reconcile--ledger 1 "2025-06-10" "out" 1000)  ; exact b1
                     (test-reconcile--ledger 2 "2025-06-10" "out" 1900)
                     (test-reconcile--ledger 3 "2025-06-10" "out" 2050)) ; closer to b2
               3)))
    (should (equal '(("b1" . 1)) (mapcar (lambda (p) (cons (caar p) (cadr p)))
                                         (plist-get res :matched))))
    (should (equal '(("b2" . 3)) (mapcar (lambda (p) (cons (caar p) (cadr p)))
                                         (plist-get res :near))))
    (should (equal '(2) (mapcar #'car (plist-get res :entry-only))))))

(ert-deftest reconcile/zero-ratio-disables-near ()
  (let ((fin-reconcile-near-ratio 0)
        (fin-reconcile-near-close-ratio 0))
    (should (null (plist-get
                   (fin-reconcile-match
                    (list (test-reconcile--bank "b1" "2025-06-10" "out" 1000))
                    (list (test-reconcile--ledger 1 "2025-06-10" "out" 1001))
                    3)
                   :near)))))

(ert-deftest reconcile/bad-ratio-fails-loud ()
  (let ((fin-reconcile-near-ratio 2))
    (should-error (fin-reconcile-match nil nil 3)))
  (let ((fin-reconcile-near-ratio 0.05)
        (fin-reconcile-near-close-ratio 0.1))   ; close beyond near
    (should-error (fin-reconcile-match nil nil 3))))

(ert-deftest reconcile/words-fold-case-and-accents ()
  (should (equal '("komprao" "koch") (fin-reconcile--words "KOMPRÃO Koch - SA")))
  (should (null (fin-reconcile--words nil)))
  (should (fin-reconcile--share-word-p "Compra no débito - KOMPRAO KOCH" "komprão"))
  (should-not (fin-reconcile--share-word-p "Dl*Google Wavesn" "recarga vivo")))

(ert-deftest reconcile/near-beyond-close-needs-shared-word ()
  (let* ((fin-reconcile-near-ratio 0.05)
         (fin-reconcile-near-close-ratio 0.01)
         (res (fin-reconcile-match
               (list (test-reconcile--bank "b1" "2025-05-10" "out" 2390 "Dl*Google Wavesn")
                     (test-reconcile--bank "b2" "2025-05-11" "out" 24975 "Mp *Auvpescola"))
               (list (test-reconcile--ledger 1 "2025-05-10" "out" 2500 "free" "recarga vivo")
                     (test-reconcile--ledger 2 "2025-05-11" "out" 24980 "personal" "escola"))
               3)))
    ;; b1: 4.4% gap, no shared word → rejected.  b2: 0.02% gap → close enough.
    (should (equal '(("b2" . 2)) (mapcar (lambda (p) (cons (caar p) (cadr p)))
                                         (plist-get res :near))))
    (should (equal '("b1") (mapcar #'car (plist-get res :bank-only))))))

(ert-deftest reconcile/net-folds-income-and-deduction-per-month ()
  (let ((out (fin-reconcile-net
              (list (test-reconcile--ledger 1 "2026-08-05" "in"  1015500 "bp")
                    (test-reconcile--ledger 2 "2026-08-05" "out"  139900 "cnpj")
                    (test-reconcile--ledger 3 "2026-08-06" "out"    5000 "food")
                    (test-reconcile--ledger 4 "2026-09-04" "in"  1015500 "bp"))
              '(("bp" . "cnpj")))))
    (should (equal '(((net "2026-08" "bp") "2026-08-05" "in" "bp" "net of cnpj" 875600 nil)
                     (3 "2026-08-06" "out" "food" nil 5000 nil)
                     ;; September has no deduction: left untouched.
                     (4 "2026-09-04" "in" "bp" nil 1015500 nil))
                   out))))

(ert-deftest reconcile/net-sums-multiple-rows-in-month ()
  (let ((out (fin-reconcile-net
              (list (test-reconcile--ledger 1 "2026-10-01" "in"  1000 "bp")
                    (test-reconcile--ledger 2 "2026-10-15" "in"   500 "bp")
                    (test-reconcile--ledger 3 "2026-10-10" "out"  200 "cnpj")
                    (test-reconcile--ledger 4 "2026-10-20" "out"  100 "cnpj"))
              '(("bp" . "cnpj")))))
    (should (= 1 (length out)))
    (should (equal "2026-10-01" (nth 1 (car out))))
    (should (= 1200 (nth 5 (car out))))))

(ert-deftest reconcile/net-keeps-rows-when-not-positive ()
  (let ((rows (list (test-reconcile--ledger 1 "2026-10-01" "in"  100 "bp")
                    (test-reconcile--ledger 2 "2026-10-10" "out" 100 "cnpj"))))
    (should (equal rows (fin-reconcile-net rows '(("bp" . "cnpj")))))))

(ert-deftest reconcile/net-rejects-bad-rules ()
  (should-error (fin-reconcile-net nil '(("bp" . cnpj)))))

(ert-deftest reconcile/net-row-matches-bank-deposit ()
  (let* ((ledger (fin-reconcile-net
                  (list (test-reconcile--ledger 1 "2026-08-05" "in"  1015500 "bp")
                        (test-reconcile--ledger 2 "2026-08-05" "out"  139900 "cnpj"))
                  '(("bp" . "cnpj"))))
         (res (fin-reconcile-match
               (list (test-reconcile--bank "s" "2026-08-05" "in" 875600))
               ledger 3)))
    (should (= 1 (length (plist-get res :matched))))
    (should (null (plist-get res :entry-only)))))

(provide 'test-reconcile)
;;; test-reconcile.el ends here
