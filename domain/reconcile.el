;;; reconcile.el --- Match bank transactions against ledger entries  -*- lexical-binding: t; -*-

;; Pure functions over row lists; no DB access.
;;
;; Bank row:   (id date type amount description)
;; Ledger row: (id date type category item amount "k/n"-or-nil)
;;
;; A bank row matches a ledger row with the same type and amount whose date
;; lies within `fin-reconcile-window' days.  Each ledger row matches at most
;; once; among candidates the closest date wins, ties by ledger order.
;;
;; Leftovers then pair as near matches: same type, within the window, amounts
;; differing by at most `fin-reconcile-near-ratio', and either a word shared
;; by bank description and ledger item or a gap within
;; `fin-reconcile-near-close-ratio'.  Closest amount wins.  Likely typos,
;; shown for review, never auto-fixed.
;;
;; Before matching, `fin-reconcile-net' folds each month's gross income and
;; its withheld deductions (item-less rows) into one net row, since the bank
;; only sees the net.  A deduction with an item was paid on its own.

(require 'cl-lib)
(require 'calendar)
(require 'ucs-normalize)

(defcustom fin-reconcile-window 3
  "Max day distance between a bank row and its matching ledger row."
  :type 'natnum :group 'fin)

(defcustom fin-reconcile-near-ratio 0.05
  "Max relative amount gap, against the larger amount, for a near match."
  :type 'number :group 'fin)

(defcustom fin-reconcile-near-close-ratio 0.01
  "Relative amount gap under which a near match needs no shared word."
  :type 'number :group 'fin)

(defcustom fin-reconcile-net-rules '(("bp" . "cnpj"))
  "Ledger (INCOME-CATEGORY . DEDUCTION-CATEGORY) pairs netted per month.
The bank receives income minus deductions as one deposit."
  :type '(alist :key-type string :value-type string) :group 'fin)

(defun fin-reconcile--day (date)
  "Absolute day number of ISO DATE (YYYY-MM-DD)."
  (unless (and (stringp date)
               (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
                             date))
    (error "fin-reconcile: bad date %S" date))
  (calendar-absolute-from-gregorian
   (list (string-to-number (match-string 2 date))
         (string-to-number (match-string 3 date))
         (string-to-number (match-string 1 date)))))

(defun fin-reconcile--index (ledger keyfn)
  "Hash (KEYFN row) -> ledger rows, in LEDGER order."
  (let ((idx (make-hash-table :test #'equal)))
    (dolist (row (reverse ledger))
      (push row (gethash (funcall keyfn row) idx)))
    idx))

(defun fin-reconcile--best (bank-row candidates used window score)
  "Unused row of CANDIDATES within WINDOW days of BANK-ROW with the lowest
SCORE, or nil.  SCORE takes (bank-row ledger-row day-distance), returns a
number or nil to reject.  Ties keep the earlier candidate."
  (let ((day (fin-reconcile--day (nth 1 bank-row)))
        best best-score)
    (dolist (row candidates)
      (unless (gethash (car row) used)
        (let* ((dist (abs (- day (fin-reconcile--day (nth 1 row)))))
               (sc   (and (<= dist window) (funcall score bank-row row dist))))
          (when (and sc (or (null best-score) (< sc best-score)))
            (setq best row best-score sc)))))
    best))

(defun fin-reconcile--pair (bank idx keyfn used window score)
  "Pair each BANK row with its best candidate from IDX under (KEYFN row).
Mark paired ledger ids in USED.  Return (PAIRS . UNPAIRED-BANK)."
  (let (pairs rest)
    (dolist (b bank)
      (let ((hit (fin-reconcile--best b (gethash (funcall keyfn b) idx)
                                      used window score)))
        (if (not hit)
            (push b rest)
          (puthash (car hit) t used)
          (push (cons b hit) pairs))))
    (cons (nreverse pairs) (nreverse rest))))

(defun fin-reconcile--exact-score (_bank-row _row dist)
  "Closest date wins."
  dist)

(defun fin-reconcile--words (s)
  "Words of S: lowercase, accents stripped, at least 3 chars."
  (when s
    (let ((plain (apply #'string
                        (cl-remove-if
                         (lambda (c) (eq (get-char-code-property c 'general-category) 'Mn))
                         (string-to-list (ucs-normalize-NFD-string (downcase s)))))))
      (cl-remove-if (lambda (w) (< (length w) 3))
                    (split-string plain "[^[:alnum:]]+" t)))))

(defun fin-reconcile--share-word-p (a b)
  "Non-nil if strings A and B share a word per `fin-reconcile--words'."
  (cl-intersection (fin-reconcile--words a) (fin-reconcile--words b)
                   :test #'string=))

(defun fin-reconcile--near-score (ratio close)
  "Score by amount gap; reject equal amounts and gaps beyond RATIO.
Gaps beyond CLOSE also need a word shared by description and item."
  (lambda (bank-row row _dist)
    (let* ((a   (nth 3 bank-row))
           (big (max a (nth 5 row)))
           (gap (abs (- a (nth 5 row)))))
      (and (> gap 0)
           (<= gap (* ratio big))
           (or (<= gap (* close big))
               (fin-reconcile--share-word-p (nth 4 bank-row) (nth 4 row)))
           gap))))

(defun fin-reconcile-match (bank ledger &optional window)
  "Match BANK rows against LEDGER rows.
WINDOW defaults to `fin-reconcile-window'.  Return a plist:
  :matched     list of (bank-row . ledger-row), same amount
  :near        list of (bank-row . ledger-row), amounts differ
  :bank-only   bank rows with no ledger match (missing in the ODS)
  :entry-only  ledger rows with no bank match (cash, other banks, typos)
Every input row lands in exactly one bucket."
  (let* ((window (or window fin-reconcile-window))
         (ratio  fin-reconcile-near-ratio)
         (close  fin-reconcile-near-close-ratio)
         (used   (make-hash-table :test #'equal)))
    (cl-assert (natnump window) nil "fin-reconcile: window must be natnum")
    (cl-assert (and (numberp ratio) (<= 0 ratio 1)) nil
               "fin-reconcile: near ratio must be in [0, 1]")
    (cl-assert (and (numberp close) (<= 0 close ratio)) nil
               "fin-reconcile: close ratio must be in [0, near ratio]")
    (let* ((sorted (sort (copy-sequence bank)
                         (lambda (x y) (string< (nth 1 x) (nth 1 y)))))
           (exact  (fin-reconcile--pair
                    sorted
                    (fin-reconcile--index ledger (lambda (r) (cons (nth 2 r) (nth 5 r))))
                    (lambda (b) (cons (nth 2 b) (nth 3 b)))
                    used window #'fin-reconcile--exact-score))
           (near   (fin-reconcile--pair
                    (cdr exact)
                    (fin-reconcile--index
                     (cl-remove-if (lambda (r) (gethash (car r) used)) ledger)
                     (lambda (r) (nth 2 r)))
                    (lambda (b) (nth 2 b))
                    used window (fin-reconcile--near-score ratio close)))
           (result (list :matched    (car exact)
                         :near       (car near)
                         :bank-only  (cdr near)
                         :entry-only (cl-remove-if (lambda (r) (gethash (car r) used))
                                                   ledger))))
      (cl-assert (= (+ (* 2 (length (plist-get result :matched)))
                       (* 2 (length (plist-get result :near)))
                       (length (plist-get result :bank-only))
                       (length (plist-get result :entry-only)))
                    (+ (length bank) (length ledger)))
                 nil "fin-reconcile: rows lost in matching")
      result)))

(defun fin-reconcile--net-groups (ledger rules)
  "Hash (YYYY-MM . income-category) -> (INCOMES . DEDUCTIONS) for RULES.
Deductions are item-less rows.  Only months holding both are kept."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (r ledger)
      (pcase-let ((`(,_id ,date ,type ,cat ,item . ,_) r))
        (dolist (rule rules)
          (let ((key (cons (substring date 0 7) (car rule))))
            (cond ((and (equal type "in") (equal cat (car rule)))
                   (push r (car (or (gethash key groups)
                                    (puthash key (cons nil nil) groups)))))
                  ((and (equal type "out") (equal cat (cdr rule)) (null item))
                   (push r (cdr (or (gethash key groups)
                                    (puthash key (cons nil nil) groups))))))))))
    (maphash (lambda (key g)
               (if (and (car g) (cdr g))
                   (puthash key (cons (nreverse (car g)) (nreverse (cdr g))) groups)
                 (remhash key groups)))
             groups)
    groups))

(defun fin-reconcile--net-row (key incomes deductions rule)
  "Synthetic ledger row: INCOMES minus DEDUCTIONS of month KEY, or nil if
not positive.  Dated at the first income; id is a list, never a DB id."
  (let ((net (- (apply #'+ (mapcar (lambda (r) (nth 5 r)) incomes))
                (apply #'+ (mapcar (lambda (r) (nth 5 r)) deductions)))))
    (when (> net 0)
      (list (list 'net (car key) (car rule)) (nth 1 (car incomes)) "in"
            (car rule) (format "net of %s" (cdr rule)) net nil))))

(defun fin-reconcile-net (ledger &optional rules)
  "LEDGER with each month's income and deduction rows per RULES replaced
by one net income row, in place of the first income row.  RULES default
to `fin-reconcile-net-rules'.  Months whose net is not positive stay as is."
  (let* ((rules (or rules fin-reconcile-net-rules))
         (groups (fin-reconcile--net-groups ledger rules))
         (first  (make-hash-table :test #'equal))
         (gone   (make-hash-table :test #'equal)))
    (cl-assert (cl-every (lambda (r) (and (consp r) (stringp (car r)) (stringp (cdr r))))
                         rules)
               nil "fin-reconcile: net rules must be (string . string) pairs")
    (maphash (lambda (key g)
               (let ((row (fin-reconcile--net-row key (car g) (cdr g)
                                                  (assoc (cdr key) rules))))
                 (when row
                   (puthash (car (caar g)) row first)
                   (dolist (r (append (car g) (cdr g)))
                     (puthash (car r) t gone)))))
             groups)
    (let (out)
      (dolist (r ledger)
        (cond ((gethash (car r) first) (push (gethash (car r) first) out))
              ((not (gethash (car r) gone)) (push r out))))
      (nreverse out))))

(defcustom fin-reconcile-shift-window 14
  "Max day distance for a ledger row logged on a date other than the bank's."
  :type 'natnum :group 'fin)

(defcustom fin-reconcile-exact-window 31
  "Max day distance for an exact, non-round amount of at least
`fin-reconcile-exact-min' to count as the same transaction."
  :type 'natnum :group 'fin)

(defcustom fin-reconcile-exact-min 10000
  "Smallest amount, in cents, for the exact-amount rule."
  :type 'natnum :group 'fin)

(defun fin-reconcile--distinct-amount-p (cents)
  "Non-nil if CENTS is large and has cents, so unlikely to coincide."
  (and (>= cents fin-reconcile-exact-min) (/= 0 (% cents 100))))

(defun fin-reconcile-installment (s)
  "\"k/n\" of an installment: bank \"... Parcela 3/10\" or ledger \"3/10\"; or nil."
  (when (and s (or (string-match "Parcela \\([0-9]+\\)/\\([0-9]+\\)" s)
                   (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" s)))
    (format "%s/%s" (match-string 1 s) (match-string 2 s))))

(defun fin-reconcile--shift-pick (b entries used related)
  "First unused row of ENTRIES that is the same transaction as bank row B.
Either the amounts are equal and distinct within the exact window; or the
same installment k/n within the exact window; or they differ by at most
`fin-reconcile-near-close-ratio' within the shift window and RELATED holds
for (B row).  The ledger names installments after the item bought, the bank
after the merchant, and dates the first one at purchase, the bank at bill."
  (let ((day (fin-reconcile--day (nth 1 b))) (a (nth 3 b))
        (inst (fin-reconcile-installment (nth 4 b))))
    (cl-find-if
     (lambda (l)
       (and (not (gethash (car l) used))
            (equal (nth 2 l) (nth 2 b))
            (let ((dist (abs (- day (fin-reconcile--day (nth 1 l)))))
                  (gap  (abs (- a (nth 5 l)))))
              (or (and (= gap 0) (fin-reconcile--distinct-amount-p a)
                       (<= dist fin-reconcile-exact-window))
                  (and inst (equal inst (fin-reconcile-installment (nth 6 l)))
                       (<= dist fin-reconcile-exact-window)
                       (<= gap (* fin-reconcile-near-close-ratio (max a (nth 5 l)))))
                  (and (<= dist fin-reconcile-shift-window)
                       (<= gap (* fin-reconcile-near-close-ratio (max a (nth 5 l))))
                       (funcall related b l))))))
     entries)))

(defun fin-reconcile-shifted (bank-only entry-only related)
  "Pair leftovers whose ledger date differs from the bank's.
BANK-ONLY and ENTRY-ONLY are the leftovers of `fin-reconcile-match'.
RELATED is a predicate on (bank-row ledger-row).  Return a plist with
:shifted (pairs), :bank-only and :entry-only."
  (cl-assert (functionp related))
  (let ((used (make-hash-table :test #'equal)) pairs rest)
    (dolist (b bank-only)
      (let ((l (fin-reconcile--shift-pick b entry-only used related)))
        (if (not l)
            (push b rest)
          (puthash (car l) t used)
          (push (cons b l) pairs))))
    (list :shifted (nreverse pairs)
          :bank-only (nreverse rest)
          :entry-only (cl-remove-if (lambda (l) (gethash (car l) used)) entry-only))))

(defun fin-reconcile-month-totals (bank ledger)
  "Per (month, type) sums of BANK and LEDGER amounts.
Return sorted rows (YYYY-MM type bank-sum ledger-sum).  Meaningful where
the ledger holds monthly lumps instead of single purchases."
  (let ((sums (make-hash-table :test #'equal))
        rows)
    (dolist (b bank)
      (let ((key (cons (substring (nth 1 b) 0 7) (nth 2 b))))
        (puthash key (cons (+ (or (car (gethash key sums)) 0) (nth 3 b))
                           (or (cdr (gethash key sums)) 0))
                 sums)))
    (dolist (l ledger)
      (let ((key (cons (substring (nth 1 l) 0 7) (nth 2 l))))
        (puthash key (cons (or (car (gethash key sums)) 0)
                           (+ (or (cdr (gethash key sums)) 0) (nth 5 l)))
                 sums)))
    (maphash (lambda (key v) (push (list (car key) (cdr key) (car v) (cdr v)) rows))
             sums)
    (sort rows (lambda (x y)
                 (or (string< (car x) (car y))
                     (and (string= (car x) (car y))
                          (string< (nth 1 x) (nth 1 y))))))))

(provide 'reconcile)
;;; reconcile.el ends here
