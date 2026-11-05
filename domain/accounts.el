;;; accounts.el --- Account balances rebuilt from the bank  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'bankdb)
(require 'fmt)

(defcustom fin-accounts-derived nil
  "Account plists rebuilt from bank statements.
:name; :reserve t counts toward the emergency reserve; either :balance (a
bank account kind whose latest LEDGERBAL is the balance) or :into and
:out-of (regexps of personal-account descriptions moving money in and
out), counted after :since (ISO date; default all) on top of :opening
\(cents; default 0).  :closed t: balance 0, the note gives the gain or
loss.  :yield (cents) adds earnings the statements never show."
  :type '(repeat plist) :group 'fin)

(defvar fin-accounts--memo nil
  "When a cons, its car caches `fin-accounts--balances'.")

(defun fin-accounts--bank-sum (type regexps since)
  "Cents of personal-account bank rows of TYPE after SINCE matching REGEXPS."
  (let ((total 0))
    (dolist (r (fin-db-query "SELECT amount, description FROM bank_txn
                               WHERE account = 'account' AND type = ? AND date > ?"
                             (list type since)))
      (when (cl-some (lambda (re) (string-match-p re (cadr r))) regexps)
        (cl-incf total (car r))))
    total))

(defun fin-accounts--first-flow (regexps since)
  "Date of the first personal-account row after SINCE matching REGEXPS, or nil."
  (caar (cl-remove-if-not (lambda (r) (cl-some (lambda (re) (string-match-p re (cadr r))) regexps))
                          (fin-db-query "SELECT date, description FROM bank_txn
                                          WHERE account = 'account' AND date > ? ORDER BY date"
                                        (list since)))))

(defun fin-accounts--derived-balance (spec)
  "(CENTS . NOTE) of account SPEC, NOTE stating the basis."
  (let ((kind (plist-get spec :balance))
        (since (or (plist-get spec :since) ""))
        (opening (or (plist-get spec :opening) 0)))
    (cond (kind (let ((b (fin-bankdb-latest-balance kind)))
                  (and b (cons (cdr b) (fin-dashboard--date (car b))))))
          ((plist-get spec :closed)
           (let ((result (- (fin-accounts--bank-sum "in" (plist-get spec :out-of) since)
                            (fin-accounts--bank-sum "out" (plist-get spec :into) since))))
             (cons 0 (format "closed, %s %s" (if (< result 0) "loss" "gain") (fin-dashboard--money-str (abs result))))))
          ((plist-get spec :yield)
           (let ((b (fin-accounts--derived-balance (cl-list* :yield nil spec))))
             (cons (+ (car b) (plist-get spec :yield))
                   (format "%s + yield %s" (cdr b) (fin-dashboard--money-str (plist-get spec :yield))))))
          (t (cl-assert (and (plist-get spec :into) (stringp since) (integerp opening)) nil
                        "fin-accounts: %S needs :balance or :into (and a valid :since, :opening)"
                        (plist-get spec :name))
             (cons (+ opening
                      (fin-accounts--bank-sum "out" (plist-get spec :into) since)
                      (- (fin-accounts--bank-sum "in" (plist-get spec :out-of) since)))
                   (let ((from (or (and (zerop opening)
                                        (fin-accounts--first-flow (plist-get spec :into) since))
                                   since)))
                     (if (string-empty-p from) "since the first statement"
                       (concat "since " (fin-dashboard--date from)))))))))

(defun fin-accounts--bank-saved (period)
  "Cents moved into `fin-accounts-derived' accounts in PERIOD, net.
PERIOD is \"YYYY\" or \"YYYY-MM\"; flows on or before an account's :since
are excluded."
  (cl-assert (string-match-p "\\`[0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)?\\'" period) nil
             "fin-accounts: bad period %S" period)
  (fin-bankdb-ensure)
  (let ((total 0))
    (dolist (r (fin-db-query "SELECT date, type, amount, description FROM bank_txn
                               WHERE account = 'account' AND substr(date, 1, length(?1)) = ?1"
                             (list period))
               total)
      (pcase-let* ((`(,date ,type ,amount ,desc) r)
                   (key (if (equal type "out") :into :out-of)))
        (when (cl-some (lambda (spec)
                         (and (string> date (or (plist-get spec :since) ""))
                              (cl-some (lambda (re) (string-match-p re desc)) (plist-get spec key))))
                       fin-accounts-derived)
          (cl-incf total (if (equal type "out") amount (- amount))))))))

(defun fin-accounts--balances ()
  "Plists :name :cents :note :cash :reserve :closed per account.
Cached in `fin-accounts--memo' when bound."
  (or (car fin-accounts--memo)
      (let ((rows (fin-accounts--compute)))
        (when fin-accounts--memo (setcar fin-accounts--memo rows))
        rows)))

(defun fin-accounts--compute ()
  "Uncached `fin-accounts--balances'."
  (fin-bankdb-ensure)
  (mapcar (lambda (spec)
            (cl-assert (stringp (plist-get spec :name)) nil "fin-accounts: bad spec %S" spec)
            (let ((d (fin-accounts--derived-balance spec)))
              (list :name (plist-get spec :name) :cents (or (car d) 0) :note (or (cdr d) "no statement")
                    :cash (and (plist-get spec :balance) t) :reserve (plist-get spec :reserve)
                    :closed (plist-get spec :closed))))
          fin-accounts-derived))

(defun fin-accounts--total (&optional key)
  "Cents across accounts, only those with KEY when given."
  (apply #'+ (mapcar (lambda (a) (if (or (null key) (plist-get a key)) (plist-get a :cents) 0))
                     (fin-accounts--balances))))

(provide 'accounts)
;;; accounts.el ends here
