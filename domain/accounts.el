;;; accounts.el --- Account balances rebuilt from the bank  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'bankdb)

(defcustom fin-accounts-derived nil
  "Accounts rebuilt from the bank statements, the source of truth for
balances.  Each is a plist: :name, :reserve t when it counts toward the
emergency reserve (runway), and either :balance (a bank account kind whose
latest LEDGERBAL is the balance) or :since (ISO date of a known balance),
:opening (cents on that date), :into and :out-of (regexps of
personal-account descriptions moving money into and out of it).  :closed t
marks an account emptied for good: its balance is 0 and the note gives the
result (money taken out less money put in, a gain or a loss).  :yield
\(cents) adds the earnings the statements never show, as the bank app or
the yearly informe gives them."
  :type '(repeat plist) :group 'fin)

(defun fin-report--bank-sum (type regexps since)
  "Cents of personal-account bank rows of TYPE after SINCE matching REGEXPS."
  (let ((total 0))
    (dolist (r (fin-db-query "SELECT amount, description FROM bank_txn
                               WHERE account = 'account' AND type = ? AND date > ?"
                             (list type since)))
      (when (cl-some (lambda (re) (string-match-p re (cadr r))) regexps)
        (cl-incf total (car r))))
    total))

(defun fin-report--first-flow (regexps since)
  "Date of the first personal-account row after SINCE matching REGEXPS, or nil."
  (caar (cl-remove-if-not (lambda (r) (cl-some (lambda (re) (string-match-p re (cadr r))) regexps))
                          (fin-db-query "SELECT date, description FROM bank_txn
                                          WHERE account = 'account' AND date > ? ORDER BY date"
                                        (list since)))))

(defun fin-report--derived-balance (spec)
  "(CENTS . AS-OF) of derived account SPEC."
  (let ((kind (plist-get spec :balance))
        (since (plist-get spec :since)))
    (cond (kind (let ((b (fin-bankdb-latest-balance kind)))
                  (and b (cons (cdr b) (car b)))))
          ((plist-get spec :closed)
           (let ((result (- (fin-report--bank-sum "in" (plist-get spec :out-of) since)
                            (fin-report--bank-sum "out" (plist-get spec :into) since))))
             (cons 0 (format "closed, %s %.2f" (if (< result 0) "loss" "gain") (/ (abs result) 100.0)))))
          ((plist-get spec :yield)
           (let ((b (fin-report--derived-balance (cl-list* :yield nil spec))))
             (cons (+ (car b) (plist-get spec :yield))
                   (format "%s + yield %.2f" (cdr b) (/ (plist-get spec :yield) 100.0)))))
          (t (cl-assert (and (stringp since) (integerp (plist-get spec :opening))) nil
                        "fin-accounts: %S needs :since and :opening" (plist-get spec :name))
             (cons (+ (plist-get spec :opening)
                      (fin-report--bank-sum "out" (plist-get spec :into) since)
                      (- (fin-report--bank-sum "in" (plist-get spec :out-of) since)))
                   (format "since %s"
                           (or (and (zerop (plist-get spec :opening))
                                    (fin-report--first-flow (plist-get spec :into) since))
                               since)))))))

(defun fin-report--bank-saved (period)
  "Cents the personal account moved into the accounts of `fin-accounts-derived'
in PERIOD (\"YYYY\" or \"YYYY-MM\"), net of what it took out of them; flows on
or before an account's :since are not its own."
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

(defun fin-report--accounts-derived ()
  "Plist per `fin-accounts-derived': :name, :cents, :note (the basis),
:cash (a bank account kind's balance), :reserve and :closed."
  (fin-bankdb-ensure)
  (mapcar (lambda (spec)
            (cl-assert (stringp (plist-get spec :name)) nil "fin-accounts: bad spec %S" spec)
            (let ((d (fin-report--derived-balance spec)))
              (list :name (plist-get spec :name) :cents (or (car d) 0) :note (or (cdr d) "no statement")
                    :cash (and (plist-get spec :balance) t) :reserve (plist-get spec :reserve)
                    :closed (plist-get spec :closed))))
          fin-accounts-derived))

(defun fin-report--accounts-total (&optional key)
  "Cents across `fin-report--accounts-derived', only rows with KEY when given."
  (apply #'+ (mapcar (lambda (a) (if (or (null key) (plist-get a key)) (plist-get a :cents) 0))
                     (fin-report--accounts-derived))))

(provide 'accounts)
;;; accounts.el ends here
