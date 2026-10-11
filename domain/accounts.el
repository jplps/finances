;;; accounts.el --- Account balances + share derivation  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'db)
(require 'bankdb)

(defun fin-report--accounts ()
  "Top-level accounts.  Share % derived from total top-level balance."
  (fin-db-query
   "SELECT category, balance,
           ROUND(100.0 * balance /
                 NULLIF((SELECT SUM(balance) FROM account WHERE parent IS NULL),0), 1)
      FROM account
     WHERE parent IS NULL
     ORDER BY balance DESC"))

(defun fin-report--account-children (parent)
  "Sub-rows for PARENT.  Share derived from parent's balance."
  (fin-db-query
   "SELECT category, balance,
           ROUND(100.0 * balance /
                 NULLIF((SELECT balance FROM account
                          WHERE category = ?1 AND parent IS NULL),0), 1)
      FROM account
     WHERE parent = ?1
     ORDER BY balance DESC"
   (list parent)))

(defcustom fin-accounts-derived nil
  "Savings accounts rebuilt from the bank, to compare with the sheet.  Each
is a plist: :name, :sheet (sheet accounts it stands for, \"parent/child\" or
a top-level name), and either :balance (a bank account kind whose latest
LEDGERBAL is the balance) or :since (ISO date of a known balance), :opening
(cents on that date), :into and :out-of (regexps of personal-account
descriptions moving money into and out of it).  Yields never reach the
statements, so a rebuilt balance trails the real one by them."
  :type '(repeat plist) :group 'fin)

(defun fin-report--sheet-balance (ref)
  "Sheet balance of REF, \"parent/child\" or a top-level name; nil if absent."
  (pcase-let ((`(,parent ,child) (split-string ref "/")))
    (caar (if child
              (fin-db-query "SELECT balance FROM account WHERE parent = ? AND category = ?"
                            (list parent child))
            (fin-db-query "SELECT balance FROM account WHERE parent IS NULL AND category = ?"
                          (list parent))))))

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
          (t (cl-assert (and (stringp since) (integerp (plist-get spec :opening))) nil
                        "fin-accounts: %S needs :since and :opening" (plist-get spec :name))
             (cons (+ (plist-get spec :opening)
                      (fin-report--bank-sum "out" (plist-get spec :into) since)
                      (- (fin-report--bank-sum "in" (plist-get spec :out-of) since)))
                   (format "since %s"
                           (or (and (zerop (plist-get spec :opening))
                                    (fin-report--first-flow (plist-get spec :into) since))
                               since)))))))

(defun fin-report--accounts-derived ()
  "(NAME DERIVED SHEET NOTE) per `fin-accounts-derived'; SHEET nil when the
sheet has none of its accounts."
  (fin-bankdb-ensure)
  (mapcar (lambda (spec)
            (cl-assert (stringp (plist-get spec :name)) nil "fin-accounts: bad spec %S" spec)
            (let* ((d (fin-report--derived-balance spec))
                   (sheet (delq nil (mapcar #'fin-report--sheet-balance (plist-get spec :sheet)))))
              (list (plist-get spec :name) (car d) (and sheet (apply #'+ sheet)) (cdr d))))
          fin-accounts-derived))

(provide 'accounts)
;;; accounts.el ends here
