;;; cardpdf.el --- Nubank credit card bill (PDF) parser  -*- lexical-binding: t; -*-

;; Card bills before OFX exports existed only as PDFs.  pdfrows.js (macOS
;; PDFKit, through osascript) turns a bill into visual rows; this file reads
;; the transaction table from them:
;;
;;   DD MON<TAB>description<TAB>1.234,56
;;
;; - `Pagamento em ...' rows are payments of the bill: skipped.
;; - `Estorno de', `Crédito de' and `Desconto Antecipação' are credits (in).
;; - `Parcelamento de Fatura' / `Crédito de parcelamento' finance an earlier
;;   bill, so they are returned apart, never as transactions.
;; - Installments `- k/n' read `- Parcela k/n', as in the card OFX.
;;
;; A row's year comes from the bill's due date: a month after the due month
;; belongs to the year before.  Each bill must balance: its purchases (all
;; rows but credits, phone top-ups and IOF, which the bill totals apart)
;; equal its `Total de compras', or parsing signals.

(require 'cl-lib)

(defconst fin-cardpdf--script
  (expand-file-name "pdfrows.js"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Row extractor run by osascript.")

(defconst fin-cardpdf--months
  '("JAN" "FEV" "MAR" "ABR" "MAI" "JUN" "JUL" "AGO" "SET" "OUT" "NOV" "DEZ")
  "Month abbreviations as Nubank bills print them.")

(defconst fin-cardpdf--row-re
  "\\`\\([0-9]\\{2\\}\\) \\([A-Z]\\{3\\}\\)\t\\(.+\\)\t\\(-?[0-9.]+,[0-9]\\{2\\}\\)\\'"
  "One transaction row: day, month, description, amount.")

(defconst fin-cardpdf--payment-re "\\`Pagamento em ")
(defconst fin-cardpdf--financing-re "\\`\\(Parcelamento de Fatura\\|Crédito de parcelamento\\)")
(defconst fin-cardpdf--credit-re "\\`\\(Estorno de\\|Crédito de\\|Desconto Antecipação\\)")
(defconst fin-cardpdf--apart-re "\\`\\(Recarga de celular\\|IOF de \\)"
  "Charges the bill totals outside `Total de compras'.")

(defun fin-cardpdf-rows (path)
  "Visual rows of the PDF at PATH, as one string."
  (unless (file-readable-p path) (error "fin-cardpdf: cannot read %s" path))
  (unless (executable-find "osascript") (user-error "fin-cardpdf: osascript (macOS) not found"))
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8)
           (rc (call-process "osascript" nil t nil "-l" "JavaScript"
                             fin-cardpdf--script (expand-file-name path))))
      (unless (eql rc 0)
        (error "fin-cardpdf: osascript failed (%s) on %s: %s" rc path (string-trim (buffer-string))))
      (buffer-string))))

(defun fin-cardpdf--month (abbr)
  "Month number of ABBR."
  (1+ (or (cl-position abbr fin-cardpdf--months :test #'equal)
          (error "fin-cardpdf: unknown month %S" abbr))))

(defun fin-cardpdf--cents (s)
  "Bill amount S (\"1.234,56\", \"-5,00\") as signed integer cents."
  (unless (string-match "\\`\\(-\\)?\\([0-9.]+\\),\\([0-9]\\{2\\}\\)\\'" s)
    (error "fin-cardpdf: bad amount %S" s))
  (let ((abs (+ (* 100 (string-to-number (string-replace "." "" (match-string 2 s))))
                (string-to-number (match-string 3 s)))))
    (if (match-string 1 s) (- abs) abs)))

(defun fin-cardpdf--due (text)
  "(YEAR MONTH DAY) of the bill's due date in TEXT."
  (unless (string-match "\\(?:FATURA\\|VENCIMENTO\\) \\([0-9]\\{2\\}\\) \\([A-Z]\\{3\\}\\) \\([0-9]\\{4\\}\\)" text)
    (error "fin-cardpdf: no due date"))
  (list (string-to-number (match-string 3 text))
        (fin-cardpdf--month (match-string 2 text))
        (string-to-number (match-string 1 text))))

(defun fin-cardpdf--total (text)
  "Cents of the bill's `Total de compras'."
  (unless (string-match "Total de compras[^\n]*?\\([0-9.]+,[0-9]\\{2\\}\\)" text)
    (error "fin-cardpdf: no `Total de compras'"))
  (fin-cardpdf--cents (match-string 1 text)))

(defun fin-cardpdf--memo (desc)
  "DESC with an installment suffix `- k/n' written `- Parcela k/n'."
  (replace-regexp-in-string "\\s-+-\\s-+\\([0-9]+/[0-9]+\\)\\'" " - Parcela \\1" desc t))

(defun fin-cardpdf--table (text)
  "(DAY MONTH DESC CENTS) of each row after the first TRANSAÇÕES header."
  (let ((in-table nil) out)
    (dolist (line (split-string text "\n"))
      (cond ((string-prefix-p "TRANSAÇÕES" line) (setq in-table t))
            ((and in-table (string-match fin-cardpdf--row-re line))
             (push (list (string-to-number (match-string 1 line))
                         (fin-cardpdf--month (match-string 2 line))
                         (string-trim (match-string 3 line))
                         (fin-cardpdf--cents (match-string 4 line)))
                   out))))
    (nreverse out)))

(defun fin-cardpdf-parse (text)
  "Parse bill rows TEXT.  Return plist (:due ISO :total CENTS
:txns ((date cents memo) ...) :financing ((date cents memo) ...)); CENTS is
negative for charges, positive for credits."
  (pcase-let ((`(,dy ,dm ,dd) (fin-cardpdf--due text))
              (total (fin-cardpdf--total text))
              (purchases 0) (txns nil) (financing nil))
    (dolist (r (fin-cardpdf--table text))
      (pcase-let ((`(,day ,month ,desc ,cents) r))
        (unless (string-match-p fin-cardpdf--payment-re desc)
          (let* ((credit (or (< cents 0) (string-match-p fin-cardpdf--credit-re desc)))
                 (tx (list (format "%04d-%02d-%02d" (if (> month dm) (1- dy) dy) month day)
                           (if credit (abs cents) (- (abs cents)))
                           (fin-cardpdf--memo desc))))
            (unless (or credit (string-match-p fin-cardpdf--apart-re desc))
              (setq purchases (+ purchases (abs cents))))
            (if (string-match-p fin-cardpdf--financing-re desc)
                (push tx financing)
              (push tx txns))))))
    (unless (= purchases total)
      (error "fin-cardpdf: bill %04d-%02d unbalanced: purchases %d, total %d cents"
             dy dm purchases total))
    (list :due (format "%04d-%02d-%02d" dy dm dd) :total total
          :txns (nreverse txns) :financing (nreverse financing))))

(provide 'cardpdf)
;;; cardpdf.el ends here
