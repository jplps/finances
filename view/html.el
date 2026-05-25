;;; html.el --- Generic HTML building blocks  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'fmt)

(defun fin-dashboard--panel (name class-suffix title &rest body)
  "<section class=\"SUFFIX\"> with H2 NAME + tooltip TITLE."
  (concat (format "<section class=\"%s\"><h2 title=\"%s\">%s</h2>"
                  class-suffix
                  (fin-dashboard--esc title)
                  (fin-dashboard--esc name))
          (apply #'concat body)
          "</section>"))

(defun fin-dashboard--kv (dt dd)
  "Format `<dt>DT</dt><dd>DD</dd>'.  Both pre-formatted HTML."
  (format "<dt>%s</dt><dd>%s</dd>" dt dd))

(defun fin-dashboard--block (name title body &optional cls)
  "Stat-block <div> with H3 NAME (HTML-allowed, callers must pre-escape user data).
TITLE → hover tooltip.  Optional CLS appended to class list (for grid-area mapping)."
  (format "<div class=\"stat-block%s\"><h3 title=\"%s\">%s</h3>%s</div>"
          (if cls (concat " " cls) "")
          (fin-dashboard--esc title)
          name
          body))

(defun fin-dashboard--pair (&rest items)
  (concat "<div class=\"stat-pair\">" (apply #'concat items) "</div>"))

(defun fin-dashboard--table (header rows &optional class row-class-fn)
  "Render <table> with thead + per-row <tr.row[ even][ EXTRA]> + zebra index."
  (let ((i 0))
    (concat
     (if class (format "<table class=\"%s\">" class) "<table>")
     "<thead><tr>"
     (mapconcat (lambda (h) (format "<th>%s</th>" (fin-dashboard--esc h))) header "")
     "</tr></thead><tbody>"
     (mapconcat
      (lambda (r)
        (cl-incf i)
        (let* ((extra (and row-class-fn (funcall row-class-fn r)))
               (cls   (concat "row"
                              (when (cl-evenp i) " even")
                              (when extra (concat " " extra)))))
          (concat (format "<tr class=\"%s\">" cls)
                  (mapconcat (lambda (c) (format "<td>%s</td>" (fin-dashboard--fmt c))) r "")
                  "</tr>")))
      rows "")
     "</tbody></table>")))

(defun fin-dashboard--alist (header rows)
  "Accordion <table>.  HEADER = column labels.
ROWS = list of (LABEL CELLS BODY &optional CLASS OPEN).
BODY non-nil wraps LABEL in <details>; a <tr.body> follows in a colspan'd <td>."
  (let ((ncols (length header))
        (i 0))
    (concat
     "<table><thead><tr>"
     (mapconcat (lambda (h) (format "<th>%s</th>" (fin-dashboard--esc h))) header "")
     "</tr></thead><tbody>"
     (mapconcat
      (lambda (tup)
        (cl-incf i)
        (let* ((label (nth 0 tup))
               (cells (nth 1 tup))
               (body  (nth 2 tup))
               (cls   (nth 3 tup))
               (open  (nth 4 tup))
               (rcls  (concat "row"
                              (when (cl-evenp i) " even")
                              (when cls (concat " " cls))))
               (first (if body
                          (format "<td><details%s><summary>%s</summary></details></td>"
                                  (if open " open" "")
                                  (fin-dashboard--esc label))
                        (format "<td>%s</td>" (fin-dashboard--esc label)))))
          (concat
           (format "<tr class=\"%s\">%s" rcls first)
           (mapconcat (lambda (c) (format "<td>%s</td>" (fin-dashboard--fmt c))) cells "")
           "</tr>"
           (when body
             (format "<tr class=\"body\"><td colspan=\"%d\">%s</td></tr>"
                     ncols body)))))
      rows "")
     "</tbody></table>")))

(provide 'html)
;;; html.el ends here
