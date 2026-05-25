;;; style.el --- CSS variables + stylesheet  -*- lexical-binding: t; -*-

(require 'fmt)

(defun fin-dashboard--css-vars ()
  "Emit `:root { --key: value; ... }' from palette + size tokens."
  (concat
   ":root {"
   (mapconcat (lambda (kv) (format "--%s:%s;" (car kv) (cdr kv)))
              fin-dashboard--palette " ")
   "--gap:1px;"
   "--font:clamp(0.75rem, 0.85vw, 1rem);"
   "--font-sm:clamp(0.75rem, 0.75vw, 0.75rem);"
   "--font-xs:clamp(0.5rem, 0.7vw, 0.75rem);"
   "--font-h:clamp(0.75rem, 0.8vw, 0.75rem);"
   "--panel-pad:clamp(0.5rem, 1.1vw, 1.25rem);"
   "--row-pad-y:0.25rem;"
   "--row-pad-x:0.5rem;"
   "}"))

(defconst fin-dashboard--panel-areas
  '("stats" "objectives" "accounts" "patrimony" "cashflow")
  "Section class names used as grid-area names in the dashboard layout.")

(defun fin-dashboard--grid-area-rules ()
  (mapconcat (lambda (n) (format "section.%s{grid-area:%s;}" n n))
             fin-dashboard--panel-areas " "))

(defun fin-dashboard--style ()
  (concat
   (fin-dashboard--css-vars) "

* { box-sizing: border-box; }
:focus { outline: 1px solid var(--border-2); outline-offset: 3px; }
::selection { background: var(--accent); color: var(--bg); }
html, body { background: var(--bg); }
body {
  font: var(--font)/1.45 ui-monospace, 'JetBrains Mono', 'SF Mono', 'Menlo', 'Consolas', monospace;
  margin:0; padding:0; color: var(--text);
  -webkit-font-smoothing: antialiased;
  font-feature-settings: 'liga' 0;
}

/* ── header ─────────────────────────────────────────── */
header {
  display: flex; justify-content: space-between; align-items: baseline;
  padding: 0.5rem 1rem;
  background: var(--panel-2);
  border-bottom: 1px solid var(--border);
}
header .meta { color: var(--muted); font-size: 10.5px; }

/* ── headings (h1, h2, h3, th) ─────────────────────── */
h1, h2, h3, th { margin: 0; font-weight: 500; text-transform: uppercase; }
h1 { font-size: 0.75rem; letter-spacing: 0.3px; text-transform: none; }
h2 { font-size: calc(var(--font-h) * 1.25); letter-spacing: 1.4px; margin: 0 0 0.5rem; }
h3 { font-size: var(--font-h); letter-spacing: 1px; margin: 0.75rem 0 0.25rem; }
th { font-size: var(--font-h); letter-spacing: 1px; background: rgba(255,255,255,0.12); }

h1::before { content:'❯ '; }
h2::before, h3::before { content:'◆ '; }
h1::before, h2::before, h3::before { color: var(--muted); }

/* ── main grid ─────────────────────────────────────── */
main {
  display: grid; grid-template-columns: 1fr 1fr; gap: var(--gap);
  padding: 0; background: var(--border);
  grid-template-areas: \"stats stats\" \"objectives cashflow\" \"accounts cashflow\" \"patrimony cashflow\";
  grid-template-rows: auto auto auto 1fr;
}
" (fin-dashboard--grid-area-rules) "
@media (max-width: 900px) {
  main { grid-template-columns: 1fr;
         grid-template-areas: \"stats\" \"cashflow\" \"objectives\" \"accounts\" \"patrimony\"; }
}

section  { background: var(--bg); border: 0; padding: var(--panel-pad); overflow: auto; }
.sub     { color: var(--muted); margin: 0.25rem 0 0.5rem; font-size: var(--font-sm); }
p        { margin: 0.25rem 0; font-size: var(--font); }
p b      { color: var(--text); font-weight: 500; }

/* ── stats sub-layout ──────────────────────────────── */
.stat-pair  { display: grid; grid-template-columns: 1fr 1fr; gap: 1.25rem; }
.stats-body { display: flex; gap: 1.25rem; margin-top: 1.25rem; }
.stats-col  { display: flex; flex-direction: column; gap: 1.25rem; flex: 1; min-width: 0; }
.stats-body > .stats-col:first-child  { flex: 0.5; }
.stats-body > .stats-col:nth-child(2) { flex: 0.75; }
@media (max-width: 700px) {
  .stats-body { flex-direction: column; }
  .stat-pair  { grid-template-columns: 1fr; }
}

/* ── colored state spans ───────────────────────────── */
.neg { color: var(--red); }
.pos { color: var(--green); }
.dim { color: var(--muted); font-weight: 400; }
h3 .pos, h3 .neg, h3 .dim { font-weight: 400; }

/* ── charts ────────────────────────────────────────── */
.chart       { display: block; width: 100%; height: auto; margin: 0.25rem 0 0.5rem; }
.chart.flow  { height: 8rem; }

/* ── definition lists (records, yoy) ───────────────── */
dl.kv {
  display: grid; grid-template-columns: max-content 1fr;
  column-gap: 0.75rem; row-gap: 0.25rem;
  margin: 0.25rem 0; font-size: var(--font);
}
dl.kv dt { color: var(--accent); }
dl.kv dd { margin: 0; font-variant-numeric: tabular-nums; }
dl.kv.runway    { font-size: var(--font-h); column-gap: 0.5rem; row-gap: 0.125rem; margin: 0.5rem 0 0.75rem; }
dl.kv.runway dt { color: var(--muted); text-transform: uppercase; letter-spacing: 0.5px; }

/* ── tables (accordion) ────────────────────────────── */
table { width: 100%; border-collapse: collapse; font-size: var(--font-sm); margin: 0.25rem 0; }
table th, table td { padding: var(--row-pad-y) 0; text-align: left; vertical-align: baseline; }
table th:first-child, table td:first-child { width: 100%; padding-left: var(--row-pad-x); }
table th + th, table td + td               { padding-left: var(--row-pad-x); }
table th:last-child, table td:last-child   { padding-right: var(--row-pad-x); }
table th:not(:first-child),
table td:not(:first-child) { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }

table > tbody > tr.row.even > td      { background: rgba(255,255,255,0.04); }
table > tbody > tr.row:hover > td,
table > tbody > tr.row:has(> td > details[open]) > td { background: var(--panel); }
table > tbody > tr.row:has(> td > details[open]) > td:first-child { box-shadow: -3px 0 0 var(--accent); }
table > tbody > tr.row.future > td               { opacity: 0.35; }
table > tbody > tr.row.future:has(> td > details[open]) > td { opacity: 0.8; }
table:has(> tbody > tr.row > td > details[open]) > thead > tr > th,
table > tbody:has(> tr.row > td > details[open]) > tr.row:not(:has(> td > details[open])) > td { opacity: 0.25; }

table > tbody > tr.body                 { display: none; }
table > tbody > tr.row:has(> td > details[open]) + tr.body { display: table-row; }
table > tbody > tr.body > td            { padding: 0.25rem 0 0.5rem; background: var(--bg); }

table td > details,
table td > details > summary        { display: block; }
table td > details > summary        { cursor: pointer; list-style: none; }
table td > details > summary::-webkit-details-marker { display: none; }
"))

(provide 'style)
;;; style.el ends here
