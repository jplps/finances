;;; style.el --- CSS variables + stylesheet  -*- lexical-binding: t; -*-

(require 'fmt)

(defun fin-dashboard--css-tokens (pick)
  "Custom-property declarations of every palette role, value by PICK."
  (mapconcat (lambda (r) (format "--%s:%s;" (car r) (funcall pick r))) fin-dashboard--palette ""))

(defun fin-dashboard--css-vars ()
  "Dark tokens by default; light ones only when <html data-theme=light>."
  (concat
   ":root{color-scheme:dark;" (fin-dashboard--css-tokens #'caddr)
   "--gap:1px;--pad:20px;--max:640px;--inline:max(var(--pad), calc((100% - var(--max)) / 2 + var(--pad)));--font:13px;--font-sm:12.5px;--font-xs:11.5px;--font-kpi:clamp(16px, 1.5vw, 22px);}"
   ":root[data-theme=\"light\"]{color-scheme:light;" (fin-dashboard--css-tokens #'cadr) "}"
   ;; The LED ticker is a physical sign: dark in either theme.
   ".ticker{color-scheme:dark;" (fin-dashboard--css-tokens #'caddr) "}"))

(defconst fin-dashboard--tabs
  '(("cockpit" . "cockpit") ("wealth" . "wealth") ("stats" . "stats"))
  "(SECTION-ID . BAR-LABEL) in status-bar order; the first is shown by default.")

(defun fin-dashboard--tab-rules ()
  "Show the targeted panel (the first when none) and mark its tab current."
  (let ((first (caar fin-dashboard--tabs)))
    (concat
     (format "body:not(:has(section:target)) section#%s{display:block;}" first)
     (format "body:not(:has(section:target)) nav.status a[href=\"#%s\"]{color:var(--accent-2);}" first)
     (format "body:not(:has(section:target)) nav.status a[href=\"#%s\"]::after{content:'*';}" first)
     (mapconcat (lambda (n)
                  (format "body:has(section#%s:target) nav.status a[href=\"#%s\"]{color:var(--accent-2);}body:has(section#%s:target) nav.status a[href=\"#%s\"]::after{content:'*';}"
                          n n n n))
                (mapcar #'car fin-dashboard--tabs) ""))))

(defun fin-dashboard--style ()
  (concat
   (fin-dashboard--css-vars) "

* { box-sizing: border-box; }
:focus-visible { outline: 1px solid var(--accent); outline-offset: 2px; }
::selection { background: var(--accent-2); color: var(--page); }
html, body { background: var(--page); }
body {
  margin: 0; color: var(--text);
  font: 400 var(--font)/1.5 'JetBrains Mono', 'SF Mono', ui-monospace, Menlo, Consolas, monospace;
  font-variant-numeric: tabular-nums; font-feature-settings: 'liga' 0;
  -webkit-font-smoothing: antialiased;
}

/* ── header ─────────────────────────────────────────── */
header {
  position: sticky; top: 0; z-index: 1;
  display: flex; justify-content: space-between; align-items: baseline; gap: 16px;
  padding: 10px var(--inline); background: var(--page);
  border-bottom: 1px solid var(--border);
}
header .meta { color: var(--muted); font-size: var(--font-xs); }

/* ── headings ──────────────────────────────────────── */
h1, h3, th { margin: 0; font-weight: 400; text-transform: lowercase; }
h1 { font-size: var(--font); color: var(--text); white-space: nowrap; }
h1::before { content: '$ '; color: var(--accent-2); }
h3 { font-size: var(--font-sm); margin: 18px 0 6px; color: var(--text-2); }
h3::before { content: '# '; color: var(--muted); }
th { font-size: var(--font-xs); color: var(--muted); border-bottom: 1px solid var(--grid); }

/* ── panes: one panel at a time, picked from the status bar ── */
main { padding-bottom: 32px; }
section { display: none; max-width: var(--max); margin: 0 auto; padding: var(--pad); overflow: auto; min-width: 0; scroll-margin-top: 100vh; }  /* :target never scrolls past the ticker */
section:target { display: block; }
" (fin-dashboard--tab-rules) "

/* ── tmux-style status bar ─────────────────────────── */
nav.status {
  position: fixed; left: 0; right: 0; bottom: 0; z-index: 1;
  display: flex; flex-wrap: wrap; align-items: center; gap: 4px 14px;
  padding: 4px var(--inline); background: var(--surface-2); border-top: 1px solid var(--border);
  font-size: var(--font-sm);
}
nav.status a { color: var(--text-2); text-decoration: none; }
nav.status a:hover { color: var(--text); }
nav.status a::after { content: ' '; white-space: pre; }
nav.status .clock { margin-left: auto; color: var(--muted); }
.sub    { color: var(--muted); margin: -6px 0 14px; font-size: var(--font-xs); }
p       { margin: 4px 0; }
p b     { font-weight: 600; }

/* ── KPI tiles ─────────────────────────────────────── */
.kpis { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 8px; margin-bottom: 6px; }
.kpi  {
  border: 1px solid var(--border); padding: 10px 12px; background: var(--page);
}
.kpi-label { color: var(--muted); font-size: var(--font-xs); text-transform: lowercase; }
.kpi-value { font-size: var(--font-kpi); font-weight: 400; line-height: 1.25; margin: 4px 0 2px; white-space: nowrap; }
.kpi-note  { color: var(--muted); font-size: var(--font-xs); }
.kpi.good .kpi-value { color: var(--good-text); }
.kpi.bad  .kpi-value { color: var(--bad-text); }
.kpis.three { grid-template-columns: repeat(3, minmax(0, 1fr)); }
@media (max-width: 700px) { .kpis { grid-template-columns: repeat(2, minmax(0, 1fr)); } }

/* ── goals table ───────────────────────────────────── */
/* patrimony and accounts below the goals: a clear break after each total */
.stat-block.register { margin-top: 36px; }
section > .stat-block.register:first-child { margin-top: 0; }
table.goals { table-layout: fixed; }
table.goals td { padding-top: 6px; padding-bottom: 6px; vertical-align: middle; }
table.goals th:nth-child(1) { width: 24%; }
table.goals th:nth-child(2) { width: 52%; }
table.goals th:nth-child(n+3) { width: 12%; }
table.goals th.viz, table.goals td.viz { text-align: left; }
table.goals td.cat { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
table.goals tr.child td.cat { padding-left: 22px; color: var(--text-2); }
table.goals tr.child td.cat::before { content: '└ '; color: var(--muted); }
table.goals tr.child.deep td.cat { padding-left: 40px; }
table.goals td.cat label { cursor: pointer; display: inline-flex; align-items: center; }
table.goals td.cat input { display: none; }
table.goals td.cat label::before {
  content: '▸'; display: inline-flex; justify-content: center; width: 12px; margin-right: 4px;
  line-height: 1; color: var(--accent); transition: transform .15s;
}
table.goals td.cat label:has(input:checked)::before { transform: rotate(90deg); color: var(--accent-2); }
svg.bullet { display: block; width: 100%; height: 16px; overflow: visible; }

/* ── ledger health to-do ───────────────────────────── */
.health .todo { padding: 4px 0; font-size: var(--font-sm); }
.health .todo b { font-weight: 600; color: var(--accent); }
.health table { margin: 2px 0 10px 22px; width: calc(100% - 22px); table-layout: fixed; }
.health table td:first-child { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.health table th:not(:first-child) { width: 96px; }

/* ── stats sub-layout ──────────────────────────────── */
.stat-pair  { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
.stats-body { display: flex; flex-direction: column; gap: 20px; margin-top: 20px; }
.stats-col  { display: flex; flex-direction: column; gap: 20px; min-width: 0; }
@media (max-width: 700px) { .stat-pair { grid-template-columns: 1fr; } }

/* ── signed values ─────────────────────────────────── */
.neg { color: var(--bad-text); }
.pos { color: var(--good-text); }
.dim { color: var(--muted); font-weight: 400; }

/* ── charts ────────────────────────────────────────── */
.chart       { display: block; width: 100%; height: auto; margin: 4px 0 8px; }
.chart.flow  { height: 8rem; }
.chart.bars  { height: 10rem; }
.chart.pie   { max-width: 420px; margin-inline: auto; }
.chart text  { fill: var(--text-2); font-family: inherit; }

/* ── definition lists (records, runway) ────────────── */
dl.kv {
  display: grid; grid-template-columns: max-content 1fr;
  column-gap: 12px; row-gap: 3px; margin: 4px 0;
}
dl.kv dt { color: var(--accent); }
dl.kv dd { margin: 0; }
dl.kv.runway    { font-size: var(--font-xs); column-gap: 8px; row-gap: 2px; margin: 8px 0 12px; }
dl.kv.runway dt { color: var(--muted); }

/* ── tables (accordion) ────────────────────────────── */
table { width: 100%; border-collapse: collapse; font-size: var(--font-sm); margin: 4px 0; }
table th, table td { padding: 5px 0; text-align: left; vertical-align: middle; }
table td { border-bottom: 1px dashed var(--border); }
table th:first-child, table td:first-child { width: 100%; padding-left: 8px; }
table th + th, table td + td               { padding-left: 12px; }
table th:last-child, table td:last-child   { padding-right: 8px; }
table th:not(:first-child),
table td:not(:first-child) { text-align: right; white-space: nowrap; }
table.goals th:first-child, table.goals td:first-child { width: auto; }

table > tbody > tr.row:hover > td,
table > tbody > tr.goal:hover > td,
table > tbody > tr.row:has(> td > details[open]) > td { background: var(--surface-2); }
table > tbody > tr.row:has(> td > details[open]) > td:first-child { box-shadow: inset 2px 0 0 var(--accent-2); }
table > tbody > tr.row.future > td               { opacity: 0.4; }
table > tbody > tr.row.future:has(> td > details[open]) > td { opacity: 0.8; }
table:has(> tbody > tr.row > td > details[open]) > thead > tr > th,
table > tbody:has(> tr.row > td > details[open]) > tr.row:not(:has(> td > details[open])) > td { opacity: 0.35; }

table > tfoot td { padding-top: 7px; border-top: 1px solid var(--grid); border-bottom: 0; color: var(--muted); font-weight: 700; }
table > tfoot td:first-child { color: var(--muted); font-weight: 400; }
table > tbody > tr.body                 { display: none; }
table > tbody > tr.row:has(> td > details[open]) + tr.body { display: table-row; }
table > tbody > tr.body > td            { padding: 4px 0 12px; background: var(--page); }

table td > details,
table td > details > summary        { display: block; }
table td > details > summary        { cursor: pointer; list-style: none; display: flex; align-items: center; }
table td > details > summary::-webkit-details-marker { display: none; }
table td > details > summary::before {
  content: '▸'; flex: none; display: inline-flex; align-items: center; justify-content: center;
  width: 12px; height: 1em; margin-right: 4px; line-height: 1; color: var(--accent); transition: transform .15s;
}
table td > details[open] > summary::before { transform: rotate(90deg); color: var(--accent-2); }

/* ── LED ticker tape (records) ─────────────────────── */
/* full-width black strip; the tape window keeps the content column */
.ticker {
  position: relative; background: #050505; border-bottom: 1px solid var(--border);
  padding: 6px 0; color: var(--accent-2);
  font-size: 22px; line-height: 1.3; font-weight: 700; text-transform: uppercase; letter-spacing: 0.1em;
  text-shadow: 0 0 6px currentColor;
}
/* dot-matrix mask: dark gaps between LED pixels */
.ticker::after {
  content: ''; position: absolute; inset: 0; pointer-events: none;
  background: radial-gradient(circle, transparent 0 1.2px, rgb(5 5 5 / 0.85) 1.7px) 0 0 / 3px 3px;
}
.ticker .window { width: min(calc(100% - 2 * var(--pad)), calc(var(--max) - 2 * var(--pad))); margin: 0 auto; overflow: hidden; }
.ticker .tape { display: flex; width: max-content; animation: tape 70s linear infinite; }
.ticker .tape > span { display: flex; gap: 48px; padding-right: 48px; white-space: nowrap; }
.ticker .tick b { font-weight: 700; color: var(--accent-2); margin-right: 6px; }
.ticker .tick { color: var(--text); display: inline-flex; align-items: center; gap: 0.35em; }
.ticker .tick b { margin-right: 0; }
.ticker .arrow { width: 0; height: 0; border: 0.32em solid transparent; filter: drop-shadow(0 0 3px currentColor); }
.ticker .arrow.up   { border-top: 0; border-bottom: 0.55em solid currentColor; }
.ticker .arrow.down { border-bottom: 0; border-top: 0.55em solid currentColor; }
.ticker:hover .tape { animation-play-state: paused; }
@keyframes tape { to { transform: translateX(-50%); } }
@media (prefers-reduced-motion: reduce) {
  .ticker .window { overflow-x: auto; }
  .ticker .tape { animation: none; }
  .ticker .tape > span[aria-hidden] { display: none; }
}

/* ── small screens: compact header, wider labels, one-line bar ── */
@media (max-width: 520px) {
  :root { --pad: 12px; --font-sm: 12px; }
  header .meta, nav.status .clock { display: none; }
  nav.status { flex-wrap: nowrap; overflow-x: auto; white-space: nowrap; gap: 10px; font-size: var(--font-xs); }
  table.goals th:nth-child(1) { width: 34%; }
  table.goals th:nth-child(2) { width: 30%; }
  table.goals th:nth-child(n+3) { width: 18%; }
}
"))

(provide 'style)
;;; style.el ends here
