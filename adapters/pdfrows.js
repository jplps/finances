// pdfrows.js --- PDF text as visual rows, via macOS PDFKit (osascript -l JavaScript).
//
// Usage: osascript -l JavaScript pdfrows.js FILE.pdf
//
// Some Nubank bills store their transaction table column by column, so the
// plain page text lists every date, then every description, then every
// amount.  Grouping text runs by baseline and sorting them left to right
// rebuilds each table row; runs of one row are joined by a tab.

ObjC.import('PDFKit');

function pageRows(page) {
  const sel = page.selectionForRect(page.boundsForBox($.kPDFDisplayBoxMediaBox));
  const runs = [];
  if (!sel.isNil()) {
    const lines = sel.selectionsByLine;
    for (let j = 0; j < lines.count; j++) {
      const l = lines.objectAtIndex(j);
      const s = l.string.js.trim();
      if (s) {
        const b = l.boundsForPage(page);
        runs.push({ y: Math.round(b.origin.y), x: b.origin.x, s: s });
      }
    }
  }
  runs.sort((a, b) => b.y - a.y || a.x - b.x);
  const rows = [];
  for (const r of runs) {
    const last = rows[rows.length - 1];
    if (last && Math.abs(last.y - r.y) <= 2) last.parts.push(r.s);
    else rows.push({ y: r.y, parts: [r.s] });
  }
  return rows.map(r => r.parts.join('\t'));
}

function run(argv) {
  if (argv.length !== 1) throw new Error('usage: pdfrows.js FILE.pdf');
  const doc = $.PDFDocument.alloc.initWithURL($.NSURL.fileURLWithPath(argv[0]));
  if (!doc || doc.isNil()) throw new Error('cannot open ' + argv[0]);
  const out = [];
  for (let i = 0; i < doc.pageCount; i++) out.push(...pageRows(doc.pageAtIndex(i)));
  return out.join('\n');
}
