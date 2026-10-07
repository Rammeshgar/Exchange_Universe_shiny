// Offline navigation checks; no app startup, credentials or network access.
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const source = fs.readFileSync(path.join(__dirname, '../www/app.js'), 'utf8');
const start = source.indexOf('  const publicAppURL =');
const end = source.indexOf('  const showView =', start);
assert(start >= 0 && end > start);
const context = vm.createContext({URL});
vm.runInContext(source.slice(start, end) + '\nglobalThis.cleanURL = publicAppURL;', context);
for (const suffix of ['', '_w_abc123/', '_w_abc123/_w_abc123/']) {
  const url = context.cleanURL('https://sadeq.shinyapps.io/exchange_universe/' + suffix + '?base=EUR#explore');
  assert.equal(url.href, 'https://sadeq.shinyapps.io/exchange_universe/?base=EUR#explore');
  url.hash = 'data';
  assert.equal(url.href, 'https://sadeq.shinyapps.io/exchange_universe/?base=EUR#data');
}
const local = 'http://127.0.0.1:4888/?base=HUF#convert';
assert.equal(context.cleanURL(local).href, local);
assert(source.includes("history.replaceState(null, '', url.href)"));
assert(!source.includes("history.replaceState(null,'','#'"));
console.log('PASS: clean, worker and duplicated-worker URLs; query/hash preservation; local preview unchanged');
