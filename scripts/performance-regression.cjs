// Offline checks: no app startup, browser, API key, network or provider requests.
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const source = fs.readFileSync(path.join(__dirname, '../www/app.js'), 'utf8');
const start = source.indexOf('  let shinyConnected = false;');
const end = source.indexOf('  let rateHelpButton', start);
assert(start >= 0 && end > start);
let sent = [], frames = [], resized = 0;
const context = vm.createContext({
  window: {Shiny: {setInputValue: (id,value) => sent.push([id,value])}},
  requestAnimationFrame: callback => { frames.push(callback); return frames.length; },
  resizeWidgets: () => { resized++; }
});
context.Shiny = context.window.Shiny;
vm.runInContext(source.slice(start,end) + `
  globalThis.test = {
    send, scheduleResize,
    connect(){shinyConnected=true;for(const [id,value] of pendingInputs)send(id,value);pendingInputs.clear();},
    disconnect(){shinyConnected=false;}
  };`, context);
context.test.send('theme','dark');
context.test.send('theme','light');
assert.equal(sent.length,0);
context.test.connect();
assert.deepEqual(sent,[['theme','light']]);
context.test.disconnect();
context.test.send('current_view','data');
assert.equal(sent.length,1);
context.test.connect();
assert.deepEqual(sent[1],['current_view','data']);
for(let i=0;i<20;i++)context.test.scheduleResize();
assert.equal(frames.length,1);
frames.shift()();
assert.equal(resized,1);
context.test.scheduleResize();
assert.equal(frames.length,1);
console.log('PASS: disconnected input buffering, latest value preserved, reconnect flush, resize coalescing');
