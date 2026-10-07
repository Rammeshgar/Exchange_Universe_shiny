const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict');
const source=fs.readFileSync(require('node:path').join(__dirname,'../www/analytics.js'),'utf8');
function run(host,choice){
  const tags=[];
  const context={location:{hostname:host,protocol:'https:',origin:'https://'+host,pathname:'/exchange_universe/',hash:'#explore'},
    localStorage:{getItem:()=>choice?JSON.stringify({value:choice,time:Date.now()}):null},
    document:{readyState:'complete',referrer:'',getElementById:()=>null,addEventListener(){},createElement:()=>({}),head:{append:x=>tags.push(x)}},
    URL,Date,console};
  context.window=context;context.addEventListener=()=>{};
  vm.runInNewContext(source,context);
  return {tags,context};
}
const auto=run('example.com',null);
assert.equal(auto.tags.length,2);
const calls=auto.context.dataLayer.map(x=>Array.from(x));
assert.equal(calls.find(x=>x[0]==='consent')[2].analytics_storage,'denied');
assert(!calls.some(x=>x[0]==='consent'&&x[1]==='update'&&x[2].analytics_storage==='granted'));
assert.equal(auto.context.clarity.q[0][1].analytics_Storage,'denied');
assert.equal(run('example.com','denied').tags.length,0);
assert.equal(run('localhost',null).tags.length,0);
vm.runInNewContext(source,auto.context);
assert.equal(auto.tags.length,2);
console.log('PASS: automatic tags, no invented consent, saved decline respected, local tracking off, no duplicate install');
