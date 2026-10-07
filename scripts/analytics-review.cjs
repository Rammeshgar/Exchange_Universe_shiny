// Test public-host behavior with entirely stubbed providers: no visitor telemetry leaves QA.
const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');const fs=require('fs');const path=require('path');
const source=fs.readFileSync('www/analytics.js','utf8');
const fixture=`<!doctype html><html lang="en"><head><title>Exchange Universe consent test</title>
<script defer src="/analytics.js"></script><script defer src="/analytics.js"></script></head><body>
<main><h1>Currency workspace</h1><button data-privacy-open="true">Privacy settings</button></main>
<section id="analytics_banner" hidden><button data-analytics-consent="denied">Decline</button><button data-analytics-consent="granted">Allow analytics</button></section>
<dialog id="privacy_dialog" aria-labelledby="privacy_title"><h2 id="privacy_title" tabindex="-1">Privacy settings</h2>
<p id="analytics_status" role="status"></p><button data-privacy-close="true">Close</button>
<button data-analytics-consent="denied">Withdraw</button><button data-analytics-consent="granted">Allow analytics</button></dialog></body></html>`;
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
 try{
  const page=await browser.newPage();const requests=[];const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.route('**/*',async route=>{
   const u=new URL(route.request().url());
   if(u.hostname==='exchange.example.test')return route.fulfill({contentType:u.pathname==='/analytics.js'?'text/javascript':'text/html',body:u.pathname==='/analytics.js'?source:fixture});
   if(u.hostname==='www.googletagmanager.com'||u.hostname==='www.clarity.ms'){
    requests.push(u.href);return route.fulfill({contentType:'text/javascript',body:'/* QA stub: no provider execution or network */'});
   }
   throw new Error('Unexpected external request: '+u.href);
  });
  await page.goto('https://exchange.example.test/app/?compare=SECRET&amount=123456#explore');
  assert(await page.locator('#analytics_banner').isVisible());assert.equal(requests.length,0);
  await page.locator('#analytics_banner [data-analytics-consent=denied]').click();
  assert.equal(requests.length,0);assert.equal(await page.locator('#analytics_banner').isVisible(),false);
  await page.reload();assert.equal(await page.locator('#analytics_banner').isVisible(),false);assert.equal(requests.length,0);
  await page.locator('[data-privacy-open]').click();await page.locator('#privacy_dialog [data-analytics-consent=granted]').click();
  await page.waitForFunction(()=>document.querySelectorAll('#exchange-ga4,#exchange-clarity').length===2);await page.waitForTimeout(100);
  assert.deepEqual(requests.sort(),['https://www.clarity.ms/tag/yu2u0am8kk','https://www.googletagmanager.com/gtag/js?id=G-7B8GY61MGS'].sort());
  const initial=await page.evaluate(()=>({ga:window.dataLayer.map(x=>Array.from(x)),clarity:window.clarity.q.map(x=>Array.from(x)),async:Array.from(document.querySelectorAll('#exchange-ga4,#exchange-clarity')).every(s=>s.async)}));
  assert(initial.async);assert.equal(initial.ga.filter(x=>x[0]==='config').length,1);
  const config=initial.ga.find(x=>x[0]==='config');assert.equal(config[1],'G-7B8GY61MGS');assert.equal(config[2].page_location,'https://exchange.example.test/app/');
  assert.equal(initial.ga[0][0],'consent');assert.equal(initial.ga[0][2].analytics_storage,'denied');
  assert.equal(initial.ga[1][2].analytics_storage,'granted');assert.equal(initial.ga[1][2].ad_storage,'denied');
  assert.equal(initial.clarity[0][0],'consentv2');assert.equal(initial.clarity[0][1].analytics_Storage,'granted');assert.equal(initial.clarity[0][1].ad_Storage,'denied');
  await page.addScriptTag({content:source});assert.equal(requests.length,2);
  await page.evaluate(()=>{ExchangeAnalytics.view('convert');ExchangeAnalytics.view('convert');ExchangeAnalytics.view('not-a-view');});
  const events=await page.evaluate(()=>dataLayer.map(x=>Array.from(x)).filter(x=>x[0]==='event'&&x[1]==='app_view'));
  assert.deepEqual(events.map(x=>x[2].view_name),['explore','convert']);
  assert(!JSON.stringify(initial.ga).includes('SECRET'));assert(!JSON.stringify(initial.ga).includes('123456'));
  await page.locator('[data-privacy-open]').click();await page.locator('#privacy_dialog [data-analytics-consent=denied]').click();
  await page.waitForFunction(()=>!document.querySelector('#exchange-ga4')&&!document.querySelector('#exchange-clarity'));
  assert.equal(requests.length,2,'Withdrawal reload must not reinstall trackers');
  // Expired permission requires a new choice, never automatic tracking.
  await page.evaluate(()=>localStorage.setItem('exchange-universe.analytics-consent.v1',JSON.stringify({value:'granted',time:Date.now()-181*86400000})));
  await page.reload();assert(await page.locator('#analytics_banner').isVisible());assert.equal(requests.length,2);
  assert.deepEqual(errors,[]);
  fs.writeFileSync(path.resolve('../Exchange-Universe-Review/analytics-review.json'),JSON.stringify({pass:true,requests:requests.map(u=>({url:u,stubbed:true})),checks:['no pre-consent requests','decline persisted','grant loads exact IDs once','consent defaults denied, ads denied','sanitized GA URL','view names allowlisted/deduplicated','withdrawal stops tags on reload','expired choice reprompts'],errors},null,2));
  console.log('PASS: production-host consent, exact tag IDs, once-only async loading, sanitized GA URL, view events, withdrawal and expiry (all providers stubbed).');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
