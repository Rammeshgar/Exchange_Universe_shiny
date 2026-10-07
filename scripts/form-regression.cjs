// Local browser fixture only: no Shiny app, API, analytics or network requests.
const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('node:fs');
const path=require('node:path');
const assert=require('node:assert/strict');
const app=fs.readFileSync(path.join(__dirname,'../www/app.js'),'utf8');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  try{
    const page=await browser.newPage();
    await page.route('**/*',route=>route.abort());
    await page.setContent('<div class="shiny-input-container"><label for="currencies">Currencies</label><select id="currencies" multiple></select></div><div id="metric" class="shiny-input-radiogroup"><label for="metric">Measure</label><input type="radio" name="metric" value="rate"></div><input type="date" id="dates_start" name="dates_start" value="2026-09-01"><input type="date" id="dates_end" name="dates_end" value="2026-10-07">');
    await page.addScriptTag({path:'C:/Users/Sadeq/AppData/Local/R/win-library/4.3/shiny/www/shared/jquery.js'});
    await page.addScriptTag({path:'C:/Users/Sadeq/AppData/Local/R/win-library/4.3/shiny/www/shared/selectize/js/selectize.js'});
    const result=await page.evaluate(source=>{
      const errors=[];
      window.addEventListener('error',event=>errors.push(event.message));
      jQuery('#currencies').selectize({maxItems:6,valueField:'value',labelField:'value',searchField:['value'],
        options:[{value:'EUR'},{value:'USD'}],items:['EUR','USD'],
        render:{item:(item,escape)=>'<div>'+escape(item.value)+'<button type="button" class="remove" aria-label="Remove '+escape(item.value)+'">×</button></div>'}});
      const a=source.indexOf('  const sendDates =');
      const b=source.indexOf('  let rateHelpButton',a);
      const helpers=new Function('send',source.slice(a,b)+';return {labelInputs,sendDates};')(()=>{});
      helpers.labelInputs();
      const start=source.indexOf("  document.addEventListener('click', event => {\n    const button=event.target.closest('button.remove')");
      const end=source.indexOf("  document.addEventListener('change'",start);
      if(start<0||end<0)throw new Error('Missing removal listener');
      new Function(source.slice(start,end))();
      document.querySelector('button.remove').click();
      const input=document.querySelector('.selectize-input input');
      return {errors,remaining:document.getElementById('currencies').selectize.items.slice(),
        inputId:input.id,inputName:input.name,label:document.querySelector('label').htmlFor,
        radioFor:document.querySelector('#metric > label').hasAttribute('for'),
        groupLabel:document.getElementById('metric').getAttribute('aria-labelledby')};
    },app);
    assert.deepEqual(result.errors,[]);
    assert.deepEqual(result.remaining,['USD']);
    assert.ok(result.inputId);
    assert.equal(result.inputName,result.inputId);
    assert.equal(result.label,result.inputId);
    assert.equal(result.radioFor,false);
    assert.equal(result.groupLabel,'metric-label');
    console.log('PASS: real Selectize initializes, native button removes currency, search IDs/names and labels match, radio-group label valid');
  }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
