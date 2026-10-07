const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');
const fs=require('fs');
const path=require('path');
const out=path.resolve('../Exchange-Universe-Review');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  const results=[];const errors=[];
  try{
    const context=await browser.newContext({viewport:{width:1556,height:812},reducedMotion:'reduce'});
    await context.addInitScript(()=>{
      window.exchangeQaStateReady=false;
      const setItem=Storage.prototype.setItem;
      Storage.prototype.setItem=function(key,value){const result=setItem.call(this,key,value);if(key==='exchange-universe.v2.last')window.exchangeQaStateReady=true;return result;};
      // Legacy all-open settings must not override the new compact defaults.
      localStorage.setItem('exchange-universe.v2.section.sidebar_period','true');
      localStorage.setItem('exchange-universe.v2.section.sidebar_saved','true');
      localStorage.setItem('exchange-universe.v2.views',JSON.stringify([{id:'legacy',name:'Old index view',state:{base:'EUR',currencies:['GBP','HUF','USD'],metric:'index',period:'30'}}]));
    });
    const page=await context.newPage();const musicRequests=[];const trackers=[];
    page.on('pageerror',e=>errors.push(e.message));
    page.on('request',r=>{if(r.url().includes('/audio/'))musicRequests.push(r.url());if(/clarity\.ms|googletagmanager|google-analytics/.test(r.url()))trackers.push(r.url());});
    await page.goto('http://127.0.0.1:4888/?base=EUR&compare=GBP,HUF,USD&metric=index#explore');
    await page.waitForSelector('#comparison_chart canvas');
    await page.waitForFunction(()=>document.querySelectorAll('.rate-info').length===3);
    assert.deepEqual(await page.locator('input[name=metric]').evaluateAll(es=>es.map(e=>e.value)),['performance','rate']);
    assert.equal(await page.locator('input[name=metric]:checked').inputValue(),'performance');
    for(const id of ['sidebar_currencies','sidebar_period','sidebar_saved'])assert.equal(await page.locator('#'+id).evaluate(e=>e.open),id==='sidebar_currencies');
    results.push('Only Currencies opens by default; old URL index maps to Change %.');
    await page.locator('#sidebar_saved > summary').click();
    await page.locator('#saved_view').selectOption('legacy');
    await page.waitForTimeout(350);
    assert.equal(await page.locator('input[name=metric]:checked').inputValue(),'performance');
    await page.locator('#sidebar_saved > summary').click();
    const info=page.locator('[data-rate-help=rate_help_GBP]');const tip=page.locator('#rate_help_GBP');
    await info.hover();await page.waitForTimeout(150);assert(await tip.isHidden());
    await page.waitForTimeout(400);assert(await tip.isVisible());
    assert.match(await tip.innerText(),/1 EUR = .* GBP/);assert.match(await tip.innerText(),/Provider observation: .* UTC/);
    assert.match(await tip.innerText(),/from .* to /);assert.match(await tip.innerText(),/exclude fees/);
    await tip.hover();await page.waitForTimeout(300);assert(await tip.isVisible(),'Help stays visible while reading it');
    await page.keyboard.press('Escape');assert(await tip.isHidden());
    await page.mouse.move(0,0);await info.focus();assert(await tip.isVisible(),'Keyboard focus opens help');
    await page.keyboard.press('Escape');assert(await tip.isHidden());
    await page.locator('#workspace_title').click();await info.click();assert(await tip.isVisible());
    await info.click();assert(await tip.isHidden(),'Second click closes pinned help');
    await page.locator('[data-focus-currency=GBP]').click();
    await page.waitForFunction(()=>Shiny.shinyapp.$inputValues['focus_currency']?.code==='GBP');
    results.push('Delayed hover, hoverable help, keyboard focus/Escape, click toggle; card map focus preserved.');
    assert.equal(musicRequests.length,0,'Audio must not load before Play');
    assert(await page.locator('#ambient_music').evaluate(a=>a.paused&&!a.getAttribute('src')));
    await page.locator('#music_toggle').click();
    await page.waitForFunction(()=>{const a=document.getElementById('ambient_music');return !a.paused&&a.currentTime>.15&&a.duration>0;});
    const media=await page.locator('#ambient_music').evaluate(a=>({duration:a.duration,currentTime:a.currentTime,volume:a.volume}));
    assert.equal(await page.locator('#music_toggle').getAttribute('aria-label'),'Pause music');
    assert.equal(await page.locator('#music_toggle').getAttribute('aria-pressed'),'true');
    await page.locator('#tab_convert').click();assert(await page.locator('#ambient_music').evaluate(a=>!a.paused));
    await page.locator('#music_toggle').click();assert(await page.locator('#ambient_music').evaluate(a=>a.paused));
    assert.equal(await page.locator('#music_toggle').getAttribute('aria-pressed'),'false');
    const requestedBeforeReload=musicRequests.length;
    await page.reload();await page.waitForFunction(()=>window.exchangeQaStateReady);await page.locator('#tab_explore').click();await page.waitForSelector('#comparison_chart canvas');
    assert.equal(musicRequests.length,requestedBeforeReload);assert(await page.locator('#ambient_music').evaluate(a=>a.paused&&!a.getAttribute('src')));
    results.push({test:'Music lazy load, decodable MP3 playback, view switching, pause and no autoplay after reload',media});
    await page.locator('#tab_data').click();await page.waitForSelector('#summary_table tbody td');
    assert(!(await page.locator('#summary_table thead').first().innerText()).includes('Index 100'));
    const download=await page.locator('#download_csv').getAttribute('href');const response=await context.request.get(new URL(download,page.url()).href);
    assert.equal(response.status(),200);assert(!(await response.text()).includes('Index 100'));
    results.push('Redundant Index column removed from UI and CSV.');
    await page.locator('#tab_explore').click();
    await page.addScriptTag({path:path.resolve('../../work/exchange-qa/node_modules/axe-core/axe.min.js')});
    for(const theme of ['dark','light']){
      if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme)await page.locator('#theme_toggle').click();
      await page.waitForTimeout(350);
      for(const size of [[1556,812],[768,1024],[390,844],[320,740],[844,390]]){
        await page.setViewportSize({width:size[0],height:size[1]});await page.waitForTimeout(200);
        await page.evaluate(()=>{document.getElementById('workspace').scrollTop=0;window.scrollTo(0,0);});
        await info.click();
        const geometry=await tip.boundingBox();
        assert(geometry.x>=0&&geometry.y>=0&&geometry.x+geometry.width<=size[0]+1&&geometry.y+geometry.height<=size[1]+1,JSON.stringify(geometry));
        const overlaps=await page.locator('.rate-card').evaluateAll(cards=>cards.map(card=>{
          const info=card.querySelector('.rate-info svg').getBoundingClientRect();
          return [...card.querySelectorAll('.rate-value,.change-pill')].some(e=>{const b=e.getBoundingClientRect();return Math.min(b.right,info.right)-Math.max(b.left,info.left)>1&&Math.min(b.bottom,info.bottom)-Math.max(b.top,info.top)>1;});
        }));assert(overlaps.every(v=>!v),'Info icon must not overlay numbers '+JSON.stringify({theme,size,overlaps}));
        const header=await page.locator('.app-header').evaluate(e=>({width:e.clientWidth,scroll:e.scrollWidth}));assert(header.scroll<=header.width+1,JSON.stringify({theme,size,header}));
        for(const id of ['music_toggle','theme_toggle','about','mobile_settings_toggle']){
          const control=page.locator('#'+id);if(!await control.isVisible())continue;
          const b=await control.boundingBox();assert(b.x>=0&&b.x+b.width<=size[0]+1&&b.y+b.height<=112,'Header control fits '+id);assert(b.width>=44&&b.height>=44);
        }
        const violations=await page.evaluate(async()=>{const r=await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}});return r.violations.map(v=>({id:v.id,targets:v.nodes.map(n=>n.target)}));});
        assert.deepEqual(violations,[],JSON.stringify({theme,size,violations}));
        await page.screenshot({path:path.join(out,`card-help-${theme}-${size[0]}.png`)});
        await page.keyboard.press('Escape');
        results.push({theme,size,helpWithinViewport:true,headerFits:true,axeViolations:0});
      }
    }
    await page.setViewportSize({width:390,height:844});await page.locator('#mobile_settings_toggle').click();
    assert.equal(await page.locator('#sidebar_currencies').evaluate(e=>e.open),true);
    assert.equal(await page.locator('#sidebar_period').evaluate(e=>e.open),false);
    await page.locator('#sidebar_period > summary').click();await page.keyboard.press('Escape');
    await page.reload();await page.waitForSelector('#comparison_chart canvas');await page.locator('#mobile_settings_toggle').click();
    assert.equal(await page.locator('#sidebar_period').evaluate(e=>e.open),true,'User disclosure choices persist after compact defaults');
    await page.keyboard.press('Escape');
    const touch=await browser.newContext({viewport:{width:320,height:740},hasTouch:true,isMobile:true});
    const phone=await touch.newPage();await phone.goto('http://127.0.0.1:4888/');await phone.waitForSelector('.rate-info');
    await phone.locator('.rate-info').first().tap();assert(await phone.locator('.rate-help:not([hidden])').isVisible());
    await phone.locator('.rate-info').first().tap();assert.equal(await phone.locator('.rate-help:not([hidden])').count(),0);
    await touch.close();results.push('Touch help toggle and mobile disclosure persistence.');
    assert.deepEqual(errors,[]);assert.deepEqual(trackers,[]);
    fs.writeFileSync(path.join(out,'card-music-review.json'),JSON.stringify(results,null,2));
    console.log(JSON.stringify(results,null,2));console.log('PASS: new card help, compact settings, music and index migration.');
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1);});
