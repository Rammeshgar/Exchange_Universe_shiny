const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');
const fs=require('fs');
const path=require('path');
const out=path.resolve('../Exchange-Universe-Review');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  const results=[];const errors=[];
  try{
    const page=await browser.newPage({viewport:{width:1556,height:812},reducedMotion:'reduce'});
    page.on('pageerror',e=>errors.push(e.message));
    await page.goto('http://127.0.0.1:4888/?base=EUR&compare=GBP,HUF,USD,CNY,CAD,JPY#explore');
    await page.waitForSelector('#comparison_chart canvas');
    await page.evaluate(()=>document.fonts.ready);
    assert.equal(await page.locator('.footer-credit').count(),1);
    assert.equal(await page.locator('.sidebar-footer').count(),0);
    assert.equal(await page.locator('.footer-credit a').getAttribute('href'),'https://rammeshgar.github.io/');
    assert.equal(await page.locator('.workspace-view .footer-credit').count(),0,'One shared credit, not a copy in each view');
    await page.addScriptTag({path:path.resolve('../../work/exchange-qa/node_modules/axe-core/axe.min.js')});
    for(const theme of ['dark','light']){
      if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme)await page.locator('#theme_toggle').click();
      for(const size of [[1556,812],[1280,720],[768,1024],[390,844],[320,740],[844,390]]){
        await page.setViewportSize({width:size[0],height:size[1]});
        for(const view of ['explore','convert','data']){
          await page.locator('#tab_'+view).click();
          if(view==='data')await page.waitForSelector('#summary_table tbody tr');
          await page.waitForTimeout(200);
          const type=await page.evaluate(()=>{
            const style=s=>{const e=document.querySelector(s),c=getComputedStyle(e);return{font:c.fontFamily,size:parseFloat(c.fontSize),color:c.color,weight:c.fontWeight};};
            return{title:style('#workspace_title'),section:style('.workspace-view:not([hidden]) h2'),body:style('.intro-copy'),
              horizontal:document.documentElement.scrollWidth>innerWidth+1 || document.getElementById('workspace').scrollWidth>document.getElementById('workspace').clientWidth+1};
          });
          assert(type.title.font.includes('Faculty Glyphic'),'Page-title display face');
          assert(type.section.font.includes('Atlas'),'Section headings use the interface face');
          assert(type.title.size>type.section.size && type.section.size>type.body.size,'Distinct size hierarchy');
          assert(!type.horizontal,JSON.stringify({theme,size,view,type}));
          await page.locator('.footer-credit').scrollIntoViewIfNeeded();
          assert(await page.locator('.footer-credit a').isVisible());
          assert.equal(await page.locator('.footer-credit').count(),1);
          const credit=await page.locator('.footer-credit a').boundingBox();
          assert(credit.x>=0 && credit.x+credit.width<=size[0]+1,'Credit wraps without horizontal clipping');
          const violations=await page.evaluate(async()=>{const r=await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}});return r.violations.map(v=>({id:v.id,targets:v.nodes.map(n=>n.target)}));});
          assert.deepEqual(violations,[],JSON.stringify({theme,size,view,violations}));
          await page.evaluate(()=>{document.activeElement?.blur();document.getElementById('workspace').scrollTop=0;window.scrollTo(0,0);});
          if([1556,390,320].includes(size[0]))await page.screenshot({path:path.join(out,`final-${theme}-${view}-${size[0]}.png`),fullPage:size[0]<801});
          results.push({theme,size,view,oneSharedCredit:true,type,axeViolations:0});
        }
      }
    }
    await page.setViewportSize({width:1556,height:812});
    await page.locator('#about').click();await page.waitForSelector('.modal-content');
    assert(!(await page.locator('.modal-content').textContent()).includes('by Sadeq Rezai'),'About does not repeat the author credit');
    assert((await page.locator('.modal-content').textContent()).includes('Strength % = (start-date rate'));
    await page.keyboard.press('Escape');
    for(const theme of ['dark','light']){
      if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme)await page.locator('#theme_toggle').click();
      await page.locator('#tab_explore').click();
      // CSS switches immediately; wait for the server-rendered widget palette too.
      await page.waitForFunction(expected=>document.querySelector('[data-map-currency="USD"] i')?.style.getPropertyValue('--currency-color').toLowerCase()===expected,theme==='dark'?'#7db2ad':'#267e77');
      await page.waitForFunction(()=>{
        const colors={};document.querySelectorAll('.rate-card').forEach(e=>colors[e.querySelector('.currency-code').textContent.trim()]=e.style.getPropertyValue('--currency-color').toLowerCase());
        const lines=echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption().series;
        return lines.length===6 && lines.every(s=>colors[s.name]===s.lineStyle.color.toLowerCase());
      });
      await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
      await page.waitForFunction(()=>{
        const colors={};document.querySelectorAll('.rate-card').forEach(e=>colors[e.querySelector('.currency-code').textContent.trim()]=e.style.getPropertyValue('--currency-color').toLowerCase());
        const traces=document.getElementById('strength_3d')?.data;
        return traces?.length===6 && traces.every(t=>colors[t.name]===t.line.color.toLowerCase());
      });
      const colors=await page.evaluate(()=>{
        const cards={};document.querySelectorAll('.rate-card').forEach(e=>cards[e.querySelector('.currency-code').textContent.trim()]=e.style.getPropertyValue('--currency-color').toLowerCase());
        const map={};document.querySelectorAll('[data-map-currency]').forEach(e=>map[e.dataset.mapCurrency]=e.querySelector('i').style.getPropertyValue('--currency-color').toLowerCase());
        const scene={};document.getElementById('strength_3d').data.forEach(t=>scene[t.name]=t.line.color.toLowerCase());
        const bars={};echarts.getInstanceByDom(document.getElementById('change_chart')).getOption().series[0].data.forEach((d,i)=>bars[echarts.getInstanceByDom(document.getElementById('change_chart')).getOption().yAxis[0].data[i]]=d.itemStyle.color.toLowerCase());
        return{cards,map,scene,bars};
      });
      assert.equal(new Set(Object.values(colors.map)).size,7,'Distinct colors include the base');
      for(const [code,color] of Object.entries(colors.cards)){
        assert.equal(colors.map[code],color);assert.equal(colors.scene[code],color);assert.equal(colors.bars[code],color);
      }
      results.push({test:'Distinct coherent seven-color mapping: cards/map/2D/3D/snapshot',theme,colors});
      await page.locator('input[name=chart_dimension][value="2d"]').check({force:true});
      await page.waitForFunction(()=>!document.getElementById('chart_caption').textContent.includes('Drag to rotate'));
      await page.screenshot({path:path.join(out,`final-${theme}-explore-1556.png`)});
    }
    const meta=await page.evaluate(()=>({title:document.title,description:document.querySelector('meta[name=description]')?.content,
      social:document.querySelector('meta[property="og:image"]')?.content,viewport:document.querySelector('meta[name=viewport]')?.content,
      localAssets:[...document.querySelectorAll('link[rel=icon],link[rel=apple-touch-icon]')].map(e=>e.href)}));
    assert(meta.title.includes('Exchange Universe'));assert(meta.description);assert(meta.social.startsWith('https://'));assert(meta.viewport?.includes('width=device-width'));assert(!/user-scalable\s*=\s*no|maximum-scale\s*=\s*1/.test(meta.viewport));
    for(const url of meta.localAssets){const r=await page.request.get(url);assert.equal(r.status(),200,'Favicon asset available');}
    assert.equal((await page.request.get('http://127.0.0.1:4888/social-preview.png')).status(),200);
    assert.deepEqual(errors,[]);assert.equal(await page.locator('.shiny-output-error').count(),0);
    fs.writeFileSync(path.join(out,'final-review.json'),JSON.stringify({results,meta,errors},null,2));
    console.log('PASS: one shared author credit; type hierarchy; 36 view/theme/layout cases; keyboard-safe reduced-motion preview; axe; metadata/assets; zero browser errors.');
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
