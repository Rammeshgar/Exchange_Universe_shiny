const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');
const fs=require('fs');
const path=require('path');
const out=path.resolve('../Exchange-Universe-Review');
const contrast=(a,b)=>{
  const lum=c=>c.match(/[\d.]+/g).slice(0,3).map(Number).map(n=>n/255).map(n=>n<=.04045?n/12.92:((n+.055)/1.055)**2.4).reduce((v,n,i)=>v+n*[.2126,.7152,.0722][i],0);
  const x=lum(a),y=lum(b);return(Math.max(x,y)+.05)/(Math.min(x,y)+.05);
};
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  const results=[];
  try{
    const page=await browser.newPage({viewport:{width:1556,height:812}});const errors=[];page.on('pageerror',e=>{errors.push(e.stack);console.error('Browser error',e.stack);});
    await page.goto('http://127.0.0.1:4888/?base=EUR&compare=GBP,HUF,USD&period=90#explore');
    await page.waitForSelector('#comparison_chart canvas');
    await page.waitForFunction(()=>echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption().series.every(s=>s.data.length===91));
    await page.evaluate(()=>document.fonts.ready);await page.waitForTimeout(350);
    assert(await page.evaluate(()=>document.fonts.check('26px "Faculty Glyphic"')));
    for(const theme of ['dark','light']){
      if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme)await page.locator('#theme_toggle').click();
      await page.waitForTimeout(350);
      await page.locator('#base-selectized').click();
      await page.waitForSelector('.selectize-dropdown .currency-option',{state:'visible'});
      const options=await page.evaluate(()=>Array.from(document.querySelectorAll('#base + .selectize-control .selectize-dropdown [data-selectable]')).map(el=>{
        const bg=getComputedStyle(el).backgroundColor;return{code:el.dataset.value,text:el.innerText,bg,primary:getComputedStyle(el.querySelector('strong')).color,secondary:getComputedStyle(el.querySelector('small')).color};
      }));
      assert(options.length>0);assert(options.every(o=>!o.text.includes('<U+')),'UTF-8 labels contain escaped glyphs');
      for(const option of options){assert(contrast(option.primary,option.bg)>=4.5);assert(contrast(option.secondary,option.bg)>=4.5);}
      await page.screenshot({path:path.join(out,`dropdown-${theme}.png`)});
      await page.keyboard.press('Escape');
      results.push({test:'dropdown',theme,minimumContrast:Math.min(...options.map(o=>contrast(o.secondary,o.bg))),sample:options[0]});
      await page.screenshot({path:path.join(out,`refined-explore-${theme}.png`)});
    }
    // Both remaining chart measures use identical real observations in 2D/3D.
    const metricValues={};
    for(const metric of ['performance','rate']){
      await page.locator(`input[name=metric][value=${metric}]`).check({force:true});
      await page.waitForFunction(metric=>{
        const c=echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption();
        return c.yAxis[0].name===({performance:'Currency strength (%)',index:'Starting strength = 100',rate:'Units per 1 EUR'})[metric];
      },metric);
      metricValues[metric]=await page.evaluate(()=>echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption().series.map(s=>({name:s.name,values:s.data.map(v=>v[1])})));
      await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
      await page.waitForFunction(metric=>document.getElementById('strength_3d')?.layout?.scene?.zaxis?.title?.text===({performance:'Strength (%)',index:'Starting strength = 100',rate:'Units per 1 EUR'})[metric],metric);
      assert(await page.locator('#metric').isVisible());
      const traces=await page.evaluate(()=>document.getElementById('strength_3d').data.map(s=>({name:s.name,values:Array.from(s.z),hover:s.text[0]})));
      for(const expected of metricValues[metric]){
        const trace=traces.find(t=>t.name===expected.name);assert(trace);
        assert.equal(trace.values.length,expected.values.length);
        trace.values.forEach((v,i)=>assert(Math.abs(v-expected.values[i])<1e-10,`${metric}, ${trace.name}, row ${i}`));
      }
      results.push({test:'2d-3d-metric-parity',metric,traceCount:traces.length,hover:traces[0].hover});
      await page.screenshot({path:path.join(out,`refined-3d-${metric}.png`)});
      await page.locator('input[name=chart_dimension][value="2d"]').check({force:true});
      await page.waitForSelector('#strength_3d',{state:'hidden'});
    }
    assert.equal(await page.locator('input[name=metric][value=index]').count(),0);
    await page.locator('#tab_data').click();await page.waitForSelector('.dataTables_scrollBody tbody td');await page.waitForTimeout(350);
    const scroller=page.locator('.dataTables_scrollBody');
    const geometry=await scroller.evaluate(el=>({w:el.clientWidth,sw:el.scrollWidth,h:el.clientHeight,sh:el.scrollHeight}));
    assert(geometry.sw>geometry.w);assert(geometry.sh<=geometry.h+1);
    await scroller.hover();await page.mouse.wheel(150,0);await page.waitForTimeout(150);
    assert(await scroller.evaluate(el=>el.scrollLeft)>=140,'Native horizontal wheel should remain available');
    await scroller.evaluate(el=>el.scrollLeft=0);
    const box=await scroller.boundingBox();await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.wheel(0,260);await page.waitForTimeout(150);
    assert(await scroller.evaluate(el=>el.scrollLeft)>200,'Ordinary wheel should reveal wide-table columns');
    const beforeArrow=await scroller.evaluate(el=>el.scrollLeft);
    await page.locator('[data-table-scroll="1"]').click();await page.waitForTimeout(100);
    const afterArrow=await scroller.evaluate(el=>el.scrollLeft);
    console.log('Table scroll geometry',geometry,'before/after arrow',beforeArrow,afterArrow);
    const arrowGeometry=await scroller.evaluate(el=>({w:el.clientWidth,sw:el.scrollWidth}));
    assert(afterArrow>beforeArrow);
    assert(Math.abs(afterArrow-Math.min(arrowGeometry.sw-arrowGeometry.w,beforeArrow+Math.max(120,arrowGeometry.w*.65)))<2);
    await scroller.hover();const beforeEdgeY=await page.locator('#workspace').evaluate(el=>el.scrollTop);
    await page.mouse.wheel(0,180);await page.waitForTimeout(150);
    assert(await page.locator('#workspace').evaluate(el=>el.scrollTop)>beforeEdgeY,'Horizontal edge should release ordinary page scrolling');
    await page.locator('#workspace').evaluate(el=>el.scrollTop=0);
    await scroller.evaluate(el=>el.scrollLeft=0);await scroller.focus();await page.keyboard.press('ArrowRight');
    assert.equal(await scroller.evaluate(el=>el.scrollLeft),100);
    await page.screenshot({path:path.join(out,'refined-data-light.png')});
    results.push({test:'summary-wheel-buttons-keyboard',geometry});
    await page.setViewportSize({width:600,height:812});await page.waitForTimeout(250);
    await page.locator('input[name=data_mode][value=history]').check({force:true});
    await page.waitForFunction(()=>document.querySelector('.dataTables_scrollBody')?.scrollHeight>460);await page.waitForTimeout(200);
    assert(await scroller.evaluate(el=>el.scrollWidth>el.clientWidth),'History fixture needs horizontal overflow');
    await scroller.hover();
    console.log('History geometry',await scroller.evaluate(el=>({w:el.clientWidth,sw:el.scrollWidth,h:el.clientHeight,sh:el.scrollHeight,top:el.getBoundingClientRect().top})),await page.evaluate(()=>({y:scrollY,viewport:innerHeight})));
    await page.mouse.wheel(0,200);await page.waitForTimeout(150);
    assert(await scroller.evaluate(el=>el.scrollTop)>0);assert.equal(await scroller.evaluate(el=>el.scrollLeft),0);
    await page.keyboard.down('Shift');await page.mouse.wheel(0,240);await page.keyboard.up('Shift');await page.waitForTimeout(150);
    assert(await scroller.evaluate(el=>el.scrollLeft)>0);results.push({test:'history-native-vertical-and-shift-horizontal'});
    await page.locator('#tab_explore').click();
    await page.setViewportSize({width:390,height:844});await page.waitForTimeout(300);
    await page.evaluate(()=>window.scrollTo(0,document.body.scrollHeight));await page.waitForTimeout(100);
    const initialY=await page.evaluate(()=>scrollY);
    const settings=page.locator('#mobile_settings_toggle');const settingsBox=await settings.boundingBox();
    assert(settingsBox.y>=0&&settingsBox.y+settingsBox.height<112,'Settings button must stay on screen');
    await settings.click();await page.waitForFunction(()=>document.getElementById('mobile_settings_dialog').open);
    assert(await page.locator('#mobile_settings_dialog #base-selectized').isVisible());
    await page.locator('#sidebar_period > summary').click();
    assert(await page.locator('#mobile_settings_dialog #period').isVisible());
    for(let i=0;i<16;i++){
      await page.keyboard.press('Tab');
      // Native dialogs permit Tab into browser chrome, but not background page controls.
      const focus=await page.evaluate(()=>({inside:document.getElementById('mobile_settings_dialog').contains(document.activeElement),pageHasFocus:document.hasFocus(),id:document.activeElement.id,tag:document.activeElement.tagName}));
      assert(focus.inside||!focus.pageHasFocus,`Background page received modal focus: ${JSON.stringify(focus)}`);
    }
    await page.locator('#mobile_settings_title').evaluate(el=>el.focus());
    await page.locator('#tab_explore').evaluate(el=>el.focus());
    assert(await page.evaluate(()=>document.getElementById('mobile_settings_dialog').contains(document.activeElement)),'Background tab must remain inert');
    await page.locator('#mobile_settings_dialog [data-toggle-theme]').click();
    assert.equal(await page.evaluate(()=>document.documentElement.dataset.theme),'dark');
    await page.locator('#period').selectOption('30');
    await page.waitForFunction(()=>document.querySelector('.period-label')?.textContent.includes('07 Sep 2026'));
    await page.waitForTimeout(300);
    await page.screenshot({path:path.join(out,'refined-mobile-settings-dark.png')});
    await page.locator('[data-mobile-settings-close]').click();
    assert(await page.evaluate(()=>document.getElementById('settings_panel').closest('.app-layout')!==null));
    await page.waitForTimeout(200);const afterY=await page.evaluate(()=>scrollY);
    await settings.click();await page.keyboard.press('Escape');
    await page.waitForFunction(()=>!document.getElementById('mobile_settings_dialog').open);
    assert.equal(await page.evaluate(()=>document.activeElement.id),'mobile_settings_toggle');
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth),390);
    await page.evaluate(()=>scrollTo(0,0));await page.screenshot({path:path.join(out,'refined-mobile-dark.png')});
    results.push({test:'persistent-mobile-settings-modal-theme-date-escape-focus',initialY,afterY});
    assert.deepEqual(errors,[]);assert.equal(await page.locator('.shiny-output-error:not(.shiny-output-error-validation)').count(),0);
    results.push({test:'current-source-status',text:await page.locator('#source_footer').innerText()});
    fs.writeFileSync(path.join(out,'interaction-theme-review.json'),JSON.stringify(results,null,2));
    console.log(JSON.stringify(results,null,2));console.log('PASS: metric parity, dropdown contrast/UTF-8, table navigation and persistent mobile settings.');
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
