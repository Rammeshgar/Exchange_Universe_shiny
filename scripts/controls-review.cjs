const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');
const fs=require('fs');
const path=require('path');
const out=path.resolve('../Exchange-Universe-Review');
const axePath=path.resolve('../../work/exchange-qa/node_modules/axe-core/axe.min.js');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  const results=[];
  try{
    const page=await browser.newPage({viewport:{width:1556,height:812}});
    const errors=[];const tracking=[];page.on('pageerror',e=>errors.push(e.stack));
    page.on('request',r=>{if(/clarity\.ms|google-analytics\.com|googletagmanager\.com/.test(r.url()))tracking.push(r.url());});
    await page.goto('http://127.0.0.1:4888/?base=EUR&compare=GBP,HUF,USD,CNY,CAD,JPY&period=180#explore');
    await page.waitForSelector('#comparison_chart canvas');
    await page.waitForFunction(()=>document.querySelectorAll('.rate-card').length===6);
    await page.evaluate(()=>document.fonts.ready);
    assert.equal(await page.locator('script[src="analytics.js"]').count(),1);
    assert.equal(await page.locator('#analytics_banner').isVisible(),false);
    assert.equal(tracking.length,0,'Local tests must not load third-party trackers');
    const hunFill=()=>page.evaluate(()=>HTMLWidgets.find('#world_map').getMap().layerManager.getLayer('shape','HUN').options.fillColor);
    const initial=await hunFill();
    await page.locator('[data-map-currency="HUF"]').click();
    await page.waitForFunction(()=>document.querySelector('[data-map-currency="HUF"]').getAttribute('aria-pressed')==='false');
    await page.waitForFunction(fill=>HTMLWidgets.find('#world_map').getMap().layerManager.getLayer('shape','HUN').options.fillColor!==fill,initial);
    assert(await page.evaluate(()=>document.getElementById('currencies').selectize.items.includes('HUF')));
    await page.locator('[data-map-currency="HUF"]').press('Enter');
    await page.waitForFunction(fill=>HTMLWidgets.find('#world_map').getMap().layerManager.getLayer('shape','HUN').options.fillColor===fill,initial);
    await page.locator('#map_mode').selectOption('change');
    await page.waitForSelector('.map-color-key');
    assert.equal(await page.locator('[data-map-currency]').count(),7);
    await page.waitForTimeout(350);
    const mapColors=await page.evaluate(()=>{
      const key=[...document.querySelectorAll('.map-color-key i')].map(e=>e.style.getPropertyValue('--currency-color').toLowerCase());
      const map=HTMLWidgets.find('#world_map').getMap();
      const islands=Object.values(map._layers).filter(l=>l.options?.radius===6 && l.getLatLng);
      return{key,islandColors:islands.map(l=>l.options.fillColor.toLowerCase()),button:document.querySelector('[data-map-currency="GBP"] i').style.getPropertyValue('--currency-color').toLowerCase(),shape:map.layerManager.getLayer('shape','GBR').options.fillColor.toLowerCase()};
    });
    assert.equal(mapColors.key.length,5);assert.equal(mapColors.button,mapColors.shape);
    assert(mapColors.islandColors.length>0);assert(mapColors.islandColors.every(c=>mapColors.key.includes(c)),'Tiny-country markers use strength colors in Period change mode');
    await page.locator('#map_mode').selectOption('locations');
    results.push({test:'Map legend pointer/keyboard toggles preserve comparison; both coloring modes',pass:true});
    for(const id of ['sidebar_currencies','sidebar_period','sidebar_saved']){
      if(!await page.locator('#'+id).evaluate(el=>el.open))await page.locator('#'+id+' > summary').click();
      await page.locator('#'+id+' > summary').click();
      assert.equal(await page.locator('#'+id).evaluate(el=>el.open),false);
      await page.locator('#'+id+' > summary').press('Enter');
      assert.equal(await page.locator('#'+id).evaluate(el=>el.open),true);
    }
    await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
    await page.waitForFunction(()=>document.getElementById('strength_3d')?._fullLayout?.scene);
    assert(await page.evaluate(()=>document.getElementById('strength_3d')._context.scrollZoom));
    const eye=()=>page.evaluate(()=>document.getElementById('strength_3d')._fullLayout.scene._scene.getCamera().eye);
    const distance=v=>Math.hypot(v.x,v.y,v.z);const before=distance(await eye());
    await page.locator('[data-zoom-3d=in]').click();
    await page.waitForFunction(before=>{const e=document.getElementById('strength_3d')._fullLayout.scene.camera.eye;return Math.hypot(e.x,e.y,e.z)<before;},before);
    await page.locator('[data-zoom-3d=reset]').click();
    await page.waitForFunction(()=>document.getElementById('strength_3d')._fullLayout.scene.camera.eye.x===1.65);
    const plot=page.locator('#strength_3d');await plot.hover();await page.mouse.wheel(0,-180);await page.waitForTimeout(700);
    assert(Math.abs(distance(await eye())-before)>.01,'Real wheel input changes 3D camera distance');
    results.push({test:'3D wheel zoom, explicit zoom/reset',pass:true});
    for(const id of ['comparison_panel','map_panel','change_panel']){
      await page.locator(`[data-fullscreen-panel=${id}]`).click();
      await page.waitForFunction(id=>document.fullscreenElement?.id===id,id);
      await page.waitForTimeout(350);
      assert(await page.locator(`[data-fullscreen-panel=${id}]`).isVisible());
      const figure=await page.locator('#'+id).boundingBox();assert(figure.height>750);
      await page.screenshot({path:path.join(out,id+'-fullscreen.png')});
      await page.locator(`[data-fullscreen-panel=${id}]`).click();
      await page.waitForFunction(()=>!document.fullscreenElement);
    }
    // Exercise the native-dialog fallback used by browsers without element fullscreen.
    await page.evaluate(()=>Object.defineProperty(document,'fullscreenEnabled',{get:()=>false,configurable:true}));
    await page.locator('[data-fullscreen-panel=comparison_panel]').click();
    await page.waitForFunction(()=>document.getElementById('figure_dialog').open);
    assert.equal(await page.locator('#figure_dialog #comparison_panel').count(),1);
    await page.keyboard.press('Escape');await page.waitForFunction(()=>!document.getElementById('figure_dialog').open);
    await page.waitForSelector('.analysis-grid #comparison_panel');
    assert.equal(await page.locator('.analysis-grid #comparison_panel').count(),1);
    await page.locator('input[name=chart_dimension][value="2d"]').check({force:true});
    results.push({test:'All three figures fullscreen + Escape-safe dialog fallback',pass:true});
    for(const [width,height] of [[1867,974],[1556,812],[1366,768],[1280,720]]){
      await page.setViewportSize({width,height});await page.locator('#tab_convert').click();
      await page.waitForSelector('.basket-item');await page.waitForTimeout(250);
      const bounds=await page.evaluate(()=>{
        const rect=selector=>{const r=document.querySelector(selector).getBoundingClientRect();return{top:r.top,bottom:r.bottom,right:r.right,left:r.left};};
        return{basket:rect('.basket-grid'),main:rect('#workspace'),converter:rect('#converter'),horizontal:document.getElementById('workspace').scrollWidth>document.getElementById('workspace').clientWidth};
      });
      assert(!bounds.horizontal,'No Convert horizontal overflow');
      assert(bounds.basket.bottom<=height+1,`Six converted amounts fit ${width}x${height}: ${JSON.stringify(bounds)}`);
      await page.screenshot({path:path.join(out,`convert-${width}x${height}.png`)});
      results.push({test:'Convert fits six amounts',width,height,bounds});
    }
    await page.locator('#tab_data').click();await page.waitForSelector('#summary_table tbody tr');
    const headers=await page.locator('.dataTables_scrollHead thead th').allTextContents();
    assert(headers.includes('Base'));assert(headers.includes('Start-date rate'));assert(!headers.includes('Opening rate'));
    await page.addScriptTag({path:axePath});
    for(const theme of ['dark','light']){
      if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme)await page.locator('#theme_toggle').click();
      for(const view of ['explore','convert','data']){
        await page.locator('#tab_'+view).click();await page.waitForTimeout(250);
        const violations=await page.evaluate(async()=>{const r=await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}});return r.violations.map(v=>({id:v.id,nodes:v.nodes.map(n=>n.target)}));});
        assert.deepEqual(violations,[],JSON.stringify({theme,view,violations}));
      }
    }
    for(const width of [375,390,768]){
      await page.setViewportSize({width,height:812});await page.locator('#tab_convert').click();
      assert.equal(await page.locator('.app-layout > .sidebar').isVisible(),false);
      assert(await page.locator('#mobile_settings_toggle').isVisible());
      assert.equal(await page.locator('.settings-summary').isVisible(),false);
      await page.locator('.footer-credit').scrollIntoViewIfNeeded();
      assert(await page.locator('.footer-credit a').isVisible());
      await page.locator('#mobile_settings_toggle').click();await page.waitForFunction(()=>document.getElementById('mobile_settings_dialog').open);
      if (!await page.locator('#sidebar_saved').evaluate(el=>el.open)) await page.locator('#sidebar_saved > summary').click();
      await page.locator('#sidebar_saved > summary').click();assert.equal(await page.locator('#sidebar_saved').evaluate(el=>el.open),false);
      await page.keyboard.press('Escape');await page.waitForFunction(()=>!document.getElementById('mobile_settings_dialog').open);
      await page.locator('#tab_explore').click();await page.waitForTimeout(300);
      const overflow=await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1);assert(!overflow,'No mobile horizontal page overflow');
      const violations=await page.evaluate(async()=>{const r=await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}});return r.violations.map(v=>({id:v.id,nodes:v.nodes.map(n=>n.target)}));});
      assert.deepEqual(violations,[],JSON.stringify(violations));
      await page.evaluate(()=>{document.activeElement?.blur();window.scrollTo(0,0);});await page.waitForTimeout(100);
      await page.screenshot({path:path.join(out,`controls-mobile-${width}.png`),fullPage:true});
      results.push({test:'Single mobile Settings entry, collapsible sections, credits, contrast/semantics',width,pass:true});
    }
    await page.locator('[data-privacy-open]').first().click();await page.waitForFunction(()=>document.getElementById('privacy_dialog').open);
    assert((await page.locator('#analytics_status').textContent()).includes('disabled'));
    const privacy=await page.evaluate(async()=>{const r=await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}});return r.violations.map(v=>({id:v.id,nodes:v.nodes.map(n=>n.target)}));});assert.deepEqual(privacy,[]);
    await page.keyboard.press('Escape');assert.equal(tracking.length,0);assert.deepEqual(errors,[]);
    assert.equal(await page.locator('.shiny-output-error').count(),0);
    fs.writeFileSync(path.join(out,'controls-review.json'),JSON.stringify({results,errors,tracking},null,2));
    console.log('PASS: map toggles, collapse, 3D zoom, fullscreen/fallback, Convert fit, mobile credits/settings, data labels, accessibility, zero local tracking.');
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
