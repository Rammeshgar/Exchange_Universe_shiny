const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const assert=require('assert/strict');
const fs=require('fs');
const path=require('path');
const out=path.resolve('../Exchange-Universe-Review');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
  const results=[];
  try {
    for(const size of [{width:1867,height:974,dpr:1},{width:1556,height:812,dpr:1.2},{width:1366,height:768,dpr:1},{width:1280,height:720,dpr:1}]){
      const context=await browser.newContext({viewport:{width:size.width,height:size.height},deviceScaleFactor:size.dpr});
      const page=await context.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
      await page.goto('http://127.0.0.1:4888/?base=EUR&compare=GBP,HUF,USD&metric=index&period=90#explore');
      await page.waitForFunction(()=>document.querySelector('#comparison_chart canvas') && document.querySelectorAll('.rate-card').length===3);
      await page.waitForFunction(()=>window.HTMLWidgets?.find('#world_map')?.getMap()?.layerManager?.getLayer('shape','JPN'));
      await page.waitForFunction(()=>{
        const chart=window.echarts?.getInstanceByDom(document.getElementById('comparison_chart'))?.getOption();
        return chart?.yAxis?.[0]?.name==='Currency strength (%)' && chart.series?.every(s=>s.data.length===91);
      });
      await page.waitForTimeout(400);
      for(const count of [3,4,6]){
        if(count!==3){
          await page.evaluate(count=>document.getElementById('currencies').selectize.setValue(['GBP','HUF','USD','JPY','CAD','CHF'].slice(0,count)),count);
          await page.waitForFunction(count=>document.querySelectorAll('.rate-card').length===count,count);await page.waitForTimeout(350);
          await page.waitForFunction(count=>window.echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption().series.length===count,count);
        }
        const geometry=await page.evaluate(()=>{
          const r=e=>{const b=e.getBoundingClientRect();return {top:b.top,bottom:b.bottom,width:b.width,height:b.height};};
          return {viewport:[innerWidth,innerHeight],main:r(document.getElementById('workspace')),row:r(document.querySelector('.analysis-grid')),
            fitted:document.querySelector('.analysis-grid').classList.contains('is-viewport-fit'),chart:r(document.getElementById('comparison_chart')),
            map:r(document.getElementById('world_map')),chartFooter:r(document.querySelector('.chart-accessible')),
            mapFooter:r(document.querySelector('.map-date')),scroll:document.getElementById('workspace').scrollTop,
            width:document.documentElement.scrollWidth,
            worldVisible:HTMLWidgets.find('#world_map').getMap().getBounds().contains([79,0]) && HTMLWidgets.find('#world_map').getMap().getBounds().contains([-57,0])};
        });
        console.log(`${size.width}x${size.height} @${size.dpr}, ${count} currencies:`,JSON.stringify(geometry));
        if(count===6)await page.screenshot({path:path.join(out,`viewport-${size.width}x${size.height}-six.png`)});
        assert(geometry.fitted,'Main figures should use the available viewport');
        assert(geometry.row.bottom<=size.height-10,'Primary panels extend below the initial screen');
        assert(geometry.chartFooter.bottom<=geometry.row.bottom+1,'Chart controls are clipped inside panel');
        assert(geometry.mapFooter.bottom<=geometry.row.bottom+1,'Map date is clipped inside panel');
        assert(geometry.chart.height>=180 && geometry.map.height>=200,'Figures became too small');
        assert.equal(geometry.scroll,0);assert.equal(geometry.width,size.width);
        assert(geometry.worldVisible,'Initial map should frame the world, not crop its southern countries');
        results.push({size,count,geometry});
        if(count===3)await page.screenshot({path:path.join(out,`viewport-${size.width}x${size.height}.png`)});
      }
      assert.deepEqual(errors,[]);await context.close();
    }
    const page=await browser.newPage({viewport:{width:1556,height:812}});
    await page.goto('http://127.0.0.1:4888/');await page.waitForSelector('#comparison_chart canvas');
    await page.waitForFunction(()=>window.HTMLWidgets?.find('#world_map')?.getMap()?.layerManager?.getLayer('shape','JPN'));
    await page.waitForTimeout(350);
    // Interact with the actual country shapes through browser pointer events.
    const clickCountry=async(iso)=>{
      const p=await page.evaluate(iso=>{
        const map=HTMLWidgets.find('#world_map').getMap();const layer=map.layerManager.getLayer('shape',iso);
        const point=map.latLngToContainerPoint(layer.getBounds().getCenter());const box=document.getElementById('world_map').getBoundingClientRect();
        return {x:box.x+point.x,y:box.y+point.y};
      },iso);await page.mouse.click(p.x,p.y);await page.waitForTimeout(400);
    };
    await clickCountry('JPN');await page.waitForSelector('[data-focus-currency="JPY"]');
    await clickCountry('CAN');await page.waitForSelector('[data-focus-currency="CAD"]');
    await clickCountry('JPN');await page.waitForSelector('[data-focus-currency="JPY"]',{state:'detached'});
    assert(await page.locator('[data-focus-currency="CAD"]').count());
    console.log('Deselect a previously clicked location (not just the latest): PASS');
    await clickCountry('USA');await clickCountry('USA');assert(await page.locator('[data-focus-currency="USD"]').count());
    console.log('Deselecting a country preserves a manually selected currency: PASS');
    await clickCountry('SEN');await page.waitForSelector('[data-focus-currency="XOF"]');
    await clickCountry('CIV');await clickCountry('SEN');assert(await page.locator('[data-focus-currency="XOF"]').count());
    await clickCountry('CIV');await page.waitForSelector('[data-focus-currency="XOF"]',{state:'detached'});
    console.log('Shared map-added currency remains until its last location is deselected: PASS');
    await page.locator('#clear_map').click();await page.waitForSelector('[data-focus-currency="CAD"]',{state:'detached'});
    assert.equal(await page.locator('.rate-card').count(),3);
    console.log('Clear map keeps the original comparison: PASS');
    await clickCountry('JPN');await page.waitForSelector('[data-focus-currency="JPY"]');
    await page.evaluate(()=>document.getElementById('currencies').selectize.removeItem('JPY'));
    await page.waitForSelector('[data-focus-currency="JPY"]',{state:'detached'});await page.waitForTimeout(150);
    await page.evaluate(()=>document.getElementById('currencies').selectize.addItem('JPY'));
    await page.waitForSelector('[data-focus-currency="JPY"]');await page.waitForTimeout(150);
    await clickCountry('JPN');await clickCountry('JPN');assert(await page.locator('[data-focus-currency="JPY"]').count());
    await page.evaluate(()=>document.getElementById('currencies').selectize.removeItem('JPY'));
    await page.waitForSelector('[data-focus-currency="JPY"]',{state:'detached'});
    console.log('Manual removal and re-addition ends map ownership: PASS');
    await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
    await page.waitForFunction(()=>document.getElementById('strength_3d')?.data?.length>0);await page.waitForTimeout(700);
    const threeD=await page.locator('#strength_3d').boundingBox();assert(threeD.y+threeD.height<=812);assert(threeD.height>=180);
    await page.screenshot({path:path.join(out,'viewport-3d.png')});console.log('Optional 3D fits the same primary frame: PASS');
    await page.locator('input[name=chart_dimension][value="2d"]').check({force:true});
    await page.locator('.chart-accessible summary').click();
    await page.waitForFunction(()=>!document.querySelector('.analysis-grid').classList.contains('is-viewport-fit'));
    await page.waitForSelector('#chart_values tbody tr');
    await page.locator('.chart-accessible summary').click();
    await page.waitForFunction(()=>document.querySelector('.analysis-grid').classList.contains('is-viewport-fit'));
    console.log('Expanded exact values retain normal accessible scrolling: PASS');
    await page.setViewportSize({width:1024,height:640});await page.waitForTimeout(300);
    assert.equal(await page.evaluate(()=>document.querySelector('.analysis-grid').classList.contains('is-viewport-fit')),false);
    assert(await page.evaluate(()=>Array.from(document.querySelectorAll('.chart-accessible,.map-date')).every(e=>e.getBoundingClientRect().bottom<=e.closest('.panel').getBoundingClientRect().bottom+1)));
    console.log('Very short desktop uses normal flow without clipping controls: PASS');
    await page.setViewportSize({width:390,height:844});await page.waitForTimeout(300);
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth),390);
    assert.equal(await page.evaluate(()=>document.querySelector('.analysis-grid').classList.contains('is-viewport-fit')),false);
    assert.equal(await page.locator('#tab_data').isVisible(),true);
    await page.screenshot({path:path.join(out,'viewport-mobile.png'),fullPage:true});
    console.log('Mobile retains stacked, readable plots: PASS');
    fs.writeFileSync(path.join(out,'viewport-review.json'),JSON.stringify(results,null,2));
  }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
