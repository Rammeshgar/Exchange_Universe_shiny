const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('fs');const path=require('path');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
 try{
  const page=await browser.newPage({viewport:{width:1440,height:1000}});const results=[];
  for(let i=0;i<3;i++){
   const start=Date.now();await page.goto('http://127.0.0.1:4888/?qa_run='+i+'#explore');await page.waitForSelector('#comparison_chart canvas');const chartMs=Date.now()-start;
   await page.waitForFunction(()=>Object.values(HTMLWidgets.find('#world_map').getMap()._layers).filter(l=>l.getLatLngs).length>200);
   const mapMs=Date.now()-start;
   const lazy3D=await page.evaluate(()=>!performance.getEntriesByType('resource').some(r=>/plotly.*\.js/i.test(r.name)));
   const dataStart=Date.now();await page.locator('#tab_data').click();await page.waitForSelector('#summary_table tbody tr');
   results.push({run:i+1,chartMs,mapMs,dataViewMs:Date.now()-dataStart,plotlyNotLoadedInitially:lazy3D});
   await page.locator('#tab_explore').click();
  }
  const start=Date.now();await page.selectOption('#period','90');await page.waitForFunction(()=>document.querySelector('#period_label')?.textContent.includes('Jul')&&echarts.getInstanceByDom(document.getElementById('comparison_chart')).getOption().series[0].data.length>31);results.push({cachedRangeUpdateMs:Date.now()-start});
  console.log(JSON.stringify(results));fs.writeFileSync(path.resolve('../Exchange-Universe-Review/performance.json'),JSON.stringify(results,null,2));
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
