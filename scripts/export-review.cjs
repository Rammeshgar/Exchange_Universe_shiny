// Verify image contents without claiming native file-download delivery.
const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('fs');const assert=require('assert/strict');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
 try{
  const page=await browser.newPage();
  await page.goto('http://127.0.0.1:4888/#explore');
  await page.waitForSelector('#comparison_chart canvas');
  await page.evaluate(()=>{window.imageExport=null;HTMLAnchorElement.prototype.click=function(){if(this.href.startsWith('data:image/png'))window.imageExport=this.href;};});
  await page.locator('[data-export-chart="comparison_chart"]').click();
  let url=await page.evaluate(()=>window.imageExport);assert(url?.length>10000);
  fs.writeFileSync('../Exchange-Universe-Review/refined-chart-export.png',Buffer.from(url.split(',')[1],'base64'));
  console.log('2D export button PNG payload: PASS');
  await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
  await page.waitForFunction(()=>document.getElementById('strength_3d')?.data?.length>0,{timeout:30000});
  await page.evaluate(()=>window.imageExport=null);
  await page.locator('[data-export-chart="comparison_chart"]').click();
  await page.waitForFunction(()=>window.imageExport?.length>10000,{timeout:30000});
  url=await page.evaluate(()=>window.imageExport);
  fs.writeFileSync('../Exchange-Universe-Review/refined-3d-export.png',Buffer.from(url.split(',')[1],'base64'));
  console.log('3D export button PNG payload: PASS');
  await page.waitForSelector('#change_chart canvas');
  await page.evaluate(()=>window.imageExport=null);
  await page.locator('[data-export-chart="change_chart"]').click();
  await page.waitForFunction(()=>window.imageExport?.length>10000);
  url=await page.evaluate(()=>window.imageExport);
  fs.writeFileSync('../Exchange-Universe-Review/strength-snapshot-export.png',Buffer.from(url.split(',')[1],'base64'));
  console.log('Strength snapshot PNG payload while main chart is 3D: PASS');
 }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exit(1)});
