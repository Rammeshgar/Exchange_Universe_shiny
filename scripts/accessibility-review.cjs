const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const fs=require('fs');
const path=require('path');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
 try {
  const page=await browser.newPage({viewport:{width:1440,height:1000}});
  await page.goto('http://127.0.0.1:4888/');
  await page.waitForSelector('#comparison_chart canvas');
  await page.addScriptTag({path:path.resolve('../..','work/exchange-qa/node_modules/axe-core/axe.min.js')});
  const results=[];
  for(const theme of ['dark','light','mobile']){
   if(theme==='light') {await page.locator('#theme_toggle').click();await page.waitForTimeout(700);}
   if(theme==='mobile') {await page.setViewportSize({width:375,height:812});}
   for(const view of ['explore','convert','data']){
   await page.locator('#tab_'+view).click();
   if(view==='data') await page.waitForSelector('#summary_table tbody tr');
   if(view==='convert') await page.waitForSelector('.conversion-value');
   await page.waitForTimeout(200);
   const result=await page.evaluate(()=>axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}}));
   const violations=result.violations.map(v=>({id:v.id,impact:v.impact,description:v.description,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));
   console.log(theme,view,JSON.stringify(violations));results.push({theme,view,violations});
   }
  }
  await page.setViewportSize({width:1440,height:1000});
  await page.locator('#tab_explore').click();
  await page.locator('input[name=chart_dimension][value="3d"]').check({force:true});
  await page.waitForFunction(()=>document.getElementById('strength_3d')?.data?.length>0,{timeout:30000});
  await page.waitForTimeout(350);
  const threeD=await page.evaluate(()=>axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}}));
  const threeDViolations=threeD.violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));
  console.log('Optional 3D view',JSON.stringify(threeDViolations));results.push({theme:'3d',violations:threeDViolations});
  await page.locator('input[name=chart_dimension][value="2d"]').check({force:true});
  await page.setViewportSize({width:375,height:812});
  await page.locator('#about').click();await page.waitForSelector('.modal.show');
  await page.waitForFunction(()=>getComputedStyle(document.querySelector('.modal.show')).opacity==='1');
  await page.waitForTimeout(350);
  const modal=await page.evaluate(()=>axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}}));
  const modalViolations=modal.violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));console.log('Mobile About dialog',JSON.stringify(modalViolations));results.push({theme:'mobile-dialog',violations:modalViolations});
  await page.keyboard.press('Escape');await page.waitForSelector('.modal.show',{state:'hidden'});console.log('Dialog Escape dismissal: PASS');
  for (const theme of ['light','dark']) {
   if(await page.evaluate(()=>document.documentElement.dataset.theme)!==theme) await page.locator('#theme_toggle').click();
   await page.locator('#mobile_settings_toggle').click();
   await page.waitForFunction(()=>document.getElementById('mobile_settings_dialog').open);
   await page.waitForTimeout(250);
   const settings=await page.evaluate(()=>axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa']}}));
   const settingsViolations=settings.violations.map(v=>({id:v.id,impact:v.impact,nodes:v.nodes.map(n=>({target:n.target,summary:n.failureSummary}))}));
   console.log('Mobile settings '+theme,JSON.stringify(settingsViolations));results.push({theme:'mobile-settings-'+theme,violations:settingsViolations});
   await page.keyboard.press('Escape');await page.waitForFunction(()=>!document.getElementById('mobile_settings_dialog').open);
  }
  if(results.some(r=>r.violations.length)) process.exitCode=1;
  fs.writeFileSync(path.resolve('../Exchange-Universe-Review/accessibility.json'),JSON.stringify(results,null,2));
 } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exit(1)});
