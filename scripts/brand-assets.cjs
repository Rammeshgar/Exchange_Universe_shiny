// Render only our local, code-native brand assets with the existing browser.
const {chromium}=require('C:/Users/Sadeq/.cache/codex-runtimes/codex-primary-runtime/dependencies/node/node_modules/playwright');
const path=require('path');
const fs=require('fs');
const {pathToFileURL}=require('url');
(async()=>{
 const root=path.resolve(__dirname,'..');
 const browser=await chromium.launch({headless:true,executablePath:'C:/Users/Sadeq/AppData/Local/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-win64/chrome-headless-shell.exe'});
 const page=await browser.newPage({viewport:{width:1200,height:630},deviceScaleFactor:1});
 await page.goto(pathToFileURL(path.join(root,'www/social-preview-source.html')).href);
 await page.evaluate(()=>document.fonts.ready);
 await page.screenshot({path:path.join(root,'www/social-preview.png')});
 const mark=fs.readFileSync(path.join(root,'www/logo-mark.svg'),'utf8');
 for(const size of [32,180]){
   await page.setViewportSize({width:size,height:size});
   await page.setContent('<style>body{margin:0}svg{display:block;width:100vw;height:100vh}</style>'+mark);
   await page.screenshot({path:path.join(root,'www',size===32?'favicon-32.png':'apple-touch-icon.png'),omitBackground:true});
 }
 await browser.close();
 console.log('Rendered social preview 1200x630, favicon 32x32 and touch icon 180x180.');
})().catch(e=>{console.error(e);process.exit(1)});
