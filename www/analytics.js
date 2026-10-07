(function () {
  'use strict';
  if (window.ExchangeAnalytics) return; // Shiny reconnects must not install tags twice.
  const gaID = 'G-7B8GY61MGS';
  const clarityID = 'yu2u0am8kk';
  const choiceKey = 'exchange-universe.analytics-consent.v1';
  const choiceLifetime = 180 * 24 * 60 * 60 * 1000;
  const host = location.hostname.toLowerCase();
  const local = !['http:','https:'].includes(location.protocol) || !host ||
    /^(localhost|.*\.localhost|127(?:\.\d+){3}|0\.0\.0\.0|\[?::1\]?|10(?:\.\d+){3}|192\.168(?:\.\d+){2}|172\.(?:1[6-9]|2\d|3[01])(?:\.\d+){2})$/.test(host);
  let active = false;
  let lastView = '';
  let privacyTrigger;
  let pageChoice = null;
  const readChoice = () => {
    if (pageChoice) return pageChoice;
    try {
      const saved = JSON.parse(localStorage.getItem(choiceKey));
      return saved && ['granted','denied'].includes(saved.value) && Number.isFinite(saved.time) &&
        saved.time <= Date.now() && Date.now() - saved.time < choiceLifetime ? saved.value : null;
    } catch (_) { return null; }
  };
  const storageConsent = value => ({analytics_storage:value,ad_storage:'denied',ad_user_data:'denied',ad_personalization:'denied'});
  const install = () => {
    if (active || local) return;
    active = true;
    window.dataLayer = window.dataLayer || [];
    window.gtag = window.gtag || function () { window.dataLayer.push(arguments); };
    window.gtag('consent','default',storageConsent('denied'));
    // Loading tags automatically is not evidence of visitor consent.
    const consent = readChoice() === 'granted' ? 'granted' : 'denied';
    if (consent === 'granted') window.gtag('consent','update',storageConsent(consent));
    window.gtag('js',new Date());
    // No query parameters, custom saved names, amounts or user identity in GA4.
    window.gtag('config',gaID,{page_location:location.origin + location.pathname,
      page_referrer:document.referrer ? (()=>{try { const u=new URL(document.referrer);return u.origin+u.pathname; } catch (_) {return '';} })() : '',
      allow_google_signals:false,allow_ad_personalization_signals:false});
    window.clarity = window.clarity || function () { (window.clarity.q = window.clarity.q || []).push(arguments); };
    window.clarity('consentv2',{analytics_Storage:consent,ad_Storage:'denied'});
    for (const [id,src] of [
      ['exchange-ga4','https://www.googletagmanager.com/gtag/js?id=' + gaID],
      ['exchange-clarity','https://www.clarity.ms/tag/' + clarityID]
    ]) {
      if (document.getElementById(id)) continue;
      const script = document.createElement('script');script.id=id;script.async=true;script.src=src;document.head.append(script);
    }
  };
  const view = name => {
    if (!active || local || !['explore','convert','data'].includes(name) || lastView === name) return;
    lastView = name;window.gtag('event','app_view',{view_name:name,send_to:gaID});
  };
  const render = () => {
    const choice = readChoice();
    document.getElementById('analytics_banner')?.toggleAttribute('hidden',local || choice !== null);
    const status = document.getElementById('analytics_status');
    if (status) status.textContent = local ? 'Tracking is disabled on this local preview.' :
      choice === 'granted' ? 'Analytics: allowed in this browser.' : choice === 'denied' ? 'Analytics: disabled in this browser.' : 'Analytics tags load automatically with cookie consent denied.';
  };
  const choose = value => {
    if (!['granted','denied'].includes(value)) return;
    pageChoice = value;
    try { localStorage.setItem(choiceKey,JSON.stringify({value,time:Date.now()})); } catch (_) { /* The choice lasts only for this page if browser storage is blocked. */ }
    const banner = document.getElementById('analytics_banner');
    if (banner?.contains(document.activeElement)) document.querySelector('[data-privacy-open]')?.focus({preventScroll:true});
    banner?.setAttribute('hidden','');
    document.getElementById('privacy_dialog')?.close();
    if (value === 'granted') { install();view(location.hash.slice(1) || 'explore'); }
    else if (active) {
      window['ga-disable-' + gaID] = true;
      window.gtag('consent','update',storageConsent('denied'));
      window.clarity('consentv2',{analytics_Storage:'denied',ad_Storage:'denied'});
      // Best-effort removal of this integration's first-party cookies before reload.
      const names = ['_ga','_ga_' + gaID.replace('G-',''),'_clck','_clsk'];
      const paths = Array.from(new Set(['/',location.pathname,location.pathname.replace(/\/$/,'')])).filter(Boolean);
      const domains = [null,host,'.' + host];
      for (const name of names) for (const path of paths) for (const domain of domains) {
        try { document.cookie = name + '=; Max-Age=0; path=' + path + (domain ? '; domain=' + domain : '') + '; SameSite=Lax'; } catch (_) {}
      }
      location.reload();return;
    }
    render();
  };
  window.ExchangeAnalytics = {view};
  document.addEventListener('click', event => {
    const choice = event.target.closest('[data-analytics-consent]');
    if (choice) choose(choice.dataset.analyticsConsent);
    const open = event.target.closest('[data-privacy-open]');
    const dialog = document.getElementById('privacy_dialog');
    if (open && dialog && !dialog.open) {
      privacyTrigger=open;render();dialog.showModal();document.getElementById('privacy_title').focus({preventScroll:true});
    }
    if (event.target.closest('[data-privacy-close]')) dialog?.close();
  });
  window.addEventListener('storage', event => {
    if (event.key !== choiceKey) return;
    pageChoice = null;
    const choice=readChoice();
    if (active && choice !== 'granted') location.reload();
    else { if (choice === 'granted') install();render(); }
  });
  const ready = () => {
    document.getElementById('privacy_dialog')?.addEventListener('close',()=>privacyTrigger?.focus({preventScroll:true}));
    if (readChoice() !== 'denied') { install();view(location.hash.slice(1) || 'explore'); }
    render();
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',ready,{once:true});else ready();
})();
