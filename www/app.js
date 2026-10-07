(function () {
  'use strict';
  document.documentElement.lang = 'en';
  const storageKey = 'exchange-universe.v2';
  const viewsKey = storageKey + '.views';
  // New disclosure defaults without discarding saved comparisons or theme.
  const sectionsKey = storageKey + '.sections-compact.';
  let lastState = null;
  let toastTimer;
  let pendingMapStyle = null;
  let mapStyleTimer;
  let currentView = 'explore';
  let settingsAnchor;
  let settingsScrollPosition = 0;
  let figureAnchor;
  let figureTrigger;
  let figureScrollPosition = 0;
  const viewCopy = {
    explore: ['Value, across borders.', 'Compare currencies through time and place. Click a country to add its currency.'],
    convert: ['One amount. A world of value.', 'Convert with the latest observation and compare your amount across selected currencies.'],
    data: ['The detail behind the view.', 'Inspect comparison statistics, daily history, and every current provider quote.']
  };
  const readStore = (key, fallback) => { try { return JSON.parse(localStorage.getItem(key)) || fallback; } catch (_) { return fallback; } };
  const writeStore = (key, value) => { try { localStorage.setItem(key, JSON.stringify(value)); return true; } catch (_) { return false; } };
  const notify = (message) => {
    const el = document.getElementById('local_toast');
    if (!el) return;
    el.textContent = message; el.classList.add('visible');
    clearTimeout(toastTimer); toastTimer = setTimeout(() => el.classList.remove('visible'), 4500);
  };
  let shinyConnected = false;
  const pendingInputs = new Map();
  const send = (id, value) => {
    if (!shinyConnected || typeof window.Shiny?.setInputValue !== 'function') {
      pendingInputs.set(id, value); return;
    }
    Shiny.setInputValue(id, value, {priority: 'event'});
  };
  let resizeFrame = null;
  const scheduleResize = () => {
    if (resizeFrame !== null) return;
    resizeFrame = requestAnimationFrame(() => { resizeFrame = null; resizeWidgets(); });
  };
  const sendDates = () => {
    const start = document.getElementById('dates_start');
    const end = document.getElementById('dates_end');
    if (start && end) send('dates', [start.value,end.value]);
  };
  const labelInputs = () => {
    document.querySelectorAll('.selectize-control').forEach(control => {
      const container=control.closest('.shiny-input-container');
      const native=container?.querySelector('select');
      const editable=control.querySelector('input');
      if (!native?.id || !editable) return;
      if (!editable.id) editable.id=native.id+'-search';
      if (!editable.name) editable.name=editable.id;
      container.querySelectorAll('label').forEach(label=>{
        if (label.htmlFor===native.id) label.htmlFor=editable.id;
      });
    });
    document.querySelectorAll('.shiny-input-radiogroup').forEach(group=>{
      const label=group.querySelector(':scope > label');
      if (!label) return;
      label.removeAttribute('for');
      if (!label.id) label.id=group.id+'-label';
      group.setAttribute('role','radiogroup');group.setAttribute('aria-labelledby',label.id);
    });
  };
  let rateHelpButton = null;
  let rateHelpTip = null;
  let rateHelpPinned = false;
  let rateHelpOpenTimer;
  let rateHelpCloseTimer;
  const closeRateHelp = () => {
    clearTimeout(rateHelpOpenTimer); clearTimeout(rateHelpCloseTimer);
    if (rateHelpTip) rateHelpTip.hidden = true;
    rateHelpButton = rateHelpTip = null; rateHelpPinned = false;
  };
  const showRateHelp = (button, pinned = false) => {
    if (!button?.isConnected) return;
    const tip = document.getElementById(button.dataset.rateHelp);
    if (!tip) return;
    if (rateHelpButton !== button) closeRateHelp();
    clearTimeout(rateHelpOpenTimer); clearTimeout(rateHelpCloseTimer);
    rateHelpButton = button; rateHelpTip = tip; rateHelpPinned = pinned || rateHelpPinned;
    tip.hidden = false;
    const anchor = button.getBoundingClientRect();
    const bounds = tip.getBoundingClientRect();
    const below = anchor.bottom + 8;
    tip.style.left = Math.max(8, Math.min(anchor.right - bounds.width, innerWidth - bounds.width - 8)) + 'px';
    tip.style.top = Math.max(8, Math.min(below + bounds.height <= innerHeight - 8 ? below : anchor.top - bounds.height - 8, innerHeight - bounds.height - 8)) + 'px';
  };
  const leaveRateHelp = () => {
    clearTimeout(rateHelpOpenTimer);
    clearTimeout(rateHelpCloseTimer);
    rateHelpCloseTimer = setTimeout(() => {
      if (!rateHelpPinned && !rateHelpButton?.matches(':hover') && !rateHelpTip?.matches(':hover') && document.activeElement !== rateHelpButton) closeRateHelp();
    }, 220);
  };
  document.addEventListener('pointerover', event => {
    if (event.pointerType === 'touch') return;
    const button = event.target.closest('[data-rate-help]');
    if (button && !button.contains(event.relatedTarget)) {
      clearTimeout(rateHelpCloseTimer); clearTimeout(rateHelpOpenTimer);
      rateHelpOpenTimer = setTimeout(() => showRateHelp(button), 450);
    }
    if (event.target.closest('.rate-help')) clearTimeout(rateHelpCloseTimer);
  });
  document.addEventListener('pointerout', event => {
    const region = event.target.closest('[data-rate-help],.rate-help');
    if (region && !region.contains(event.relatedTarget)) leaveRateHelp();
  });
  document.addEventListener('focusin', event => {
    const button = event.target.closest('[data-rate-help]');
    if (button) showRateHelp(button);
  });
  document.addEventListener('focusout', event => {
    if (event.target.closest('[data-rate-help]')) leaveRateHelp();
  });
  document.addEventListener('keydown', event => { if (event.key === 'Escape') closeRateHelp(); });
  window.addEventListener('resize', closeRateHelp);
  document.addEventListener('scroll', event => { if (!event.target.closest?.('.rate-help')) closeRateHelp(); }, true);

  let musicRequested = false;
  const syncMusic = () => {
    const audio = document.getElementById('ambient_music');
    const button = document.getElementById('music_toggle');
    if (!audio || !button) return;
    const playing = !audio.paused;
    button.setAttribute('aria-pressed', String(playing));
    button.setAttribute('aria-label', playing ? 'Pause music' : 'Play music');
    button.title = (playing ? 'Pause' : 'Play') + ' music · 25 12 2021';
    button.querySelector('.music-idle').hidden = playing;
    button.querySelector('.music-playing').hidden = !playing;
  };
  const toggleMusic = async () => {
    const audio = document.getElementById('ambient_music');
    if (!audio) return;
    if (musicRequested || !audio.paused) {
      musicRequested = false; audio.pause(); syncMusic(); return;
    }
    musicRequested = true;
    // No media request, playback or stored autoplay state before this gesture.
    if (!audio.getAttribute('src')) audio.src = audio.dataset.src;
    try { await audio.play(); }
    catch (error) {
      if (error.name !== 'AbortError') notify('Music could not play. Try the music button again.');
    }
    musicRequested = false; syncMusic();
  };
  const getChart = (id) => {
    const element = document.getElementById(id);
    return element && window.echarts ? echarts.getInstanceByDom(element) : null;
  };
  const fitExploreFrame = () => {
    const workspace = document.getElementById('workspace');
    const row = document.querySelector('#view_explore .analysis-grid');
    if (!workspace || !row) return;
    // Keep this stable while scrolling; expanded tables and small screens use normal flow.
    const canFit = !document.fullscreenElement && !document.getElementById('figure_dialog')?.open && currentView === 'explore' && innerWidth > 1000 && innerHeight >= 640 &&
      !row.querySelector('.chart-accessible[open]');
    const offset = row.getBoundingClientRect().top - workspace.getBoundingClientRect().top + workspace.scrollTop;
    const available = Math.floor(workspace.clientHeight - offset - 16);
    const fitted = canFit && available >= 380;
    row.classList.toggle('is-viewport-fit', fitted);
    if (fitted) {
      const height = available + 'px';
      if (row.style.getPropertyValue('--explore-frame-height') !== height) row.style.setProperty('--explore-frame-height', height);
      // Wrapped captions or an unusually short window must never hide controls.
      const clipped = ['.chart-accessible','.map-date'].some(selector=>{
        const footer=row.querySelector(selector);
        return footer && footer.getBoundingClientRect().bottom > footer.closest('.panel').getBoundingClientRect().bottom + 1;
      });
      if (clipped) {
        row.classList.remove('is-viewport-fit');row.style.removeProperty('--explore-frame-height');
      }
    } else row.style.removeProperty('--explore-frame-height');
  };
  const resizeWidgets = () => {
    fitExploreFrame();
    ['comparison_chart','change_chart'].forEach(id => getChart(id)?.resize());
    const map = window.HTMLWidgets?.find('#world_map')?.getMap?.();
    map?.invalidateSize({pan:false});
    // Keep the default world view fitted when chips wrap or the viewport changes.
    // Once the user pans, zooms or asks for a country focus, preserve that view.
    if (map && !map._exchangeViewportEvents) {
      map.on('movestart',()=>{if (!map._exchangeFitting) map._exchangeUserView=true;});
      map._exchangeViewportEvents=true;
    }
    const mapSize = map?.getSize();
    const mapFrame = mapSize ? mapSize.x + 'x' + mapSize.y : '';
    if (map && !map._exchangeUserView && map._exchangeWorldFrame !== mapFrame && currentView === 'explore' && document.querySelector('.rate-card')) {
      map._exchangeFitting=true;
      map.fitBounds([[-57,-175],[79,180]],{padding:[12,12],animate:false});
      map._exchangeFitting=false;map._exchangeWorldFrame=mapFrame;
      map._exchangeViewportReady=true;
    }
    const plot = document.getElementById('strength_3d');
    if (currentView === 'explore' && document.querySelector('input[name=chart_dimension]:checked')?.value === '3d' && plot?._fullLayout && plot.isConnected && plot.clientWidth && window.Plotly) Plotly.Plots.resize(plot);
    if (currentView === 'data' && window.jQuery?.fn.dataTable) jQuery.fn.dataTable.tables({visible:true,api:true}).columns.adjust();
  };
  const publicAppURL = (href) => {
    const url = new URL(href);
    // Keep worker routing in Shiny's base element, not in public navigation/share URLs.
    if (url.hostname.endsWith('.shinyapps.io')) {
      url.pathname = url.pathname.replace(/\/_w_[^/]+(?=\/|$)/g, '');
    }
    return url;
  };
  const showView = (view, updateURL = true) => {
    if (!viewCopy[view]) view = 'explore';
    currentView = view;
    document.querySelectorAll('.workspace-view').forEach(panel => { panel.hidden = panel.id !== 'view_' + view; });
    document.querySelectorAll('[role=tab][data-workspace-view]').forEach(tab => {
      const active = tab.dataset.workspaceView === view;
      tab.classList.toggle('active',active);tab.setAttribute('aria-selected',String(active));tab.tabIndex = active ? 0 : -1;
    });
    const heading = document.getElementById('workspace_title');
    const description = document.getElementById('workspace_description');
    if (heading) heading.textContent = viewCopy[view][0];
    if (description) description.textContent = viewCopy[view][1];
    if (updateURL) {
      const url = publicAppURL(location.href);
      url.hash = view;
      // An absolute URL avoids resolving a hash against Shiny's worker-specific <base>.
      history.replaceState(null, '', url.href);
    }
    send('current_view',view);
    window.ExchangeAnalytics?.view(view);
    const workspace = document.getElementById('workspace');
    if (workspace) workspace.scrollTop = 0;
    scheduleResize();
    // Custom tab panels must tell Shiny which outputs are now visible.
    if (window.jQuery) {
      document.querySelectorAll('.workspace-view').forEach(panel=>jQuery(panel).trigger(panel.hidden?'hidden':'shown'));
    }
  };
  const views = () => {
    const list = readStore(viewsKey, []);
    return Array.isArray(list) ? list.filter(view => view && typeof view.id==='string' && typeof view.name==='string' && view.state).slice(-20) : [];
  };
  // Encode nested arrays explicitly; Shiny's default array input handler flattens them.
  const syncViews = () => send('saved_views', JSON.stringify(views()));
  const setTheme = (theme) => {
    document.documentElement.dataset.theme = theme;
    document.querySelectorAll('#theme_toggle,[data-toggle-theme]').forEach(button => {
      button.setAttribute('aria-label', 'Switch to ' + (theme === 'dark' ? 'light' : 'dark') + ' theme');
      button.setAttribute('aria-pressed', String(theme === 'dark'));
      button.title = button.getAttribute('aria-label');
      const icon = button.querySelector('svg');
      if (icon) icon.innerHTML = theme === 'dark' ? '<circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M2 12h2M20 12h2M5 5l1.5 1.5M17.5 17.5 19 19M5 19l1.5-1.5M17.5 6.5 19 5"/>' : '<path d="M20 13A8 8 0 0 1 11 4a8 8 0 1 0 9 9Z"/>';
    });
    try { localStorage.setItem(storageKey + '.theme', theme); } catch (_) {}
    send('theme', theme);
  };
  const applyMapStyle = () => {
    const widget = window.HTMLWidgets?.find('#world_map');
    const map = widget?.getMap?.();
    if (!map || !pendingMapStyle || !map.layerManager) { mapStyleTimer = setTimeout(applyMapStyle,100); return; }
    let applied = 0;
    pendingMapStyle.ids.forEach((id,index) => {
      const layer = map.layerManager.getLayer('shape',id);
      if (layer) {
        const country = (pendingMapStyle.countries || [pendingMapStyle.country]).includes(id);
        layer.setStyle({fillColor:pendingMapStyle.fills[index],color:country?pendingMapStyle.countryBorder:pendingMapStyle.border,weight:country?3:pendingMapStyle.focused[index]?1.6:0.8});
        if (country) layer.bringToFront();
        applied++;
      }
    });
    if (!applied) mapStyleTimer = setTimeout(applyMapStyle,100);
  };
  try { document.documentElement.dataset.theme = localStorage.getItem(storageKey + '.theme') || 'dark'; } catch (_) { document.documentElement.dataset.theme = 'dark'; }

  // Only convert vertical wheel movement when the table has no vertical rows to scroll.
  // Trackpads, touch scrolling, browser zoom and scrolling at the edges stay native.
  window.ExchangeTableScroll = (scroller) => {
    if (scroller.dataset.exchangeScrollBound) return;
    scroller.dataset.exchangeScrollBound = 'true';
    const update = () => {
      if (!scroller.isConnected) return;
      const max = scroller.scrollWidth - scroller.clientWidth;
      const panel = scroller.closest('.data-panel');
      panel?.querySelector('.table-navigation')?.toggleAttribute('hidden', max <= 1);
      panel?.querySelectorAll('[data-table-scroll]').forEach(button => {
        const atEdge = Number(button.dataset.tableScroll) < 0 ? scroller.scrollLeft <= 1 : scroller.scrollLeft >= max - 1;
        button.setAttribute('aria-disabled', String(atEdge));
      });
    };
    scroller._exchangeScrollUpdate = update;
    scroller.addEventListener('scroll', update, {passive:true});
    scroller.addEventListener('wheel', event => {
      if (event.ctrlKey || event.metaKey || Math.abs(event.deltaX) > 0 || !event.deltaY) return;
      const vertical = scroller.scrollHeight > scroller.clientHeight + 1;
      if (vertical && !event.shiftKey) return;
      const before = scroller.scrollLeft;
      const scale = event.deltaMode === 1 ? 20 : event.deltaMode === 2 ? scroller.clientWidth : 1;
      scroller.scrollLeft += event.deltaY * scale;
      if (Math.abs(scroller.scrollLeft - before) > .5) event.preventDefault();
    }, {passive:false});
    scroller.addEventListener('keydown', event => {
      if (!['ArrowLeft','ArrowRight'].includes(event.key) || event.ctrlKey || event.metaKey || event.altKey ||
          event.target.closest('input,select,textarea,button,a')) return;
      const before = scroller.scrollLeft;
      scroller.scrollLeft += event.key === 'ArrowRight' ? 100 : -100;
      if (scroller.scrollLeft !== before) event.preventDefault();
    });
    const observer = new ResizeObserver(update); observer.observe(scroller);
    // DT replaces its scroller on a data-set change; release obsolete observers.
    const removal = new MutationObserver(() => {
      if (!scroller.isConnected) { observer.disconnect(); removal.disconnect(); }
    });
    removal.observe(document.getElementById('summary_table'), {childList:true,subtree:true});
    requestAnimationFrame(update);
  };
  const openMobileSettings = () => {
    if (!matchMedia('(max-width:800px)').matches) return;
    const dialog = document.getElementById('mobile_settings_dialog');
    const sidebar = document.querySelector('.sidebar');
    if (!dialog || !sidebar || dialog.open) return;
    settingsScrollPosition = window.scrollY;
    settingsAnchor = document.createComment('comparison-settings-home');
    sidebar.before(settingsAnchor);
    dialog.append(sidebar);
    document.getElementById('settings_panel').open = true;
    document.body.classList.add('settings-dialog-open');
    dialog.showModal();
    requestAnimationFrame(() => document.getElementById('mobile_settings_title')?.focus({preventScroll:true}));
  };
  const syncFigureButtons = () => {
    const expanded = document.fullscreenElement || document.getElementById('figure_dialog')?.querySelector('.panel');
    document.querySelectorAll('[data-fullscreen-panel]').forEach(button => {
      if (!button.dataset.expandLabel) button.dataset.expandLabel = button.getAttribute('aria-label');
      const active = expanded?.id === button.dataset.fullscreenPanel;
      button.setAttribute('aria-label', active ? 'Exit expanded figure' : button.dataset.expandLabel);
      button.setAttribute('aria-pressed', String(active));
      button.title = button.getAttribute('aria-label');
    });
    requestAnimationFrame(resizeWidgets);setTimeout(resizeWidgets,180);
  };
  const expandFigure = async (button) => {
    const dialog = document.getElementById('figure_dialog');
    if (dialog?.open) { dialog.close(); return; }
    if (document.fullscreenElement) { await document.exitFullscreen(); return; }
    const panel = document.getElementById(button.dataset.fullscreenPanel);
    if (!panel) return;
    figureTrigger = button;figureScrollPosition = window.scrollY;
    if (document.fullscreenEnabled && panel.requestFullscreen) {
      try { await panel.requestFullscreen();syncFigureButtons();return; } catch (_) { /* Use the accessible in-page fallback. */ }
    }
    if (!dialog?.showModal) { notify('Expanded figures are unavailable in this browser.');return; }
    figureAnchor = document.createComment('figure-home');panel.before(figureAnchor);
    dialog.append(panel);
    document.getElementById('figure_dialog_title').textContent = panel.querySelector('h2')?.textContent || 'Expanded figure';
    document.body.classList.add('figure-dialog-open');dialog.showModal();
    document.getElementById('figure_dialog_title').focus({preventScroll:true});syncFigureButtons();
  };

  const stateFromURL = () => {
    const q = new URLSearchParams(location.search);
    if (!q.has('base') && !q.has('compare')) return null;
    const result = {base: q.get('base'), currencies: (q.get('compare') || '').split(',').filter(Boolean),
      metric: q.get('metric') || 'performance', period: q.get('period') || '30', from: q.get('from'), to: q.get('to'), view:location.hash.slice(1)||'explore'};
    if (q.get('start') && q.get('end')) result.dates = [q.get('start'), q.get('end')];
    return result;
  };

  document.addEventListener('click', async (event) => {
    const info = event.target.closest('[data-rate-help]');
    if (info) {
      if (rateHelpButton === info && rateHelpPinned) closeRateHelp();
      else showRateHelp(info, true);
      return;
    }
    if (!event.target.closest('.rate-help')) closeRateHelp();
    if (event.target.closest('#music_toggle')) { await toggleMusic(); return; }
    const nav = event.target.closest('[data-workspace-view]');
    if (nav) { event.preventDefault(); showView(nav.dataset.workspaceView); }
    const preset = event.target.closest('[data-amount]');
    if (preset) send('quick_amount',{value:Number(preset.dataset.amount),nonce:Date.now()});
    const theme = event.target.closest('#theme_toggle,[data-toggle-theme]');
    if (theme) setTheme(document.documentElement.dataset.theme === 'dark' ? 'light' : 'dark');
    if (event.target.closest('#mobile_settings_toggle')) openMobileSettings();
    if (event.target.closest('[data-mobile-settings-close]')) document.getElementById('mobile_settings_dialog')?.close();
    const tableArrow = event.target.closest('[data-table-scroll]');
    if (tableArrow) {
      const scroller = document.querySelector('#summary_table .dataTables_scrollBody');
      if (scroller) { scroller.scrollLeft += Number(tableArrow.dataset.tableScroll) * Math.max(120, scroller.clientWidth * .65); }
    }
    const add = event.target.closest('[data-add-currency]');
    if (add) send('add_currency', {code: add.dataset.addCurrency, nonce: Date.now()});
    const focus = event.target.closest('[data-focus-currency]');
    if (focus) send('focus_currency', {code: focus.dataset.focusCurrency, nonce: Date.now()});
    const mapCurrency = event.target.closest('[data-map-currency]');
    if (mapCurrency) send('toggle_map_currency', {code:mapCurrency.dataset.mapCurrency,nonce:Date.now()});
    const zoom3D = event.target.closest('[data-zoom-3d]');
    if (zoom3D) {
      const plot = document.getElementById('strength_3d');
      if (window.Plotly && plot?._fullLayout?.scene) {
        // WebGL wheel/drag updates the live camera before Plotly's layout snapshot.
        const scene = plot._fullLayout.scene;
        const camera = JSON.parse(JSON.stringify(scene._scene?.getCamera?.() || scene.camera));
        const action = zoom3D.getAttribute('data-zoom-3d');
        if (action === 'reset') { camera.eye = {x:1.65,y:-1.8,z:1.1};camera.center={x:0,y:0,z:0};camera.up={x:0,y:0,z:1}; }
        else {
          const distance = Math.hypot(camera.eye.x,camera.eye.y,camera.eye.z);
          const target = Math.max(.35, Math.min(12,distance*(action==='in'?.8:1.25)));
          for (const axis of ['x','y','z']) camera.eye[axis] *= target / (distance || 1);
        }
        await Plotly.relayout(plot,{'scene.camera':camera});
      }
    }
    const legend = event.target.closest('[data-legend-currency]');
    if (legend) {
      const chart = getChart('comparison_chart');
      const plot = document.getElementById('strength_3d');
      const is3D = document.querySelector('input[name=chart_dimension]:checked')?.value === '3d';
      if (is3D && plot?.data && window.Plotly) {
        const index = plot.data.findIndex(trace=>trace.name===legend.dataset.legendCurrency);
        if (index>=0) {
          const hidden = plot.data[index].visible !== 'legendonly';
          Plotly.restyle(plot,{visible:hidden?'legendonly':true},[index]);
          legend.classList.toggle('is-muted',hidden);
          legend.setAttribute('aria-pressed',String(!hidden));
        }
      } else if (chart) {
        chart.dispatchAction({type:'legendToggleSelect', name:legend.dataset.legendCurrency});
        const hidden = !legend.classList.contains('is-muted');
        legend.classList.toggle('is-muted', hidden);
        legend.setAttribute('aria-pressed', String(!hidden));
      }
      send('focus_currency', {code: legend.dataset.legendCurrency, nonce: Date.now()});
    }
    const exportButton = event.target.closest('[data-export-chart]');
    if (exportButton) {
      if (exportButton.dataset.exportChart === 'comparison_chart' && document.querySelector('input[name=chart_dimension]:checked')?.value === '3d') {
        const plot = document.getElementById('strength_3d');
        if (!window.Plotly || !plot?.data) { notify('Load the 3D chart before exporting.'); return; }
        try {
          const url = await Plotly.toImage(plot,{format:'png',width:1200,height:800,scale:1});
          const link=document.createElement('a');link.download='exchange-universe-3d.png';link.href=url;link.click();
          notify('3D image download requested.');
        } catch (_) { notify('3D image export is unavailable. Switch to 2D or export the data.'); }
        return;
      }
      const chart = getChart(exportButton.dataset.exportChart);
      if (!chart || !chart.getOption().series?.length) { notify('Load a comparison before exporting.'); return; }
      const link = document.createElement('a');
      link.download = 'exchange-universe-' + new Date().toISOString().slice(0,10) + '.png';
      link.href = chart.getDataURL({type:'png',pixelRatio:2,backgroundColor:getComputedStyle(document.documentElement).getPropertyValue('--surface').trim()});
      link.click(); notify('Chart image download requested.');
    }
    const fullscreen = event.target.closest('[data-fullscreen-panel]');
    if (fullscreen) await expandFigure(fullscreen);
    if (event.target.closest('[data-figure-close]')) document.getElementById('figure_dialog')?.close();
  });
  document.addEventListener('keydown',event=>{
    const tab=event.target.closest('[role=tab][data-workspace-view]');
    if(!tab || !['ArrowLeft','ArrowRight','Home','End'].includes(event.key)) return;
    const tabs=Array.from(document.querySelectorAll('[role=tab][data-workspace-view]'));
    const index=tabs.indexOf(tab);
    const next=event.key==='Home'?0:event.key==='End'?tabs.length-1:(index+(event.key==='ArrowRight'?1:-1)+tabs.length)%tabs.length;
    event.preventDefault();tabs[next].focus();showView(tabs[next].dataset.workspaceView);
  });
  window.addEventListener('hashchange',()=>{const view=location.hash.slice(1);if(viewCopy[view])showView(view,false);});
  // Keep one lazily created WebGL widget, hidden but bound between dimension
  // changes. Its event callbacks always retain a valid chart element.
  document.addEventListener('change', event => {
    if (event.target.name !== 'chart_dimension') return;
    const slot = document.getElementById('three_d_slot');
    if (slot) {
      slot.hidden = event.target.value !== '3d';
      if (window.jQuery) jQuery(slot).trigger(slot.hidden ? 'hidden' : 'shown');
    }
    requestAnimationFrame(resizeWidgets);
  }, true);
  window.addEventListener('resize', scheduleResize);
  document.addEventListener('click', event => {
    const button=event.target.closest('button.remove');
    const native=document.getElementById('currencies');
    if (!button || !native?.selectize || !native.selectize.$control[0].contains(button)) return;
    event.preventDefault();event.stopImmediatePropagation();
    const value=button.parentElement.dataset.value;
    if (value && !native.selectize.isLocked) {
      native.selectize.removeItem(value);native.selectize.focus();
    }
  },true);
  document.addEventListener('change', event => {
    if (event.target.id==='dates_start'||event.target.id==='dates_end') sendDates();
  });
  document.addEventListener('paste', event => {
    if (event.target.matches('.selectize-input input') && !event.target.disabled && !event.target.readOnly) {
      // Let the browser paste into search. This also survives widget rebuilds.
      event.stopImmediatePropagation();
    }
  }, true);
  window.addEventListener('pagehide', () => { shinyConnected = false; });
  // A restored page can contain a closed Shiny socket. Restore saved state via a
  // fresh connection instead of sending events into the abandoned connection.
  window.addEventListener('pageshow', event => { if (event.persisted) location.reload(); });
  document.addEventListener('toggle',event=>{
    if (event.target.matches('.chart-accessible')) requestAnimationFrame(resizeWidgets);
    if (event.target.matches('.sidebar-section')) {
      try { localStorage.setItem(sectionsKey + event.target.id, String(event.target.open)); } catch (_) {}
    }
  },true);

  function bindShiny() {
    if (!window.Shiny || !window.jQuery) { setTimeout(bindShiny, 50); return; }
    Shiny.addCustomMessageHandler('exchange-state', (state) => {
      lastState = state;
      writeStore(storageKey + '.last', state);
    });
    Shiny.addCustomMessageHandler('exchange-toast', notify);
    Shiny.addCustomMessageHandler('exchange-view',view=>showView(view));
    Shiny.addCustomMessageHandler('exchange-dates', dates => {
      if (!Array.isArray(dates)||dates.length!==2) return;
      document.getElementById('dates_start').value=dates[0];
      document.getElementById('dates_end').value=dates[1];
      sendDates();
    });
    Shiny.addCustomMessageHandler('exchange-map-style', (style) => {
      pendingMapStyle = style; clearTimeout(mapStyleTimer); applyMapStyle();
    });
    Shiny.addCustomMessageHandler('exchange-save', (view) => {
      const list = views().filter(v => v.name !== view.name);
      list.push(view);
      if (writeStore(viewsKey, list.slice(-20))) { syncViews(); notify('Comparison saved in this browser.'); }
      else notify('Browser storage is unavailable. Use Share to keep this comparison.');
    });
    Shiny.addCustomMessageHandler('exchange-delete', (id) => {
      writeStore(viewsKey, views().filter(v => v.id !== id)); syncViews(); notify('Saved view removed.');
    });
    Shiny.addCustomMessageHandler('exchange-share', async (state) => {
      const url = publicAppURL(location.href);
      url.search = ''; url.hash = state.view || currentView;
      url.searchParams.set('base', state.base);
      url.searchParams.set('compare', state.currencies.join(','));
      url.searchParams.set('metric', state.metric);
      url.searchParams.set('period', state.period);
      if (state.period === 'custom') { url.searchParams.set('start', state.dates[0]); url.searchParams.set('end', state.dates[1]); }
      url.searchParams.set('from', state.from); url.searchParams.set('to', state.to);
      try { await navigator.clipboard.writeText(url.toString()); notify('Comparison link copied.'); }
      catch (_) { send('share_fallback', url.toString()); }
    });
    jQuery(document).on('shiny:connected', () => {
      shinyConnected = true;
      for (const [id,value] of pendingInputs) send(id,value);
      pendingInputs.clear();
      setTheme(document.documentElement.dataset.theme || 'light');
      const state = stateFromURL() || readStore(storageKey + '.last', {});
      if (location.hash && viewCopy[location.hash.slice(1)]) state.view = location.hash.slice(1);
      showView(state.view || 'explore');
      send('restore_state', state);
      syncViews();
      labelInputs();
      const details = document.getElementById('settings_panel');
      if (details && matchMedia('(max-width:800px)').matches) details.open = false;
      ['base','currencies'].forEach(id => {
        const native = document.getElementById(id);
        if (native?.selectize) {
          const editable = native.selectize.$control_input[0];
          editable.setAttribute('aria-label', id === 'base' ? 'Search base currency' : 'Search comparison currencies');
          editable.setAttribute('aria-describedby', id === 'base' ? 'base_help' : 'compare_help');
        }
      });
    });
    jQuery(document).on('shiny:disconnected', () => { shinyConnected = false; });
    matchMedia('(max-width:800px)').addEventListener('change',event=>{
      if (!event.matches) document.getElementById('mobile_settings_dialog')?.close();
      const details=document.getElementById('settings_panel');
      if(details) details.open=!event.matches;
    });
    jQuery(document).on('shiny:value', (event) => {
      if (event.name === 'rate_cards') closeRateHelp();
      if (event.name === 'chart_legend') setTimeout(()=>{
        const chart=getChart('comparison_chart');
        const plot=document.getElementById('strength_3d');
        const is3D=document.querySelector('input[name=chart_dimension]:checked')?.value==='3d';
        document.querySelectorAll('[data-legend-currency]').forEach(button=>{
          const hidden=is3D?plot?.data?.find(trace=>trace.name===button.dataset.legendCurrency)?.visible==='legendonly':chart?.getOption().legend?.[0]?.selected?.[button.dataset.legendCurrency]===false;
          button.classList.toggle('is-muted',hidden);button.setAttribute('aria-pressed',String(!hidden));
        });
      },150);
      if (['rate_cards','data_notice','chart_caption','chart_legend','map_legend','observation_input','world_map','three_d_slot','strength_3d','comparison_chart','summary_table'].includes(event.name)) scheduleResize();
      if (['comparison_chart','change_chart'].includes(event.name)) setTimeout(() => {
        const element = document.getElementById(event.name);
        if (element) element.setAttribute('role','img');
      }, 100);
      if (event.name === 'comparison_chart') setTimeout(() => {
        const chart = getChart('comparison_chart');
        if (!chart) return;
        chart.off('click');
        chart.on('click', (params) => {
          const value = params.value;
          if (Array.isArray(value)) send('chart_date', String(value[0]).slice(0,10));
        });
      }, 100);
    });
    document.addEventListener('fullscreenchange', () => {
      syncFigureButtons();
      if (!document.fullscreenElement) figureTrigger?.focus({preventScroll:true});
    });
  }
  document.addEventListener('DOMContentLoaded',()=>{
    sendDates();labelInputs();
    let labelFrame=null;
    const labelsObserver=new MutationObserver(()=>{
      if(labelFrame!==null)return;
      labelFrame=requestAnimationFrame(()=>{labelFrame=null;labelInputs();});
    });
    labelsObserver.observe(document.body,{childList:true,subtree:true});
    document.querySelectorAll('.sidebar-section').forEach(section => {
      try { const saved = localStorage.getItem(sectionsKey + section.id);if (saved !== null) section.open = saved === 'true'; } catch (_) {}
    });
    const music = document.getElementById('ambient_music');
    if (music) {
      music.volume = .3;
      ['play','pause','ended'].forEach(name => music.addEventListener(name, syncMusic));
      music.addEventListener('error', () => { musicRequested = false; syncMusic(); notify('The music file is unavailable. Please reload and try again.'); });
    }
    const figureDialog = document.getElementById('figure_dialog');
    figureDialog?.addEventListener('close', () => {
      const panel = figureDialog.querySelector('.panel');
      if (panel && figureAnchor) { figureAnchor.replaceWith(panel);figureAnchor=null; }
      document.body.classList.remove('figure-dialog-open');syncFigureButtons();
      figureTrigger?.focus({preventScroll:true});
      requestAnimationFrame(()=>window.scrollTo(0,figureScrollPosition));
    });
    const dialog = document.getElementById('mobile_settings_dialog');
    dialog?.addEventListener('close', () => {
      const sidebar = dialog.querySelector('.sidebar');
      if (sidebar && settingsAnchor) { settingsAnchor.replaceWith(sidebar); settingsAnchor = null; }
      document.getElementById('settings_panel').open = !matchMedia('(max-width:800px)').matches;
      document.body.classList.remove('settings-dialog-open');
      if (matchMedia('(max-width:800px)').matches) document.getElementById('mobile_settings_toggle')?.focus({preventScroll:true});
      requestAnimationFrame(() => { resizeWidgets(); if (matchMedia('(max-width:800px)').matches) window.scrollTo(0,settingsScrollPosition); });
    });
    dialog?.addEventListener('click', event => {
      if (event.target !== dialog) return;
      const box=dialog.getBoundingClientRect();
      if (event.clientX < box.left || event.clientX > box.right || event.clientY < box.top || event.clientY > box.bottom) dialog.close();
    });
    showView(location.hash.slice(1)||'explore',false);
    syncFigureButtons();
    // Text wrapping and local-font loading can change the space above the figures.
    if (window.ResizeObserver) {
      const observer = new ResizeObserver(scheduleResize);
      ['.intro-row','#rate_cards','#data_notice'].forEach(selector=>{
        const element=document.querySelector(selector);if(element)observer.observe(element);
      });
    }
    document.fonts?.ready.then(()=>requestAnimationFrame(resizeWidgets));
  });
  bindShiny();
})();
