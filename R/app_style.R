# =====================================================================
# Colors and motion of the interface (no calculations here)
#   - a calm, flat palette on warm paper; headings in a book typeface
#   - every section has its own accent: Forecasting = slate blue,
#     Aggregate Planning = terracotta, Learning Curve = ochre
#   - quiet motion only: tabs fade in, summary numbers count up,
#     a thin line runs at the top while the app is calculating
#   - on wide screens the menu is a left sidebar
#   - all motion is switched off for users who ask for reduced motion
# The chart colors (app_colors$series) are NOT changed here.
# =====================================================================

app_style_css <- function() {
  tags$style(HTML("
    /* ---------- palette ---------- */
    :root {
      --paper: #f7f5f0; --card: #ffffff; --ink: #1f2328; --muted: #6b6f76; --line: #e4e0d8;
      --side: #1e2731; --side-ink: #c3cad2; --gold: #d9a441;
      --accent: #2f5d8a; --accent-soft: #edf2f7; --accent-ink: #24496d;
      --serif: 'Iowan Old Style', 'Palatino Linotype', Palatino, Georgia, 'Times New Roman', serif;
    }
    .theme-forecast { --accent: #2f5d8a; --accent-soft: #edf2f7; --accent-ink: #24496d; }
    .theme-plan     { --accent: #b5532a; --accent-soft: #f7ede6; --accent-ink: #8c3f1f; }
    .theme-learn    { --accent: #8a7425; --accent-soft: #f4f0e0; --accent-ink: #6b5a1c; }

    body { background: var(--paper) !important; color: var(--ink); }

    /* ---------- headings in a book face ---------- */
    .result-title, .hero h1, .home-card h3, .navbar .navbar-brand, .empty-state .empty-title {
      font-family: var(--serif) !important; letter-spacing: 0 !important; }
    .result-title { font-weight: 600 !important; font-size: 1.3rem !important; color: var(--ink) !important;
      padding-bottom: 8px; margin-bottom: 14px !important; border-bottom: 2px solid var(--accent); display: inline-block; }

    /* ---------- top bar (small screens) ---------- */
    .navbar { background: var(--side) !important; box-shadow: none !important; border-bottom: 1px solid #000; }
    .navbar .navbar-brand { font-weight: 600 !important; color: #fff !important; }
    .navbar .nav-link { color: var(--side-ink) !important; border: none !important; outline: none !important; }
    .navbar .nav-link:hover, .navbar .nav-link.active { color: #fff !important; }
    .navbar .nav-link.active { box-shadow: inset 0 -2px 0 var(--gold) !important; }
    .dropdown-menu { border-radius: 6px !important; border: 1px solid var(--line) !important;
      box-shadow: 0 6px 20px rgba(0,0,0,.08) !important; }
    .dropdown-item:hover { background: #f3f1ec !important; }
    .dropdown-item.active, .dropdown-item:active { background: var(--side) !important; color: #fff !important; }

    /* ---------- panels ---------- */
    .well { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-top: 3px solid var(--accent) !important; border-radius: 6px !important; box-shadow: none !important; }
    .well h4 { color: var(--muted) !important; }
    .formula-box { background: var(--accent-soft) !important; border: none !important;
      border-left: 3px solid var(--accent) !important; border-radius: 4px !important; font-size: .8rem !important; }
    .formula-box .MathJax_CHTML, .formula-box .MathJax { font-size: 108% !important; }
    .btn-outline-primary, .example-btn { color: var(--accent-ink) !important; border-color: var(--accent) !important;
      background: transparent !important; border-radius: 4px !important; }
    .btn-outline-primary:hover, .example-btn:hover { background: var(--accent) !important; color: #fff !important; }
    .btn-outline-secondary { border-radius: 4px !important; }
    .form-control, .selectize-input { border-radius: 4px !important; border-color: #d6d2c9 !important; }
    .form-control:focus, .selectize-input.focus { border-color: var(--accent) !important;
      box-shadow: 0 0 0 3px var(--accent-soft) !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-single { background: var(--accent) !important; border-color: var(--accent) !important; }
    .form-check-input:checked { background-color: var(--accent) !important; border-color: var(--accent) !important; }
    .help-icon { background: transparent !important; border: 1px solid #b9b4aa; color: var(--muted) !important; }
    .help-icon:hover, .help-icon:focus { background: var(--ink) !important; border-color: var(--ink); color: #fff !important; }
    .data-check { border-radius: 4px !important; }
    a, .template-link { color: var(--accent-ink); }

    /* ---------- results ---------- */
    .kpi-row { display: grid !important; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px !important; }
    .kpi { min-width: 0; padding: 10px 12px !important; }
    .kpi .kpi-value { font-size: 1.3rem !important; }
    .kpi .kpi-text { font-size: 1rem !important; }
    .kpi { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-top: 3px solid var(--accent) !important; border-radius: 6px !important; box-shadow: none !important;
      animation: fadeUp .35s ease both; }
    .kpi:nth-child(2) { animation-delay: .05s; } .kpi:nth-child(3) { animation-delay: .1s; }
    .kpi:nth-child(4) { animation-delay: .15s; }
    .kpi .kpi-label { color: var(--muted) !important; }
    .kpi .kpi-value { color: var(--ink) !important; font-variant-numeric: tabular-nums; }
    .shiny-plot-output { border-radius: 6px !important; border-color: var(--line) !important; box-shadow: none !important; }
    table.shiny-table { border-radius: 6px !important; border-color: var(--line) !important; }
    table.shiny-table thead th { background: #faf9f6 !important; color: var(--muted) !important;
      border-bottom: 2px solid var(--line) !important; }
    table.shiny-table tbody tr:nth-child(even) { background: #fcfbf9 !important; }
    table.shiny-table tbody tr:hover { background: var(--accent-soft) !important; }
    .empty-state { border-radius: 6px !important; border: 1px dashed #c9c4b9 !important; background: var(--card) !important; }
    .empty-state:hover, .empty-state.drop-active { border-color: var(--accent) !important; background: var(--accent-soft) !important; }
    .empty-state .empty-icon { color: var(--accent) !important; }
    .well.drop-active { outline: 2px dashed var(--accent) !important; background: var(--accent-soft) !important; }

    /* before any data: show only the drop card, not the empty headings */
    .col-sm-8:has(.empty-state) > h4, .col-sm-8:has(.empty-state) > .table-head,
    .col-sm-8:has(.empty-state) > .row, .col-sm-8:has(.empty-state) > p.result-note,
    .col-sm-8:has(.empty-state) > .result-title { display: none !important; }

    /* tab change: a short, quiet fade */
    .tab-content > .tab-pane.active { animation: fadeUp .3s ease both; }

    /* thin line while calculating */
    html.shiny-busy body::before { content: ''; position: fixed; top: 0; left: 0; height: 2px; width: 40%; z-index: 2000;
      background: var(--gold); animation: busyRun 1s ease-in-out infinite; }

    /* ---------- home page ---------- */
    .hero { background: var(--card) !important; color: var(--ink) !important; border: 1px solid var(--line);
      border-radius: 6px !important; padding: 36px 34px 30px !important; animation: none !important; }
    .hero h1 { color: var(--ink) !important; font-weight: 600 !important; font-size: 2.2rem !important; }
    .hero p { color: var(--muted) !important; font-size: 1.02rem; max-width: 640px; }
    .hero::after { content: ''; display: block; width: 56px; height: 3px; background: var(--gold); margin-top: 18px; }
    .hero-chips { margin-top: 16px; }
    .hero-chip { background: transparent !important; border: 1px solid var(--line) !important; color: var(--muted) !important;
      border-radius: 3px !important; font-size: .8rem !important; }
    .home-card { background: var(--card) !important; border: 1px solid var(--line) !important; border-radius: 6px !important;
      box-shadow: none !important; border-top: 3px solid var(--accent) !important; animation: fadeUp .35s ease both; }
    .home-card .tag { background: transparent !important; color: var(--accent-ink) !important; padding: 0 !important;
      text-transform: uppercase; letter-spacing: .08em; font-size: .7rem !important; }
    .home-card h3 { font-size: 1.3rem !important; font-weight: 600 !important; }
    .home-icon { width: 38px; height: 38px; border-radius: 6px; display: flex; align-items: center; justify-content: center;
      color: var(--accent); background: var(--accent-soft); font-size: 1rem; margin-bottom: 12px; }
    .home-card .card-arrow, .home-card .card-hint { color: var(--accent-ink) !important; }
    .home-card.clickable { cursor: pointer; transition: box-shadow .2s, border-color .2s; }
    .home-card.clickable:hover { border-color: #cfc9bd !important; border-top-color: var(--accent) !important;
      box-shadow: 0 4px 14px rgba(0,0,0,.06) !important; }
    .module-btn { background: transparent !important; border: 1px solid var(--line) !important; border-radius: 3px !important;
      color: var(--ink) !important; font-weight: 500 !important; }
    .module-btn:hover { background: var(--accent) !important; border-color: var(--accent) !important; color: #fff !important; }
    .module-group-title { color: var(--muted) !important; }
    .member { border-radius: 3px !important; border-color: var(--line) !important; font-weight: 500 !important; }

    @keyframes fadeUp  { from { opacity: 0; transform: translateY(6px); } to { opacity: 1; transform: none; } }
    @keyframes busyRun { 0% { left: -40%; } 100% { left: 100%; } }

    /* ---------- left sidebar (wide screens) ---------- */
    @media (min-width: 992px) {
      body { padding-left: 252px; }
      .navbar { position: fixed !important; top: 0; left: 0; bottom: 0; width: 252px; z-index: 1030;
        display: block !important; overflow-y: auto; padding: 26px 14px 30px !important;
        background: var(--side) !important; border-bottom: none; border-right: 1px solid #10161d; }
      .navbar > .container-fluid { display: block !important; padding: 0 !important; }
      .navbar .navbar-brand { display: block; white-space: normal; line-height: 1.3; font-size: 1.25rem;
        margin: 0 8px 24px; padding: 0 0 18px; border-bottom: 1px solid rgba(255,255,255,.10); }
      .navbar .navbar-toggler { display: none; }
      .navbar .navbar-collapse { display: block !important; }
      .navbar .navbar-nav { flex-direction: column !important; width: 100%; gap: 1px; }
      .navbar .nav-item { width: 100%; }
      .navbar .nav-link { display: flex; align-items: center; padding: 8px 10px !important; border-radius: 4px;
        font-weight: 500; transition: background .15s, color .15s; }
      .navbar .nav-link:hover { background: rgba(255,255,255,.05); }
      .navbar .nav-link.active { background: rgba(255,255,255,.06); box-shadow: inset 3px 0 0 var(--gold) !important; }
      .navbar .dropdown { margin-top: 14px; }
      .navbar .dropdown-toggle { font-size: .7rem; text-transform: uppercase; letter-spacing: .1em; font-weight: 600;
        color: #8a949f !important; cursor: pointer; background: transparent !important; box-shadow: none !important; }
      .navbar .dropdown-toggle::after { margin-left: auto; transition: transform .2s; }
      .navbar .dropdown.nav-collapsed .dropdown-toggle::after { transform: rotate(-90deg); }
      .navbar .dropdown-menu { position: static !important; display: block !important; transform: none !important;
        inset: auto !important; float: none; width: 100%; margin: 2px 0 0 !important; padding: 0 0 4px 4px;
        background: transparent !important; border: none !important; box-shadow: none !important; animation: none; }
      .navbar .dropdown.nav-collapsed .dropdown-menu { display: none !important; }
      .navbar .dropdown-header { color: #6f7a85 !important; font-size: .64rem; padding: 10px 10px 3px; letter-spacing: .08em; }
      .navbar .dropdown-divider { border-color: rgba(255,255,255,.07) !important; margin: 6px 10px; }
      .navbar .dropdown-item { color: var(--side-ink) !important; border-radius: 4px; padding: 5px 10px; font-size: .86rem;
        white-space: normal; transition: background .15s, color .15s; }
      .navbar .dropdown-item:hover { background: rgba(255,255,255,.05) !important; color: #fff !important; }
      .navbar .dropdown-item.active { color: #fff !important; font-weight: 600; background: rgba(255,255,255,.07) !important;
        box-shadow: inset 3px 0 0 var(--gold); }
      .container-fluid > .tab-content { padding: 30px 26px 44px !important; }
      html.shiny-busy body::before { margin-left: 252px; }
    }
    /* medium screens: menu stays on top, in one row */
    @media (min-width: 576px) and (max-width: 991.98px) {
      .navbar .navbar-nav { flex-direction: row !important; flex-wrap: wrap; align-items: center; }
      .navbar .nav-link { padding: 8px 12px !important; }
    }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after { animation: none !important; transition: none !important; }
    }
  "))
}

# Puts the section class (theme-forecast / theme-plan / theme-learn) on every tab,
# counts the numbers in the summary tiles up from 0 when they appear, and lets the
# section headers of the left sidebar fold their list.
app_style_js <- function() {
  tagList(
  # hide MathJax's "Processing math" message
  tags$script(type = "text/x-mathjax-config", HTML("MathJax.Hub.Config({ messageStyle: 'none' });")),
  tags$script(HTML("
    (function () {
      function themeTabs() {
        document.querySelectorAll('.navbar .dropdown').forEach(function (dd) {
          var t = dd.querySelector('.dropdown-toggle');
          var cls = t && /Forecast/i.test(t.textContent) ? 'theme-forecast' : 'theme-plan';
          dd.querySelectorAll('a[data-value]').forEach(function (a) {
            document.querySelectorAll('.tab-pane[data-value]').forEach(function (p) {
              if (p.getAttribute('data-value') === a.getAttribute('data-value')) p.classList.add(cls);
            });
          });
        });
        document.querySelectorAll('.tab-pane[data-value=\"Learning Curve\"]').forEach(function (p) { p.classList.add('theme-learn'); });
      }

      var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
      function countUp(el) {
        if (reduce || el.dataset.counted) return;
        var txt = el.textContent.trim();
        var m = txt.match(/^(-?[0-9][0-9,]*\\.?[0-9]*)(.*)$/);
        if (!m) return;
        el.dataset.counted = '1';
        var target = parseFloat(m[1].replace(/,/g, ''));
        var dec = (m[1].split('.')[1] || '').length;
        var rest = m[2], t0 = null, dur = 600;
        function step(ts) {
          if (!t0) t0 = ts;
          var p = Math.min(1, (ts - t0) / dur), e = 1 - Math.pow(1 - p, 3);
          el.textContent = (target * e).toLocaleString('en-US', { minimumFractionDigits: dec, maximumFractionDigits: dec }) + rest;
          if (p < 1) requestAnimationFrame(step); else el.textContent = txt;
        }
        requestAnimationFrame(step);
      }
      var obs = new MutationObserver(function (list) {
        list.forEach(function (r) {
          r.addedNodes.forEach(function (n) {
            if (n.nodeType !== 1) return;
            n.querySelectorAll && n.querySelectorAll('.kpi-value:not(.kpi-text)').forEach(countUp);
          });
        });
      });
      document.addEventListener('click', function (e) {
        var t = e.target.closest && e.target.closest('.navbar .dropdown-toggle');
        if (!t || !window.matchMedia('(min-width: 992px)').matches) return;
        e.preventDefault(); e.stopImmediatePropagation();
        t.closest('.dropdown').classList.toggle('nav-collapsed');
      }, true);
      document.addEventListener('DOMContentLoaded', function () {
        themeTabs();
        obs.observe(document.body, { childList: true, subtree: true });
      });
    })();
  ")))
}
