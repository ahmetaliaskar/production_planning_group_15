# =====================================================================
# Colors and motion of the interface (no calculations here)
#   - every section gets its own accent color:
#       Forecasting = blue / green, Aggregate Planning = orange / amber,
#       Learning Curve = violet / pink
#   - tabs fade in, summary tiles pop in and count up, the Home page moves,
#     a colored bar runs at the top while the app is calculating
#   - all motion is switched off for users who ask for reduced motion
# The chart colors (app_colors$series) are NOT changed here.
# =====================================================================

app_style_css <- function() {
  tags$style(HTML("
    :root { --accent: #2a78d6; --accent2: #1baf7a; --accent-soft: #eaf2fc; --accent-ink: #1c5cab; }
    .theme-forecast { --accent: #2a78d6; --accent2: #1baf7a; --accent-soft: #eaf2fc; --accent-ink: #1c5cab; }
    .theme-plan     { --accent: #eb6834; --accent2: #eda100; --accent-soft: #fdf1ea; --accent-ink: #b44a1c; }
    .theme-learn    { --accent: #6250d6; --accent2: #e87ba4; --accent-soft: #f1effc; --accent-ink: #4a3aa7; }

    body { background: radial-gradient(1200px 500px at 100% -10%, #e6effb 0%, rgba(230,239,251,0) 60%),
                       radial-gradient(900px 400px at -10% 110%, #fdeee6 0%, rgba(253,238,230,0) 60%),
                       #f6f6f4 !important; background-attachment: fixed !important; }

    /* navigation bar: moving gradient + animated underline */
    .navbar { background: linear-gradient(110deg, #10294a, #14365c 30%, #1f5a96 65%, #2a78d6) !important;
              background-size: 200% 100% !important; animation: navShift 18s ease-in-out infinite alternate; }
    .navbar .nav-link { position: relative; transition: color .2s; }
    .navbar .nav-link::before { content: ''; position: absolute; left: 12%; right: 12%; bottom: 2px; height: 3px;
              border-radius: 3px; background: linear-gradient(90deg, #1baf7a, #eda100);
              transform: scaleX(0); transition: transform .25s ease; }
    .navbar .nav-link:hover::before, .navbar .nav-link.active::before { transform: scaleX(1); }
    .dropdown-menu { animation: dropIn .18s ease both; }
    .dropdown-item { transition: background .15s, padding-left .15s; }
    .dropdown-item:hover { padding-left: 1.4rem; background: #eef4fc; }

    /* tab change */
    .tab-content > .tab-pane.active { animation: fadeUp .45s ease both; }

    /* left panel */
    .well { border-top: 4px solid var(--accent) !important; position: relative; }
    .formula-box { background: var(--accent-soft) !important; border-color: transparent !important;
              border-left: 4px solid var(--accent) !important; }
    .example-btn, .btn-outline-primary { color: var(--accent-ink) !important; border-color: var(--accent) !important; }
    .example-btn:hover, .btn-outline-primary:hover { color: #fff !important;
              background: linear-gradient(90deg, var(--accent), var(--accent2)) !important; border-color: transparent !important; }
    .btn-outline-secondary { transition: transform .15s, box-shadow .15s; }
    .btn-outline-secondary:hover { transform: translateY(-1px); box-shadow: 0 4px 12px rgba(0,0,0,.12); }
    .form-control:focus, .selectize-input.focus { border-color: var(--accent) !important;
              box-shadow: 0 0 0 .2rem color-mix(in srgb, var(--accent) 22%, transparent) !important; }
    .irs--shiny .irs-bar, .irs--shiny .irs-single { background: var(--accent) !important; border-color: var(--accent) !important; }
    .form-check-input:checked { background-color: var(--accent) !important; border-color: var(--accent) !important; }
    .help-icon { background: var(--accent-soft) !important; color: var(--accent-ink) !important; }
    .help-icon:hover { background: var(--accent) !important; color: #fff !important; }

    /* results */
    .result-title { display: inline-block; position: relative; padding-bottom: 6px; }
    .result-title::after { content: ''; position: absolute; left: 0; bottom: 0; height: 4px; width: 64px;
              border-radius: 4px; background: linear-gradient(90deg, var(--accent), var(--accent2));
              animation: growBar .6s .1s ease both; }
    .kpi { position: relative; overflow: hidden; border: none !important;
              background: linear-gradient(135deg, #fff 55%, var(--accent-soft)) !important;
              box-shadow: 0 1px 2px rgba(0,0,0,.05), 0 6px 18px rgba(20,54,92,.06);
              animation: popIn .5s cubic-bezier(.2,.9,.3,1.2) both; transition: transform .2s, box-shadow .2s; }
    .kpi::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 5px;
              background: linear-gradient(180deg, var(--accent), var(--accent2)); }
    .kpi:hover { transform: translateY(-3px); box-shadow: 0 10px 24px rgba(20,54,92,.12); }
    .kpi:nth-child(2) { animation-delay: .07s; } .kpi:nth-child(3) { animation-delay: .14s; }
    .kpi:nth-child(4) { animation-delay: .21s; }
    .kpi .kpi-value { color: var(--accent-ink) !important; }
    .shiny-plot-output img { animation: fadeIn .5s ease both; }
    .shiny-plot-output:has(img) { box-shadow: 0 6px 18px rgba(20,54,92,.06); }
    table.shiny-table thead th { background: var(--accent-soft) !important; color: var(--accent-ink) !important; }
    table.shiny-table tbody tr { transition: background .15s; }
    table.shiny-table tbody tr:hover { background: var(--accent-soft) !important; }
    .empty-state .empty-icon { animation: floatY 2.4s ease-in-out infinite; color: var(--accent) !important; }
    .empty-state:hover { border-color: var(--accent) !important; }
    .data-check { animation: fadeUp .35s ease both; }

    /* busy bar while the app calculates */
    html.shiny-busy body::before { content: ''; position: fixed; top: 0; left: 0; right: 0; height: 3px; z-index: 2000;
              background: linear-gradient(90deg, #2a78d6, #1baf7a, #eda100, #eb6834, #6250d6, #2a78d6);
              background-size: 300% 100%; animation: busyRun 1.1s linear infinite; }

    /* home page */
    .hero { position: relative; overflow: hidden;
            background: linear-gradient(120deg, #14365c, #1f5a96, #2a78d6, #1baf7a, #1f5a96) !important;
            background-size: 300% 300% !important; animation: heroShift 16s ease-in-out infinite; }
    .hero h1, .hero p, .hero .hero-chips { position: relative; z-index: 1; }
    .hero h1 { animation: fadeUp .6s ease both; }
    .hero p { animation: fadeUp .6s .12s ease both; }
    .hero .blob { position: absolute; border-radius: 50%; filter: blur(2px); opacity: .25; }
    .hero .blob.b1 { width: 220px; height: 220px; right: -40px; top: -60px; background: #eda100; animation: floatY 7s ease-in-out infinite; }
    .hero .blob.b2 { width: 140px; height: 140px; right: 180px; bottom: -70px; background: #e87ba4; animation: floatY 9s 1s ease-in-out infinite; }
    .hero .blob.b3 { width: 90px; height: 90px; right: 90px; top: 40px; background: #1baf7a; animation: floatY 6s .5s ease-in-out infinite; }
    .hero-chips { margin-top: 14px; animation: fadeUp .6s .24s ease both; }
    .hero-chip { display: inline-block; background: rgba(255,255,255,.16); border: 1px solid rgba(255,255,255,.28);
            color: #fff; border-radius: 999px; padding: 4px 12px; margin: 0 6px 6px 0; font-size: .82rem; }
    .home-card { position: relative; overflow: hidden; border: none !important;
            box-shadow: 0 1px 2px rgba(0,0,0,.05), 0 8px 22px rgba(20,54,92,.07); animation: fadeUp .5s ease both; }
    .home-card::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 5px;
            background: linear-gradient(90deg, var(--accent), var(--accent2)); }
    .row > div:nth-child(2) > .home-card { animation-delay: .08s; }
    .row > div:nth-child(3) > .home-card { animation-delay: .16s; }
    .home-icon { width: 46px; height: 46px; border-radius: 14px; display: flex; align-items: center; justify-content: center;
            color: #fff; font-size: 1.2rem; margin-bottom: 10px;
            background: linear-gradient(135deg, var(--accent), var(--accent2));
            box-shadow: 0 6px 14px color-mix(in srgb, var(--accent) 35%, transparent); transition: transform .3s; }
    .home-card.clickable:hover .home-icon { transform: rotate(-8deg) scale(1.08); }
    .home-card .tag { background: var(--accent-soft) !important; color: var(--accent-ink) !important; }
    .home-card .card-arrow, .home-card .card-hint { color: var(--accent) !important; }
    .home-card.clickable:hover { box-shadow: 0 14px 30px rgba(20,54,92,.14) !important; }
    .module-btn { background: var(--accent-soft) !important; border-color: transparent !important;
            color: var(--accent-ink) !important; transition: transform .15s, background .15s; }
    .module-btn:hover { background: linear-gradient(90deg, var(--accent), var(--accent2)) !important;
            color: #fff !important; transform: translateY(-1px); }
    .home-card.open .home-modules { animation: fadeUp .3s ease both; }
    .member { transition: transform .2s, background .2s; }
    .member:hover { transform: translateY(-2px); background: #eef4fc; }

    @keyframes fadeUp  { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: none; } }
    @keyframes fadeIn  { from { opacity: 0; } to { opacity: 1; } }
    @keyframes popIn   { from { opacity: 0; transform: translateY(8px) scale(.97); } to { opacity: 1; transform: none; } }
    @keyframes dropIn  { from { opacity: 0; transform: translateY(-6px); } to { opacity: 1; transform: none; } }
    @keyframes growBar { from { width: 0; } to { width: 64px; } }
    @keyframes floatY  { 0%, 100% { transform: translateY(0); } 50% { transform: translateY(-8px); } }
    @keyframes heroShift { 0% { background-position: 0% 50%; } 50% { background-position: 100% 50%; } 100% { background-position: 0% 50%; } }
    @keyframes navShift  { from { background-position: 0% 50%; } to { background-position: 100% 50%; } }
    @keyframes busyRun   { from { background-position: 0% 0; } to { background-position: 300% 0; } }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after { animation: none !important; transition: none !important; }
    }
  "))
}

# Puts the section class (theme-forecast / theme-plan / theme-learn) on every tab,
# and counts the numbers in the summary tiles up from 0 when they appear.
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
        var rest = m[2], t0 = null, dur = 700;
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
      document.addEventListener('DOMContentLoaded', function () {
        themeTabs();
        obs.observe(document.body, { childList: true, subtree: true });
      });
    })();
  ")))
}
