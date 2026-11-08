# =====================================================================
# Visual design of the interface (no calculations here)
#   - enterprise look: white and light-grey surfaces, charcoal text,
#     a dark navy sidebar and one restrained blue accent
#   - Inter typeface, compact tables, thin borders, almost no shadow
#   - one accent for every section, so all pages look like one product
#   - no decorative motion; only the thin "calculating" line remains
#   - on wide screens the menu is a left sidebar
# This file only changes how things look. Content, labels, inputs,
# outputs and the behaviour of every tab are defined elsewhere.
# =====================================================================

app_style_css <- function() {
  tagList(
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
  tags$style(HTML("
    /* ---------- design tokens ---------- */
    :root {
      --page: #f3f4f6; --card: #ffffff; --card-2: #f8f9fb;
      --ink: #1e2329; --ink-2: #434b56; --muted: #6b7380; --faint: #9aa1ab;
      --line: #dde1e6; --line-2: #e9ecef; --line-strong: #c6ccd4;
      --side: #0f1d30; --side-2: #172a44; --side-ink: #a7b2c1; --side-head: #66758a;
      --accent: #1f4e8c; --accent-hover: #183f72; --accent-soft: #edf2f9; --accent-line: #b9cbe3;
      --accent-ink: #1f4e8c;
      --ok: #1d7a55; --ok-soft: #eef7f2; --ok-line: #c3e2d3;
      --warn: #9a6700; --warn-soft: #fdf6e7; --warn-line: #ecd59f;
      --font: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      --radius: 4px; --radius-sm: 3px;
      --shadow: 0 1px 2px rgba(16, 24, 40, .04);
    }
    /* every section uses the same accent */
    .theme-forecast, .theme-plan, .theme-learn {
      --accent: #1f4e8c; --accent-soft: #edf2f9; --accent-ink: #1f4e8c; }

    html, body { font-family: var(--font) !important; font-feature-settings: 'cv11', 'ss01'; }
    body { background: var(--page) !important; color: var(--ink); font-size: 14px; line-height: 1.5;
      -webkit-font-smoothing: antialiased; }
    body, .form-control, .selectize-input, .btn, table { font-family: var(--font) !important; }
    a, .template-link { color: var(--accent); text-decoration: none; }
    a:hover, .template-link:hover { color: var(--accent-hover); text-decoration: underline; }
    :focus-visible { outline: 2px solid var(--accent) !important; outline-offset: 1px; }

    /* ---------- page headings ---------- */
    .result-title, .hero h1, .home-card h3, .navbar .navbar-brand, .empty-state .empty-title {
      font-family: var(--font) !important; }
    .result-title { display: block !important; font-size: 1.125rem !important; font-weight: 600 !important;
      color: var(--ink) !important; letter-spacing: -.01em !important; line-height: 1.35;
      margin: 0 0 16px !important; padding: 0 0 12px !important;
      border-bottom: 1px solid var(--line) !important; }
    .col-sm-8 > h4, .col-sm-8 h4, .table-head h4 {
      font-size: .6875rem !important; font-weight: 600 !important; text-transform: uppercase;
      letter-spacing: .07em !important; color: var(--muted) !important; margin: 26px 0 8px !important; }
    .table-head { margin: 26px 0 8px !important; align-items: center !important; }
    .result-note, .help-block, .form-text { color: var(--muted) !important; font-size: .8125rem !important; }
    hr { border-color: var(--line) !important; opacity: 1; }

    /* ---------- navigation: top bar on small screens ---------- */
    .navbar { background: var(--side) !important; box-shadow: none !important; border: none !important;
      font-size: .875rem; }
    .navbar .navbar-brand { font-weight: 600 !important; font-size: .9375rem !important; color: #fff !important;
      letter-spacing: -.005em !important; }
    .navbar .nav-link { color: var(--side-ink) !important; border: none !important; outline: none;
      font-weight: 500; }
    .navbar .nav-link:hover, .navbar .nav-link.active, .navbar .show > .nav-link { color: #fff !important; }
    .navbar .nav-link.active { box-shadow: inset 0 -2px 0 #5d8fd6 !important; }
    .navbar .navbar-toggler { border-color: rgba(255,255,255,.2) !important; }
    .dropdown-menu { border-radius: var(--radius) !important; border: 1px solid var(--line) !important;
      box-shadow: 0 8px 24px rgba(16,24,40,.10) !important; font-size: .875rem; padding: 4px !important; }
    .dropdown-item { border-radius: var(--radius-sm); padding: 6px 10px; color: var(--ink-2); }
    .dropdown-item:hover { background: var(--card-2) !important; color: var(--ink) !important; }
    .dropdown-item.active, .dropdown-item:active { background: var(--accent) !important; color: #fff !important; }
    .dropdown-header { font-size: .6875rem !important; font-weight: 600; letter-spacing: .07em !important;
      color: var(--faint) !important; text-transform: uppercase; }

    /* ---------- page layout ---------- */
    .container-fluid > .tab-content { padding: 20px 12px 40px !important; }
    .tab-pane > .container-fluid, .tab-pane > .row { max-width: 1480px; }

    /* ---------- input panel (left) ---------- */
    .well { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; box-shadow: var(--shadow) !important; padding: 18px !important; }
    .well h4 { font-size: .6875rem !important; font-weight: 600 !important; text-transform: uppercase;
      letter-spacing: .07em !important; color: var(--muted) !important;
      margin: 20px -18px 14px !important; padding: 14px 18px 0 !important; border-top: 1px solid var(--line-2); }
    .well > h4:first-child, .well > .formula-box + h4 { border-top: none; padding-top: 0 !important; margin-top: 4px !important; }
    .form-label, .control-label, .shiny-input-container > label {
      font-size: .8125rem !important; font-weight: 500 !important; color: var(--ink-2) !important;
      margin-bottom: 4px !important; }
    .shiny-input-container { margin-bottom: 14px; }
    .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; max-width: 100%; }

    .form-control, .selectize-input, .form-select {
      font-size: .875rem !important; color: var(--ink) !important; background-color: var(--card) !important;
      border: 1px solid var(--line-strong) !important; border-radius: var(--radius-sm) !important;
      box-shadow: none !important; min-height: 34px; padding: 6px 10px !important; }
    .selectize-input { padding: 6px 28px 6px 10px !important; line-height: 20px !important; }
    .selectize-input.dropdown-active { border-radius: var(--radius-sm) var(--radius-sm) 0 0 !important; }
    .form-control:hover, .selectize-input:hover { border-color: #aab2bd !important; }
    .form-control:focus, .selectize-input.focus, .form-select:focus {
      border-color: var(--accent) !important; box-shadow: 0 0 0 3px rgba(31,78,140,.14) !important; }
    .selectize-dropdown { border: 1px solid var(--line-strong) !important; border-radius: 0 0 var(--radius-sm) var(--radius-sm) !important;
      box-shadow: 0 8px 20px rgba(16,24,40,.08) !important; font-size: .875rem !important; }
    .selectize-dropdown .option { padding: 6px 10px !important; }
    .selectize-dropdown .active { background: var(--accent-soft) !important; color: var(--ink) !important; }
    .selectize-dropdown .optgroup-header { font-size: .6875rem !important; font-weight: 600; text-transform: uppercase;
      letter-spacing: .07em; color: var(--faint) !important; }
    .input-group .btn-file, .input-group-btn .btn, .input-group > .btn { border-radius: var(--radius-sm) 0 0 var(--radius-sm) !important; }
    .form-check-input { border-color: var(--line-strong) !important; }
    .form-check-input:checked { background-color: var(--accent) !important; border-color: var(--accent) !important; }
    .form-check-input:focus { box-shadow: 0 0 0 3px rgba(31,78,140,.14) !important; }
    .form-check-label, .radio label, .checkbox label { font-size: .875rem; color: var(--ink); }
    .progress { height: 4px !important; border-radius: 2px !important; background: var(--line-2) !important; }
    .progress-bar { background: var(--accent) !important; font-size: 0 !important; }

    .irs--shiny .irs-bar { background: var(--accent) !important; border-color: var(--accent) !important; height: 4px !important; top: 26px !important; }
    .irs--shiny .irs-line { height: 4px !important; top: 26px !important; background: var(--line-2) !important; border: none !important; }
    .irs--shiny .irs-handle { width: 16px !important; height: 16px !important; top: 20px !important;
      border: 2px solid var(--accent) !important; background: #fff !important; box-shadow: none !important; }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--side) !important;
      border-radius: var(--radius-sm) !important; font-size: .75rem !important; }
    .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text { color: var(--muted) !important;
      background: transparent !important; font-size: .6875rem !important; }

    /* ---------- buttons ---------- */
    .btn { font-size: .8125rem !important; font-weight: 500 !important; border-radius: var(--radius-sm) !important;
      padding: 6px 12px !important; box-shadow: none !important; line-height: 1.4; transition: background-color .12s, border-color .12s, color .12s; }
    .btn-primary, .send-btn { background: var(--accent) !important; border: 1px solid var(--accent) !important; color: #fff !important; }
    .btn-primary:hover, .send-btn:hover { background: var(--accent-hover) !important; border-color: var(--accent-hover) !important; filter: none !important; }
    .btn-default, .btn-outline-secondary, .btn-secondary, .shiny-download-link.btn, .btn-file {
      background: var(--card) !important; border: 1px solid var(--line-strong) !important; color: var(--ink-2) !important; }
    .btn-default:hover, .btn-outline-secondary:hover, .btn-secondary:hover, .shiny-download-link.btn:hover, .btn-file:hover {
      background: var(--card-2) !important; border-color: #aab2bd !important; color: var(--ink) !important; }
    .btn-outline-primary, .example-btn { background: var(--card) !important; border: 1px solid var(--accent-line) !important;
      color: var(--accent) !important; }
    .btn-outline-primary:hover, .example-btn:hover { background: var(--accent-soft) !important; border-color: var(--accent) !important;
      color: var(--accent-hover) !important; }
    .btn .fa, .btn .fas, .btn svg { margin-right: 2px; }

    /* ---------- helper boxes ---------- */
    .formula-box { background: var(--card-2) !important; border: 1px solid var(--line) !important;
      border-left: 2px solid var(--accent) !important; border-radius: var(--radius-sm) !important;
      color: var(--ink) !important; font-size: .8125rem !important; padding: 10px 12px !important; }
    .formula-box .MathJax_CHTML, .formula-box .MathJax { font-size: 106% !important; }
    .data-actions { margin: -4px 0 14px !important; }
    .data-check { font-size: .8125rem !important; border-radius: var(--radius-sm) !important; padding: 8px 10px !important;
      background: var(--ok-soft) !important; border: 1px solid var(--ok-line) !important; border-left: 2px solid var(--ok) !important;
      color: var(--ink) !important; }
    .data-check.warn { background: var(--warn-soft) !important; border-color: var(--warn-line) !important; border-left-color: var(--warn) !important; }
    .help-icon { width: 15px !important; height: 15px !important; background: transparent !important;
      border: 1px solid var(--line-strong) !important; color: var(--muted) !important; font-size: .625rem !important;
      font-weight: 600 !important; margin-left: 5px !important; }
    .help-icon:hover, .help-icon:focus { background: var(--accent) !important; border-color: var(--accent) !important; color: #fff !important; }
    .tooltip-inner { background: var(--side) !important; color: #e6ebf1 !important; border-radius: var(--radius-sm) !important;
      font-size: .78rem !important; line-height: 1.45; padding: 8px 10px !important; max-width: 300px !important; }
    .tooltip .tooltip-arrow::before { border-top-color: var(--side) !important; border-bottom-color: var(--side) !important; }
    .shiny-input-container:has(input[type=file])::after { font-size: .75rem !important; color: var(--faint) !important; }
    .shiny-input-container:has(input[type=file]) .form-control { border-style: dashed !important; background: var(--card-2) !important; }

    /* notifications */
    #shiny-notification-panel { width: 340px; max-width: calc(100vw - 32px); }
    .shiny-notification { background: var(--card) !important; color: var(--ink) !important; border: 1px solid var(--line) !important;
      border-left: 3px solid var(--accent) !important; border-radius: var(--radius) !important; opacity: 1 !important;
      box-shadow: 0 8px 24px rgba(16,24,40,.12) !important; font-size: .8125rem; padding: 10px 14px !important; }
    .shiny-notification-warning { border-left-color: var(--warn) !important; }
    .shiny-notification-error { border-left-color: #b42318 !important; }
    .shiny-notification-message { border-left-color: var(--ok) !important; }
    .shiny-notification-close { color: var(--muted) !important; }
    .shiny-output-error-validation { color: var(--muted) !important; background: var(--card) !important;
      border: 1px dashed var(--line-strong) !important; border-radius: var(--radius) !important; font-size: .875rem; }

    /* ---------- summary figures: one connected strip ---------- */
    .kpi-row { display: grid !important; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
      gap: 1px !important; background: var(--line) !important; border: 1px solid var(--line);
      border-radius: var(--radius); overflow: hidden; margin: 0 0 18px !important; box-shadow: var(--shadow); }
    .kpi { min-width: 0; background: var(--card) !important; border: none !important; border-radius: 0 !important;
      box-shadow: none !important; padding: 12px 16px !important; animation: none !important; }
    .kpi .kpi-label { font-size: .6875rem !important; font-weight: 600; text-transform: uppercase;
      letter-spacing: .06em !important; color: var(--muted) !important; }
    .kpi .kpi-value { font-size: 1.375rem !important; font-weight: 600 !important; color: var(--ink) !important;
      letter-spacing: -.015em; font-variant-numeric: tabular-nums; line-height: 1.3; margin-top: 2px; }
    .kpi .kpi-text { font-size: 1rem !important; line-height: 1.35; padding-top: 3px; }
    .kpi .kpi-sub { font-size: .75rem !important; color: var(--muted) !important; margin-top: 1px; }

    /* ---------- charts ---------- */
    .shiny-plot-output { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; box-shadow: var(--shadow) !important; }
    .shiny-plot-output:not(:has(img)) { border: none !important; background: transparent !important; box-shadow: none !important; }
    .shiny-plot-output img { display: block; }

    /* ---------- tables ---------- */
    .shiny-table-output { overflow-x: auto; }
    table.shiny-table { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; border-collapse: separate !important; border-spacing: 0;
      overflow: hidden; font-size: .8125rem !important; color: var(--ink); box-shadow: var(--shadow);
      font-variant-numeric: tabular-nums; margin-bottom: 6px; }
    table.shiny-table thead th { background: var(--card-2) !important; color: var(--ink-2) !important;
      font-size: .75rem !important; font-weight: 600 !important; letter-spacing: .01em;
      border-bottom: 1px solid var(--line) !important; border-top: none !important; white-space: nowrap;
      padding: 8px 12px !important; }
    table.shiny-table td { padding: 6px 12px !important; border-top: none !important;
      border-bottom: 1px solid var(--line-2) !important; }
    table.shiny-table tbody tr:last-child td { border-bottom: none !important; }
    table.shiny-table tbody tr:nth-child(even) { background: transparent !important; }
    table.shiny-table tbody tr:hover { background: #f5f8fc !important; }
    table.shiny-table tbody td:first-child { color: var(--ink-2); font-weight: 500; }

    /* ---------- empty states / drop areas ---------- */
    .empty-state { background: var(--card) !important; border: 1px dashed var(--line-strong) !important;
      border-radius: var(--radius) !important; padding: 40px 24px !important; color: var(--muted) !important;
      font-size: .875rem; transition: border-color .12s, background-color .12s !important; }
    .empty-state:hover, .empty-state.drop-active { border-color: var(--accent) !important; background: var(--accent-soft) !important; }
    .empty-state .empty-icon { color: var(--faint) !important; font-size: 1.5rem !important; margin-bottom: 8px !important; }
    .empty-state:hover .empty-icon { color: var(--accent) !important; }
    .empty-state .empty-title { font-weight: 600 !important; font-size: .9375rem !important; color: var(--ink) !important; }
    .well.drop-active { outline: 2px dashed var(--accent) !important; outline-offset: -4px; background: var(--accent-soft) !important; }

    /* before any data: show only the drop card, not the empty headings */
    .col-sm-8:has(.empty-state) > h4, .col-sm-8:has(.empty-state) > .table-head,
    .col-sm-8:has(.empty-state) > .row, .col-sm-8:has(.empty-state) > p.result-note,
    .col-sm-8:has(.empty-state) > .result-title { display: none !important; }

    /* no decorative motion */
    .tab-content > .tab-pane.active, .kpi, .home-card { animation: none !important; }

    /* thin line while calculating */
    html.shiny-busy body::before { content: ''; position: fixed; top: 0; left: 0; height: 2px; width: 30%; z-index: 2000;
      background: #5d8fd6; animation: busyRun 1.1s ease-in-out infinite; }
    @keyframes busyRun { 0% { left: -30%; } 100% { left: 100%; } }

    /* ---------- home page ---------- */
    .hero { background: var(--card) !important; color: var(--ink) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; padding: 24px 26px !important; margin-bottom: 16px !important;
      box-shadow: var(--shadow); animation: none !important; }
    .hero::after { display: none !important; }
    .hero h1 { color: var(--ink) !important; font-weight: 600 !important; font-size: 1.5rem !important;
      letter-spacing: -.02em !important; margin: 0 0 4px !important; }
    .hero p { color: var(--muted) !important; font-size: .9375rem !important; max-width: 680px; }
    .hero-chips { display: flex; flex-wrap: wrap; gap: 0; margin-top: 18px !important; padding-top: 14px;
      border-top: 1px solid var(--line-2); }
    .hero-chip { display: inline-flex; align-items: center; gap: 6px; background: transparent !important;
      border: none !important; border-right: 1px solid var(--line) !important; border-radius: 0 !important;
      color: var(--ink-2) !important; font-size: .8125rem !important; font-weight: 500; padding: 0 16px !important; margin: 0 !important; }
    .hero-chip:first-child { padding-left: 0 !important; }
    .hero-chip:last-child { border-right: none !important; }
    .hero-chip .fa, .hero-chip svg { color: var(--faint); }

    .home-card { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; box-shadow: var(--shadow) !important; padding: 18px 20px !important;
      animation: none !important; }
    .home-card .tag { background: transparent !important; color: var(--muted) !important; padding: 0 !important;
      text-transform: uppercase; letter-spacing: .07em; font-size: .6875rem !important; font-weight: 600 !important;
      border-radius: 0 !important; margin-bottom: 4px !important; }
    .home-card h3 { font-size: 1rem !important; font-weight: 600 !important; color: var(--ink) !important;
      letter-spacing: -.01em; margin-bottom: 10px !important; }
    .home-card ul { color: var(--ink-2) !important; font-size: .875rem; }
    .home-icon { width: 30px; height: 30px; border-radius: var(--radius-sm); display: flex; align-items: center;
      justify-content: center; color: var(--accent); background: var(--accent-soft); font-size: .8125rem; margin-bottom: 12px; }
    .home-card .card-arrow { color: var(--faint) !important; }
    .home-card .card-hint { color: var(--accent) !important; font-size: .75rem !important; font-weight: 500; }
    .home-card.clickable { transition: border-color .12s, box-shadow .12s !important; }
    .home-card.clickable:hover { border-color: var(--accent-line) !important; box-shadow: 0 2px 8px rgba(16,24,40,.06) !important;
      transform: none !important; }
    .home-card.clickable:hover .card-arrow { color: var(--accent) !important; }
    .home-modules { border-top: 1px solid var(--line-2) !important; }
    .module-group-title { color: var(--faint) !important; font-size: .6875rem !important; font-weight: 600; }
    .module-btn { background: var(--card) !important; border: 1px solid var(--line-strong) !important;
      border-radius: var(--radius-sm) !important; color: var(--ink-2) !important; font-weight: 500 !important;
      font-size: .8125rem !important; padding: 3px 10px !important; }
    .module-btn:hover { background: var(--accent) !important; border-color: var(--accent) !important; color: #fff !important; }
    .member { background: var(--card-2) !important; border: 1px solid var(--line) !important; border-radius: var(--radius-sm) !important;
      font-weight: 500 !important; font-size: .875rem; color: var(--ink) !important; padding: 5px 12px !important; }

    /* ---------- left sidebar (wide screens) ---------- */
    @media (min-width: 992px) {
      body { padding-left: 248px; }
      .navbar { position: fixed !important; top: 0; left: 0; bottom: 0; width: 248px; z-index: 1030;
        display: block !important; overflow-y: auto; padding: 0 10px 28px !important;
        background: var(--side) !important; border-right: 1px solid #0a1422 !important; scrollbar-width: thin;
        scrollbar-color: #2a3b55 transparent; }
      .navbar > .container-fluid { display: block !important; padding: 0 !important; }
      .navbar .navbar-brand { display: block; white-space: normal; line-height: 1.35; font-size: .9375rem !important;
        margin: 0 -10px 14px; padding: 18px 20px 16px; border-bottom: 1px solid rgba(255,255,255,.07);
        background: #0c1828; }
      .navbar .navbar-toggler { display: none; }
      .navbar .navbar-collapse { display: block !important; }
      .navbar .navbar-nav { flex-direction: column !important; width: 100%; gap: 1px; }
      .navbar .nav-item { width: 100%; }
      .navbar .nav-link { display: flex; align-items: center; padding: 7px 10px !important; border-radius: var(--radius-sm);
        font-size: .8125rem; font-weight: 500; transition: background-color .12s, color .12s; }
      .navbar .nav-link:hover { background: rgba(255,255,255,.05); }
      .navbar .nav-link.active { background: var(--side-2); box-shadow: inset 2px 0 0 #5d8fd6 !important; color: #fff !important; }
      .navbar .dropdown { margin-top: 16px; }
      .navbar .dropdown-toggle { font-size: .6875rem; text-transform: uppercase; letter-spacing: .09em; font-weight: 600;
        color: var(--side-head) !important; cursor: pointer; background: transparent !important; box-shadow: none !important;
        padding: 6px 10px !important; }
      .navbar .dropdown-toggle:hover { color: #c7d0dc !important; background: transparent !important; }
      .navbar .dropdown-toggle::after { margin-left: auto; transition: transform .15s; opacity: .7; }
      .navbar .dropdown.nav-collapsed .dropdown-toggle::after { transform: rotate(-90deg); }
      .navbar .dropdown-menu { position: static !important; display: block !important; transform: none !important;
        inset: auto !important; float: none; width: 100%; margin: 0 !important; padding: 0 0 2px !important;
        background: transparent !important; border: none !important; box-shadow: none !important; animation: none; }
      .navbar .dropdown.nav-collapsed .dropdown-menu { display: none !important; }
      .navbar .dropdown-header { color: #56657a !important; font-size: .625rem !important; padding: 10px 10px 3px !important;
        letter-spacing: .08em !important; }
      .navbar .dropdown-divider { border-color: rgba(255,255,255,.06) !important; margin: 6px 10px !important; }
      .navbar .dropdown-item { color: var(--side-ink) !important; border-radius: var(--radius-sm); padding: 5px 10px 5px 12px;
        font-size: .8125rem; white-space: normal; line-height: 1.35; transition: background-color .12s, color .12s; }
      .navbar .dropdown-item:hover { background: rgba(255,255,255,.05) !important; color: #fff !important; }
      .navbar .dropdown-item.active { color: #fff !important; font-weight: 500; background: var(--side-2) !important;
        box-shadow: inset 2px 0 0 #5d8fd6; }
      .container-fluid > .tab-content { padding: 24px 28px 48px !important; }
      html.shiny-busy body::before { margin-left: 248px; }
    }
    /* medium screens: menu stays on top, in one row */
    @media (min-width: 576px) and (max-width: 991.98px) {
      .navbar .navbar-nav { flex-direction: row !important; flex-wrap: wrap; align-items: center; }
      .navbar .nav-link { padding: 8px 12px !important; }
    }
    /* phones: inputs above results, comfortable gutters */
    @media (max-width: 767.98px) {
      .container-fluid > .tab-content { padding: 16px 4px 32px !important; }
      .well { margin-bottom: 16px; }
      .hero { padding: 18px 16px !important; }
      .hero h1 { font-size: 1.25rem !important; }
      .hero-chip { border-right: none !important; padding: 2px 12px 2px 0 !important; }
      .kpi-row { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .navbar .navbar-collapse { max-height: 75vh; overflow-y: auto; }
    }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after { animation: none !important; transition: none !important; }
    }
  ")))
}

# Puts the section class (theme-forecast / theme-plan / theme-learn) on every tab
# and lets the section headers of the left sidebar fold their list.
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
      document.addEventListener('click', function (e) {
        var t = e.target.closest && e.target.closest('.navbar .dropdown-toggle');
        if (!t || !window.matchMedia('(min-width: 992px)').matches) return;
        e.preventDefault(); e.stopImmediatePropagation();
        t.closest('.dropdown').classList.toggle('nav-collapsed');
      }, true);
      document.addEventListener('DOMContentLoaded', themeTabs);
    })();
  ")))
}
