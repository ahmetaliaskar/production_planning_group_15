# =====================================================================
# Visual design of the interface (no calculations here)
#   - modern, calm product look: light theme by default, a dark theme
#     one click away (button at the top of the menu; the choice is kept)
#   - dark navy sidebar in both themes; white / graphite panels
#   - every section has its own color, so pages are not uniform:
#       Forecasting = teal, Aggregate Planning = amber,
#       Learning Curve = violet, Home / Data Library = blue
#   - typefaces: Manrope (titles and figures), IBM Plex Sans (text)
#   - Home: small module tiles; a tile with methods opens a panel that
#     branches out below the tiles, so many modules fit on one screen
#   - charts are drawn on a transparent background (see app_colors)
# This file only changes how things look. Content, labels, inputs,
# outputs and the behaviour of every tab are defined elsewhere.
# =====================================================================

app_style_css <- function() {
  tagList(
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet",
            href = paste0("https://fonts.googleapis.com/css2?family=Manrope:wght@500;600;700;800",
                          "&family=IBM+Plex+Sans:wght@400;500;600&display=swap")),
  tags$style(HTML("
    /* ---------- design tokens: light theme ---------- */
    :root {
      color-scheme: light;
      --page: #f4f6f9; --card: #ffffff; --card-2: #f8fafc; --hover: #f2f6fc;
      --ink: #0f172a; --ink-2: #334155; --muted: #64748b; --faint: #94a3b8;
      --line: #e2e8f0; --line-2: #eef2f6; --line-strong: #cbd5e1;
      --side: #161a3a; --side-2: rgba(129, 140, 248, .18); --side-ink: #a9b6c8; --side-head: #6c7a8e;
      --accent: #4f46e5; --accent-rgb: 79, 70, 229; --accent-2: #818cf8; --on-accent: #ffffff;
      --ok: #0f9f6e; --ok-rgb: 15, 159, 110; --warn: #c27c0e; --warn-rgb: 194, 124, 14; --bad: #d14343;
      --shadow: 0 1px 2px rgba(15, 23, 42, .04), 0 4px 14px rgba(15, 23, 42, .045);
      --shadow-lift: 0 2px 4px rgba(15, 23, 42, .05), 0 12px 28px rgba(15, 23, 42, .09);
      --display: 'Manrope', 'IBM Plex Sans', 'Segoe UI', sans-serif;
      --font: 'IBM Plex Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
      --r-lg: 12px; --r: 8px; --r-sm: 6px;
    }
    /* ---------- dark theme ---------- */
    :root[data-theme=dark] {
      color-scheme: dark;
      --page: #0e131a; --card: #151c26; --card-2: #111821; --hover: #1a2330;
      --ink: #e7ebf0; --ink-2: #bcc5d1; --muted: #8a95a5; --faint: #5f6b7b;
      --line: #243041; --line-2: #1c2633; --line-strong: #334155;
      --side: #0c0f22; --side-2: rgba(129, 140, 248, .16); --side-ink: #9eabbd; --side-head: #5d6a7d;
      --accent: #818cf8; --accent-rgb: 129, 140, 248; --accent-2: #a5b4fc; --on-accent: #0b1220;
      --ok: #34d399; --ok-rgb: 52, 211, 153; --warn: #fbbf24; --warn-rgb: 251, 191, 36; --bad: #f87171;
      --shadow: 0 1px 2px rgba(0, 0, 0, .3), 0 4px 14px rgba(0, 0, 0, .22);
      --shadow-lift: 0 2px 4px rgba(0, 0, 0, .35), 0 14px 30px rgba(0, 0, 0, .35);
    }
    /* section colors (light, then dark) */
    .theme-forecast { --accent: #0e7490; --accent-rgb: 14, 116, 144; --accent-2: #06b6d4; }
    .theme-plan     { --accent: #c2410c; --accent-rgb: 194, 65, 12; --accent-2: #f59e0b; }
    .theme-learn    { --accent: #7c3aed; --accent-rgb: 124, 58, 237; --accent-2: #c084fc; }
    :root[data-theme=dark] .theme-forecast { --accent: #22d3ee; --accent-rgb: 34, 211, 238; --accent-2: #67e8f9; }
    :root[data-theme=dark] .theme-plan     { --accent: #fb923c; --accent-rgb: 251, 146, 60; --accent-2: #fbbf24; }
    :root[data-theme=dark] .theme-learn    { --accent: #a78bfa; --accent-rgb: 167, 139, 250; --accent-2: #e879f9; }
    /* tints follow the section color, so they are computed on each section element */
    :root, .theme-forecast, .theme-plan, .theme-learn {
      --soft: rgba(var(--accent-rgb), .08); --soft-2: rgba(var(--accent-rgb), .15); --ring: rgba(var(--accent-rgb), .22); }

    html, body { font-family: var(--font) !important; }
    body { color: var(--ink) !important; font-size: 14px; line-height: 1.55;
      background: radial-gradient(1100px 520px at 0% -8%, rgba(99, 102, 241, .10), transparent 65%),
                  radial-gradient(900px 480px at 105% 0%, rgba(6, 182, 212, .09), transparent 60%),
                  radial-gradient(800px 500px at 60% 110%, rgba(245, 158, 11, .06), transparent 60%), var(--page) !important;
      background-attachment: fixed !important;
      -webkit-font-smoothing: antialiased; transition: background-color .2s; }
    body, .form-control, .selectize-input, .btn, table { font-family: var(--font) !important; }
    a, .template-link { color: var(--accent) !important; text-decoration: none; }
    a:hover, .template-link:hover { text-decoration: underline; text-underline-offset: 3px; }
    :focus-visible { outline: 2px solid var(--accent) !important; outline-offset: 2px; }
    ::selection { background: rgba(var(--accent-rgb), .25); }

    /* ---------- headings ---------- */
    .result-title, .hero h1, .home-card h3, .module-branch h3, .navbar .navbar-brand, .empty-state .empty-title {
      font-family: var(--display) !important; }
    .result-title { display: flex !important; align-items: center; gap: 10px; font-size: 1.3rem !important; font-weight: 700 !important;
      color: var(--ink) !important; letter-spacing: -.015em !important; line-height: 1.3;
      margin: 0 0 18px !important; padding: 0 !important; border: none !important; }
    .result-title::before { content: ''; width: 5px; height: 1.1em; flex: none; border-radius: 3px; background: linear-gradient(180deg, var(--accent-2), var(--accent)); }
    .result-title:empty, .result-title:has(> .shiny-text-output:empty), .result-title:has(> .shiny-html-output:empty),
    .col-sm-8 h4:empty, .table-head h4:empty, .col-sm-8 h4:has(> .shiny-text-output:empty) { display: none !important; }
    .col-sm-8 > h4, .col-sm-8 h4, .table-head h4 {
      font-size: .72rem !important; font-weight: 600 !important; text-transform: uppercase; letter-spacing: .08em !important;
      color: var(--muted) !important; margin: 28px 0 10px !important; display: flex; align-items: center; gap: 12px; }
    .col-sm-8 > h4::after { content: ''; flex: 1; height: 1px; background: linear-gradient(90deg, rgba(var(--accent-rgb), .35), var(--line) 40%, transparent); }
    .col-sm-8 > h4::before { content: ''; width: 8px; height: 8px; border-radius: 2px; flex: none; background: linear-gradient(135deg, var(--accent-2), var(--accent)); }
    .table-head { margin: 28px 0 10px !important; align-items: center !important; }
    .table-head h4 { margin: 0 !important; }
    .result-note, .help-block, .form-text { color: var(--muted) !important; font-size: .8125rem !important; }
    hr { border-color: var(--line) !important; opacity: 1; }

    /* ---------- navigation: top bar on small screens ---------- */
    .navbar { background: var(--side) !important; box-shadow: none !important; border: none !important; font-size: .875rem; }
    .navbar .navbar-brand { font-weight: 800 !important; font-size: 1rem !important; color: #fff !important; letter-spacing: -.01em !important; }
    .navbar .nav-link { color: var(--side-ink) !important; border: none !important; outline: none; font-weight: 500; }
    .navbar .nav-link:hover, .navbar .nav-link.active, .navbar .show > .nav-link { color: #fff !important; }
    .navbar .nav-link.active { box-shadow: inset 0 -2px 0 #60a5fa !important; }
    .navbar .navbar-toggler { border-color: rgba(255, 255, 255, .2) !important; }
    .navbar .navbar-toggler-icon { filter: invert(1) brightness(1.4); }
    .dropdown-menu { background: var(--card) !important; border-radius: var(--r) !important; border: 1px solid var(--line) !important;
      box-shadow: var(--shadow-lift) !important; font-size: .875rem; padding: 4px !important; }
    .dropdown-item { border-radius: var(--r-sm); padding: 6px 10px; color: var(--ink-2) !important; }
    .dropdown-item:hover { background: var(--hover) !important; color: var(--ink) !important; }
    .dropdown-item.active, .dropdown-item:active { background: var(--soft-2) !important; color: var(--ink) !important; }
    .dropdown-header { font-size: .66rem !important; font-weight: 600; letter-spacing: .1em !important;
      color: var(--faint) !important; text-transform: uppercase; }
    .dropdown-divider { border-color: var(--line) !important; }

    /* theme switch (added by app_style_js) */
    .theme-toggle { position: fixed; top: 10px; right: 12px; z-index: 1040; width: 34px; height: 34px; padding: 0;
      display: inline-flex; align-items: center; justify-content: center; border-radius: 50%; cursor: pointer;
      background: rgba(255, 255, 255, .08); border: 1px solid rgba(255, 255, 255, .16); color: #dbe3ee;
      transition: background-color .15s, color .15s; }
    .theme-toggle:hover { background: rgba(255, 255, 255, .16); color: #fff; }
    .theme-toggle svg { width: 16px; height: 16px; }
    .theme-toggle .i-sun { display: none; }
    :root[data-theme=dark] .theme-toggle .i-sun { display: block; }
    :root[data-theme=dark] .theme-toggle .i-moon { display: none; }

    /* ---------- page layout ---------- */
    .container-fluid > .tab-content { padding: 20px 12px 40px !important; }
    .tab-content > .tab-pane.active { animation: none; }

    /* ---------- input panel (left) ---------- */
    .well::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 4px; background: linear-gradient(90deg, var(--accent), var(--accent-2)); }
    .well { position: relative; overflow: hidden; background: var(--card) !important; border: 1px solid var(--line) !important; border-radius: var(--r-lg) !important;
      box-shadow: var(--shadow) !important; padding: 20px !important; }
    .well h4 { font-size: .72rem !important; font-weight: 600 !important; text-transform: uppercase; letter-spacing: .08em !important;
      color: var(--accent) !important; margin: 22px -20px 14px !important; padding: 16px 20px 0 !important;
      border-top: 1px solid var(--line-2); }
    .well > h4:first-child, .well > .formula-box + h4 { border-top: none; padding-top: 0 !important; margin-top: 4px !important; }
    .form-label, .control-label, .shiny-input-container > label {
      font-size: .8rem !important; font-weight: 500 !important; color: var(--ink-2) !important; margin-bottom: 5px !important; }
    .shiny-input-container { margin-bottom: 14px; }
    .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; max-width: 100%; }

    .form-control, .selectize-input, .form-select {
      font-size: .875rem !important; color: var(--ink) !important; background-color: var(--card) !important;
      border: 1px solid var(--line-strong) !important; border-radius: var(--r-sm) !important;
      box-shadow: none !important; min-height: 36px; padding: 7px 10px !important; transition: border-color .15s, box-shadow .15s; }
    :root[data-theme=dark] .form-control, :root[data-theme=dark] .selectize-input { background-color: var(--card-2) !important; }
    .selectize-input { padding: 7px 28px 7px 10px !important; line-height: 20px !important; }
    .selectize-input input, .selectize-input .item { color: var(--ink) !important; }
    .selectize-input.dropdown-active { border-radius: var(--r-sm) var(--r-sm) 0 0 !important; }
    .selectize-control.single .selectize-input::after { border-top-color: var(--muted) !important; }
    .form-control:hover, .selectize-input:hover { border-color: var(--faint) !important; }
    .form-control:focus, .selectize-input.focus, .form-select:focus {
      border-color: var(--accent) !important; box-shadow: 0 0 0 3px var(--ring) !important; }
    .form-control::placeholder { color: var(--faint) !important; }
    .selectize-dropdown { background: var(--card) !important; color: var(--ink) !important; border: 1px solid var(--line-strong) !important;
      border-radius: 0 0 var(--r-sm) var(--r-sm) !important; box-shadow: var(--shadow-lift) !important; font-size: .875rem !important; }
    .selectize-dropdown .option { padding: 7px 10px !important; color: var(--ink-2) !important; }
    .selectize-dropdown .active { background: var(--soft-2) !important; color: var(--ink) !important; }
    .selectize-dropdown .optgroup-header { background: var(--card-2) !important; font-size: .66rem !important; font-weight: 600;
      text-transform: uppercase; letter-spacing: .1em; color: var(--faint) !important; }
    .input-group .btn-file, .input-group > .btn, .input-group-btn .btn { border-radius: var(--r-sm) 0 0 var(--r-sm) !important; }
    input[type=radio], input[type=checkbox] { accent-color: var(--accent); }
    .form-check-input, .radio input[type=radio], .checkbox input[type=checkbox] {
      background-color: var(--card) !important; border-color: var(--line-strong) !important; }
    .form-check-input:checked, .radio input[type=radio]:checked, .checkbox input[type=checkbox]:checked {
      background-color: var(--accent) !important; border-color: var(--accent) !important; }
    .form-check-input:focus { box-shadow: 0 0 0 3px var(--ring) !important; }
    .form-check-label, .radio label, .checkbox label { font-size: .875rem; color: var(--ink) !important; }
    .progress { height: 4px !important; border-radius: 2px !important; background: var(--line) !important; }
    .progress-bar { background: var(--accent) !important; font-size: 0 !important; }

    .irs--shiny .irs-bar { background: var(--accent) !important; border: none !important; height: 4px !important; top: 26px !important; }
    .irs--shiny .irs-line { height: 4px !important; top: 26px !important; background: var(--line) !important; border: none !important; }
    .irs--shiny .irs-handle { width: 16px !important; height: 16px !important; top: 20px !important;
      border: 2px solid var(--accent) !important; background: var(--card) !important; box-shadow: var(--shadow) !important; }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--accent) !important; color: var(--on-accent) !important;
      border-radius: var(--r-sm) !important; font-size: .75rem !important; }
    .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text { color: var(--muted) !important;
      background: transparent !important; font-size: .68rem !important; }
    .irs--shiny .irs-grid-pol { background: var(--line-strong) !important; }

    /* ---------- buttons ---------- */
    .btn { font-size: .8125rem !important; font-weight: 500 !important; border-radius: var(--r-sm) !important;
      padding: 7px 13px !important; line-height: 1.4; transition: background-color .15s, border-color .15s, color .15s, box-shadow .15s; }
    .btn-primary, .send-btn { background: linear-gradient(135deg, var(--accent-2), var(--accent)) !important; border: 1px solid var(--accent) !important;
      color: var(--on-accent) !important; font-weight: 600 !important; }
    .btn-primary:hover, .send-btn:hover { filter: brightness(1.08) !important; box-shadow: 0 4px 12px var(--ring) !important; }
    .btn-default, .btn-outline-secondary, .btn-secondary, .shiny-download-link.btn, .btn-file {
      background: var(--card) !important; border: 1px solid var(--line-strong) !important; color: var(--ink-2) !important; }
    .btn-default:hover, .btn-outline-secondary:hover, .btn-secondary:hover, .shiny-download-link.btn:hover, .btn-file:hover {
      background: var(--hover) !important; border-color: var(--faint) !important; color: var(--ink) !important; }
    .btn-outline-primary, .example-btn { background: var(--soft) !important; border: 1px solid rgba(var(--accent-rgb), .35) !important;
      color: var(--accent) !important; }
    .btn-outline-primary:hover, .example-btn:hover { background: var(--accent) !important; border-color: var(--accent) !important;
      color: var(--on-accent) !important; }

    /* ---------- helper boxes ---------- */
    .formula-box { background: var(--card-2) !important; border: 1px solid var(--line) !important; border-left: 3px solid var(--accent) !important;
      border-radius: var(--r-sm) !important; color: var(--ink) !important; font-size: .8125rem !important; padding: 11px 13px !important; }
    .formula-box .MathJax_CHTML, .formula-box .MathJax { font-size: 106% !important; color: var(--ink) !important; }
    .data-actions { margin: -4px 0 14px !important; }
    .data-check { font-size: .8125rem !important; border-radius: var(--r-sm) !important; padding: 8px 11px !important;
      background: rgba(var(--ok-rgb), .08) !important; border: 1px solid rgba(var(--ok-rgb), .28) !important; color: var(--ink) !important; }
    .data-check.warn { background: rgba(var(--warn-rgb), .09) !important; border-color: rgba(var(--warn-rgb), .32) !important; }
    .help-icon { width: 15px !important; height: 15px !important; background: transparent !important; border: 1px solid var(--line-strong) !important;
      color: var(--muted) !important; font-size: .625rem !important; font-weight: 600 !important; margin-left: 6px !important; }
    .help-icon:hover, .help-icon:focus { background: var(--accent) !important; border-color: var(--accent) !important; color: var(--on-accent) !important; }
    .tooltip-inner { background: #0f172a !important; color: #e2e8f0 !important; border-radius: var(--r-sm) !important;
      font-size: .78rem !important; line-height: 1.45; padding: 8px 10px !important; max-width: 300px !important; }
    .tooltip .tooltip-arrow::before { border-top-color: #0f172a !important; border-bottom-color: #0f172a !important; }
    .shiny-input-container:has(input[type=file])::after { font-size: .75rem !important; color: var(--faint) !important; }
    .shiny-input-container:has(input[type=file]) .form-control { border-style: dashed !important; }

    /* notifications */
    #shiny-notification-panel { width: 360px; max-width: calc(100vw - 32px); }
    .shiny-notification { background: var(--card) !important; color: var(--ink) !important; border: 1px solid var(--line) !important;
      border-left: 3px solid var(--accent) !important; border-radius: var(--r) !important; opacity: 1 !important;
      box-shadow: var(--shadow-lift) !important; font-size: .8125rem; padding: 10px 14px !important; }
    .shiny-notification-warning { border-left-color: var(--warn) !important; }
    .shiny-notification-error { border-left-color: var(--bad) !important; }
    .shiny-notification-message { border-left-color: var(--ok) !important; }
    .shiny-notification-close { color: var(--muted) !important; }
    .shiny-output-error-validation { color: var(--muted) !important; background: var(--card) !important;
      border: 1px dashed var(--line-strong) !important; border-radius: var(--r) !important; font-size: .875rem; }

    /* ---------- summary figures ---------- */
    .kpi-row { display: grid !important; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 12px !important; margin: 0 0 20px !important; }
    .kpi { position: relative; min-width: 0; background: linear-gradient(150deg, rgba(var(--accent-rgb), .13), rgba(var(--accent-rgb), .02) 55%), var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--r-lg) !important; padding: 15px 16px 14px !important; box-shadow: var(--shadow) !important; animation: none !important; }
    .kpi::before { content: ''; position: absolute; left: 0; top: 0; right: 0; height: 3px; background: linear-gradient(90deg, var(--accent), var(--accent-2)); }
    .kpi { overflow: hidden; }
    .kpi .kpi-label { font-size: .7rem !important; font-weight: 700; text-transform: uppercase; letter-spacing: .07em !important; color: var(--accent) !important; }
    .kpi .kpi-value { font-family: var(--display) !important; font-size: 1.6rem !important; font-weight: 700 !important; color: var(--ink) !important;
      letter-spacing: -.02em; font-variant-numeric: tabular-nums; line-height: 1.25; margin-top: 4px; }
    .kpi .kpi-text { font-size: 1.05rem !important; line-height: 1.35; padding-top: 3px; letter-spacing: -.005em; }
    .kpi .kpi-sub { font-size: .75rem !important; color: var(--muted) !important; margin-top: 2px; }

    /* ---------- charts ---------- */
    .shiny-plot-output { position: relative; background: var(--card) !important; border: 1px solid var(--line) !important; border-top: 3px solid var(--accent) !important; border-radius: var(--r-lg) !important;
      box-shadow: var(--shadow) !important; overflow: hidden; }
    .shiny-plot-output:not(:has(img)) { border: none !important; background: transparent !important; box-shadow: none !important; }
    .shiny-plot-output img { display: block; }

    /* ---------- tables ---------- */
    .shiny-table-output { overflow-x: auto; }
    table.shiny-table { --bs-table-bg: transparent; --bs-table-color: var(--ink); background: var(--card) !important;
      border: 1px solid var(--line) !important; border-radius: var(--r-lg) !important; border-collapse: separate !important; border-spacing: 0;
      overflow: hidden; font-size: .8125rem !important; color: var(--ink) !important; box-shadow: var(--shadow); margin-bottom: 6px;
      font-variant-numeric: tabular-nums; }
    table.shiny-table thead th { background: rgba(var(--accent-rgb), .07) !important; color: var(--accent) !important; font-size: .72rem !important;
      font-weight: 600 !important; letter-spacing: .02em; border-bottom: 1px solid var(--line) !important; border-top: none !important;
      white-space: nowrap; padding: 9px 12px !important; }
    table.shiny-table td { padding: 7px 12px !important; border-top: none !important; color: var(--ink) !important;
      background: transparent !important; border-bottom: 1px solid var(--line-2) !important; white-space: nowrap; }
    table.shiny-table tbody tr:last-child td { border-bottom: none !important; }
    table.shiny-table tbody tr:nth-child(even) { background: transparent !important; }
    table.shiny-table tbody tr:hover { background: var(--hover) !important; }
    table.shiny-table tbody td:first-child { color: var(--ink-2) !important; font-weight: 500; }

    /* ---------- empty states / drop areas ---------- */
    .empty-state { background: var(--card) !important; border: 1.5px dashed var(--line-strong) !important; border-radius: var(--r-lg) !important;
      padding: 46px 24px !important; color: var(--muted) !important; font-size: .875rem; transition: border-color .15s, background-color .15s !important; }
    .empty-state:hover, .empty-state.drop-active { border-color: var(--accent) !important; background: var(--soft) !important; }
    .empty-state .empty-icon { color: var(--accent) !important; font-size: 1.6rem !important; margin-bottom: 10px !important; }
    .empty-state .empty-title { font-weight: 700 !important; font-size: 1.05rem !important; color: var(--ink) !important; }
    .well.drop-active { outline: 2px dashed var(--accent) !important; outline-offset: -5px; background: var(--soft) !important; }

    /* before any data: show only the drop card, not the empty headings */
    .col-sm-8:has(.empty-state) > h4, .col-sm-8:has(.empty-state) > .table-head,
    .col-sm-8:has(.empty-state) > .row, .col-sm-8:has(.empty-state) > p.result-note,
    .col-sm-8:has(.empty-state) > .result-title { display: none !important; }

    /* thin line while calculating */
    html.shiny-busy body::before { content: ''; position: fixed; top: 0; left: 0; height: 2px; width: 30%; z-index: 2000;
      background: var(--accent); animation: busyRun 1.1s ease-in-out infinite; }

    /* ---------- home: hero ---------- */
    .hero { position: relative; overflow: hidden; color: #fff !important; border: none !important;
      border-radius: 18px !important; padding: 38px 38px 32px !important; margin-bottom: 22px !important;
      box-shadow: 0 14px 40px rgba(79, 70, 229, .25);
      background:
        radial-gradient(600px 300px at 95% -10%, rgba(255, 255, 255, .22), transparent 60%),
        radial-gradient(500px 320px at 10% 120%, rgba(245, 158, 11, .35), transparent 60%),
        linear-gradient(120deg, #4338ca 0%, #6d28d9 42%, #0e7490 100%) !important; animation: none !important; }
    :root[data-theme=dark] .hero { box-shadow: 0 14px 40px rgba(0, 0, 0, .45);
      background:
        radial-gradient(600px 300px at 95% -10%, rgba(255, 255, 255, .12), transparent 60%),
        radial-gradient(500px 320px at 10% 120%, rgba(245, 158, 11, .25), transparent 60%),
        linear-gradient(120deg, #312e81 0%, #4c1d95 45%, #155e75 100%) !important; }
    /* right side: layered demand curves (decoration only) */
    .hero::after { content: ''; display: block !important; position: absolute; right: 0; bottom: 0; width: 48%; height: 100%;
      pointer-events: none; background: url(\"data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 600 300%22 preserveAspectRatio=%22none%22%3E%3Cdefs%3E%3ClinearGradient id=%22g0%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%220.16%22/%3E%3C/linearGradient%3E%3ClinearGradient id=%22s0%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%22.8%22/%3E%3C/linearGradient%3E%3ClinearGradient id=%22g1%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%220.13%22/%3E%3C/linearGradient%3E%3ClinearGradient id=%22s1%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%22.8%22/%3E%3C/linearGradient%3E%3ClinearGradient id=%22g2%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%220.11%22/%3E%3C/linearGradient%3E%3ClinearGradient id=%22s2%22 x1=%220%22 x2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23fff%22 stop-opacity=%220%22/%3E%3Cstop offset=%221%22 stop-color=%22%23fff%22 stop-opacity=%22.8%22/%3E%3C/linearGradient%3E%3C/defs%3E%3Cpath d=%22M0.0,166.8 C8.3,163.7 33.3,152.6 50.0,148.2 C66.7,143.8 83.3,141.2 100.0,140.5 C116.7,139.7 133.3,142.1 150.0,143.7 C166.7,145.2 183.3,149.0 200.0,149.8 C216.7,150.6 233.3,150.9 250.0,148.5 C266.7,146.1 283.3,140.8 300.0,135.1 C316.7,129.4 333.3,120.6 350.0,114.1 C366.7,107.5 383.3,100.0 400.0,95.6 C416.7,91.3 433.3,88.8 450.0,88.1 C466.7,87.4 483.3,89.9 500.0,91.4 C516.7,93.0 533.3,96.7 550.0,97.5 C566.7,98.3 591.7,96.3 600.0,96.0 L600,300 L0,300 Z%22 fill=%22url%28%23g0%29%22/%3E%3Cpath d=%22M0.0,166.8 C8.3,163.7 33.3,152.6 50.0,148.2 C66.7,143.8 83.3,141.2 100.0,140.5 C116.7,139.7 133.3,142.1 150.0,143.7 C166.7,145.2 183.3,149.0 200.0,149.8 C216.7,150.6 233.3,150.9 250.0,148.5 C266.7,146.1 283.3,140.8 300.0,135.1 C316.7,129.4 333.3,120.6 350.0,114.1 C366.7,107.5 383.3,100.0 400.0,95.6 C416.7,91.3 433.3,88.8 450.0,88.1 C466.7,87.4 483.3,89.9 500.0,91.4 C516.7,93.0 533.3,96.7 550.0,97.5 C566.7,98.3 591.7,96.3 600.0,96.0%22 fill=%22none%22 stroke=%22url%28%23s0%29%22 stroke-width=%222%22/%3E%3Cpath d=%22M0.0,191.2 C8.3,190.8 33.3,188.2 50.0,188.7 C66.7,189.2 83.3,192.4 100.0,194.2 C116.7,195.9 133.3,198.8 150.0,199.0 C166.7,199.1 183.3,198.1 200.0,195.1 C216.7,192.1 233.3,186.4 250.0,181.1 C266.7,175.7 283.3,168.2 300.0,163.1 C316.7,158.0 333.3,152.8 350.0,150.3 C366.7,147.8 383.3,147.5 400.0,148.1 C416.7,148.6 433.3,151.9 450.0,153.6 C466.7,155.3 483.3,158.1 500.0,158.3 C516.7,158.4 533.3,157.2 550.0,154.2 C566.7,151.1 591.7,142.4 600.0,140.0 L600,300 L0,300 Z%22 fill=%22url%28%23g1%29%22/%3E%3Cpath d=%22M0.0,191.2 C8.3,190.8 33.3,188.2 50.0,188.7 C66.7,189.2 83.3,192.4 100.0,194.2 C116.7,195.9 133.3,198.8 150.0,199.0 C166.7,199.1 183.3,198.1 200.0,195.1 C216.7,192.1 233.3,186.4 250.0,181.1 C266.7,175.7 283.3,168.2 300.0,163.1 C316.7,158.0 333.3,152.8 350.0,150.3 C366.7,147.8 383.3,147.5 400.0,148.1 C416.7,148.6 433.3,151.9 450.0,153.6 C466.7,155.3 483.3,158.1 500.0,158.3 C516.7,158.4 533.3,157.2 550.0,154.2 C566.7,151.1 591.7,142.4 600.0,140.0%22 fill=%22none%22 stroke=%22url%28%23s1%29%22 stroke-width=%222%22/%3E%3Cpath d=%22M0.0,239.8 C8.3,240.7 33.3,243.6 50.0,244.8 C66.7,246.0 83.3,247.5 100.0,247.0 C116.7,246.6 133.3,244.8 150.0,242.1 C166.7,239.4 183.3,234.7 200.0,230.8 C216.7,227.0 233.3,222.0 250.0,219.0 C266.7,215.9 283.3,213.5 300.0,212.6 C316.7,211.7 333.3,212.7 350.0,213.7 C366.7,214.7 383.3,217.5 400.0,218.7 C416.7,219.8 433.3,221.3 450.0,220.8 C466.7,220.3 483.3,218.5 500.0,215.7 C516.7,213.0 533.3,208.3 550.0,204.4 C566.7,200.6 591.7,194.6 600.0,192.6 L600,300 L0,300 Z%22 fill=%22url%28%23g2%29%22/%3E%3Cpath d=%22M0.0,239.8 C8.3,240.7 33.3,243.6 50.0,244.8 C66.7,246.0 83.3,247.5 100.0,247.0 C116.7,246.6 133.3,244.8 150.0,242.1 C166.7,239.4 183.3,234.7 200.0,230.8 C216.7,227.0 233.3,222.0 250.0,219.0 C266.7,215.9 283.3,213.5 300.0,212.6 C316.7,211.7 333.3,212.7 350.0,213.7 C366.7,214.7 383.3,217.5 400.0,218.7 C416.7,219.8 433.3,221.3 450.0,220.8 C466.7,220.3 483.3,218.5 500.0,215.7 C516.7,213.0 533.3,208.3 550.0,204.4 C566.7,200.6 591.7,194.6 600.0,192.6%22 fill=%22none%22 stroke=%22url%28%23s2%29%22 stroke-width=%222%22/%3E%3Ccircle cx=%22595.0%22 cy=%2296.0%22 r=%225%22 fill=%22%23fff%22/%3E%3C/svg%3E\") right bottom / 100% 100% no-repeat; }
    .hero h1 { color: #fff !important; font-weight: 800 !important; font-size: 2.3rem !important; letter-spacing: -.03em !important;
      margin: 0 0 8px !important; position: relative; z-index: 1; }
    .hero p { color: rgba(255, 255, 255, .86) !important; font-size: 1.02rem !important; max-width: 560px; position: relative; z-index: 1; }
    .hero-chips { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 24px !important; position: relative; z-index: 1; max-width: 60%; }
    .hero-chip { display: inline-flex; align-items: center; gap: 8px; background: rgba(255, 255, 255, .14) !important;
      border: 1px solid rgba(255, 255, 255, .28) !important; border-radius: 999px !important; color: #fff !important;
      font-size: .8rem !important; font-weight: 600; padding: 5px 13px !important; margin: 0 !important; }
    .hero-chip i, .hero-chip svg, .hero-chip .fa, .hero-chip .fas { display: none !important; }
    .hero-chip::before { content: ''; width: 8px; height: 8px; border-radius: 50%; flex: none; }
    .hero-chip:nth-child(1)::before { background: #67e8f9; }
    .hero-chip:nth-child(2)::before { background: #fbbf24; }
    .hero-chip:nth-child(3)::before { background: #6ee7b7; }
    .hero-chip:nth-child(4)::before { background: #f0abfc; }

    /* ---------- home: modules as app icons, each opens a branch panel ---------- */
    .module-hub { margin-bottom: 22px; background: var(--card); border: 1px solid var(--line); border-radius: 18px;
      box-shadow: var(--shadow); padding: 22px 22px 18px; }
    .module-grid { display: grid; grid-template-columns: repeat(auto-fill, 118px); gap: 18px 14px; }
    .home-card.module-tile { position: relative; height: auto !important; margin: 0 !important; padding: 8px 4px 10px !important;
      display: flex; flex-direction: column; align-items: center; text-align: center; gap: 0;
      background: transparent !important; border: none !important; border-radius: 16px !important; box-shadow: none !important;
      animation: none !important; transition: background-color .15s !important; }
    .module-tile .app-icon { width: 74px; height: 74px; border-radius: 22px; display: flex; align-items: center; justify-content: center;
      color: #fff; margin-bottom: 9px; background: linear-gradient(145deg, var(--accent-2), var(--accent));
      box-shadow: 0 8px 18px rgba(var(--accent-rgb), .32), inset 0 1px 0 rgba(255, 255, 255, .35);
      transition: transform .18s, box-shadow .18s; }
    .module-tile .app-icon svg { width: 38px; height: 38px; }
    .module-tile .app-icon .fa, .module-tile .app-icon i { font-size: 1.6rem; }
    .home-card.module-tile h3 { font-size: .86rem !important; font-weight: 700 !important; margin: 0 !important; line-height: 1.25; color: var(--ink) !important; }
    .home-card.module-tile .tag { margin: 3px 0 0 !important; font-size: .6rem !important; color: var(--muted) !important; letter-spacing: .08em; }
    .home-card.module-tile:hover { background: var(--soft) !important; }
    .module-tile:hover .app-icon { transform: translateY(-3px) scale(1.03); box-shadow: 0 12px 24px rgba(var(--accent-rgb), .38), inset 0 1px 0 rgba(255, 255, 255, .35); }
    .home-card.module-tile.open { background: var(--soft-2) !important; }
    .module-tile.open .app-icon { box-shadow: 0 0 0 3px var(--card), 0 0 0 6px rgba(var(--accent-rgb), .45), 0 10px 22px rgba(var(--accent-rgb), .35); }
    /* stem from the open tile down to its panel */
    .module-tile.open::after { content: ''; position: absolute; left: 50%; top: 100%; width: 2px; height: 16px; margin-left: -1px;
      background: var(--accent); }

    .module-branch { display: none; position: relative; margin-top: 16px; border: 1px solid rgba(var(--accent-rgb), .25);
      background: linear-gradient(160deg, rgba(var(--accent-rgb), .07), transparent 55%), var(--card-2);
      border-top: 3px solid var(--accent); border-radius: 14px; padding: 18px 22px 20px; }
    .module-branch.show { display: block; animation: branchIn .22s ease-out; }
    .module-branch::before { content: ''; position: absolute; top: -9px; left: var(--branch-x, 40px); width: 12px; height: 12px;
      margin-left: -6px; border-radius: 50%; background: var(--accent); border: 3px solid var(--card); }
    .module-branch .branch-head { display: flex; align-items: flex-start; gap: 28px; flex-wrap: wrap; padding-bottom: 14px;
      border-bottom: 1px solid var(--line-2); }
    .module-branch .branch-head .tag { color: var(--accent); font-size: .68rem; font-weight: 600; text-transform: uppercase; letter-spacing: .1em; }
    .module-branch h3 { font-size: 1.15rem !important; font-weight: 700 !important; margin: 2px 0 0 !important; color: var(--ink); letter-spacing: -.01em; }
    .module-branch .branch-items { display: flex; flex-wrap: wrap; gap: 4px 20px; margin: 4px 0 0; padding: 0; list-style: none;
      color: var(--muted); font-size: .84rem; flex: 1; min-width: 260px; }
    .module-branch .branch-items li { display: flex; align-items: baseline; gap: 7px; }
    .module-branch .branch-items li::before { content: ''; width: 5px; height: 5px; border-radius: 50%; flex: none; background: var(--accent); transform: translateY(-2px); }
    /* the method groups hang from one line, like branches */
    .module-branch .home-modules { display: flex !important; flex-wrap: wrap; gap: 16px 0; margin: 0 !important; padding: 18px 0 0 !important;
      border: none !important; position: relative; }
    .module-branch .module-group { position: relative; margin: 0 !important; padding: 0 22px 0 14px; border-left: 2px solid var(--soft-2); }
    .module-branch .module-group::before { content: ''; position: absolute; left: -2px; top: -18px; width: 2px; height: 18px; background: var(--soft-2); }
    .module-branch .module-group-title { color: var(--muted) !important; font-size: .66rem !important; font-weight: 600;
      text-transform: uppercase; letter-spacing: .1em; margin-bottom: 7px !important; }
    .module-branch .module-group-buttons { display: flex; flex-wrap: wrap; gap: 6px; }
    .module-branch .module-btn { margin: 0 !important; }

    /* ---------- home: shared card pieces ---------- */
    .home-card { background: var(--card) !important; border: 1px solid var(--line) !important; border-radius: var(--r-lg) !important;
      box-shadow: var(--shadow) !important; padding: 22px !important; animation: none !important; }
    .home-card .tag { background: transparent !important; color: var(--accent) !important; padding: 0 !important; border-radius: 0 !important;
      text-transform: uppercase; letter-spacing: .1em; font-size: .66rem !important; font-weight: 600 !important; margin-bottom: 4px !important; }
    .home-card h3 { font-size: 1.15rem !important; font-weight: 700 !important; color: var(--ink) !important; letter-spacing: -.01em;
      margin-bottom: 10px !important; }
    .home-card ul { color: var(--ink-2) !important; font-size: .875rem; }
    .home-card li::marker { color: var(--accent); }
    .home-icon { width: 38px; height: 38px; border-radius: 10px; display: flex; align-items: center; justify-content: center;
      color: var(--accent); background: var(--soft-2); font-size: 1rem; margin-bottom: 14px; }
    .home-card .card-hint { color: var(--accent) !important; font-weight: 600; font-size: .75rem !important; }
    .home-card:not(.module-tile) .home-icon { display: none; }
    .home-card:not(.module-tile) { position: relative; overflow: hidden; }
    .home-card:not(.module-tile)::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 4px;
      background: linear-gradient(90deg, var(--accent), var(--accent-2)); }
    .module-btn { background: var(--card) !important; border: 1px solid var(--line-strong) !important; border-radius: 999px !important;
      color: var(--ink-2) !important; font-weight: 500 !important; font-size: .8rem !important; padding: 4px 12px !important; transition: all .15s; }
    .module-btn:hover { background: var(--accent) !important; border-color: var(--accent) !important; color: var(--on-accent) !important; }
    .member { background: rgba(var(--accent-rgb), .08) !important; border: 1px solid rgba(var(--accent-rgb), .25) !important; border-radius: 999px !important;
      font-weight: 500 !important; font-size: .875rem; color: var(--ink) !important; padding: 5px 14px !important; }

    @keyframes branchIn { from { transform: translateY(-4px); } to { transform: none; } }
    @keyframes busyRun  { 0% { left: -30%; } 100% { left: 100%; } }

    /* ---------- left sidebar (wide screens) ---------- */
    @media (min-width: 992px) {
      body { padding-left: 252px; }
      .navbar { position: fixed !important; top: 0; left: 0; bottom: 0; width: 252px; z-index: 1030;
        display: block !important; overflow-y: auto; padding: 0 12px 28px !important;
        background: linear-gradient(180deg, #23205a 0%, var(--side) 38%, #0f1226 100%) !important;
        scrollbar-width: thin; scrollbar-color: #2a3a52 transparent; }
      .navbar > .container-fluid { display: block !important; padding: 0 !important; }
      .navbar .navbar-brand { display: block; white-space: normal; line-height: 1.3; font-size: 1.02rem !important;
        margin: 0 -12px 14px; padding: 22px 60px 18px 22px; border-bottom: 1px solid rgba(255, 255, 255, .07); }
      .theme-toggle { position: fixed; top: 20px; left: 204px; right: auto; }
      .navbar .navbar-toggler { display: none; }
      .navbar .navbar-collapse { display: block !important; }
      .navbar .navbar-nav { flex-direction: column !important; width: 100%; gap: 2px; }
      .navbar .nav-item { width: 100%; }
      .navbar .nav-link { display: flex; align-items: center; padding: 8px 12px !important; border-radius: var(--r-sm);
        font-size: .84rem; font-weight: 500; transition: background-color .15s, color .15s; }
      .navbar .nav-link:hover { background: rgba(255, 255, 255, .05); }
      .navbar .nav-link.active { background: linear-gradient(90deg, rgba(129, 140, 248, .32), rgba(129, 140, 248, .06)) !important; box-shadow: inset 3px 0 0 #a5b4fc !important; color: #fff !important; }
      .navbar .navbar-nav > li:nth-child(3) > .nav-link.active { background: linear-gradient(90deg, rgba(192, 132, 252, .32), rgba(192, 132, 252, .06)) !important; box-shadow: inset 3px 0 0 #c084fc !important; }
      .navbar .dropdown { margin-top: 16px; }
      .navbar .dropdown-toggle { font-size: .68rem; text-transform: uppercase; letter-spacing: .12em; font-weight: 600;
        color: var(--side-head) !important; cursor: pointer; background: transparent !important; box-shadow: none !important;
        padding: 6px 12px !important; gap: 9px; }
      .navbar .dropdown-toggle::before { content: ''; width: 8px; height: 8px; flex: none; border-radius: 3px; background: #818cf8; }
      .navbar .navbar-nav > li:nth-child(4) .dropdown-toggle::before { background: linear-gradient(135deg, #67e8f9, #0891b2); }
      .navbar .navbar-nav > li:nth-child(5) .dropdown-toggle::before { background: linear-gradient(135deg, #fbbf24, #ea580c); }
      .navbar .dropdown-toggle:hover { color: #c7d0dc !important; background: transparent !important; }
      .navbar .dropdown-toggle::after { margin-left: auto; transition: transform .2s; opacity: .7; }
      .navbar .dropdown.nav-collapsed .dropdown-toggle::after { transform: rotate(-90deg); }
      .navbar .dropdown-menu { position: static !important; display: block !important; transform: none !important;
        inset: auto !important; float: none; width: 100%; margin: 2px 0 0 !important; padding: 0 0 2px !important;
        background: transparent !important; border: none !important; box-shadow: none !important; animation: none; }
      .navbar .dropdown.nav-collapsed .dropdown-menu { display: none !important; }
      .navbar .dropdown-header { color: #56647a !important; font-size: .62rem !important; padding: 10px 12px 3px !important; }
      .navbar .dropdown-divider { border-color: rgba(255, 255, 255, .06) !important; margin: 6px 12px !important; }
      .navbar .dropdown-item { color: var(--side-ink) !important; border-radius: var(--r-sm); padding: 6px 12px; font-size: .82rem;
        white-space: normal; line-height: 1.35; transition: background-color .15s, color .15s; }
      .navbar .dropdown-item:hover { background: rgba(255, 255, 255, .05) !important; color: #fff !important; }
      .navbar .dropdown-item.active { color: #fff !important; font-weight: 500; background: var(--side-2) !important; }
      .navbar .navbar-nav > li:nth-child(4) .dropdown-item.active { box-shadow: inset 3px 0 0 #22d3ee; background: linear-gradient(90deg, rgba(34, 211, 238, .26), rgba(34, 211, 238, .04)) !important; }
      .navbar .navbar-nav > li:nth-child(5) .dropdown-item.active { box-shadow: inset 3px 0 0 #fbbf24; background: linear-gradient(90deg, rgba(251, 146, 60, .26), rgba(251, 146, 60, .04)) !important; }
      .container-fluid > .tab-content { padding: 28px 30px 48px !important; }
      html.shiny-busy body::before { margin-left: 252px; }
    }
    /* medium screens: menu stays on top, in one row */
    @media (min-width: 576px) and (max-width: 991.98px) {
      .navbar .navbar-nav { flex-direction: row !important; flex-wrap: wrap; align-items: center; }
      .navbar .nav-link { padding: 8px 12px !important; }
      .navbar .container-fluid { padding-right: 56px !important; }
    }
    @media (max-width: 1099.98px) {
      .hero::after { width: 60%; opacity: .5; }
      .hero-chips { max-width: 100%; }
    }
    /* phones */
    @media (max-width: 767.98px) {
      .container-fluid > .tab-content { padding: 16px 4px 32px !important; }
      .well { margin-bottom: 16px; }
      .hero { padding: 24px 18px !important; }
      .hero h1 { font-size: 1.6rem !important; }
      .kpi-row { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .module-grid { grid-template-columns: repeat(auto-fill, minmax(96px, 1fr)); }
      .navbar .navbar-collapse { max-height: 75vh; overflow-y: auto; }
      .navbar .container-fluid { padding-right: 56px !important; }
    }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after { animation: none !important; transition: none !important; }
    }
  ")))
}

# Puts the section class (theme-forecast / theme-plan / theme-learn) on every tab,
# adds the light / dark theme switch, opens the branch panel of a module tile on
# the Home page (one at a time), counts the numbers in the summary tiles up, and
# lets the section headers of the left sidebar fold their list.
app_style_js <- function() {
  tagList(
  # hide MathJax's "Processing math" message
  tags$script(type = "text/x-mathjax-config", HTML("MathJax.Hub.Config({ messageStyle: 'none' });")),
  tags$script(HTML("
    (function () {
      // ---- theme: light by default, the choice is kept in this browser ----
      var root = document.documentElement;
      function getSaved() { try { return localStorage.getItem('pp-theme'); } catch (e) { return null; } }
      function setTheme(t) {
        if (t === 'dark') root.setAttribute('data-theme', 'dark'); else root.removeAttribute('data-theme');
        try { localStorage.setItem('pp-theme', t); } catch (e) {}
      }
      if (getSaved() === 'dark') root.setAttribute('data-theme', 'dark');
      function addToggle() {
        if (document.querySelector('.theme-toggle')) return;
        var b = document.createElement('button');
        b.type = 'button'; b.className = 'theme-toggle';
        b.setAttribute('aria-label', 'Switch between light and dark theme');
        b.title = 'Light / dark theme';
        b.innerHTML = '<svg class=\"i-moon\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z\"/></svg>' +
                      '<svg class=\"i-sun\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"><circle cx=\"12\" cy=\"12\" r=\"4.5\"/><path d=\"M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4\"/></svg>';
        b.addEventListener('click', function () { setTheme(root.getAttribute('data-theme') === 'dark' ? 'light' : 'dark'); });
        document.body.appendChild(b);
      }

      // ---- section colors ----
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

      // ---- Home: the open module tile shows its branch panel below the tiles ----
      function syncBranches() {
        document.querySelectorAll('.module-tile[data-key]').forEach(function (t) {
          var b = document.querySelector('.module-branch[data-key=\"' + t.getAttribute('data-key') + '\"]');
          if (!b) return;
          var open = t.classList.contains('open');
          b.classList.toggle('show', open);
          if (open) {
            var r = t.getBoundingClientRect(), h = b.getBoundingClientRect();
            b.style.setProperty('--branch-x', Math.round(r.left - h.left + r.width / 2) + 'px');
          }
        });
      }
      // runs after the Home page script has toggled the tile: keep only that tile open
      document.addEventListener('click', function (e) {
        var tile = e.target.closest && e.target.closest('.module-tile.expandable');
        if (!tile) return;
        if (tile.classList.contains('open')) {
          document.querySelectorAll('.module-tile.open').forEach(function (o) { if (o !== tile) o.classList.remove('open'); });
        }
        syncBranches();
      });
      document.addEventListener('keydown', function (e) {
        var tile = e.target.closest && e.target.closest('.module-tile');
        if (tile && (e.key === 'Enter' || e.key === ' ')) { e.preventDefault(); tile.click(); }
      });
      window.addEventListener('resize', syncBranches);

      // ---- summary numbers count up when they appear ----
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

      // ---- sidebar section headers fold their list ----
      document.addEventListener('click', function (e) {
        var t = e.target.closest && e.target.closest('.navbar .dropdown-toggle');
        if (!t || !window.matchMedia('(min-width: 992px)').matches) return;
        e.preventDefault(); e.stopImmediatePropagation();
        t.closest('.dropdown').classList.toggle('nav-collapsed');
      }, true);

      document.addEventListener('DOMContentLoaded', function () {
        themeTabs();
        addToggle();
        obs.observe(document.body, { childList: true, subtree: true });
      });
    })();
  ")))
}
