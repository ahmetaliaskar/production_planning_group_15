# =====================================================================
# Visual design of the interface (no calculations here)
#   - "control room" look: deep navy surfaces, a faint blueprint grid,
#     thin HUD corner marks on the panels, numbers in a technical mono face
#   - every section has its own signal color, so the pages are not uniform:
#       Forecasting = cyan, Aggregate Planning = amber,
#       Learning Curve = violet, Home / Data Library = blue
#   - typefaces: Chakra Petch (titles), IBM Plex Sans (text), IBM Plex Mono (numbers)
#   - quiet motion only: tabs fade in, summary numbers count up,
#     a thin line runs at the top while the app is calculating
#   - on wide screens the menu is a left sidebar
# This file only changes how things look. Content, labels, inputs,
# outputs and the behaviour of every tab are defined elsewhere.
# The chart colors are in app_colors (R/app_theme.R).
# =====================================================================

app_style_css <- function() {
  tagList(
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet",
            href = paste0("https://fonts.googleapis.com/css2?family=Chakra+Petch:wght@500;600;700",
                          "&family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Mono:wght@400;500;600&display=swap")),
  tags$style(HTML("
    /* ---------- design tokens ---------- */
    :root {
      color-scheme: dark;
      --page: #080e1a; --card: #0f1829; --card-2: #0b1322; --raise: #142036;
      --ink: #e6edf7; --ink-2: #b4c0d3; --muted: #7d8ba3; --faint: #56647c;
      --line: #1d2a42; --line-2: #18233a; --line-strong: #2b3c5a;
      --side: #050a14; --side-2: #0d1626; --side-ink: #95a3b9; --side-head: #56647c;
      --accent: #60a5fa; --accent-rgb: 96, 165, 250;
      --ok: #34d399; --ok-rgb: 52, 211, 153; --warn: #f5a524; --warn-rgb: 245, 165, 36; --bad: #f87171;
      --display: 'Chakra Petch', 'Rajdhani', 'Segoe UI', sans-serif;
      --font: 'IBM Plex Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
      --mono: 'IBM Plex Mono', SFMono-Regular, Menlo, Consolas, monospace;
      --radius: 4px; --radius-sm: 3px;
      --soft: rgba(var(--accent-rgb), .10); --soft-2: rgba(var(--accent-rgb), .18); --glow: rgba(var(--accent-rgb), .35);
      /* HUD corner marks, drawn with backgrounds so no extra elements are needed */
      --corners:
        linear-gradient(var(--accent), var(--accent)) left top / 12px 1px no-repeat,
        linear-gradient(var(--accent), var(--accent)) left top / 1px 12px no-repeat,
        linear-gradient(var(--accent), var(--accent)) right top / 12px 1px no-repeat,
        linear-gradient(var(--accent), var(--accent)) right top / 1px 12px no-repeat,
        linear-gradient(var(--accent), var(--accent)) left bottom / 12px 1px no-repeat,
        linear-gradient(var(--accent), var(--accent)) left bottom / 1px 12px no-repeat,
        linear-gradient(var(--accent), var(--accent)) right bottom / 12px 1px no-repeat,
        linear-gradient(var(--accent), var(--accent)) right bottom / 1px 12px no-repeat;
    }
    /* section signal colors */
    .theme-forecast { --accent: #22d3ee; --accent-rgb: 34, 211, 238; }
    .theme-plan     { --accent: #fbbf24; --accent-rgb: 251, 191, 36; }
    .theme-learn    { --accent: #a78bfa; --accent-rgb: 167, 139, 250; }

    html, body { font-family: var(--font) !important; }
    body { color: var(--ink) !important; font-size: 14px; line-height: 1.5; -webkit-font-smoothing: antialiased;
      background-color: var(--page) !important;
      background-image:
        radial-gradient(900px 500px at 100% -10%, rgba(56, 189, 248, .07), transparent 60%),
        linear-gradient(rgba(148, 163, 184, .045) 1px, transparent 1px),
        linear-gradient(90deg, rgba(148, 163, 184, .045) 1px, transparent 1px) !important;
      background-size: auto, 32px 32px, 32px 32px !important; background-attachment: fixed !important; }
    body, .form-control, .selectize-input, .btn, table { font-family: var(--font) !important; }
    ::selection { background: rgba(56, 189, 248, .35); color: #fff; }
    a, .template-link { color: var(--accent) !important; text-decoration: none; }
    a:hover, .template-link:hover { text-decoration: underline; text-underline-offset: 3px; }
    :focus-visible { outline: 2px solid var(--accent) !important; outline-offset: 2px; }
    ::-webkit-scrollbar { width: 10px; height: 10px; }
    ::-webkit-scrollbar-track { background: transparent; }
    ::-webkit-scrollbar-thumb { background: #22314b; border-radius: 6px; border: 2px solid var(--page); }
    ::-webkit-scrollbar-thumb:hover { background: #2f4366; }

    /* ---------- headings ---------- */
    .result-title, .hero h1, .home-card h3, .navbar .navbar-brand, .empty-state .empty-title {
      font-family: var(--display) !important; }
    .result-title { display: block !important; font-size: 1.375rem !important; font-weight: 600 !important;
      color: var(--ink) !important; letter-spacing: .01em !important; line-height: 1.25;
      margin: 0 0 18px !important; padding: 2px 0 2px 14px !important; border: none !important;
      border-left: 3px solid var(--accent) !important; text-shadow: 0 0 24px var(--glow); }
    .col-sm-8 > h4, .col-sm-8 h4, .table-head h4 {
      font-family: var(--mono) !important; font-size: .6875rem !important; font-weight: 500 !important;
      text-transform: uppercase; letter-spacing: .12em !important; color: var(--muted) !important;
      margin: 28px 0 10px !important; display: flex; align-items: center; gap: 10px; }
    .col-sm-8 > h4::before, .col-sm-8 h4::before, .table-head h4::before {
      content: ''; width: 6px; height: 6px; flex: none; background: var(--accent); transform: rotate(45deg);
      box-shadow: 0 0 8px var(--glow); }
    .col-sm-8 h4:empty, .table-head h4:empty, .col-sm-8 h4:has(> .shiny-text-output:empty) { display: none !important; }
    .col-sm-8 > h4::after { content: ''; flex: 1; height: 1px; background: linear-gradient(90deg, var(--line-strong), transparent); }
    .table-head { margin: 28px 0 10px !important; align-items: center !important; }
    .table-head h4 { margin: 0 !important; }
    .result-note, .help-block, .form-text { color: var(--muted) !important; font-size: .8125rem !important; }
    hr { border-color: var(--line) !important; opacity: 1; }

    /* ---------- navigation: top bar on small screens ---------- */
    .navbar { background: var(--side) !important; box-shadow: none !important; border: none !important;
      border-bottom: 1px solid var(--line) !important; font-size: .875rem; }
    .navbar .navbar-brand { font-weight: 600 !important; font-size: 1rem !important; color: #fff !important;
      text-transform: uppercase; letter-spacing: .06em !important; }
    .navbar .nav-link { color: var(--side-ink) !important; border: none !important; outline: none; font-weight: 500; }
    .navbar .nav-link:hover, .navbar .nav-link.active, .navbar .show > .nav-link { color: #fff !important; }
    .navbar .nav-link.active { box-shadow: inset 0 -2px 0 #38bdf8 !important; }
    .navbar .navbar-toggler { border-color: var(--line-strong) !important; }
    .navbar .navbar-toggler-icon { filter: invert(1) brightness(1.4); }
    .dropdown-menu { background: var(--card) !important; border-radius: var(--radius) !important;
      border: 1px solid var(--line-strong) !important; box-shadow: 0 16px 40px rgba(0, 0, 0, .5) !important;
      font-size: .875rem; padding: 4px !important; }
    .dropdown-item { border-radius: var(--radius-sm); padding: 6px 10px; color: var(--ink-2) !important; }
    .dropdown-item:hover { background: var(--raise) !important; color: #fff !important; }
    .dropdown-item.active, .dropdown-item:active { background: rgba(56, 189, 248, .14) !important; color: #fff !important; }
    .dropdown-header { font-family: var(--mono); font-size: .625rem !important; font-weight: 500;
      letter-spacing: .14em !important; color: var(--faint) !important; text-transform: uppercase; }
    .dropdown-divider { border-color: var(--line) !important; }

    /* ---------- page layout ---------- */
    .container-fluid > .tab-content { padding: 20px 12px 40px !important; }
    .tab-content > .tab-pane.active { animation: paneIn .28s ease-out; }

    /* ---------- input panel (left) ---------- */
    .well { background: var(--corners), var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; box-shadow: 0 1px 0 rgba(255, 255, 255, .02) inset, 0 12px 32px rgba(0, 0, 0, .25) !important;
      padding: 20px !important; }
    .well h4 { font-family: var(--mono) !important; font-size: .6875rem !important; font-weight: 500 !important;
      text-transform: uppercase; letter-spacing: .12em !important; color: var(--accent) !important;
      margin: 22px -20px 14px !important; padding: 16px 20px 0 !important; border-top: 1px dashed var(--line-strong); }
    .well > h4:first-child, .well > .formula-box + h4 { border-top: none; padding-top: 0 !important; margin-top: 4px !important; }
    .form-label, .control-label, .shiny-input-container > label {
      font-size: .75rem !important; font-weight: 500 !important; color: var(--ink-2) !important;
      letter-spacing: .02em; margin-bottom: 5px !important; }
    .shiny-input-container { margin-bottom: 14px; }
    .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; max-width: 100%; }

    .form-control, .selectize-input, .form-select {
      font-size: .875rem !important; color: var(--ink) !important; background-color: var(--card-2) !important;
      border: 1px solid var(--line-strong) !important; border-radius: var(--radius-sm) !important;
      box-shadow: none !important; min-height: 36px; padding: 7px 10px !important; }
    input[type=number].form-control { font-family: var(--mono) !important; }
    .selectize-input { padding: 7px 28px 7px 10px !important; line-height: 20px !important; }
    .selectize-input input { color: var(--ink) !important; }
    .selectize-input .item { color: var(--ink) !important; }
    .selectize-input.dropdown-active { border-radius: var(--radius-sm) var(--radius-sm) 0 0 !important; }
    .selectize-control.single .selectize-input::after { border-top-color: var(--muted) !important; }
    .form-control:hover, .selectize-input:hover { border-color: #3b5078 !important; }
    .form-control:focus, .selectize-input.focus, .form-select:focus {
      border-color: var(--accent) !important; box-shadow: 0 0 0 3px var(--soft), 0 0 16px rgba(var(--accent-rgb), .12) !important; }
    .form-control::placeholder { color: var(--faint) !important; }
    .selectize-dropdown { background: var(--card) !important; color: var(--ink) !important;
      border: 1px solid var(--line-strong) !important; border-radius: 0 0 var(--radius-sm) var(--radius-sm) !important;
      box-shadow: 0 16px 36px rgba(0, 0, 0, .5) !important; font-size: .875rem !important; }
    .selectize-dropdown .option { padding: 7px 10px !important; color: var(--ink-2) !important; }
    .selectize-dropdown .active { background: var(--soft-2) !important; color: #fff !important; }
    .selectize-dropdown .optgroup-header { background: var(--card-2) !important; font-family: var(--mono);
      font-size: .625rem !important; text-transform: uppercase; letter-spacing: .12em; color: var(--faint) !important; }
    .input-group .btn-file, .input-group > .btn, .input-group-btn .btn { border-radius: var(--radius-sm) 0 0 var(--radius-sm) !important; }
    .form-check-input { background-color: var(--card-2) !important; border-color: #3b5078 !important; }
    input[type=radio], input[type=checkbox] { accent-color: var(--accent); }
    .radio input[type=radio], .checkbox input[type=checkbox] { background-color: var(--card-2) !important; border-color: #3b5078 !important; }
    .form-check-input:checked { background-color: var(--accent) !important; border-color: var(--accent) !important;
      box-shadow: 0 0 10px var(--glow) !important; }
    .form-check-input:focus { box-shadow: 0 0 0 3px var(--soft) !important; }
    .form-check-label, .radio label, .checkbox label { font-size: .875rem; color: var(--ink) !important; }
    .progress { height: 4px !important; border-radius: 2px !important; background: var(--line) !important; }
    .progress-bar { background: var(--accent) !important; font-size: 0 !important; box-shadow: 0 0 10px var(--glow); }

    .irs--shiny .irs-bar { background: var(--accent) !important; border: none !important; height: 4px !important; top: 26px !important;
      box-shadow: 0 0 10px var(--glow); }
    .irs--shiny .irs-line { height: 4px !important; top: 26px !important; background: var(--line-strong) !important; border: none !important; }
    .irs--shiny .irs-handle { width: 16px !important; height: 16px !important; top: 20px !important;
      border: 2px solid var(--accent) !important; background: var(--page) !important; box-shadow: 0 0 10px var(--glow) !important; }
    .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--accent) !important; color: #04111d !important;
      font-family: var(--mono); border-radius: var(--radius-sm) !important; font-size: .75rem !important; }
    .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text { color: var(--muted) !important;
      background: transparent !important; font-family: var(--mono); font-size: .6875rem !important; }
    .irs--shiny .irs-grid-pol { background: var(--line-strong) !important; }

    /* ---------- buttons ---------- */
    .btn { font-size: .8125rem !important; font-weight: 500 !important; border-radius: var(--radius-sm) !important;
      padding: 7px 13px !important; line-height: 1.4; letter-spacing: .01em;
      transition: background-color .15s, border-color .15s, color .15s, box-shadow .15s; }
    .btn-primary, .send-btn { background: var(--accent) !important; border: 1px solid var(--accent) !important;
      color: #04111d !important; font-weight: 600 !important; box-shadow: 0 0 0 0 transparent !important; }
    .btn-primary:hover, .send-btn:hover { filter: brightness(1.1) !important; box-shadow: 0 0 18px var(--glow) !important; }
    .btn-default, .btn-outline-secondary, .btn-secondary, .shiny-download-link.btn, .btn-file {
      background: var(--raise) !important; border: 1px solid var(--line-strong) !important; color: var(--ink) !important; }
    .btn-default:hover, .btn-outline-secondary:hover, .btn-secondary:hover, .shiny-download-link.btn:hover, .btn-file:hover {
      background: #1a2944 !important; border-color: var(--accent) !important; color: #fff !important; }
    .btn-outline-primary, .example-btn { background: var(--soft) !important; border: 1px solid rgba(var(--accent-rgb), .45) !important;
      color: var(--accent) !important; }
    .btn-outline-primary:hover, .example-btn:hover { background: var(--soft-2) !important; border-color: var(--accent) !important;
      color: #fff !important; box-shadow: 0 0 16px rgba(var(--accent-rgb), .25) !important; }

    /* ---------- helper boxes ---------- */
    .formula-box { background: var(--card-2) !important; border: 1px solid var(--line) !important;
      border-left: 2px solid var(--accent) !important; border-radius: var(--radius-sm) !important;
      color: var(--ink) !important; font-size: .8125rem !important; padding: 11px 13px !important; }
    .formula-box .MathJax_CHTML, .formula-box .MathJax { font-size: 106% !important; color: var(--ink) !important; }
    .data-actions { margin: -4px 0 14px !important; }
    .data-check { font-size: .8125rem !important; border-radius: var(--radius-sm) !important; padding: 8px 11px !important;
      background: rgba(var(--ok-rgb), .08) !important; border: 1px solid rgba(var(--ok-rgb), .28) !important;
      border-left: 2px solid var(--ok) !important; color: var(--ink) !important; }
    .data-check.warn { background: rgba(var(--warn-rgb), .08) !important; border-color: rgba(var(--warn-rgb), .3) !important;
      border-left-color: var(--warn) !important; }
    .help-icon { width: 15px !important; height: 15px !important; background: transparent !important;
      border: 1px solid #3b5078 !important; color: var(--muted) !important; font-size: .625rem !important;
      font-weight: 600 !important; margin-left: 6px !important; }
    .help-icon:hover, .help-icon:focus { background: var(--accent) !important; border-color: var(--accent) !important; color: #04111d !important; }
    .tooltip-inner { background: #e6edf7 !important; color: #0b1322 !important; border-radius: var(--radius-sm) !important;
      font-size: .78rem !important; line-height: 1.45; padding: 8px 10px !important; max-width: 300px !important; }
    .tooltip .tooltip-arrow::before { border-top-color: #e6edf7 !important; border-bottom-color: #e6edf7 !important; }
    .shiny-input-container:has(input[type=file])::after { font-size: .75rem !important; color: var(--faint) !important; }
    .shiny-input-container:has(input[type=file]) .form-control { border-style: dashed !important; }

    /* notifications */
    #shiny-notification-panel { width: 360px; max-width: calc(100vw - 32px); }
    .shiny-notification { background: var(--raise) !important; color: var(--ink) !important; border: 1px solid var(--line-strong) !important;
      border-left: 3px solid #38bdf8 !important; border-radius: var(--radius) !important; opacity: 1 !important;
      box-shadow: 0 16px 40px rgba(0, 0, 0, .55) !important; font-size: .8125rem; padding: 10px 14px !important; }
    .shiny-notification-warning { border-left-color: var(--warn) !important; }
    .shiny-notification-error { border-left-color: var(--bad) !important; }
    .shiny-notification-message { border-left-color: var(--ok) !important; }
    .shiny-notification-close { color: var(--muted) !important; }
    .shiny-output-error-validation { color: var(--muted) !important; background: var(--card) !important;
      border: 1px dashed var(--line-strong) !important; border-radius: var(--radius) !important; font-size: .875rem; }

    /* ---------- summary figures ---------- */
    .kpi-row { display: grid !important; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
      gap: 10px !important; margin: 0 0 20px !important; }
    .kpi { position: relative; min-width: 0; background: var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; padding: 14px 16px 13px !important; overflow: hidden;
      box-shadow: 0 10px 28px rgba(0, 0, 0, .22) !important; animation: fadeUp .35s ease-out; }
    .kpi::before { content: ''; position: absolute; left: 0; top: 0; right: 0; height: 2px;
      background: linear-gradient(90deg, var(--accent), rgba(var(--accent-rgb), 0)); }
    .kpi::after { content: ''; position: absolute; right: -40px; top: -40px; width: 110px; height: 110px; border-radius: 50%;
      background: radial-gradient(circle, rgba(var(--accent-rgb), .12), transparent 70%); pointer-events: none; }
    .kpi .kpi-label { font-family: var(--mono); font-size: .625rem !important; font-weight: 500;
      text-transform: uppercase; letter-spacing: .12em !important; color: var(--muted) !important; }
    .kpi .kpi-value { font-family: var(--mono) !important; font-size: 1.5rem !important; font-weight: 500 !important;
      color: #fff !important; letter-spacing: -.02em; font-variant-numeric: tabular-nums; line-height: 1.25; margin-top: 4px;
      text-shadow: 0 0 20px rgba(var(--accent-rgb), .25); }
    .kpi .kpi-text { font-family: var(--font) !important; font-size: 1rem !important; font-weight: 600 !important;
      line-height: 1.35; padding-top: 3px; letter-spacing: 0; }
    .kpi .kpi-sub { font-size: .75rem !important; color: var(--accent) !important; margin-top: 2px; opacity: .85; }

    /* ---------- charts ---------- */
    .shiny-plot-output { background: var(--card) !important; border: 1px solid var(--line) !important;
      border-top: 1px solid rgba(var(--accent-rgb), .55) !important; border-radius: var(--radius) !important;
      box-shadow: 0 12px 32px rgba(0, 0, 0, .25) !important; }
    .shiny-plot-output:not(:has(img)) { border: none !important; background: transparent !important; box-shadow: none !important; }
    .shiny-plot-output img { display: block; }

    /* ---------- tables ---------- */
    .shiny-table-output { overflow-x: auto; }
    table.shiny-table { --bs-table-bg: transparent; --bs-table-color: var(--ink); background: var(--card) !important;
      border: 1px solid var(--line) !important; border-radius: var(--radius) !important;
      border-collapse: separate !important; border-spacing: 0; overflow: hidden; font-size: .8125rem !important;
      color: var(--ink) !important; box-shadow: 0 10px 28px rgba(0, 0, 0, .2); margin-bottom: 6px; }
    table.shiny-table thead th { background: var(--card-2) !important; color: var(--muted) !important;
      font-family: var(--mono); font-size: .6875rem !important; font-weight: 500 !important; text-transform: uppercase;
      letter-spacing: .08em; border-bottom: 1px solid rgba(var(--accent-rgb), .4) !important; border-top: none !important;
      white-space: nowrap; padding: 9px 12px !important; }
    table.shiny-table td { padding: 7px 12px !important; border-top: none !important; color: var(--ink) !important;
      background: transparent !important; border-bottom: 1px solid var(--line-2) !important;
      font-family: var(--mono); font-size: .78rem; font-variant-numeric: tabular-nums; }
    table.shiny-table tbody tr:last-child td { border-bottom: none !important; }
    table.shiny-table tbody tr:nth-child(even) { background: rgba(255, 255, 255, .015) !important; }
    table.shiny-table tbody tr:hover { background: var(--soft) !important; }
    table.shiny-table tbody td:first-child { font-family: var(--font); color: var(--ink-2) !important; font-weight: 500; }

    /* ---------- empty states / drop areas ---------- */
    .empty-state { background: var(--corners), rgba(15, 24, 41, .7) !important; border: 1px dashed var(--line-strong) !important;
      border-radius: var(--radius) !important; padding: 46px 24px !important; color: var(--muted) !important;
      font-size: .875rem; transition: border-color .15s, background-color .15s, box-shadow .15s !important; }
    .empty-state:hover, .empty-state.drop-active { border-color: var(--accent) !important;
      box-shadow: inset 0 0 40px rgba(var(--accent-rgb), .08), 0 0 24px rgba(var(--accent-rgb), .12) !important; }
    .empty-state .empty-icon { color: var(--accent) !important; font-size: 1.6rem !important; margin-bottom: 10px !important;
      filter: drop-shadow(0 0 10px var(--glow)); }
    .empty-state .empty-title { font-weight: 600 !important; font-size: 1.05rem !important; color: var(--ink) !important;
      letter-spacing: .02em; }
    .well.drop-active { outline: 2px dashed var(--accent) !important; outline-offset: -5px; }

    /* before any data: show only the drop card, not the empty headings */
    .col-sm-8:has(.empty-state) > h4, .col-sm-8:has(.empty-state) > .table-head,
    .col-sm-8:has(.empty-state) > .row, .col-sm-8:has(.empty-state) > p.result-note,
    .col-sm-8:has(.empty-state) > .result-title { display: none !important; }

    /* thin line while calculating */
    html.shiny-busy body::before { content: ''; position: fixed; top: 0; left: 0; height: 2px; width: 35%; z-index: 2000;
      background: linear-gradient(90deg, transparent, #38bdf8, #a78bfa, transparent); box-shadow: 0 0 12px rgba(56, 189, 248, .6);
      animation: busyRun 1s ease-in-out infinite; }

    /* ---------- home page ---------- */
    .hero { position: relative; overflow: hidden; color: var(--ink) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; padding: 38px 34px 30px !important; margin-bottom: 18px !important;
      background:
        var(--corners),
        radial-gradient(520px 260px at 88% 10%, rgba(56, 189, 248, .20), transparent 65%),
        radial-gradient(420px 220px at 70% 120%, rgba(167, 139, 250, .14), transparent 65%),
        linear-gradient(rgba(56, 189, 248, .06) 1px, transparent 1px) 0 0 / 24px 24px,
        linear-gradient(90deg, rgba(56, 189, 248, .06) 1px, transparent 1px) 0 0 / 24px 24px,
        var(--card) !important;
      box-shadow: 0 20px 50px rgba(0, 0, 0, .35); animation: none !important; }
    .hero::after { content: ''; display: block !important; position: absolute; right: 38px; top: 50%; width: 170px; height: 170px;
      margin-top: -85px; border-radius: 50%; border: 1px solid rgba(56, 189, 248, .35);
      background: radial-gradient(circle, transparent 54%, rgba(56, 189, 248, .14) 55%, transparent 56%),
                  radial-gradient(circle, transparent 28%, rgba(56, 189, 248, .10) 29%, transparent 30%),
                  conic-gradient(from 0deg, transparent 0deg, transparent 250deg, rgba(56, 189, 248, .38) 358deg, rgba(56, 189, 248, .7) 360deg);
      box-shadow: 0 0 40px rgba(56, 189, 248, .15), inset 0 0 30px rgba(56, 189, 248, .10);
      animation: sweep 6s linear infinite; }
    .hero h1 { color: #fff !important; font-weight: 600 !important; font-size: 2.4rem !important; text-transform: uppercase;
      letter-spacing: .04em !important; margin: 0 0 8px !important; text-shadow: 0 0 30px rgba(56, 189, 248, .35); }
    .hero p { color: var(--ink-2) !important; font-size: 1rem !important; max-width: 640px; position: relative; }
    .hero-chips { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 24px !important; position: relative; max-width: calc(100% - 200px); }
    .hero-chip { display: inline-flex; align-items: center; gap: 8px; background: rgba(8, 14, 26, .6) !important;
      border: 1px solid var(--line-strong) !important; border-radius: var(--radius-sm) !important; color: var(--ink) !important;
      font-family: var(--mono); font-size: .75rem !important; font-weight: 500; letter-spacing: .03em;
      padding: 6px 12px !important; margin: 0 !important; }
    .hero-chip:nth-child(1) .fa, .hero-chip:nth-child(1) svg { color: #22d3ee; }
    .hero-chip:nth-child(2) .fa, .hero-chip:nth-child(2) svg { color: #fbbf24; }
    .hero-chip:nth-child(3) .fa, .hero-chip:nth-child(3) svg { color: #34d399; }
    .hero-chip:nth-child(4) .fa, .hero-chip:nth-child(4) svg { color: #a78bfa; }

    .home-card { position: relative; background: var(--corners), var(--card) !important; border: 1px solid var(--line) !important;
      border-radius: var(--radius) !important; box-shadow: 0 12px 32px rgba(0, 0, 0, .25) !important; padding: 22px 22px 18px !important;
      overflow: hidden; animation: fadeUp .4s ease-out; }
    .home-card::before { content: ''; position: absolute; left: 0; right: 0; top: 0; height: 2px;
      background: linear-gradient(90deg, var(--accent), rgba(var(--accent-rgb), 0) 80%); }
    .home-card .tag { background: transparent !important; color: var(--accent) !important; padding: 0 !important;
      font-family: var(--mono); text-transform: uppercase; letter-spacing: .14em; font-size: .6875rem !important;
      font-weight: 500 !important; border-radius: 0 !important; margin-bottom: 6px !important; }
    .home-card h3 { font-size: 1.3rem !important; font-weight: 600 !important; color: #fff !important;
      letter-spacing: .02em; margin-bottom: 12px !important; }
    .home-card ul { color: var(--ink-2) !important; font-size: .875rem; }
    .home-card li::marker { color: var(--accent); }
    .home-icon { width: 40px; height: 40px; border-radius: var(--radius-sm); display: flex; align-items: center;
      justify-content: center; color: var(--accent); background: var(--soft); border: 1px solid rgba(var(--accent-rgb), .35);
      font-size: 1rem; margin-bottom: 14px; box-shadow: 0 0 20px rgba(var(--accent-rgb), .15); }
    .home-card .card-arrow { color: var(--accent) !important; }
    .home-card .card-hint { color: var(--accent) !important; font-family: var(--mono); font-size: .6875rem !important;
      text-transform: uppercase; letter-spacing: .1em; }
    .home-card.clickable { transition: border-color .2s, box-shadow .2s, transform .2s !important; }
    .home-card.clickable:hover { border-color: rgba(var(--accent-rgb), .55) !important; transform: translateY(-3px) !important;
      box-shadow: 0 0 0 1px rgba(var(--accent-rgb), .2), 0 18px 40px rgba(0, 0, 0, .4), 0 0 32px rgba(var(--accent-rgb), .12) !important; }
    .home-modules { border-top: 1px dashed var(--line-strong) !important; }
    .module-group-title { color: var(--faint) !important; font-family: var(--mono); font-size: .625rem !important; letter-spacing: .14em; }
    .module-btn { background: var(--card-2) !important; border: 1px solid var(--line-strong) !important;
      border-radius: var(--radius-sm) !important; color: var(--ink) !important; font-weight: 500 !important;
      font-size: .8125rem !important; padding: 4px 11px !important; transition: all .15s; }
    .module-btn:hover { background: var(--accent) !important; border-color: var(--accent) !important; color: #04111d !important;
      box-shadow: 0 0 14px var(--glow); }
    .member { background: var(--card-2) !important; border: 1px solid var(--line-strong) !important; border-radius: var(--radius-sm) !important;
      font-weight: 500 !important; font-size: .875rem; color: var(--ink) !important; padding: 6px 13px !important; }
    .member::before { content: ''; display: inline-block; width: 6px; height: 6px; border-radius: 50%; margin-right: 8px;
      vertical-align: middle; background: var(--accent); box-shadow: 0 0 8px var(--glow); }

    @keyframes fadeUp  { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; transform: none; } }
    @keyframes paneIn  { from { opacity: 0; } to { opacity: 1; } }
    @keyframes busyRun { 0% { left: -35%; } 100% { left: 100%; } }
    @keyframes sweep   { to { transform: rotate(360deg); } }

    /* ---------- left sidebar (wide screens) ---------- */
    @media (min-width: 992px) {
      body { padding-left: 256px; }
      .navbar { position: fixed !important; top: 0; left: 0; bottom: 0; width: 256px; z-index: 1030;
        display: block !important; overflow-y: auto; padding: 0 12px 28px !important;
        background: linear-gradient(180deg, #071022 0%, var(--side) 40%) !important;
        border-bottom: none !important; border-right: 1px solid var(--line) !important;
        scrollbar-width: thin; scrollbar-color: #1d2a42 transparent; }
      .navbar > .container-fluid { display: block !important; padding: 0 !important; }
      .navbar .navbar-brand { display: block; white-space: normal; line-height: 1.3; font-size: 1.05rem !important;
        margin: 0 -12px 16px; padding: 24px 22px 20px; border-bottom: 1px solid var(--line); }
      .navbar .navbar-brand::after { content: ''; display: block; width: 44px; height: 2px; margin-top: 12px;
        background: linear-gradient(90deg, #38bdf8, #a78bfa); box-shadow: 0 0 10px rgba(56, 189, 248, .6); }
      .navbar .navbar-toggler { display: none; }
      .navbar .navbar-collapse { display: block !important; }
      .navbar .navbar-nav { flex-direction: column !important; width: 100%; gap: 2px; }
      .navbar .nav-item { width: 100%; }
      .navbar .nav-link { display: flex; align-items: center; padding: 8px 12px !important; border-radius: var(--radius-sm);
        font-size: .84rem; font-weight: 500; transition: background-color .15s, color .15s; }
      .navbar .nav-link:hover { background: rgba(255, 255, 255, .04); }
      .navbar .nav-link.active { background: linear-gradient(90deg, rgba(56, 189, 248, .16), rgba(56, 189, 248, .02)) !important;
        box-shadow: inset 2px 0 0 #38bdf8 !important; color: #fff !important; }
      .navbar .navbar-nav > li:nth-child(3) > .nav-link.active {
        background: linear-gradient(90deg, rgba(167, 139, 250, .18), rgba(167, 139, 250, .02)) !important; box-shadow: inset 2px 0 0 #a78bfa !important; }
      .navbar .dropdown { margin-top: 18px; }
      .navbar .dropdown-toggle { font-family: var(--mono); font-size: .65rem; text-transform: uppercase; letter-spacing: .16em;
        font-weight: 500; color: var(--side-head) !important; cursor: pointer; background: transparent !important;
        box-shadow: none !important; padding: 6px 12px !important; gap: 10px; }
      .navbar .dropdown-toggle::before { content: ''; width: 6px; height: 6px; flex: none; transform: rotate(45deg);
        background: #38bdf8; box-shadow: 0 0 8px rgba(56, 189, 248, .7); }
      .navbar .navbar-nav > li:nth-child(4) .dropdown-toggle::before { background: #22d3ee; box-shadow: 0 0 8px rgba(34, 211, 238, .7); }
      .navbar .navbar-nav > li:nth-child(5) .dropdown-toggle::before { background: #fbbf24; box-shadow: 0 0 8px rgba(251, 191, 36, .7); }
      .navbar .dropdown-toggle:hover { color: #c7d0dc !important; background: transparent !important; }
      .navbar .dropdown-toggle::after { margin-left: auto; transition: transform .2s; opacity: .7; }
      .navbar .dropdown.nav-collapsed .dropdown-toggle::after { transform: rotate(-90deg); }
      .navbar .dropdown-menu { position: static !important; display: block !important; transform: none !important;
        inset: auto !important; float: none; width: 100%; margin: 2px 0 0 !important; padding: 0 0 2px 0 !important;
        background: transparent !important; border: none !important; box-shadow: none !important; animation: none;
        border-left: 1px solid var(--line) !important; margin-left: 15px !important; width: calc(100% - 15px); }
      .navbar .dropdown.nav-collapsed .dropdown-menu { display: none !important; }
      .navbar .dropdown-header { color: #46536a !important; font-size: .6rem !important; padding: 10px 12px 3px !important; }
      .navbar .dropdown-divider { border-color: rgba(255, 255, 255, .05) !important; margin: 6px 12px !important; }
      .navbar .dropdown-item { color: var(--side-ink) !important; border-radius: 0 var(--radius-sm) var(--radius-sm) 0; padding: 5px 12px;
        font-size: .8125rem; white-space: normal; line-height: 1.35; transition: background-color .15s, color .15s; }
      .navbar .dropdown-item:hover { background: rgba(255, 255, 255, .04) !important; color: #fff !important; }
      .navbar .dropdown-item.active { color: #fff !important; font-weight: 500; margin-left: -1px; }
      .navbar .navbar-nav > li:nth-child(4) .dropdown-item.active {
        background: linear-gradient(90deg, rgba(34, 211, 238, .16), rgba(34, 211, 238, .02)) !important; box-shadow: inset 2px 0 0 #22d3ee; }
      .navbar .navbar-nav > li:nth-child(5) .dropdown-item.active {
        background: linear-gradient(90deg, rgba(251, 191, 36, .16), rgba(251, 191, 36, .02)) !important; box-shadow: inset 2px 0 0 #fbbf24; }
      .container-fluid > .tab-content { padding: 28px 30px 48px !important; }
      html.shiny-busy body::before { margin-left: 256px; }
    }
    /* medium screens: menu stays on top, in one row */
    @media (min-width: 576px) and (max-width: 991.98px) {
      .navbar .navbar-nav { flex-direction: row !important; flex-wrap: wrap; align-items: center; }
      .navbar .nav-link { padding: 8px 12px !important; }
    }
    /* phones */
    @media (max-width: 767.98px) {
      .container-fluid > .tab-content { padding: 16px 4px 32px !important; }
      .well { margin-bottom: 16px; }
      .hero { padding: 24px 18px !important; }
      .hero::after { display: none !important; }
      .hero h1 { font-size: 1.6rem !important; }
      .hero-chips { max-width: 100%; }
      .kpi-row { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .navbar .navbar-collapse { max-height: 75vh; overflow-y: auto; }
    }

    @media (prefers-reduced-motion: reduce) {
      *, *::before, *::after { animation: none !important; transition: none !important; }
    }
  ")))
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
