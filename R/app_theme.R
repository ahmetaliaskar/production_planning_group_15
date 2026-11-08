# =====================================================================
# Look and feel of the whole application
#   1) colors, the Bootstrap 5 theme (bslib) and the page CSS
#   2) helpers for base-R plots so that every chart looks the same
#   3) summary "tiles" (big numbers above the results)
#   4) the Home page
# No calculations are done in this file.
# =====================================================================


# ---------------------------------------------------------------------
# 1) COLORS, THEME AND CSS
# ---------------------------------------------------------------------
app_colors <- list(
  surface   = "#ffffff",
  page      = "#f4f6f9",
  # charts are drawn on a transparent background so they sit on the panel color
  # of both the light and the dark theme; text and lines use mid tones that
  # stay readable on either
  ink       = "#5a6576",   # chart titles
  ink_2     = "#6b7686",   # axis labels, actual demand line
  muted     = "#7d8796",   # axis numbers
  grid      = "#94a3b833",
  border    = "#94a3b855",
  navy      = "#0f1b2d",   # navigation bar
  plot_bg     = "transparent",
  bar_dim     = "#3b7de85c",   # bars that are not highlighted
  band_future = "#3b7de814",   # shaded future area
  band_test   = "#14a37f17",   # shaded hold-out area
  series    = c("#3b7de8", "#e8862a", "#14a37f", "#8b5cf6",
                "#e0507f", "#64748b", "#0ea5c0", "#dc5050")
)

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = app_colors$page, fg = "#0f172a",
    primary = "#2563eb", secondary = "#64748b",
    base_font = bslib::font_collection("IBM Plex Sans", "-apple-system", "BlinkMacSystemFont", "Segoe UI",
                                       "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Manrope", "IBM Plex Sans", "Segoe UI", "sans-serif"),
    "border-radius" = "0.375rem",
    "font-size-base" = "0.875rem"
  )
}

app_css <- function() {
  tags$style(HTML(sprintf("
    body { background: %1$s; }
    .navbar { background: %2$s !important; box-shadow: 0 1px 0 rgba(0,0,0,.08); }
    .navbar .navbar-brand { font-weight: 700; letter-spacing: .2px; color: #fff !important; }
    .navbar .nav-link { color: rgba(255,255,255,.82) !important; }
    .navbar .nav-link:hover, .navbar .nav-link.active, .navbar .show > .nav-link { color: #fff !important; }
    .navbar .dropdown-menu { border: 1px solid %3$s; box-shadow: 0 8px 24px rgba(0,0,0,.10); }
    .dropdown-header { font-size: .72rem; text-transform: uppercase; letter-spacing: .06em; color: %4$s; }
    .container-fluid > .tab-content { padding: 18px 8px 40px; }

    /* left panel */
    .well { background: %5$s; border: 1px solid %3$s; border-radius: 12px;
            box-shadow: 0 1px 2px rgba(0,0,0,.04); padding: 18px; }
    .well h4 { font-size: .8rem; text-transform: uppercase; letter-spacing: .06em;
               color: %4$s; margin-top: 18px; }
    .well .help-block, .well .form-text { color: %4$s; font-size: .85rem; }
    .form-label, .control-label { font-weight: 600; font-size: .88rem; }

    /* results */
    .col-sm-8 > h4, .col-sm-8 h4 { font-size: .8rem; text-transform: uppercase; letter-spacing: .06em;
               color: %4$s; margin: 22px 0 8px; font-weight: 700; }
    .result-title { font-size: 1.25rem; font-weight: 700; color: %6$s; margin: 4px 0 12px; }
    .result-note { color: %4$s; font-size: .9rem; }
    .shiny-plot-output { background: %5$s; border: 1px solid %3$s; border-radius: 12px;
               overflow: hidden; box-sizing: border-box; }
    .shiny-table-output { overflow-x: auto; }
    table.shiny-table { background: %5$s; border: 1px solid %3$s; border-radius: 10px;
               border-collapse: separate; border-spacing: 0; overflow: hidden;
               font-size: .85rem; font-variant-numeric: tabular-nums; margin-bottom: 6px; }
    table.shiny-table thead th { background: #f1f1ee; color: %4$s; font-weight: 600;
               border-bottom: 1px solid %3$s !important; white-space: nowrap; }
    table.shiny-table td, table.shiny-table th { padding: 6px 12px !important; }
    table.shiny-table tbody tr:nth-child(even) { background: #fafaf8; }
    table.shiny-table tbody tr:hover { background: #eef4fc; }
    .shiny-output-error-validation { color: %4$s; background: %5$s; border: 1px dashed %3$s;
               border-radius: 10px; padding: 14px; }

    /* summary tiles */
    .kpi-row { display: flex; flex-wrap: wrap; gap: 12px; margin: 4px 0 14px; }
    .kpi { flex: 1 1 150px; background: %5$s; border: 1px solid %3$s; border-radius: 12px;
           padding: 12px 16px; }
    .kpi .kpi-label { font-size: .75rem; text-transform: uppercase; letter-spacing: .06em; color: %4$s; }
    .kpi .kpi-value { font-size: 1.45rem; font-weight: 700; color: %6$s;
           font-variant-numeric: tabular-nums; line-height: 1.3; }
    .kpi .kpi-sub { font-size: .8rem; color: %4$s; }
    .kpi .kpi-text { font-size: 1.1rem; line-height: 1.35; }

    .formula-box { background: #f3f7fd; border: 1px solid #d6e4f7; border-radius: 10px;
           padding: 10px 12px; margin-bottom: 14px; color: %6$s; font-size: .92rem; overflow-x: auto; }
    .formula-box > div + div { margin-top: 4px; }
    .data-actions { display: flex; align-items: center; justify-content: space-between; gap: 8px;
           flex-wrap: wrap; margin: -6px 0 14px; }
    .data-actions .example-btn { margin: 0; }
    .template-link { font-size: .82rem; }
    .data-check { font-size: .82rem; border-radius: 8px; padding: 8px 10px; margin: -4px 0 14px;
           background: #edf8f3; border: 1px solid #c7e9da; color: %6$s; }
    .data-check.warn { background: #fff7e6; border-color: #f3d9a4; }
    .data-check ul { margin: 4px 0 0; padding-left: 18px; }
    .help-icon { display: inline-flex; align-items: center; justify-content: center; width: 16px; height: 16px;
           margin-left: 6px; border-radius: 50%%; background: #e3ecf9; color: %7$s; font-size: .7rem;
           font-weight: 700; cursor: help; vertical-align: middle; }
    .help-icon:hover, .help-icon:focus { background: %7$s; color: #fff; outline: none; }
    .tooltip-inner { max-width: 280px; text-align: left; font-size: .8rem; }
    .table-head { display: flex; align-items: flex-end; justify-content: space-between; gap: 10px;
           margin: 22px 0 8px; }
    .table-head h4 { margin: 0 !important; }
    /* drag and drop: the whole left panel accepts a dropped file */
    .shiny-input-container:has(input[type=file]) .form-control { border-style: dashed; }
    .shiny-input-container:has(input[type=file])::after {
           content: 'or drag and drop a file anywhere on this panel'; display: block;
           font-size: .78rem; color: %4$s; margin-top: 4px; }
    .well.drop-active { outline: 3px dashed %7$s; outline-offset: -6px; background: #eef4fc; }
    .empty-state { background: %5$s; border: 1px dashed #c9c8c2; border-radius: 12px;
           padding: 44px 24px; color: %4$s; text-align: center; margin-bottom: 14px; }
    .empty-state { cursor: pointer; transition: background .15s, border-color .15s; }
    .empty-state:hover { border-color: %7$s; background: #f7faff; }
    .empty-state.drop-active { border: 2px dashed %7$s; background: #eef4fc; }
    .empty-state .empty-icon { font-size: 1.8rem; color: %7$s; margin-bottom: 6px; }
    .empty-state .empty-title { font-weight: 700; color: %6$s; font-size: 1.05rem; margin-bottom: 4px; }
    .shiny-plot-output:not(:has(img)) { border: none; background: transparent; padding: 0; }
    .col-sm-8:has(.empty-state) > h4 { display: none; }

    /* home page */
    .hero { background: linear-gradient(135deg, %2$s 0%%, #1f5a96 100%%); color: #fff;
            border-radius: 16px; padding: 34px 32px; margin-bottom: 20px; }
    .hero h1 { font-weight: 800; font-size: 2rem; margin: 0 0 6px; }
    .hero p { color: rgba(255,255,255,.85); margin: 0; font-size: 1.02rem; }
    .home-card { background: %5$s; border: 1px solid %3$s; border-radius: 14px; padding: 20px 22px;
            height: calc(100%% - 16px); margin-bottom: 16px; }
    .home-card h3 { font-size: 1.05rem; font-weight: 700; margin: 0 0 4px; }
    .home-card .tag { display: inline-block; font-size: .72rem; font-weight: 600; color: %7$s;
            background: #e8f1fc; border-radius: 999px; padding: 2px 10px; margin-bottom: 10px; }
    .home-card ul { padding-left: 18px; margin: 0; color: %4$s; }
    .home-card li { margin: 2px 0; }
    .home-card.clickable { cursor: pointer; transition: border-color .15s, box-shadow .15s, transform .15s; }
    .home-card.clickable:hover { border-color: %7$s; box-shadow: 0 6px 18px rgba(20,54,92,.10); transform: translateY(-2px); }
    .home-card .card-arrow { float: right; color: %7$s; font-weight: 400; transition: transform .2s; }
    .home-card.open .card-arrow { transform: rotate(180deg); }
    .home-card .card-hint { font-size: .78rem; color: %7$s; margin-top: 10px; }
    .home-card.open .card-hint { display: none; }
    .home-modules { display: none; margin-top: 12px; border-top: 1px solid %3$s; padding-top: 10px; cursor: default; }
    .home-card.open .home-modules { display: block; }
    .module-group { margin-bottom: 8px; }
    .module-group-title { font-size: .7rem; text-transform: uppercase; letter-spacing: .06em; color: %4$s;
           margin-bottom: 4px; }
    .module-btn { border: 1px solid #cfe0f6; background: #f3f7fd; color: %2$s; border-radius: 999px;
           padding: 3px 11px; margin: 0 6px 6px 0; font-size: .82rem; font-weight: 600; cursor: pointer; }
    .module-btn:hover { background: %7$s; border-color: %7$s; color: #fff; }
    .member { display: inline-block; background: %5$s; border: 1px solid %3$s; border-radius: 999px;
            padding: 6px 14px; margin: 0 8px 8px 0; font-weight: 600; }
    @media (max-width: 767px) { .hero { padding: 24px 18px; } .hero h1 { font-size: 1.5rem; } }
  ", app_colors$page, app_colors$navy, app_colors$border, app_colors$ink_2,
     app_colors$surface, app_colors$ink, app_colors$series[1])))
}


# Home page: clicking a card opens its tab or shows its method buttons;
# a method button opens that tab (the same as choosing it in the menu).
app_nav_js <- function() {
  tags$script(HTML("
    (function () {
      function openTab(name) {
        var a = Array.prototype.find.call(document.querySelectorAll('.navbar a[data-value]'),
                                          function (x) { return x.getAttribute('data-value') === name; });
        if (!a) return;
        if (window.bootstrap && bootstrap.Tab) bootstrap.Tab.getOrCreateInstance(a).show(); else a.click();
        document.querySelectorAll('.navbar .dropdown-menu.show, .navbar .dropdown-toggle.show')
          .forEach(function (e) { e.classList.remove('show'); });
        window.scrollTo({ top: 0, behavior: 'smooth' });
      }
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('open-tab', function (name) { openTab(name); });
      }
      document.addEventListener('click', function (e) {
        var btn = e.target.closest && e.target.closest('.module-btn[data-goto]');
        if (btn) { e.stopPropagation(); openTab(btn.getAttribute('data-goto')); return; }
        var card = e.target.closest && e.target.closest('.home-card.clickable');
        if (!card) return;
        if (card.classList.contains('expandable')) card.classList.toggle('open');
        else if (card.getAttribute('data-goto')) openTab(card.getAttribute('data-goto'));
      });
    })();
  "))
}

# Drag and drop: a file dropped anywhere on a left panel (.well) that has a
# file upload is given to that upload, exactly as if it had been chosen with
# "Browse...". Shiny itself already handles drops on the file box; those are
# left to Shiny. Files dropped elsewhere on the page are ignored instead of
# being opened by the browser.
app_dropzone_js <- function() {
  tags$script(HTML("
    (function () {
      // Where does a drop at this element go? -> {box: element to highlight, input: file input}
      function targetOf(el) {
        if (!el || !el.closest) return null;
        var card = el.closest('.empty-state[data-drop]');
        if (card) {
          var inp = card.classList.contains('empty-ap')
            ? document.getElementById('aggregate_data-file')
            : (card.closest('.tab-pane') || document).querySelector('input[type=file]');
          return inp ? { box: card, input: inp } : null;
        }
        var w = el.closest('.well');
        if (w && w.querySelector('input[type=file]')) return { box: w, input: w.querySelector('input[type=file]') };
        return null;
      }
      function clearActive() {
        document.querySelectorAll('.drop-active').forEach(function (b) { b.classList.remove('drop-active'); });
      }
      function hasFiles(e) {
        return e.dataTransfer && Array.prototype.indexOf.call(e.dataTransfer.types || [], 'Files') >= 0;
      }
      document.addEventListener('dragover', function (e) {
        if (!hasFiles(e)) return;
        e.preventDefault();
        clearActive();
        var t = targetOf(e.target);
        if (t) { t.box.classList.add('drop-active'); e.dataTransfer.dropEffect = 'copy'; }
        else { e.dataTransfer.dropEffect = 'none'; }
      });
      document.addEventListener('dragleave', function (e) {
        if (e.relatedTarget === null) clearActive();
      });
      document.addEventListener('drop', function (e) {
        if (!hasFiles(e)) return;
        e.preventDefault();
        clearActive();
        if (e.target.closest && e.target.closest('.input-group')) return;   // Shiny's own drop box
        var t = targetOf(e.target);
        if (!t || !e.dataTransfer.files.length) return;
        var input = t.input;
        var dt = new DataTransfer();
        var files = input.multiple ? e.dataTransfer.files : [e.dataTransfer.files[0]];
        Array.prototype.forEach.call(files, function (f) { dt.items.add(f); });
        input.files = dt.files;
        input.dispatchEvent(new Event('change', { bubbles: true }));
      });
      // Clicking the card opens the file chooser of its upload
      document.addEventListener('click', function (e) {
        var t = targetOf(e.target);
        if (t && t.box.classList.contains('empty-state')) t.input.click();
      });
    })();
  "))
}


# ---------------------------------------------------------------------
# 2) PLOT HELPERS (base R graphics)
# ---------------------------------------------------------------------

# Common settings: light axes, horizontal numbers, no box.
plot_setup <- function(mar = c(4, 4.6, 2.6, 1)) {
  par(mar = mar, las = 1, bty = "n", family = "sans",
      col.axis = app_colors$muted, col.lab = app_colors$ink_2, fg = "#94a3b866",
      cex.axis = 0.8, cex.lab = 0.85, mgp = c(2.8, 0.55, 0), tcl = -0.2,
      font.main = 2, cex.main = 0.95, col.main = app_colors$ink, font.lab = 1)
}

# Empty plot with light horizontal grid lines.
plot_frame <- function(xlim, ylim, xlab = "", ylab = "", main = "", xaxt = "s") {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, axes = FALSE)
  if (nzchar(main)) title(main = main, adj = 0, line = 1.6)
  abline(h = pretty(ylim), col = app_colors$grid, lwd = 0.8)
  axis(2, at = pretty(ylim), lwd = 0, labels = format(pretty(ylim), big.mark = ",", scientific = FALSE))
  if (xaxt != "n") axis(1, lwd = 0, lwd.ticks = 1)
  invisible(NULL)
}

# Legend in one row above the plot area.
# (horiz = FALSE stacks the entries in the top-left corner instead)
plot_legend <- function(labels, cols, pch = NA, lty = 1, lwd = 2, horiz = TRUE, ncol = NULL) {
  if (!is.null(ncol)) horiz <- FALSE
  legend("topleft", inset = c(0, -0.02), legend = labels, col = cols, pch = pch,
         lty = lty, lwd = lwd, horiz = horiz, cex = 0.82,
         ncol = if (is.null(ncol)) 1 else ncol,
         bty = if (horiz || !is.null(ncol)) "n" else "o", box.col = NA,
         bg = adjustcolor(app_colors$plot_bg, 0.92),
         text.col = app_colors$ink_2, x.intersp = 0.6, y.intersp = 1.1,
         seg.len = 1.6, xpd = TRUE)
}


# ---------------------------------------------------------------------
# 3) SUMMARY TILES
# ---------------------------------------------------------------------
kpi_tile <- function(label, value, sub = NULL) {
  div(class = "kpi",
      div(class = "kpi-label", label),
      div(class = if (grepl("[A-Za-z]", value)) "kpi-value kpi-text" else "kpi-value", value),
      if (!is.null(sub)) div(class = "kpi-sub", sub))
}

# Formula box at the top of the left panel. Each element of `lines` is one line;
# \( ... \) inside a line is typeset by MathJax.
formula_box <- function(lines) {
  withMathJax(div(class = "formula-box", lapply(lines, function(l) div(HTML(l)))))
}

# Card shown before any data has been uploaded. It is also an upload area:
# a file dropped on it (or chosen after clicking it) goes to the file upload
# of the same tab, or - with aggregate = TRUE - to the 'Data and Costs' tab.
empty_state <- function(title, text, aggregate = FALSE) {
  div(class = paste("empty-state", if (aggregate) "empty-ap"), `data-drop` = "1",
      div(class = "empty-icon", icon("file-arrow-up")),
      div(class = "empty-title", title), div(text))
}

fmt_num <- function(x, digits = 2) {
  if (length(x) == 0) return("-")
  out <- vapply(x, function(v) {
    if (is.na(v)) "-" else format(round(v, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, character(1))
  unname(out)
}


# ---------------------------------------------------------------------
# 4) HOME PAGE
# ---------------------------------------------------------------------
# The modules shown on the Home page. To add a module, add one entry here:
#   key:     short id (used to pair a tile with its branch panel)
#   goto:    tab to open when the tile is clicked (a module without methods), or
#   modules: named list of groups -> c(button label = tab name); clicking the
#            tile opens a panel below the tiles with these buttons.
home_modules <- function() list(
  list(key = "learning", glyph = "<svg viewBox=\"0 0 32 32\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M6 5v21h21\"/><path d=\"M9.5 8c1.6 7 5 12.5 14.5 13.5\"/><circle cx=\"9.8\" cy=\"9\" r=\"1.6\" fill=\"currentColor\"/><circle cx=\"13.6\" cy=\"16.2\" r=\"1.6\" fill=\"currentColor\"/><circle cx=\"20\" cy=\"20.6\" r=\"1.6\" fill=\"currentColor\"/></svg>", title = "Learning Curve",
       items = list("Time per unit for a chosen learning rate"),
       goto = "Learning Curve", icon_name = "graduation-cap", theme = "theme-learn"),
  list(key = "forecasting", glyph = "<svg viewBox=\"0 0 32 32\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M4 22l4.5-5 4 3 5.5-8\"/><path d=\"M18 12l3.5-2.5 3 1.5 3.5-4\" stroke-dasharray=\"0.1 4.2\" stroke-width=\"3\"/><circle cx=\"18\" cy=\"12\" r=\"2.2\" fill=\"currentColor\"/><path d=\"M4 27h24\" opacity=\".55\"/></svg>", title = "Forecasting",
       items = list("Moving average, weighted moving average, exponential smoothing",
                    "Seasonal factors and seasonal decomposition",
                    "Holt's and Winters' methods",
                    "Box-Jenkins: AR, MA, ARMA, ARIMA"),
       modules = list(
         "Stationary series" = c("Moving Average" = "Moving Average",
                                 "Weighted MA" = "Weighted Moving Average",
                                 "Exponential Smoothing" = "Simple Exponential Smoothing",
                                 "Seasonal Factors" = "Seasonal Factors (Quick and Dirty)"),
         "Trend series" = c("Holt's Method" = "Holt's Method"),
         "Seasonal series" = c("Seasonal Decomposition" = "Seasonal Decomposition",
                               "Winters' Method" = "Winters' Method"),
         "Box-Jenkins" = c("AR" = "AR", "MA" = "MA", "ARMA" = "ARMA", "ARIMA" = "ARIMA"),
         "Comparison" = c("Compare Methods" = "Compare Methods")),
       icon_name = "chart-line", theme = "theme-forecast"),
  list(key = "aggregate", glyph = "<svg viewBox=\"0 0 32 32\" fill=\"none\" stroke=\"currentColor\" stroke-linecap=\"round\"><path d=\"M7 26v-9M13 26V11M19 26v-8M25 26V8\" stroke-width=\"3.6\"/><path d=\"M4 14.5h24\" stroke-width=\"1.8\" stroke-dasharray=\"2.5 2.5\" opacity=\".85\"/></svg>", title = "Aggregate Planning",
       items = list("Chase, level and mixed plans",
                    "Linear programming and the transportation method",
                    "Disaggregation with the square-root rule"),
       modules = list(
         "Start here" = c("Data and Costs" = "Data and Costs"),
         "Heuristic plans" = c("Chase Plan" = "Zero-Inventory (Chase) Plan",
                               "Level Plan" = "Constant-Workforce (Level) Plan",
                               "Mixed Plan" = "Mixed Plan"),
         "Optimization" = c("Linear Programming" = "Linear Programming",
                            "Transportation" = "Transportation Method"),
         "Disaggregation" = c("Square-Root Rule" = "Disaggregation (Square-Root Rule)"),
         "Comparison" = c("Compare Plans" = "Compare Plans")),
       icon_name = "industry", theme = "theme-plan")
)

# Small tile of one module. A tile with methods opens / closes its branch panel;
# a tile without methods opens its tab directly.
home_tile <- function(m) {
  has_methods <- !is.null(m$modules)
  hint <- if (has_methods) "Click to see the methods" else "Click to open"
  div(class = paste("home-card module-tile clickable", m$theme, if (has_methods) "expandable"),
      `data-goto` = m$goto, `data-key` = m$key, tabindex = "0", role = "button",
      title = paste(c(unlist(m$items), hint), collapse = "\n"),
      div(class = "app-icon", if (!is.null(m$glyph)) HTML(m$glyph) else icon(m$icon_name)),
      h3(m$title),
      span(class = "visually-hidden", paste(c(unlist(m$items), hint), collapse = ". ")))
}

# Panel that branches out below the tiles when a module tile is opened.
module_branch <- function(m) {
  if (is.null(m$modules)) return(NULL)
  div(class = paste("module-branch", m$theme), `data-key` = m$key,
      div(class = "branch-head",
          h3(m$title),
          tags$ul(class = "branch-items", lapply(m$items, tags$li))),
      div(class = "home-modules",
          lapply(names(m$modules), function(g) div(class = "module-group",
            div(class = "module-group-title", g),
            div(class = "module-group-buttons",
                lapply(names(m$modules[[g]]), function(lbl)
                  tags$button(type = "button", class = "module-btn", `data-goto` = m$modules[[g]][[lbl]], lbl)))))))
}

home_ui <- function() {
  mods <- home_modules()
  fluidPage(
    div(class = "hero",
        h1("Production Planning"),
        p("Group 15 \u2014 tools for forecasting and aggregate planning, following Nahmias' Production and Operations Analysis."),
        div(class = "hero-chips",
            span(class = "hero-chip", icon("chart-line"), " 12 forecasting methods"),
            span(class = "hero-chip", icon("industry"), " 6 planning methods"),
            span(class = "hero-chip", icon("file-excel"), " Excel in & out"),
            span(class = "hero-chip", icon("flask"), " Hold-out testing"))),
    div(class = "module-hub",
        div(class = "module-grid", lapply(mods, home_tile)),
        lapply(mods, module_branch))
  )
}



# ---------------------------------------------------------------------
# 5) HOW TO USE (English / Turkish) AND GROUP MEMBERS
# ---------------------------------------------------------------------
# Each text is given in both languages; the language switch shows one of them
# (the choice is kept in the browser, English by default).
how_to_ui <- function() {
  steps <- list(
    en = c("Choose a method from the menu on the left (or click a module on the Home page).",
           "Have many files? Load them all (or a whole folder) once in 'Data Library', then pick a dataset in any tab.",
           "Upload an Excel (.xlsx) or .csv file and pick the data column.",
           "Enter the parameters or let the app search for the best values.",
           "Aggregate Planning: enter the data once in 'Data and Costs'; every plan uses it."),
    tr = c("Soldaki men\u00fcden bir y\u00f6ntem se\u00e7in (ya da ana sayfadaki bir mod\u00fcle t\u0131klay\u0131n).",
           "\u00c7ok say\u0131da dosyan\u0131z m\u0131 var? Hepsini (ya da b\u00fct\u00fcn bir klas\u00f6r\u00fc) 'Data Library' sekmesinde bir kez y\u00fckleyin, sonra her sekmede bir veri seti se\u00e7in.",
           "Bir Excel (.xlsx) ya da .csv dosyas\u0131 y\u00fckleyin ve veri s\u00fctununu se\u00e7in.",
           "Parametreleri girin ya da uygulaman\u0131n en iyi de\u011ferleri aramas\u0131n\u0131 sa\u011flay\u0131n.",
           "Toplu Planlama (Aggregate Planning): veriyi 'Data and Costs' sekmesinde bir kez girin; b\u00fct\u00fcn planlar bu veriyi kullan\u0131r."))
  fluidPage(div(class = "info-page",
    div(class = "info-head",
        h2(class = "info-title",
           span(`data-lang` = "en", "How to use"),
           span(`data-lang` = "tr", "Nas\u0131l kullan\u0131l\u0131r")),
        div(class = "lang-switch", role = "group", `aria-label` = "Language",
            tags$button(type = "button", class = "lang-btn", `data-set-lang` = "en", "English"),
            tags$button(type = "button", class = "lang-btn", `data-set-lang` = "tr", "T\u00fcrk\u00e7e"))),
    tags$ol(class = "steps", `data-lang` = "en", lang = "en", lapply(steps$en, tags$li)),
    tags$ol(class = "steps", `data-lang` = "tr", lang = "tr", lapply(steps$tr, tags$li))))
}

members_ui <- function() {
  members <- c("Ahmet Ali Askar", "Eren Yal\u00e7\u0131nkaya", "Bulut Sakall\u0131o\u011flu", "Bilal \u0130skender")
  initials <- vapply(strsplit(members, " "), function(w) paste0(substr(w[1], 1, 1), substr(w[length(w)], 1, 1)), "")
  fluidPage(div(class = "info-page",
    div(class = "info-head", h2(class = "info-title", "Group members")),
    div(class = "member-grid",
        lapply(seq_along(members), function(i)
          div(class = paste0("member-card m", i),
              div(class = "avatar", `aria-hidden` = "true", initials[i]),
              div(class = "member-name", members[i]))))))
}
