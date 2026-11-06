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
  page      = "#f6f6f4",
  ink       = "#1d1d1b",   # main text
  ink_2     = "#52514e",   # secondary text, axes
  muted     = "#8a8984",
  grid      = "#ebeae6",
  border    = "#e2e1dc",
  navy      = "#14365c",   # navigation bar
  series    = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                "#e87ba4", "#008300", "#6250d6", "#e34948")
)

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = app_colors$page, fg = app_colors$ink,
    primary = app_colors$series[1], secondary = app_colors$ink_2,
    base_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI",
                                       "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    "border-radius" = "0.6rem",
    "font-size-base" = "0.95rem"
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
    .example-btn { margin: -6px 0 14px; }
    .table-head { display: flex; align-items: flex-end; justify-content: space-between; gap: 10px;
           margin: 22px 0 8px; }
    .table-head h4 { margin: 0 !important; }
    .empty-state { background: %5$s; border: 1px dashed #c9c8c2; border-radius: 12px;
           padding: 28px 24px; color: %4$s; text-align: center; margin-bottom: 14px; }
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
    .member { display: inline-block; background: %5$s; border: 1px solid %3$s; border-radius: 999px;
            padding: 6px 14px; margin: 0 8px 8px 0; font-weight: 600; }
    @media (max-width: 767px) { .hero { padding: 24px 18px; } .hero h1 { font-size: 1.5rem; } }
  ", app_colors$page, app_colors$navy, app_colors$border, app_colors$ink_2,
     app_colors$surface, app_colors$ink, app_colors$series[1])))
}


# ---------------------------------------------------------------------
# 2) PLOT HELPERS (base R graphics)
# ---------------------------------------------------------------------

# Common settings: light axes, horizontal numbers, no box.
plot_setup <- function(mar = c(4, 4.6, 2.6, 1)) {
  par(mar = mar, las = 1, bty = "n", family = "sans",
      col.axis = app_colors$ink_2, col.lab = app_colors$ink_2, fg = app_colors$border,
      cex.axis = 0.85, cex.lab = 0.9, mgp = c(2.8, 0.6, 0), tcl = -0.2,
      font.main = 2, cex.main = 1, col.main = app_colors$ink)
}

# Empty plot with light horizontal grid lines.
plot_frame <- function(xlim, ylim, xlab = "", ylab = "", main = "", xaxt = "s") {
  plot(NA, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, axes = FALSE)
  if (nzchar(main)) title(main = main, adj = 0, line = 1.6)
  abline(h = pretty(ylim), col = app_colors$grid, lwd = 1)
  axis(2, at = pretty(ylim), lwd = 0, labels = format(pretty(ylim), big.mark = ",", scientific = FALSE))
  if (xaxt != "n") axis(1, lwd = 0, lwd.ticks = 1)
  invisible(NULL)
}

# Legend in one row above the plot area.
# (horiz = FALSE stacks the entries in the top-left corner instead)
plot_legend <- function(labels, cols, pch = NA, lty = 1, lwd = 2, horiz = TRUE) {
  legend("topleft", inset = c(0, -0.02), legend = labels, col = cols, pch = pch,
         lty = lty, lwd = lwd, horiz = horiz, cex = 0.82,
         bty = if (horiz) "n" else "o", box.col = NA,
         bg = adjustcolor("white", 0.9),
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

# Card shown before any data has been uploaded.
empty_state <- function(title, text) {
  div(class = "empty-state", div(class = "empty-title", title), div(text))
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
home_card <- function(title, tag, items) {
  column(4, div(class = "home-card",
                div(class = "tag", tag),
                h3(title),
                tags$ul(lapply(items, tags$li))))
}

home_ui <- function() {
  fluidPage(
    div(class = "hero",
        h1("Production Planning"),
        p("Group 15 - interactive tools for forecasting and aggregate planning (Nahmias).")),
    fluidRow(
      home_card("Learning Curve", "Week 1",
                list("Time per unit for a chosen learning rate")),
      home_card("Forecasting", "Assignment 2",
                list("Moving average, weighted moving average, exponential smoothing",
                     "Seasonal factors and seasonal decomposition",
                     "Holt's and Winters' methods",
                     "Box-Jenkins: AR, MA, ARMA, ARIMA")),
      home_card("Aggregate Planning", "Assignment 3",
                list("Chase, level and mixed plans",
                     "Linear programming and the transportation method",
                     "Disaggregation with the square-root rule"))
    ),
    fluidRow(
      column(6, div(class = "home-card",
        h3("How to use"),
        tags$ul(
          tags$li("Choose a method from the menu above."),
          tags$li("Upload an Excel (.xlsx) or .csv file and pick the data column."),
          tags$li("Enter the parameters or let the app search for the best values."),
          tags$li("Aggregate Planning: enter the data once in 'Data and Costs'; every plan uses it.")
        ))),
      column(6, div(class = "home-card",
        h3("Group members"),
        div(style = "margin-top: 8px;",
            span(class = "member", "Ahmet Ali Askar"),
            span(class = "member", "Eren Yalçınkaya"),
            span(class = "member", "Bulut Sakallıoğlu"),
            span(class = "member", "Bilal İskender"))))
    )
  )
}
