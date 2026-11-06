# =====================================================================
# Topic: Comparing the aggregate plans
#
# Shows the cost of every plan side by side. For each plan the version that
# can really be used is taken: the whole-worker plan when the workers were
# fractional, and the cheapest-cell-first solution of the transportation
# method. Each plan uses the settings of its own tab (e.g. the LP options,
# the transportation workforce, the mixed plan's change periods).
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# One row per plan: cost items, total and workforce summary.
compare_plans_table <- function(ap, results) {
  rows <- lapply(names(results), function(nm) {
    r <- results[[nm]]
    if (is.null(r)) return(NULL)
    version <- names(r$plans)[length(r$plans)]
    p <- r$plans[[length(r$plans)]]
    k <- ap_costs(p, ap)
    data.frame(Plan = nm, Version = version, as.list(k),
               Workers = paste0(round(min(p$Workers), 1), " - ", round(max(p$Workers), 1)),
               check.names = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab <- tab[order(tab$Total), ]
  rownames(tab) <- NULL
  tab
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
compare_plans_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      formula_box(c("The total cost of every plan, side by side.",
                    "Whole-worker versions are compared.")),
      helpText("Each plan uses the settings of its own tab (LP options, transportation",
               "workforce, mixed-plan change periods). Data and costs come from",
               "the 'Data and Costs' tab.")
    ),
    mainPanel(
      div(class = "result-title", "Comparison of the aggregate plans"),
      uiOutput(ns("kpis")),
      plotOutput(ns("bar_plot"), height = "320px"),
      div(class = "table-head", h4("Cost of each item by plan"),
          downloadButton(ns("download"), "Download .csv", class = "btn-sm btn-outline-secondary")),
      tableOutput(ns("table")),
      p(class = "result-note", textOutput(ns("note"), inline = TRUE))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
# `plans` is a named list of the plan modules' results (see app.R).
compare_plans_server <- function(id, ap, plans) {
  moduleServer(id, function(input, output, session) {

    table <- reactive({
      a <- ap_require(ap)
      results <- lapply(plans, function(p) tryCatch(p(), error = function(e) NULL))
      tab <- compare_plans_table(a, results)
      validate(need(!is.null(tab) && nrow(tab) > 0, "No plan could be computed."))
      attr(tab, "missing") <- names(results)[vapply(results, is.null, logical(1))]
      tab
    })

    output$kpis <- renderUI({
      a <- tryCatch(ap(), error = function(e) NULL)
      if (is.null(a)) {
        return(empty_state("Drop the demand file here",
                           "or click to choose it - it is loaded into the 'Data and Costs' tab.",
                           aggregate = TRUE))
      }
      tab <- tryCatch(table(), error = function(e) NULL)
      if (is.null(tab)) return(NULL)
      worst <- tab$Total[nrow(tab)]
      div(class = "kpi-row",
          kpi_tile("Cheapest plan", tab$Plan[1], tab$Version[1]),
          kpi_tile("Its total cost", fmt_num(tab$Total[1])),
          kpi_tile("Saving vs. most expensive", fmt_num(worst - tab$Total[1]),
                   paste0(fmt_num(100 * (worst - tab$Total[1]) / worst, 1), " % of ", tab$Plan[nrow(tab)])))
    })

    output$bar_plot <- renderPlot({
      tab <- table()
      tab <- tab[rev(seq_len(nrow(tab))), ]           # cheapest at the top
      v <- tab$Total
      cols <- ifelse(seq_along(v) == length(v), app_colors$series[1], "#b7d3f6")
      plot_setup(mar = c(3.5, 15, 2.6, 5))
      plot(NA, xlim = c(0, max(v) * 1.18), ylim = c(0.5, length(v) + 0.5),
           axes = FALSE, xlab = "", ylab = "")
      title("Total cost by plan (lower is better)", adj = 0, line = 1.2)
      abline(v = pretty(c(0, max(v))), col = app_colors$grid)
      axis(1, at = pretty(c(0, max(v))), lwd = 0, lwd.ticks = 1,
           labels = format(pretty(c(0, max(v))), big.mark = ",", scientific = FALSE))
      rect(0, seq_along(v) - 0.35, v, seq_along(v) + 0.35, col = cols, border = NA)
      axis(2, at = seq_along(v), labels = tab$Plan, lwd = 0, las = 1, cex.axis = 0.82)
      text(v, seq_along(v), fmt_num(v, 0), pos = 4, cex = 0.78, col = app_colors$ink_2, xpd = TRUE)
    }, bg = "white", res = 96)

    output$table <- renderTable({
      tab <- table()
      tab$Plan <- ifelse(seq_len(nrow(tab)) == 1, paste("★", tab$Plan), tab$Plan)
      # hide cost items that are zero for every plan
      zero <- vapply(tab, function(x) is.numeric(x) && all(abs(x) < 1e-9), logical(1))
      tab[, !zero, drop = FALSE]
    }, digits = 0)

    output$note <- renderText({
      miss <- attr(table(), "missing")
      if (length(miss) == 0) return("")
      paste0("Not included (no feasible plan with its current settings - open its tab for the reason): ",
             paste(miss, collapse = ", "), ".")
    })

    output$download <- downloadHandler(
      filename = function() "aggregate_plans_comparison.csv",
      content = function(file) utils::write.csv(table(), file, row.names = FALSE))
  })
}
