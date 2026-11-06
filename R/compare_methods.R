# =====================================================================
# Topic: Comparing the forecasting methods on one series
#
# Every method is run with its best parameters (found with the same search
# as in its own tab, using the chosen criterion). For a fair comparison,
# all methods are then evaluated over the SAME periods: start ... n, where
# start = (largest MA length tried) + 1, so that every method has a forecast.
# Initial values of the smoothing methods come from a regression line.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# Runs all methods. Returns a list of list(method, params, fitted, future).
compare_methods_run <- function(d, N, criterion, h) {
  n <- length(d)
  Nmax <- max(1, min(8, n - 3))
  out <- list()
  add <- function(method, params, fitted, future) {
    out[[length(out) + 1]] <<- list(method = method, params = params,
                                    fitted = fitted, future = future)
  }
  try_add <- function(expr) tryCatch(expr, error = function(e) NULL)
  st <- regression_start(d)

  try_add({
    s <- moving_average_search(d, Nmax, criterion)
    F <- moving_average_forecast(d, s$N[1])
    add("Moving average", paste0("N = ", s$N[1]), F[1:n], rep(F[n + 1], h))
  })
  try_add({
    k <- min(3, n - 2)
    g <- weight_grid(k, 0.05)
    s <- search_parameters(g, function(p) forecast_errors(
      d, weighted_moving_average_forecast(d, unname(p)), (k + 1):n), criterion)
    w <- unname(unlist(s[1, ]))[1:k]
    F <- weighted_moving_average_forecast(d, w)
    add("Weighted moving average", paste0("w = (", paste(w, collapse = ", "), ")"),
        F[1:n], rep(F[n + 1], h))
  })
  try_add({
    s <- search_parameters(data.frame(alpha = seq(0.01, 1, by = 0.01)), function(p)
      forecast_errors(d, exponential_smoothing_run(d, p[["alpha"]], st[["level"]])$F, 1:n),
      criterion)
    r <- exponential_smoothing_run(d, s$alpha[1], st[["level"]])
    add("Simple exponential smoothing", paste0("alpha = ", s$alpha[1]), r$F, rep(r$S[n], h))
  })
  try_add({
    s <- search_smoothing_constants(c("alpha", "beta"), function(p) forecast_errors(
      d, holt_run(d, p[["alpha"]], p[["beta"]], st[["level"]], st[["slope"]])$F, 1:n), criterion)
    r <- holt_run(d, s$alpha[1], s$beta[1], st[["level"]], st[["slope"]])
    add("Holt's method", paste0("alpha = ", s$alpha[1], ", beta = ", s$beta[1]),
        r$F, r$S[n] + (1:h) * r$G[n])
  })
  if (n >= N) try_add({
    r <- seasonal_factors_run(d, N)
    add("Seasonal factors (quick and dirty)", paste0("N = ", N),
        r$mean * r$factors[season_position(1:n, N)],
        r$mean * r$factors[season_position((n + 1):(n + h), N)])
  })
  if (n >= 2 * N) try_add({
    r <- seasonal_decomposition_run(d, N)
    tf <- (n + 1):(n + h)
    add("Seasonal decomposition", paste0("N = ", N), r$fitted,
        (r$a + r$b * tf) * r$factors[season_position(tf, N)])
  })
  if (n >= 2 * N) try_add({
    c0 <- winters_initial_factors(d, N, st[["level"]], st[["slope"]])
    s <- search_smoothing_constants(c("alpha", "beta", "gamma"), function(p) forecast_errors(
      d, winters_run(d, N, p[["alpha"]], p[["beta"]], p[["gamma"]],
                     st[["level"]], st[["slope"]], c0)$F, 1:n), criterion, coarse = 0.1)
    r <- winters_run(d, N, s$alpha[1], s$beta[1], s$gamma[1], st[["level"]], st[["slope"]], c0)
    tf <- (n + 1):(n + h)
    add("Winters' method",
        paste0("alpha = ", s$alpha[1], ", beta = ", s$beta[1], ", gamma = ", s$gamma[1]),
        r$F, (r$S[n] + (tf - n) * r$G[n]) * r$last_factors[season_position(tf, N)])
  })
  try_add({
    grid <- expand.grid(p = 0:2, d = 0:1, q = 0:2)
    grid <- grid[rowSums(grid) > 0, ]
    s <- box_jenkins_search(d, grid, TRUE, "AIC")
    fit <- box_jenkins_fit(d, s$p[1], s$d[1], s$q[1], TRUE)
    add("Box-Jenkins (best ARIMA by AIC)",
        paste0("ARIMA(", s$p[1], ",", s$d[1], ",", s$q[1], ")"),
        box_jenkins_fitted(d, fit), as.numeric(stats::predict(fit, n.ahead = h)$pred))
  })
  list(results = out, start = Nmax + 1)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
compare_methods_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      formula_box(c("Every method is run with its best parameters.",
                    "All methods are evaluated over the same periods.")),
      series_input_ui(ns("data")),
      numericInput(ns("N"), "Season length N (for the seasonal methods)",
                   value = 4, min = 2, step = 1),
      selectInput(ns("criterion"), "Compare by", choices = c("MAD", "MSE", "MAPE")),
      horizon_ui(ns),
      helpText("Running all methods may take a few seconds.")
    ),
    mainPanel(
      div(class = "result-title", "Comparison of the forecasting methods"),
      uiOutput(ns("kpis")),
      plotOutput(ns("bar_plot"), height = "340px"),
      div(class = "table-head", h4("All methods"),
          downloadButton(ns("download"), "Download .csv", class = "btn-sm btn-outline-secondary")),
      tableOutput(ns("table")),
      p(class = "result-note", textOutput(ns("note"), inline = TRUE)),
      h4("Best method: actual demand and forecasts"),
      plotOutput(ns("best_plot"), height = "320px")
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
compare_methods_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = "Seasonal")

    run <- reactive({
      d <- demand()
      req(input$N)
      validate(need(length(d) >= 6, "At least 6 observations are needed for a comparison."))
      withProgress(message = "Running all methods...",
                   compare_methods_run(d, as.integer(input$N), input$criterion, horizon(input)))
    })

    table <- reactive({
      r <- run(); d <- demand(); periods <- r$start:length(d)
      rows <- lapply(r$results, function(m) {
        e <- forecast_errors(d, m$fitted, periods)
        data.frame(Method = m$method, Parameters = m$params,
                   MAD = e[["MAD"]], MSE = e[["MSE"]], MAPE = e[["MAPE"]],
                   "Next forecast" = m$future[1], check.names = FALSE)
      })
      tab <- do.call(rbind, rows)
      tab <- tab[order(tab[[input$criterion]]), ]
      rownames(tab) <- NULL
      tab
    })

    best <- reactive({
      r <- run(); nm <- table()$Method[1]
      r$results[[which(vapply(r$results, `[[`, "", "method") == nm)]]
    })

    output$kpis <- renderUI({
      tab <- tryCatch(table(), error = function(e) e)
      if (inherits(tab, "error")) {
        if (nzchar(conditionMessage(tab))) return(NULL)
        return(empty_state("Drop an Excel or .csv file here to compare the methods",
                           "or click to choose a file - or use 'Load example data' on the left."))
      }
      div(class = "kpi-row",
          kpi_tile("Best method", tab$Method[1], tab$Parameters[1]),
          kpi_tile(paste("Best", input$criterion),
                   paste0(fmt_num(tab[[input$criterion]][1]), if (input$criterion == "MAPE") " %" else "")),
          kpi_tile("Its next-period forecast", fmt_num(tab[["Next forecast"]][1])))
    })

    output$bar_plot <- renderPlot({
      tab <- table(); k <- input$criterion
      tab <- tab[rev(seq_len(nrow(tab))), ]           # best at the top
      v <- tab[[k]]
      cols <- ifelse(seq_along(v) == length(v), app_colors$series[1], "#b7d3f6")
      plot_setup(mar = c(3.5, 15, 2.6, 4))
      plot(NA, xlim = c(0, max(v) * 1.15), ylim = c(0.5, length(v) + 0.5),
           axes = FALSE, xlab = "", ylab = "")
      title(paste(k, "by method (lower is better)"), adj = 0, line = 1.2)
      abline(v = pretty(c(0, max(v))), col = app_colors$grid)
      axis(1, at = pretty(c(0, max(v))), lwd = 0, lwd.ticks = 1)
      rect(0, seq_along(v) - 0.35, v, seq_along(v) + 0.35, col = cols, border = NA)
      axis(2, at = seq_along(v), labels = tab$Method, lwd = 0, las = 1, cex.axis = 0.82)
      text(v, seq_along(v), fmt_num(v), pos = 4, cex = 0.78, col = app_colors$ink_2, xpd = TRUE)
    }, bg = "white", res = 96)

    output$table <- renderTable({
      tab <- table()
      tab$Method <- ifelse(seq_len(nrow(tab)) == 1, paste("★", tab$Method), tab$Method)
      tab
    }, digits = 3)

    output$note <- renderText({
      r <- run()
      paste0("All methods are evaluated over periods ", r$start, " - ", length(demand()),
             ". The best method (★) has the smallest ", input$criterion, ".")
    })

    output$best_plot <- renderPlot({
      m <- best(); d <- demand(); n <- length(d); h <- length(m$future)
      yr <- range(c(d, m$fitted, m$future), na.rm = TRUE); yr <- yr + c(-0.04, 0.12) * diff(yr)
      plot_setup()
      plot_frame(c(1, n + h), yr, xlab = "Period", ylab = "Demand")
      lines(1:n, d, col = app_colors$ink_2, lwd = 2); points(1:n, d, pch = 16, col = app_colors$ink_2, cex = 0.9)
      lines(1:n, m$fitted, col = app_colors$series[1], lwd = 2, lty = 2)
      lines(c(n, (n + 1):(n + h)), c(m$fitted[n], m$future), col = app_colors$series[2], lwd = 2.5)
      points((n + 1):(n + h), m$future, pch = 21, bg = app_colors$series[2], col = "white", cex = 1.5)
      plot_legend(c("Actual demand", paste("One-step forecast -", m$method), "Future forecast"),
                  c(app_colors$ink_2, app_colors$series[1], app_colors$series[2]), lty = c(1, 2, 1))
    }, bg = "white", res = 96)

    output$download <- downloadHandler(
      filename = function() "forecasting_methods_comparison.csv",
      content = function(file) utils::write.csv(table(), file, row.names = FALSE))
  })
}
