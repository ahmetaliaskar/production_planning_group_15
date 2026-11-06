# =====================================================================
# Topic: Box-Jenkins models - AR(p), MA(q), ARMA(p,q), ARIMA(p,d,q)
# Nahmias, Chapter 2
#
# AR(p):    D_t = mu + phi_1 D_{t-1} + ... + phi_p D_{t-p} + eps_t
# MA(q):    D_t = mu + eps_t - theta_1 eps_{t-1} - ... - theta_q eps_{t-q}
# ARMA:     both parts together
# ARIMA:    an ARMA(p,q) model for the series differenced d times
#           (d = 1: D_t - D_{t-1}, d = 2: difference of the differences)
#
# Model identification: ACF and PACF plots
#   AR(p): PACF cuts off after lag p, ACF dies down
#   MA(q): ACF cuts off after lag q, PACF dies down
# Parameters are estimated with R's arima() (maximum likelihood).
# Note: R writes the MA part with "+" signs, so its ma coefficients
#       are -theta of the textbook form.
#
# The same module is used four times (AR, MA, ARMA, ARIMA tabs);
# `type` decides which orders the user can choose.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# Fits the model; returns NULL if R cannot estimate it.
box_jenkins_fit <- function(d, p, dd, q, include_mean = TRUE) {
  tryCatch(
    suppressWarnings(stats::arima(d, order = c(p, dd, q),
                                  include.mean = include_mean && dd == 0,
                                  method = "CSS-ML")),
    error = function(e) NULL)
}

# One-step forecasts in the data period: F_t = D_t - residual_t
box_jenkins_fitted <- function(d, fit) as.numeric(d - stats::residuals(fit))

# Which (p, d, q) combinations to try for each model type.
box_jenkins_grid <- function(type, pmax, dmax, qmax) {
  switch(type,
    AR    = expand.grid(p = 1:pmax, d = 0, q = 0),
    MA    = expand.grid(p = 0, d = 0, q = 1:qmax),
    ARMA  = expand.grid(p = 1:pmax, d = 0, q = 1:qmax),
    ARIMA = expand.grid(p = 0:pmax, d = 1:dmax, q = 0:qmax))
}

box_jenkins_search <- function(d, grid, include_mean, criterion) {
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    p <- grid$p[i]; dd <- grid$d[i]; q <- grid$q[i]
    fit <- box_jenkins_fit(d, p, dd, q, include_mean)
    if (is.null(fit)) return(NULL)
    err <- forecast_errors(d, box_jenkins_fitted(d, fit), (dd + 1):length(d))
    data.frame(p = p, d = dd, q = q, AIC = fit$aic,
               MAD = err[["MAD"]], MSE = err[["MSE"]], MAPE = err[["MAPE"]])
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(NULL)
  out <- out[order(out[[criterion]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
box_jenkins_ui <- function(id, type = "ARIMA") {
  ns <- NS(id)
  has_p <- type %in% c("AR", "ARMA", "ARIMA")
  has_d <- type == "ARIMA"
  has_q <- type %in% c("MA", "ARMA", "ARIMA")
  min_order <- if (type == "ARIMA") 0 else 1
  formula_text <- switch(type,
    AR    = c("\\(D_t = \\mu + \\sum_{i=1}^{p} \\phi_i D_{t-i} + \\epsilon_t\\)",
              "PACF cuts off after lag \\(p\\)."),
    MA    = c("\\(D_t = \\mu + \\epsilon_t - \\sum_{i=1}^{q} \\theta_i \\epsilon_{t-i}\\)",
              "ACF cuts off after lag \\(q\\)."),
    ARMA  = c("\\(D_t = \\mu + \\sum_{i=1}^{p} \\phi_i D_{t-i} + \\epsilon_t - \\sum_{i=1}^{q} \\theta_i \\epsilon_{t-i}\\)"),
    ARIMA = c("ARMA\\((p, q)\\) for \\(\\nabla^d D_t\\)",
              "\\(\\nabla D_t = D_t - D_{t-1}\\)"))

  sidebarLayout(
    sidebarPanel(
      formula_box(formula_text),
      series_input_ui(ns("data")),
      parameter_mode_ui(ns,
        manual = tagList(
          if (has_p) numericInput(ns("p"), "p (AR order)", value = 1,
                                  min = min_order, max = 6, step = 1),
          if (has_d) numericInput(ns("d"), "d (number of differences)", value = 1,
                                  min = 0, max = 2, step = 1),
          if (has_q) numericInput(ns("q"), "q (MA order)", value = 1,
                                  min = min_order, max = 6, step = 1)
        ),
        auto = tagList(
          if (has_p) numericInput(ns("pmax"), "Largest p to try", value = 3,
                                  min = 1, max = 6, step = 1),
          if (has_d) numericInput(ns("dmax"), "Largest d to try", value = 2,
                                  min = 1, max = 2, step = 1),
          if (has_q) numericInput(ns("qmax"), "Largest q to try", value = 3,
                                  min = 1, max = 6, step = 1),
          if (has_d) helpText("AIC values are only comparable between models",
                              "with the same d.")
        ),
        criteria = c("AIC", "MAD", "MSE", "MAPE")
      ),
      if (!has_d) checkboxInput(ns("mean"), "Include a constant (mean)", value = TRUE),
      horizon_ui(ns),
      holdout_ui(ns)
    ),
    mainPanel(
      forecast_results_ui(ns, extra = tagList(
        h4("Model identification: ACF and PACF"),
        plotOutput(ns("acf_plot"), height = if (has_d) "460px" else "260px"),
        h4("Parameter estimates"),
        fluidRow(column(7, tableOutput(ns("coef_table"))),
                 column(5, tableOutput(ns("model_table"))))
      ))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
box_jenkins_server <- function(id, type = "ARIMA") {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = if (type == "ARIMA") "Trend" else "Stationary")
    include_mean <- reactive(if (type == "ARIMA") FALSE else isTRUE(input$mean))
    get_order <- function(name, default) {
      v <- input[[name]]
      if (is.null(v) || is.na(v)) default else as.integer(v)
    }

    # Chooses the orders, fits the model on data d and forecasts h periods
    # (also used for the hold-out test).
    compute <- function(d, h) {
      n <- length(d)
      search <- NULL
      if (input$mode == "manual") {
        p  <- get_order("p", 0); dd <- get_order("d", 0); q <- get_order("q", 0)
        validate(need(p + q + dd > 0, "Choose at least one order greater than 0."))
      } else {
        grid <- box_jenkins_grid(type, get_order("pmax", 1), get_order("dmax", 1),
                                 get_order("qmax", 1))
        search <- withProgress(message = "Fitting models...",
                               box_jenkins_search(d, grid, include_mean(), input$criterion))
        validate(need(!is.null(search), "No model could be estimated for this data."))
        p <- search$p[1]; dd <- search$d[1]; q <- search$q[1]
      }
      validate(need(n > p + dd + q + 2, "Not enough observations for these orders."))
      fit <- box_jenkins_fit(d, p, dd, q, include_mean())
      validate(need(!is.null(fit), "R could not estimate this model. Try other orders."))
      pr <- stats::predict(fit, n.ahead = h)
      list(method  = paste0(type, " model: ARIMA(", p, ",", dd, ",", q, ")"),
           d = d, fitted = box_jenkins_fitted(d, fit),
           future  = as.numeric(pr$pred),
           future_lower = as.numeric(pr$pred - 1.96 * pr$se),
           future_upper = as.numeric(pr$pred + 1.96 * pr$se),
           periods = (dd + 1):n,
           params  = data.frame(Parameter = c("p", "d", "q"), Value = c(p, dd, q)),
           search  = search,
           model   = list(fit = fit, p = p, d = dd, q = q))
    }
    result <- reactive(add_holdout(compute(demand(), horizon(input)), input, compute))
    model <- reactive(result()$model)

    output$acf_plot <- renderPlot({
      d <- demand()
      dd <- if (input$mode == "manual") get_order("d", 0) else model()$d
      series <- list("Series" = d)
      if (dd >= 1) series[["1st difference"]] <- diff(d)
      if (dd >= 2) series[["2nd difference"]] <- diff(d, differences = 2)
      par(mfrow = c(length(series), 2))
      plot_setup(mar = c(4, 4.2, 2.6, 1))
      for (nm in names(series)) {
        stats::acf(series[[nm]], main = "", col = app_colors$series[1], lwd = 3,
                   ci.col = app_colors$series[2])
        title(paste("ACF -", nm), adj = 0, line = 1)
        stats::pacf(series[[nm]], main = "", col = app_colors$series[1], lwd = 3,
                    ci.col = app_colors$series[2])
        title(paste("PACF -", nm), adj = 0, line = 1)
      }
    }, bg = "white", res = 96)

    output$coef_table <- renderTable({
      fit <- model()$fit
      est <- stats::coef(fit)
      if (length(est) == 0) return(data.frame(Note = "The model has no coefficients."))
      se <- sqrt(diag(fit$var.coef))
      data.frame(Coefficient = names(est), Estimate = est,
                 "Std. error" = se, "t value" = est / se, check.names = FALSE)
    }, digits = 4)

    output$model_table <- renderTable({
      fit <- model()$fit
      data.frame(Measure = c("sigma^2 (error variance)", "Log-likelihood", "AIC"),
                 Value = c(fit$sigma2, fit$loglik, fit$aic))
    }, digits = 3)


    render_forecast_results(output, result)
  })
}
