# =====================================================================
# Topic: Simple Exponential Smoothing (SES)             (stationary series)
# Nahmias, Chapter 2
#
# S_t = alpha D_t + (1 - alpha) S_{t-1}        0 < alpha <= 1
# F_t = S_{t-1}   (forecast for period t made at the end of t-1)
# The forecast for every future period is S_n.
# Initial value S_0: from a regression line (its value at t = 0)
#                    or typed by the user.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# Returns the smoothed values S_0..S_n and the forecasts F_1..F_n.
exponential_smoothing_run <- function(d, alpha, S0) {
  n <- length(d)
  S <- numeric(n)
  prev <- S0
  F <- numeric(n)
  for (t in 1:n) {
    F[t] <- prev
    S[t] <- alpha * d[t] + (1 - alpha) * prev
    prev <- S[t]
  }
  list(S = S, F = F)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
exponential_smoothing_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    c("\\(S_t = \\alpha D_t + (1-\\alpha)\\, S_{t-1}\\)", "\\(F_t = S_{t-1}\\)"),
    parameter_mode_ui(ns,
      manual = numericInput(ns("alpha"), "alpha (0 < alpha <= 1)",
                            value = 0.2, min = 0.01, max = 1, step = 0.05)
    ),
    initial_values_ui(ns, slope = FALSE)
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
exponential_smoothing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = "Stationary")
    observeEvent(demand(), suggest_initial_values(session, demand()))

    result <- reactive({
      d <- demand(); n <- length(d)
      S0 <- initial_values(input, d)[["level"]]
      search <- NULL
      if (input$mode == "manual") {
        req(input$alpha)
        alpha <- input$alpha
        validate(need(alpha > 0 && alpha <= 1, "alpha must be in (0, 1]."))
      } else {
        search <- search_parameters(
          data.frame(alpha = seq(0.01, 1, by = 0.01)),
          function(p) forecast_errors(d, exponential_smoothing_run(d, p[["alpha"]], S0)$F, 1:n),
          input$criterion)
        alpha <- search$alpha[1]
      }
      run <- exponential_smoothing_run(d, alpha, S0)
      list(method  = paste0("Simple exponential smoothing (alpha = ", alpha, ")"),
           d = d, fitted = run$F,
           future  = rep(run$S[n], horizon(input)),
           periods = 1:n,
           params  = data.frame(Parameter = c("alpha", "S0 (initial level)"),
                                Value = c(alpha, S0)),
           detail  = data.frame("S_t" = run$S, check.names = FALSE),
           search  = search)
    })

    render_forecast_results(output, result)
  })
}
