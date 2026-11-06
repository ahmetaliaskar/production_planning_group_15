# =====================================================================
# Topic: Holt's method - double exponential smoothing   (trend series)
# Nahmias, Chapter 2
#
# S_t = alpha D_t + (1 - alpha)(S_{t-1} + G_{t-1})     level
# G_t = beta (S_t - S_{t-1}) + (1 - beta) G_{t-1}      slope (trend)
# One-step forecast:   F_t = S_{t-1} + G_{t-1}
# tau steps ahead:     F_{n+tau} = S_n + tau G_n
# Initial values S_0, G_0: regression line D_t = a + b t  ->  S_0 = a, G_0 = b
#                          or typed by the user.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
holt_run <- function(d, alpha, beta, S0, G0) {
  n <- length(d)
  S <- G <- F <- numeric(n)
  S_prev <- S0; G_prev <- G0
  for (t in 1:n) {
    F[t] <- S_prev + G_prev
    S[t] <- alpha * d[t] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    S_prev <- S[t]; G_prev <- G[t]
  }
  list(S = S, G = G, F = F)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
holt_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    c("\\(S_t = \\alpha D_t + (1-\\alpha)(S_{t-1} + G_{t-1})\\)", "\\(G_t = \\beta (S_t - S_{t-1}) + (1-\\beta)\\, G_{t-1}\\)", "\\(F_{t+\\tau} = S_t + \\tau\\, G_t\\)"),
    parameter_mode_ui(ns,
      manual = tagList(
        numericInput(ns("alpha"), "alpha (level)", value = 0.2,
                     min = 0.01, max = 1, step = 0.05),
        numericInput(ns("beta"), "beta (trend)", value = 0.1,
                     min = 0.01, max = 1, step = 0.05)
      )
    ),
    initial_values_ui(ns, slope = TRUE)
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
holt_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = "Trend")
    observeEvent(demand(), suggest_initial_values(session, demand()))

    # The whole calculation for data d and horizon h (also used for the hold-out test)
    compute <- function(d, h) {
      n <- length(d)
      st <- initial_values(input, d)
      S0 <- st[["level"]]; G0 <- st[["slope"]]
      search <- NULL
      if (input$mode == "manual") {
        req(input$alpha, input$beta)
        alpha <- input$alpha; beta <- input$beta
        validate(need(alpha > 0 && alpha <= 1 && beta > 0 && beta <= 1,
                      "alpha and beta must be in (0, 1]."))
      } else {
        search <- search_smoothing_constants(
          c("alpha", "beta"),
          function(p) forecast_errors(d, holt_run(d, p[["alpha"]], p[["beta"]], S0, G0)$F, 1:n),
          input$criterion)
        alpha <- search$alpha[1]; beta <- search$beta[1]
      }
      run <- holt_run(d, alpha, beta, S0, G0)
      list(method  = paste0("Holt's method (alpha = ", alpha, ", beta = ", beta, ")"),
           d = d, fitted = run$F,
           future  = run$S[n] + (1:h) * run$G[n],
           periods = 1:n,
           params  = data.frame(
             Parameter = c("alpha", "beta", "S0 (initial level)", "G0 (initial slope)"),
             Value = c(alpha, beta, S0, G0)),
           detail  = data.frame("S_t" = run$S, "G_t" = run$G, check.names = FALSE),
           search  = search)
    }
    result <- reactive(add_holdout(compute(demand(), horizon(input)), input, compute))

    render_forecast_results(output, result)
  })
}
