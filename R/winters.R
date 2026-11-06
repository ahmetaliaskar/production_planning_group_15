# =====================================================================
# Topic: Winters' method - triple exponential smoothing (seasonal series)
# Nahmias, Chapter 2
#
# N = length of one season
# S_t = alpha (D_t / c_{t-N}) + (1 - alpha)(S_{t-1} + G_{t-1})   level
# G_t = beta (S_t - S_{t-1}) + (1 - beta) G_{t-1}                slope
# c_t = gamma (D_t / S_t) + (1 - gamma) c_{t-N}                  seasonal factor
# One-step forecast:  F_t = (S_{t-1} + G_{t-1}) c_{t-N}
# tau steps ahead:    F_{n+tau} = (S_n + tau G_n) c_{n+tau-N}
#
# Initial values:
#   S_0, G_0 : regression line D_t = a + b t  ->  S_0 = a, G_0 = b
#              (or typed by the user)
#   c_j      : average of D_t / (S_0 + G_0 t) over the periods of position j,
#              scaled so that the factors add up to N
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
winters_initial_factors <- function(d, N, S0, G0) {
  t <- seq_along(d)
  ratio <- d / (S0 + G0 * t)
  raw <- tapply(ratio, season_position(t, N), mean)
  as.numeric(raw) * N / sum(raw)
}

winters_run <- function(d, N, alpha, beta, gamma, S0, G0, c0) {
  n <- length(d)
  S <- G <- F <- cc <- numeric(n)
  factors <- c0                  # the latest factor of every position
  S_prev <- S0; G_prev <- G0
  for (t in 1:n) {
    j <- season_position(t, N)
    F[t] <- (S_prev + G_prev) * factors[j]
    S[t] <- alpha * d[t] / factors[j] + (1 - alpha) * (S_prev + G_prev)
    G[t] <- beta * (S[t] - S_prev) + (1 - beta) * G_prev
    cc[t] <- gamma * d[t] / S[t] + (1 - gamma) * factors[j]
    factors[j] <- cc[t]
    S_prev <- S[t]; G_prev <- G[t]
  }
  list(S = S, G = G, c = cc, F = F, last_factors = factors)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
winters_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    c("\\(S_t = \\alpha \\dfrac{D_t}{c_{t-N}} + (1-\\alpha)(S_{t-1} + G_{t-1})\\)", "\\(G_t = \\beta (S_t - S_{t-1}) + (1-\\beta)\\, G_{t-1}\\)", "\\(c_t = \\gamma \\dfrac{D_t}{S_t} + (1-\\gamma)\\, c_{t-N}\\)", "\\(F_{t+\\tau} = (S_t + \\tau G_t)\\; c_{t+\\tau-N}\\)"),
    numericInput(ns("N"), "Season length N (e.g. 4 = quarters, 12 = months)",
                 value = 4, min = 2, step = 1),
    parameter_mode_ui(ns,
      manual = tagList(
        numericInput(ns("alpha"), "alpha (level)", value = 0.2,
                     min = 0.01, max = 1, step = 0.05),
        numericInput(ns("beta"), "beta (trend)", value = 0.1,
                     min = 0.01, max = 1, step = 0.05),
        numericInput(ns("gamma"), "gamma (seasonal factors)", value = 0.1,
                     min = 0.01, max = 1, step = 0.05)
      ),
      auto = helpText("The search may take a few seconds.")
    ),
    initial_values_ui(ns, slope = TRUE)
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
winters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = "Seasonal")
    observeEvent(demand(), suggest_initial_values(session, demand()))

    # The whole calculation for data d and horizon h (also used for the hold-out test)
    compute <- function(d, h) {
      n <- length(d)
      req(input$N)
      N <- as.integer(input$N)
      validate(need(N >= 2 && n >= 2 * N,
                    "The data must contain at least two complete seasons."))
      st <- initial_values(input, d)
      S0 <- st[["level"]]; G0 <- st[["slope"]]
      c0 <- winters_initial_factors(d, N, S0, G0)
      validate(need(all(is.finite(c0)) && all(c0 > 0),
                    "The initial line gives zero or negative values; check S0 and G0."))
      search <- NULL
      if (input$mode == "manual") {
        req(input$alpha, input$beta, input$gamma)
        alpha <- input$alpha; beta <- input$beta; gamma <- input$gamma
        validate(need(all(c(alpha, beta, gamma) > 0 & c(alpha, beta, gamma) <= 1),
                      "alpha, beta and gamma must be in (0, 1]."))
      } else {
        search <- withProgress(message = "Searching alpha, beta, gamma...", {
          search_smoothing_constants(
            c("alpha", "beta", "gamma"),
            function(p) forecast_errors(
              d, winters_run(d, N, p[["alpha"]], p[["beta"]], p[["gamma"]], S0, G0, c0)$F, 1:n),
            input$criterion, coarse = 0.1)
        })
        alpha <- search$alpha[1]; beta <- search$beta[1]; gamma <- search$gamma[1]
      }
      run <- winters_run(d, N, alpha, beta, gamma, S0, G0, c0)
      tf <- (n + 1):(n + h)
      list(method  = paste0("Winters' method (alpha = ", alpha, ", beta = ", beta,
                            ", gamma = ", gamma, "), N = ", N),
           d = d, fitted = run$F,
           future  = (run$S[n] + (tf - n) * run$G[n]) * run$last_factors[season_position(tf, N)],
           periods = 1:n,
           params  = data.frame(
             Parameter = c("alpha", "beta", "gamma", "S0 (initial level)",
                           "G0 (initial slope)", paste0("initial c", 1:N)),
             Value = c(alpha, beta, gamma, S0, G0, c0)),
           detail  = data.frame("Season position" = season_position(1:n, N),
                                "S_t" = run$S, "G_t" = run$G, "c_t" = run$c,
                                check.names = FALSE),
           search  = search)
    }
    result <- reactive(add_holdout(compute(demand(), horizon(input)), input, compute))

    render_forecast_results(output, result)
  })
}
