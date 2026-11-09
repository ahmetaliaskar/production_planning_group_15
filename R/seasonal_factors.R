# =====================================================================
# Topic: Seasonal factors - "quick and dirty" method
#        (stationary series with a seasonal pattern)
# Nahmias, Chapter 2
#
# N = length of one season (e.g. 4 quarters, 12 months)
# 1. Overall mean:      Dbar = mean of all demands in the complete seasons
# 2. Ratios:            D_t / Dbar
# 3. Seasonal factor:   c_j = average of the ratios of position j
#                       (the factors add up to N)
# 4. Forecast:          F_t = Dbar * c_j   (j = position of period t)
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
seasonal_factors_run <- function(d, N) {
  n_cycles <- floor(length(d) / N)
  used <- d[1:(n_cycles * N)]                 # only complete seasons
  mean_d <- mean(used)
  ratio <- used / mean_d
  pos <- season_position(seq_along(used), N)
  factors <- tapply(ratio, pos, mean)
  factors <- as.numeric(factors) * N / sum(factors)   # sum = N
  list(mean = mean_d, factors = factors, ratio = ratio, n_used = length(used))
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    c("\\(c_j = \\text{average of } D_t / \\bar{D}\\) over the periods of position \\(j\\)", "\\(F_t = \\bar{D}\\; c_j\\)"),
    numericInput(ns("N"), "Season length N (e.g. 4 = quarters, 12 = months)",
                 value = 4, min = 2, step = 1),
    helpText("This method has no smoothing constant to search for;",
             "only the season length N is needed.")
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
seasonal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data", example_column = "Seasonal (no trend)")
    # season length N from the period names / dates (monthly -> 12, quarterly -> 4, ...)
    observeEvent(demand(), auto_season_length(session, demand(), input$N))

    # The whole calculation for data d and horizon h (also used for the hold-out test)
    compute <- function(d, h) {
      n <- length(d)
      req(input$N)
      N <- as.integer(input$N)
      validate(need(N >= 2 && n >= N,
                    "The data must contain at least one complete season."))
      r <- seasonal_factors_run(d, N)
      pos <- season_position(1:n, N)
      fpos <- season_position((n + 1):(n + h), N)
      ratio <- c(r$ratio, rep(NA, n - r$n_used))
      list(method  = paste0("Seasonal factors (quick and dirty), N = ", N),
           d = d, fitted = r$mean * r$factors[pos],
           future  = r$mean * r$factors[fpos],
           periods = 1:n,
           params  = data.frame(
             Parameter = c("Overall mean", paste0("c", 1:N)),
             Value = c(r$mean, r$factors)),
           detail  = data.frame("Season position" = pos,
                                "D_t / mean" = ratio,
                                "Factor c_j" = r$factors[pos],
                                check.names = FALSE))
    }
    result <- reactive(add_holdout(compute(demand(), horizon(input)), input, compute))

    render_forecast_results(output, result)
  })
}
