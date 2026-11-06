# =====================================================================
# Topic: Weighted Moving Average - WMA(N)               (stationary series)
# Nahmias, Chapter 2
#
# F_t = w_1 D_{t-1} + w_2 D_{t-2} + ... + w_N D_{t-N}
#   w_1 is the weight of the most recent period, w_i >= 0, sum(w_i) = 1
# The forecast for every future period is the last WMA value.
# Best weights: every combination on a grid (step 0.05, or 0.1 for N = 5)
# is tried and the one with the smallest MAD / MSE / MAPE is kept.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# Forecasts with weights w (w[1] = most recent). Length n + 1.
weighted_moving_average_forecast <- function(d, w) {
  n <- length(d); N <- length(w)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) {
    F[t] <- sum(w * d[(t - 1):(t - N)])
  }
  F
}

# All weight vectors of length N on a grid with the given step that add up to 1.
weight_grid <- function(N, step) {
  k <- round(1 / step)
  build <- function(parts, total) {
    if (parts == 1) return(matrix(total, 1, 1))
    do.call(rbind, lapply(0:total, function(first) {
      cbind(first, build(parts - 1, total - first))
    }))
  }
  g <- build(N, k) * step
  colnames(g) <- paste0("w", 1:N)
  as.data.frame(g)
}

# Reads "0.5, 0.3, 0.2" into a numeric vector.
parse_weights <- function(text) {
  parts <- strsplit(gsub(";", ",", text), ",")[[1]]
  suppressWarnings(as.numeric(trimws(parts)))
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    "F_t = w1 D(t-1) + w2 D(t-2) + ... + wN D(t-N). Weights add up to 1.",
    parameter_mode_ui(ns,
      manual = textInput(ns("weights"),
                         "Weights, most recent period first (comma separated)",
                         value = "0.5, 0.3, 0.2"),
      auto   = numericInput(ns("N"), "N (number of weights, 2 to 5)",
                            value = 3, min = 2, max = 5, step = 1)
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
weighted_moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data")

    result <- reactive({
      d <- demand(); n <- length(d)
      search <- NULL
      if (input$mode == "manual") {
        w <- parse_weights(input$weights)
        validate(need(length(w) >= 1 && all(!is.na(w)),
                      "Type the weights as numbers separated by commas."),
                 need(all(w >= 0), "Weights cannot be negative."),
                 need(abs(sum(w) - 1) < 1e-6,
                      paste0("The weights must add up to 1 (now ", sum(w), ").")),
                 need(length(w) < n, "Too many weights for this data."))
      } else {
        req(input$N)
        N <- as.integer(input$N)
        validate(need(N >= 2 && N <= 5, "N must be between 2 and 5."),
                 need(N < n, "N must be smaller than the number of observations."))
        step <- if (N <= 4) 0.05 else 0.1
        grid <- weight_grid(N, step)
        search <- search_parameters(
          grid,
          function(p) forecast_errors(d, weighted_moving_average_forecast(d, unname(p)),
                                      (N + 1):n),
          input$criterion)
        w <- unlist(search[1, paste0("w", 1:N)])
      }
      w <- unname(w); N <- length(w)
      F <- weighted_moving_average_forecast(d, w)
      list(method  = paste0("Weighted moving average, weights = (",
                            paste(round(w, 3), collapse = ", "), ")"),
           d = d, fitted = F[1:n],
           future  = rep(F[n + 1], horizon(input)),
           periods = (N + 1):n,
           params  = data.frame(Parameter = paste0("w", 1:N, " (period t-", 1:N, ")"),
                                Value = w),
           search  = search)
    })

    render_forecast_results(output, result)
  })
}
