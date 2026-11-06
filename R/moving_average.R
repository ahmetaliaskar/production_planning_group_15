# =====================================================================
# Topic: Forecasting with Moving Averages - MA(N)       (stationary series)
# Nahmias, Chapter 2
#
# MA(N) forecast:  F_t = (D_{t-1} + D_{t-2} + ... + D_{t-N}) / N
# The forecast for every future period is the last MA value.
# Shared tools (file reading, MAD/MSE/MAPE, outputs): forecast_common.R
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS (the calculations; independent of the screen)
# ---------------------------------------------------------------------

# MA(N) forecasts for the demand vector d.
# The output has length n + 1: the last element is the next-period forecast.
# No forecast can be made for the first N periods (NA).
moving_average_forecast <- function(d, N) {
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) {
    F[t] <- mean(d[(t - N):(t - 1)])
  }
  F
}

# Tries every N = 1, 2, ..., Nmax.
# For a fair comparison, all N are evaluated over the SAME periods (Nmax+1 ... n).
moving_average_search <- function(d, Nmax, criterion) {
  search_parameters(
    data.frame(N = 1:Nmax),
    function(p) forecast_errors(d, moving_average_forecast(d, p[["N"]]),
                                (Nmax + 1):length(d)),
    criterion
  )
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
moving_average_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    "F_t = average of the last N demands. For stationary series.",
    parameter_mode_ui(ns,
      manual = numericInput(ns("N"), "N (number of past periods)",
                            value = 3, min = 1, step = 1),
      auto   = numericInput(ns("Nmax"), "Largest N to try",
                            value = 12, min = 2, step = 1)
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data")

    result <- reactive({
      d <- demand(); n <- length(d)
      search <- NULL
      if (input$mode == "manual") {
        req(input$N)
        N <- as.integer(input$N)
        validate(need(N >= 1 && N < n, "N must be between 1 and n - 1."))
        start <- N + 1
      } else {
        req(input$Nmax)
        Nmax <- as.integer(input$Nmax)
        validate(need(Nmax >= 1 && Nmax < n,
                      "The largest N must be smaller than the number of observations."))
        search <- moving_average_search(d, Nmax, input$criterion)
        N <- search$N[1]
        start <- Nmax + 1
      }
      F <- moving_average_forecast(d, N)
      list(method  = paste0("Moving average MA(", N, ")"),
           d = d, fitted = F[1:n],
           future  = rep(F[n + 1], horizon(input)),
           periods = start:n,
           params  = data.frame(Parameter = "N", Value = N),
           search  = search)
    })

    render_forecast_results(output, result)
  })
}
