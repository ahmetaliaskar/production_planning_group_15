# =====================================================================
# Topic: Seasonal decomposition with moving averages    (seasonal series)
# Nahmias, Chapter 2
#
# N = length of one season
# 1. Centered moving average CMA_t of length N
#      N odd : average of D_{t-(N-1)/2} ... D_{t+(N-1)/2}
#      N even: average of two neighbouring MA(N)s
#              = (D_{t-N/2}/2 + D_{t-N/2+1} + ... + D_{t+N/2-1} + D_{t+N/2}/2) / N
# 2. Ratios           D_t / CMA_t
# 3. Seasonal factor  c_j = average of the ratios of position j,
#                     scaled so that the factors add up to N
# 4. Deseasonalize    D_t / c_j
# 5. Fit a regression line a + b t to the deseasonalized series
# 6. Forecast         F_t = (a + b t) * c_j
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
centered_moving_average <- function(d, N) {
  n <- length(d)
  cma <- rep(NA_real_, n)
  if (N %% 2 == 1) {
    k <- (N - 1) / 2
    for (t in (k + 1):(n - k)) cma[t] <- mean(d[(t - k):(t + k)])
  } else {
    k <- N / 2
    w <- c(0.5, rep(1, N - 1), 0.5) / N
    if (n > 2 * k) for (t in (k + 1):(n - k)) cma[t] <- sum(w * d[(t - k):(t + k)])
  }
  cma
}

seasonal_decomposition_run <- function(d, N) {
  n <- length(d)
  t <- 1:n
  pos <- season_position(t, N)
  cma <- centered_moving_average(d, N)
  ratio <- d / cma
  raw <- tapply(ratio, pos, mean, na.rm = TRUE)
  factors <- as.numeric(raw) * N / sum(raw)
  deseason <- d / factors[pos]
  fit <- stats::lm(deseason ~ t)
  a <- unname(stats::coef(fit)[1]); b <- unname(stats::coef(fit)[2])
  list(pos = pos, cma = cma, ratio = ratio, factors = factors,
       deseason = deseason, a = a, b = b,
       trend = a + b * t, fitted = (a + b * t) * factors[pos])
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
seasonal_decomposition_ui <- function(id) {
  ns <- NS(id)
  forecast_page(ns,
    "Centered MA -> seasonal factors -> deseasonalize -> trend line -> F_t = (a + b t) x c_j.",
    numericInput(ns("N"), "Season length N (e.g. 4 = quarters, 12 = months)",
                 value = 4, min = 2, step = 1),
    helpText("This method has no smoothing constant to search for;",
             "only the season length N is needed.")
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
seasonal_decomposition_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demand <- series_input_server("data")

    result <- reactive({
      d <- demand(); n <- length(d)
      req(input$N)
      N <- as.integer(input$N)
      validate(need(N >= 2 && n >= 2 * N,
                    "The data must contain at least two complete seasons."))
      r <- seasonal_decomposition_run(d, N)
      validate(need(all(!is.na(r$factors)),
                    "Not enough data to compute every seasonal factor."))
      h <- horizon(input)
      tf <- (n + 1):(n + h)
      list(method  = paste0("Seasonal decomposition, N = ", N,
                            ", trend line ", round(r$a, 3), " + ", round(r$b, 3), " t"),
           d = d, fitted = r$fitted,
           future  = (r$a + r$b * tf) * r$factors[season_position(tf, N)],
           periods = 1:n,
           params  = data.frame(
             Parameter = c("Trend intercept a", "Trend slope b", paste0("c", 1:N)),
             Value = c(r$a, r$b, r$factors)),
           detail  = data.frame("Season position" = r$pos,
                                "Centered MA" = r$cma,
                                "D_t / CMA" = r$ratio,
                                "Factor c_j" = r$factors[r$pos],
                                "Deseasonalized" = r$deseason,
                                "Trend a + b t" = r$trend,
                                check.names = FALSE))
    })

    render_forecast_results(output, result)
  })
}
