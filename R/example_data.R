# =====================================================================
# Built-in example data ("Load example data" buttons)
# The same data is in data/forecast_example.xlsx and data/aggregate_example.xlsx.
# =====================================================================

# 24 quarters: a stationary, a trend and a seasonal (trend + 4-quarter season) series
example_forecast_data <- function() {
  data.frame(
    Period = paste0("Q", rep(1:4, 6), "-", rep(2021:2026, each = 4)),
    Stationary = c(162, 155, 159, 142, 154, 161, 144, 150, 167, 159, 162, 156,
                   147, 162, 142, 134, 154, 159, 180, 138, 146, 165, 136, 169),
    Trend = c(110, 118, 122, 130, 129, 146, 144, 143, 150, 163, 173, 171,
              180, 181, 186, 188, 193, 201, 212, 223, 222, 239, 229, 233),
    Seasonal = c(166, 246, 277, 183, 184, 243, 306, 201, 195, 266, 338, 205,
                 213, 287, 369, 224, 228, 319, 374, 236, 255, 352, 406, 252)
  )
}

# 6 months (the Densepack example of Nahmias, Chapter 3)
example_aggregate_data <- function() {
  data.frame(
    Month = c("January", "February", "March", "April", "May", "June"),
    Demand = c(1280, 640, 900, 1200, 2000, 1400),
    "Working days" = c(20, 24, 18, 26, 22, 15),
    check.names = FALSE
  )
}

# Button shown under every file upload.
example_button <- function(ns) {
  actionButton(ns("example"), "Load example data", icon = icon("table"),
               class = "btn-sm btn-outline-primary example-btn")
}
