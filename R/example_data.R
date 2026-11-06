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

# Example datasets preloaded into the Data Library (same data as data/company_data.xlsx
# and data/weekly_demand.csv; the two smaller examples come from the functions above).
example_library <- function() {
  list(
    `Example > Monthly sales (4 products)` = data.frame(
      `Month` = c("2023-01", "2023-02", "2023-03", "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09", "2023-10", "2023-11", "2023-12", "2024-01", "2024-02", "2024-03", "2024-04", "2024-05", "2024-06", "2024-07", "2024-08", "2024-09", "2024-10", "2024-11", "2024-12", "2025-01", "2025-02", "2025-03", "2025-04", "2025-05", "2025-06", "2025-07", "2025-08", "2025-09", "2025-10", "2025-11", "2025-12"),
      `Product A (stable)` = c(516, 468, 504, 497, 480, 425, 478, 469, 503, 486, 488, 478, 493, 493, 424, 540, 518, 507, 476, 521, 490, 495, 458, 544, 501, 557, 552, 502, 519, 552, 484, 505, 492, 510, 505, 535),
      `Product B (growing)` = c(317, 303, 333, 320, 323, 360, 338, 369, 359, 401, 399, 390, 416, 418, 402, 425, 422, 451, 442, 457, 449, 462, 486, 477, 498, 511, 513, 520, 506, 536, 521, 573, 549, 574, 588, 588),
      `Product C (seasonal)` = c(674, 659, 771, 776, 893, 937, 991, 980, 811, 798, 691, 641, 639, 701, 760, 781, 869, 980, 951, 997, 828, 785, 674, 673, 616, 668, 733, 809, 870, 908, 994, 932, 865, 774, 681, 717),
      `Product D (growing, seasonal)` = c(301, 329, 368, 400, 445, 512, 538, 534, 471, 460, 465, 380, 421, 454, 470, 514, 492, 594, 632, 621, 563, 526, 483, 515, 452, 469, 543, 566, 581, 677, 751, 718, 635, 577, 566, 498),
      check.names = FALSE),
    `Example > Quarterly sales` = data.frame(
      `Quarter` = c("2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4", "2022-Q1", "2022-Q2", "2022-Q3", "2022-Q4", "2023-Q1", "2023-Q2", "2023-Q3", "2023-Q4", "2024-Q1", "2024-Q2", "2024-Q3", "2024-Q4", "2025-Q1", "2025-Q2", "2025-Q3", "2025-Q4", "2026-Q1", "2026-Q2", "2026-Q3", "2026-Q4"),
      `Total sales` = c(1701, 2233, 2776, 1775, 1774, 2370, 2965, 1876, 1947, 2670, 3207, 2053, 1974, 2779, 3380, 2132, 2228, 2940, 3536, 2286, 2248, 3155, 3832, 2331),
      check.names = FALSE),
    `Example > Aggregate plan (12 months)` = data.frame(
      `Month` = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
      `Demand forecast` = c(2200, 1800, 2400, 2600, 3100, 3600, 3900, 3500, 2900, 2500, 2300, 2700),
      `Working days` = c(21, 20, 22, 21, 22, 20, 21, 22, 21, 22, 20, 18),
      check.names = FALSE),
    `Example > Product families` = data.frame(
      `Family` = c("Small", "Medium", "Large"),
      `Setup cost` = c(800, 1200, 2000),
      `Demand` = c(1500, 900, 400),
      `Lower bound` = c(0, 0, 100),
      `Upper bound` = c(5000, 5000, 5000),
      check.names = FALSE),
    `Example > Weekly demand (52 weeks)` = data.frame(
      `Week` = c("W01", "W02", "W03", "W04", "W05", "W06", "W07", "W08", "W09", "W10", "W11", "W12", "W13", "W14", "W15", "W16", "W17", "W18", "W19", "W20", "W21", "W22", "W23", "W24", "W25", "W26", "W27", "W28", "W29", "W30", "W31", "W32", "W33", "W34", "W35", "W36", "W37", "W38", "W39", "W40", "W41", "W42", "W43", "W44", "W45", "W46", "W47", "W48", "W49", "W50", "W51", "W52"),
      `Demand` = c(257, 276, 281, 278, 290, 263, 258, 248, 218, 244, 265, 260, 267, 308, 308, 324, 315, 313, 313, 291, 277, 258, 273, 268, 312, 306, 308, 350, 332, 319, 319, 322, 304, 304, 289, 270, 296, 333, 323, 337, 355, 384, 354, 367, 340, 331, 320, 327, 326, 338, 351, 380),
      check.names = FALSE),
    `Example > Densepack (Nahmias)` = example_aggregate_data(),
    `Example > Quarterly series (stationary, trend, seasonal)` = example_forecast_data()
  )
}
