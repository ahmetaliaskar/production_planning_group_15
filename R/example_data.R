# =====================================================================
# Built-in example data ("Load example data" buttons)
# The same data is in data/forecast_example.xlsx and data/aggregate_example.xlsx.
# =====================================================================

# 24 quarters: a stationary, a trend, a seasonal (trend + 4-quarter season) and a
# seasonal series without trend (for the quick-and-dirty seasonal factors)
example_forecast_data <- function() {
  data.frame(
    Period = paste0("Q", rep(1:4, 6), "-", rep(2021:2026, each = 4)),
    Stationary = c(162, 155, 159, 142, 154, 161, 144, 150, 167, 159, 162, 156,
                   147, 162, 142, 134, 154, 159, 180, 138, 146, 165, 136, 169),
    Trend = c(110, 118, 122, 130, 129, 146, 144, 143, 150, 163, 173, 171,
              180, 181, 186, 188, 193, 201, 212, 223, 222, 239, 229, 233),
    Seasonal = c(166, 246, 277, 183, 184, 243, 306, 201, 195, 266, 338, 205,
                 213, 287, 369, 224, 228, 319, 374, 236, 255, 352, 406, 252),
    "Seasonal (no trend)" = c(158, 224, 262, 163, 165, 217, 255, 158, 161, 226, 266, 155,
                              156, 219, 259, 166, 163, 222, 251, 160, 159, 215, 264, 162),
    check.names = FALSE
  )
}

# 60 months of Box-Jenkins series, simulated from the model of each tab:
#   AR(1)       D_t = mu + 0.75 (D_{t-1} - mu) + eps_t
#   MA(1)       D_t = mu + eps_t + 0.8 eps_{t-1}
#   ARMA(1,1)   D_t = mu + 0.6 (D_{t-1} - mu) + eps_t + 0.6 eps_{t-1}
#   ARIMA(1,1,0) the differences follow an AR(1) with phi = 0.6 (a wandering level)
example_box_jenkins_data <- function() {
  data.frame(
    Month = paste0(rep(2021:2025, each = 12), "-", sprintf("%02d", rep(1:12, 5))),
    "AR(1)" = c(599,544,533,541,499,509,517,530,493,478,473,450,506,492,502,495,536,560,561,491,
                494,513,535,511,553,507,489,515,512,559,549,521,506,478,451,479,499,532,505,546,
                527,560,534,507,512,534,530,508,472,444,471,503,498,472,501,468,458,484,432,459),
    "MA(1)" = c(375,409,451,461,411,405,395,420,435,417,391,374,430,390,383,409,391,381,393,384,
                385,421,428,456,384,364,412,425,411,382,402,395,365,378,428,469,438,367,367,371,
                357,368,438,429,362,344,359,382,435,426,367,377,419,464,448,388,369,425,458,491),
    "ARMA(1,1)" = c(714,643,608,621,590,585,607,628,598,564,558,539,589,607,605,602,637,679,683,604,
                    571,600,636,621,649,624,578,600,615,660,671,629,599,566,531,555,597,639,624,645,
                    645,662,650,605,598,629,637,610,564,523,545,599,610,577,592,576,549,574,537,538),
    "ARIMA(1,1,0)" = c(477,481,485,494,488,489,494,506,501,493,484,468,477,478,482,483,501,525,547,538,
                       535,540,554,558,578,579,573,580,585,609,626,631,631,620,602,598,603,619,622,642,
                       651,674,684,683,686,699,709,710,698,678,671,678,681,673,678,669,657,657,635,627),
    check.names = FALSE
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
    `Example > Quarterly series (stationary, trend, seasonal)` = example_forecast_data(),
    `Example > Box-Jenkins series (AR, MA, ARMA, ARIMA)` = example_box_jenkins_data()
  )
}

# ---------------------------------------------------------------------
# Aggregate planning examples, one for each plan tab.
# The LP is the optimum of the same cost model, so no heuristic can be
# cheaper than it. Each example is a situation in which its own method is
# the right choice: the heuristic then reaches (or nearly reaches) the LP cost.
# Each example sets the data AND the costs, plant data and K in 'Data and
# Costs' (and the options of the LP and transportation tabs).
# 'densepack' is the example of the 'Data and Costs' tab.
# Checked costs (whole workers):
#   chase      chase 19,600 = LP 19,600     level 400,500   mixed 407,150
#   level      level 9,541 ~ LP 9,403       chase 405,618   mixed 98,173
#   mixed      mixed 66,656 ~ LP 64,218     chase 86,348    level 180,056
#   lp         LP 359,345                   level 407,456   transport 389,496
#   transport  transport 365,990 = LP 365,990   level 460,456
# ---------------------------------------------------------------------
example_aggregate_scenarios <- function() {
  months <- c("January", "February", "March", "April", "May", "June", "July",
              "August", "September", "October", "November", "December")
  days <- c(21, 20, 22, 21, 22, 20, 21, 22, 21, 22, 20, 18)
  frame <- function(D) data.frame(Month = months, Demand = D, "Working days" = days,
                                  check.names = FALSE)
  costs <- function(hire, fire, hold, reg = 0, ot = 0, idle = 0, sub = 0)
    list(hire = hire, fire = fire, hold = hold, reg = reg, ot = ot, idle = idle, sub = sub)
  no_lp_extras <- list(allow_ot = FALSE, ot_limit = 0, allow_idle = TRUE, allow_sub = FALSE, sub_limit = 0)
  no_transport_extras <- list(W = NULL, ot_pct = 0, sub_cap = 0)
  peak <- c(2000, 1900, 2100, 2300, 2700, 3300, 3900, 3600, 2800, 2300, 2100, 2400)

  list(
    densepack = list(
      label = "Textbook example: Densepack (Nahmias, Chapter 3)",
      why = paste("Six months, hiring 500, firing 1,000, holding 80; K is computed from",
                  "past data (245 units by 76 workers in 22 days)."),
      data = example_aggregate_data(),
      costs = costs(hire = 500, fire = 1000, hold = 80),
      W0 = 300, I0 = 500, IT = 600, K = 0.14653, k_past = c(units = 245, workers = 76, days = 22),
      lp = no_lp_extras, transport = list(W = NULL, ot_pct = 20, sub_cap = 0)),
    chase = list(
      label = "Chase plan example: fresh food, storage is expensive",
      why = paste("A fresh product cannot wait in stock (holding 25 per unit per month), while",
                  "seasonal workers are cheap to hire (200) and to let go (300). Following demand",
                  "is the best plan: the chase plan costs the same as the LP."),
      data = frame(c(1680, 1520, 1980, 2184, 2640, 2800, 3276, 3168, 2520, 2200, 1760, 1728)),
      costs = costs(hire = 200, fire = 300, hold = 25, idle = 15),
      W0 = 40, I0 = 0, IT = 0, K = 2,
      lp = no_lp_extras, transport = no_transport_extras),
    level = list(
      label = "Level plan example: skilled workers, cheap storage",
      why = paste("Skilled workers are expensive to hire (1,500) and to fire (3,000), and storing",
                  "a unit costs only 1 per month. Keeping one workforce all year and building",
                  "stock before the summer peak is best: the level plan is within 2 % of the LP."),
      data = frame(c(1700, 1500, 1900, 2000, 2300, 2600, 2800, 2500, 2100, 1900, 1800, 2000)),
      costs = costs(hire = 1500, fire = 3000, hold = 1, idle = 10),
      W0 = 101, I0 = 300, IT = 300, K = 1,
      lp = no_lp_extras, transport = no_transport_extras),
    mixed = list(
      label = "Mixed plan example: a quiet half-year and a busy half-year",
      why = paste("Demand doubles in July and stays high. Changing the workforce every month",
                  "(chase) or never (level) are both costly; one change in July (the mixed plan's",
                  "default) is the best heuristic and close to the LP."),
      data = frame(c(1000, 950, 1050, 1000, 1020, 980, 2000, 2100, 1950, 2050, 2000, 1900)),
      costs = costs(hire = 600, fire = 900, hold = 8, idle = 5),
      W0 = 95, I0 = 0, IT = 0, K = 0.5,
      lp = no_lp_extras, transport = no_transport_extras),
    lp = list(
      label = "LP example: overtime, subcontracting and a summer peak",
      why = paste("Wages (10 per unit), overtime (15, up to 25 % of regular time),",
                  "subcontracting (19, up to 500 units a month), hiring, firing and holding all",
                  "have a cost. Only the LP mixes all of them; it is 12 % cheaper than the best",
                  "heuristic."),
      data = frame(peak),
      costs = costs(hire = 1000, fire = 1500, hold = 4, reg = 10, ot = 15, idle = 10, sub = 19),
      W0 = 110, I0 = 200, IT = 200, K = 1,
      lp = list(allow_ot = TRUE, ot_limit = 25, allow_idle = TRUE, allow_sub = TRUE, sub_limit = 500),
      transport = list(W = NULL, ot_pct = 25, sub_cap = 500)),
    transport = list(
      label = "Transportation example: a fixed workforce with overtime and subcontracting",
      why = paste("The 105 workers stay (hiring 3,000, firing 5,000); the peak is covered with",
                  "regular time, overtime (25 %) and subcontracting (500 units) of the same or",
                  "earlier months. The transportation method finds the optimum: the same cost",
                  "as the LP."),
      data = frame(peak),
      costs = costs(hire = 3000, fire = 5000, hold = 4, reg = 10, ot = 15, idle = 10, sub = 19),
      W0 = 105, I0 = 200, IT = 200, K = 1,
      lp = list(allow_ot = TRUE, ot_limit = 25, allow_idle = TRUE, allow_sub = TRUE, sub_limit = 500),
      transport = list(W = 105, ot_pct = 25, sub_cap = 500))
  )
}

# "Load this method's example" signal shared by the aggregate tabs: list(key, time).
ap_example_signal <- function(session) {
  if (is.null(session$userData$ap_example)) session$userData$ap_example <- reactiveVal(NULL)
  session$userData$ap_example
}

# Box with the example button of an aggregate plan tab.
ap_example_box <- function(ns, key) {
  sc <- example_aggregate_scenarios()[[key]]
  div(class = "method-example",
      actionButton(ns("example"), "Load this method's example", icon = icon("table"),
                   class = "btn-sm btn-outline-primary example-btn"),
      helpText(strong(sc$label), br(), sc$why,
               "It replaces the data and costs in 'Data and Costs'."))
}

# Server side of the box: sends the signal.
ap_example_observe <- function(input, session, key) {
  observeEvent(input$example, {
    ap_example_signal(session)(list(key = key, time = Sys.time()))
  })
}
