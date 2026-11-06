# =====================================================================
# Shared tools for all Forecasting modules
# Nahmias, Chapter 2
#
# Notation used in every module:
#   D_t  = observed demand in period t        (t = 1, ..., n)
#   F_t  = one-step-ahead forecast for period t, made at the end of t-1
#   e_t  = F_t - D_t                           (forecast error)
#   MAD  = mean of |e_t|
#   MSE  = mean of e_t^2
#   MAPE = mean of |e_t / D_t| x 100
#
# This file has no tab of its own. It contains:
#   1) reading Excel / csv files
#   2) error measures and the parameter search
#   3) small pieces of screen used by every module
#   4) the common outputs (plot, error table, period table)
# =====================================================================


# ---------------------------------------------------------------------
# 1) READING THE DATA FILE
# ---------------------------------------------------------------------

# Reads an .xlsx / .xls / .csv file into a data frame.
# csv files written by a Turkish Excel use ";" as the separator and ","
# as the decimal mark, so the separator is detected from the first line.
read_data_file <- function(path, file_name) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (ext %in% c("csv", "txt")) {
    first_line <- readLines(path, n = 1, warn = FALSE)
    n_semicolon <- lengths(regmatches(first_line, gregexpr(";", first_line)))
    n_comma     <- lengths(regmatches(first_line, gregexpr(",", first_line)))
    if (n_semicolon > n_comma) {
      return(utils::read.csv(path, sep = ";", dec = ",", check.names = FALSE,
                             fileEncoding = "UTF-8-BOM"))
    }
    return(utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM"))
  }
  stop("Please upload an .xlsx, .xls or .csv file.")
}

# Turns a column into numbers ("1.234,5" style text is also accepted).
as_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- ifelse(grepl(",", x) & !grepl("\\.", x), sub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}

# Names of the columns that contain numbers.
numeric_columns <- function(df) {
  ok <- vapply(df, function(col) {
    v <- as_number(col)
    sum(!is.na(v)) >= 2 && sum(!is.na(v)) >= 0.8 * sum(!is.na(col) & col != "")
  }, logical(1))
  names(df)[ok]
}

# Screen part: file upload + column choice (a small module of its own).
series_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload data (.xlsx, .xls or .csv)",
              accept = c(".xlsx", ".xls", ".csv")),
    selectInput(ns("column"), "Series (demand column)", choices = NULL)
  )
}

# Returns a reactive with the demand vector of the chosen column.
series_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$file)
      tryCatch(read_data_file(input$file$datapath, input$file$name),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    })
    observeEvent(data(), {
      cols <- numeric_columns(data())
      updateSelectInput(session, "column", choices = cols,
                        selected = tail(cols, 1))
    })
    reactive({
      req(input$column, input$column %in% names(data()))
      d <- as_number(data()[[input$column]])
      d <- d[!is.na(d)]
      validate(need(length(d) >= 3, "The series needs at least 3 numbers."))
      d
    })
  })
}


# ---------------------------------------------------------------------
# 2) ERROR MEASURES AND PARAMETER SEARCH
# ---------------------------------------------------------------------

# MAD, MSE and MAPE over the given periods (periods without a forecast are skipped).
forecast_errors <- function(d, F, periods) {
  periods <- periods[periods >= 1 & periods <= length(d)]
  periods <- periods[!is.na(F[periods])]
  if (length(periods) == 0) return(c(MAD = NA, MSE = NA, MAPE = NA))
  e  <- F[periods] - d[periods]
  nz <- d[periods] != 0
  c(MAD  = mean(abs(e)),
    MSE  = mean(e^2),
    MAPE = if (any(nz)) mean(abs(e[nz] / d[periods][nz])) * 100 else NA)
}

# Tries every row of `grid` (a data frame of parameter values).
# `evaluate` takes one row (named vector) and returns c(MAD, MSE, MAPE).
# The result is sorted so that the best row (by `criterion`) is first.
search_parameters <- function(grid, evaluate, criterion) {
  errs <- t(apply(grid, 1, function(p) evaluate(p)))
  out <- cbind(grid, as.data.frame(errs))
  out <- out[order(out[[criterion]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Search for smoothing constants in (0, 1]:
# first a coarse grid, then a fine grid (step 0.01) around the best point.
search_smoothing_constants <- function(names, evaluate, criterion, coarse = 0.05) {
  coarse_grid <- expand.grid(setNames(rep(list(seq(coarse, 1, by = coarse)),
                                          length(names)), names))
  first <- search_parameters(coarse_grid, evaluate, criterion)
  ranges <- lapply(names, function(nm) {
    v <- first[1, nm]
    unique(round(seq(max(0.01, v - coarse), min(1, v + coarse), by = 0.01), 2))
  })
  fine_grid <- expand.grid(setNames(ranges, names))
  search_parameters(fine_grid, evaluate, criterion)
}

# Initial level and slope from a regression line D_t = a + b t  (t = 1..n).
# S0 = a (the line's value at t = 0), G0 = b.
regression_start <- function(d) {
  t <- seq_along(d)
  fit <- stats::lm(d ~ t)
  c(level = unname(stats::coef(fit)[1]), slope = unname(stats::coef(fit)[2]))
}

# Position of period t inside a season of length N  (1, 2, ..., N).
season_position <- function(t, N) ((t - 1) %% N) + 1


# ---------------------------------------------------------------------
# 3) SMALL SCREEN PIECES USED BY THE MODULES
# ---------------------------------------------------------------------

# "Enter manually" / "Search for the best values" choice.
parameter_mode_ui <- function(ns, manual, auto = NULL,
                              criteria = c("MAD", "MSE", "MAPE")) {
  tagList(
    radioButtons(ns("mode"), "Parameters",
                 choices = c("Enter manually" = "manual",
                             "Search for the best values" = "auto")),
    conditionalPanel("input.mode == 'manual'", ns = ns, manual),
    conditionalPanel("input.mode == 'auto'", ns = ns, auto,
                     selectInput(ns("criterion"), "Choose the best values by",
                                 choices = criteria))
  )
}

# Initial values: from a regression line, or typed by the user.
initial_values_ui <- function(ns, slope = TRUE) {
  tagList(
    radioButtons(ns("init"), "Initial values",
                 choices = c("Fit a regression line to the data" = "regression",
                             "Enter manually" = "manual")),
    conditionalPanel("input.init == 'manual'", ns = ns,
      numericInput(ns("S0"), "Initial level S0", value = 0),
      if (slope) numericInput(ns("G0"), "Initial slope G0", value = 0)
    )
  )
}

# Returns c(level, slope) according to the user's choice.
initial_values <- function(input, d) {
  if (identical(input$init, "manual")) {
    req(!is.null(input$S0), !is.na(input$S0))
    G0 <- if (is.null(input$G0) || is.na(input$G0)) 0 else input$G0
    c(level = input$S0, slope = G0)
  } else {
    regression_start(d)
  }
}

# When new data arrives, put the regression values into the manual boxes
# as a starting suggestion.
suggest_initial_values <- function(session, d) {
  st <- regression_start(d)
  updateNumericInput(session, "S0", value = round(st[["level"]], 3))
  updateNumericInput(session, "G0", value = round(st[["slope"]], 3))
}

horizon_ui <- function(ns, value = 4) {
  numericInput(ns("h"), "Number of future periods to forecast",
               value = value, min = 1, step = 1)
}

# Reads the horizon safely.
horizon <- function(input) {
  h <- input$h
  if (is.null(h) || is.na(h) || h < 1) 1 else as.integer(h)
}


# ---------------------------------------------------------------------
# 4) COMMON OUTPUTS
# ---------------------------------------------------------------------
# Every module builds a "result" list and passes it to
# render_forecast_results(). The list has:
#   method   text, e.g. "Holt (alpha = 0.2, beta = 0.1)"
#   d        demand, length n
#   fitted   one-step forecasts F_1..F_n (NA where no forecast exists)
#   future   forecasts for n+1 .. n+h
#   periods  periods used for MAD / MSE / MAPE
#   params   data frame with the parameter values used        (optional)
#   detail   data frame with n rows of extra columns (S_t, ...)  (optional)
#   future_lower / future_upper  prediction interval             (optional)
#   search   data frame of the parameter search                  (optional)

forecast_results_ui <- function(ns) {
  tagList(
    h4(textOutput(ns("headline"))),
    plotOutput(ns("forecast_plot"), height = "350px"),
    fluidRow(
      column(6, h4("Error measures"), tableOutput(ns("error_table"))),
      column(6, h4("Parameters used"), tableOutput(ns("param_table")))
    ),
    h4(textOutput(ns("search_title"))),
    tableOutput(ns("search_table")),
    h4("Forecasts for future periods"),
    tableOutput(ns("future_table")),
    h4("Calculations by period"),
    tableOutput(ns("period_table"))
  )
}

render_forecast_results <- function(output, result) {

  output$headline <- renderText({
    r <- result()
    paste0(r$method, "  -  next-period forecast: ", round(r$future[1], 2))
  })

  output$forecast_plot <- renderPlot({
    r <- result()
    n <- length(r$d); h <- length(r$future)
    yr <- range(c(r$d, r$fitted, r$future, r$future_lower, r$future_upper),
                na.rm = TRUE)
    plot(1:n, r$d, type = "o", pch = 19, col = "grey30",
         xlim = c(1, n + h), ylim = yr, xlab = "Period", ylab = "Demand",
         main = r$method)
    lines(1:n, r$fitted, type = "o", pch = 17, lty = 2, col = "#2F6FB0")
    lines((n + 1):(n + h), r$future, type = "o", pch = 17, cex = 1.4,
          col = "#B4501E")
    if (!is.null(r$future_lower)) {
      lines((n + 1):(n + h), r$future_lower, lty = 3, col = "#B4501E")
      lines((n + 1):(n + h), r$future_upper, lty = 3, col = "#B4501E")
    }
    abline(v = n + 0.5, col = "grey80", lty = 3)
    legend("topleft", bty = "n",
           legend = c("Actual demand", "One-step forecast", "Future forecast"),
           col = c("grey30", "#2F6FB0", "#B4501E"),
           pch = c(19, 17, 17), lty = c(1, 2, 1))
  })

  output$error_table <- renderTable({
    r <- result()
    err <- forecast_errors(r$d, r$fitted, r$periods)
    data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
               Value = round(unname(err), 3),
               "Periods evaluated" = paste0(min(r$periods), " - ", max(r$periods)),
               check.names = FALSE)
  })

  output$param_table <- renderTable({
    r <- result()
    r$params
  }, digits = 4)

  output$search_title <- renderText({
    if (is.null(result()$search)) "" else "Parameter search (best 10)"
  })

  output$search_table <- renderTable({
    s <- result()$search
    if (is.null(s)) return(NULL)
    head(s, 10)
  }, digits = 3)

  output$future_table <- renderTable({
    r <- result()
    n <- length(r$d); h <- length(r$future)
    out <- data.frame(Period = as.integer((n + 1):(n + h)),
                      Forecast = round(r$future, 3))
    if (!is.null(r$future_lower)) {
      out[["Lower 95%"]] <- round(r$future_lower, 3)
      out[["Upper 95%"]] <- round(r$future_upper, 3)
    }
    out
  })

  output$period_table <- renderTable({
    r <- result()
    n <- length(r$d)
    out <- data.frame(Period = 1:n,
                      "Demand D_t" = r$d,
                      "Forecast F_t" = round(r$fitted, 3),
                      "Error e_t = F_t - D_t" = round(r$fitted - r$d, 3),
                      check.names = FALSE)
    if (!is.null(r$detail)) out <- cbind(out, round(r$detail, 4))
    out
  }, na = "")
}

# A complete page for a forecasting method:
# data input + the module's own controls on the left, results on the right.
forecast_page <- function(ns, explanation, ...) {
  sidebarLayout(
    sidebarPanel(
      helpText(explanation),
      series_input_ui(ns("data")),
      ...,
      horizon_ui(ns)
    ),
    mainPanel(forecast_results_ui(ns))
  )
}
