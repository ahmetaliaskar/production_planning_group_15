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
# csv files saved by a Turkish Excel:
#   - use ";" as the separator, "," as the decimal mark and "." for thousands
#     (1.280 = one thousand two hundred eighty)
#   - are often not UTF-8 but Windows-1254 (Turkish letters such as S, I with dots)
# Both are detected automatically.
read_data_file <- function(path, file_name) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (ext %in% c("csv", "txt")) {
    raw <- readBin(path, "raw", file.info(path)$size)
    raw <- raw[raw != as.raw(0)]
    bom <- as.raw(c(0xEF, 0xBB, 0xBF))                   # byte order mark
    if (length(raw) >= 3 && identical(raw[1:3], bom)) raw <- raw[-(1:3)]
    text <- rawToChar(raw)
    Encoding(text) <- "unknown"
    text <- if (validUTF8(text)) text else iconv(text, "windows-1254", "UTF-8")
    Encoding(text) <- "UTF-8"
    first_line <- strsplit(text, "\r?\n")[[1]][1]
    n_semicolon <- lengths(regmatches(first_line, gregexpr(";", first_line)))
    n_comma     <- lengths(regmatches(first_line, gregexpr(",", first_line)))
    if (n_semicolon > n_comma) {
      df <- utils::read.csv(text = text, sep = ";", check.names = FALSE,
                            colClasses = "character", encoding = "UTF-8")
      df[] <- lapply(df, function(col) {
        v <- trimws(col)
        is_num <- grepl("^-?[0-9.]*(,[0-9]+)?$", v) & grepl("[0-9]", v)
        if (all(is_num | v == "" | is.na(v))) {
          suppressWarnings(as.numeric(sub(",", ".", gsub(".", "", v, fixed = TRUE))))
        } else col
      })
      return(df)
    }
    return(utils::read.csv(text = text, check.names = FALSE, encoding = "UTF-8"))
  }
  stop("Please upload an .xlsx, .xls or .csv file.")
}

# Turns a column into numbers. Text such as "12,5" or "1.234,5" is accepted.
as_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  both <- grepl(",", x) & grepl(".", x, fixed = TRUE)
  x[both] <- gsub(".", "", x[both], fixed = TRUE)    # 1.234,5 -> 1234,5
  x <- sub(",", ".", x)                               # 12,5    -> 12.5
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
    example_button(ns),
    selectInput(ns("column"), "Series (demand column)", choices = NULL)
  )
}

# Returns a reactive with the demand vector of the chosen column.
# `example_column`: the column chosen when the example data is loaded.
series_input_server <- function(id, example_column = "Seasonal") {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    preferred <- reactiveVal(NULL)

    observeEvent(input$file, {
      df <- tryCatch(read_data_file(input$file$datapath, input$file$name),
                     error = function(e) e)
      if (inherits(df, "error")) {
        showNotification(conditionMessage(df), type = "error")
        return()
      }
      preferred(NULL)
      data(df)
    })
    observeEvent(input$example, {
      preferred(example_column)
      data(example_forecast_data())
    })

    observeEvent(data(), {
      cols <- numeric_columns(data())
      sel <- if (!is.null(preferred()) && preferred() %in% cols) preferred() else tail(cols, 1)
      updateSelectInput(session, "column", choices = cols, selected = sel)
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

# `extra`: module-specific outputs shown right below the forecast plot.
forecast_results_ui <- function(ns, extra = NULL) {
  tagList(
    div(class = "result-title", textOutput(ns("headline"), inline = TRUE)),
    uiOutput(ns("kpis")),
    plotOutput(ns("forecast_plot"), height = "360px"),
    extra,
    fluidRow(
      column(6, h4("Error measures"), tableOutput(ns("error_table"))),
      column(6, h4("Parameters used"), tableOutput(ns("param_table")))
    ),
    h4(textOutput(ns("search_title"))),
    tableOutput(ns("search_table")),
    h4("Forecasts for future periods"),
    tableOutput(ns("future_table")),
    div(class = "table-head", h4("Calculations by period"),
        downloadButton(ns("download"), "Download .csv", class = "btn-sm btn-outline-secondary")),
    tableOutput(ns("period_table"))
  )
}

render_forecast_results <- function(output, result) {

  output$headline <- renderText(result()$method)

  output$kpis <- renderUI({
    r <- tryCatch(result(), error = function(e) e)
    if (inherits(r, "error")) {
      if (nzchar(conditionMessage(r))) return(NULL)     # a real message is shown below
      return(empty_state("Drop an Excel or .csv file here to see the forecasts",
                         "or click to choose a file - or use 'Load example data' on the left."))
    }
    err <- forecast_errors(r$d, r$fitted, r$periods)
    div(class = "kpi-row",
        kpi_tile("Next-period forecast", fmt_num(r$future[1]),
                 paste("Period", length(r$d) + 1)),
        kpi_tile("MAD", fmt_num(err[["MAD"]])),
        kpi_tile("MSE", fmt_num(err[["MSE"]])),
        kpi_tile("MAPE", paste0(fmt_num(err[["MAPE"]]), " %")))
  })

  output$forecast_plot <- renderPlot({
    r <- result()
    n <- length(r$d); h <- length(r$future)
    cols <- app_colors
    yr <- range(c(r$d, r$fitted, r$future, r$future_lower, r$future_upper),
                na.rm = TRUE)
    yr <- yr + c(-0.04, 0.12) * diff(yr)               # room for the legend
    plot_setup()
    plot_frame(c(1, n + h), yr, xlab = "Period", ylab = "Demand")
    rect(n + 0.5, yr[1] - diff(yr), n + h + 1, yr[2] + diff(yr),
         col = "#f4f8fd", border = NA)                  # future area
    abline(h = pretty(yr), col = cols$grid)
    if (!is.null(r$future_lower)) {
      polygon(c((n + 1):(n + h), (n + h):(n + 1)), c(r$future_lower, rev(r$future_upper)),
              col = adjustcolor(cols$series[2], 0.15), border = NA)
    }
    lines(1:n, r$d, col = cols$ink_2, lwd = 2)
    points(1:n, r$d, pch = 16, col = cols$ink_2, cex = 0.9)
    lines(1:n, r$fitted, col = cols$series[1], lwd = 2, lty = 2)
    lines(c(n, (n + 1):(n + h)), c(r$fitted[n], r$future), col = cols$series[2], lwd = 2.5)
    points((n + 1):(n + h), r$future, pch = 21, bg = cols$series[2], col = "white",
           cex = 1.5, lwd = 1.5)
    text(n + 0.6, yr[2], "Forecast", adj = c(0, 1), cex = 0.8, col = cols$ink_2)
    plot_legend(c("Actual demand", "One-step forecast", "Future forecast"),
                c(cols$ink_2, cols$series[1], cols$series[2]), lty = c(1, 2, 1))
  }, bg = "white", res = 96)

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

  output$period_table <- renderTable(forecast_period_table(result()), na = "")

  output$download <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", result()$method), ".csv"),
    content = function(file) {
      r <- result()
      out <- forecast_period_table(r)
      n <- length(r$d); h <- length(r$future)
      fut <- out[rep(1, h), ]; fut[] <- NA
      fut$Period <- (n + 1):(n + h); fut[["Forecast F_t"]] <- round(r$future, 3)
      utils::write.csv(rbind(out, fut), file, row.names = FALSE, na = "")
    })
}

# Period-by-period table (screen and download).
forecast_period_table <- function(r) {
  n <- length(r$d)
  out <- data.frame(Period = 1:n,
                    "Demand D_t" = r$d,
                    "Forecast F_t" = round(r$fitted, 3),
                    "Error e_t = F_t - D_t" = round(r$fitted - r$d, 3),
                    check.names = FALSE)
  if (!is.null(r$detail)) out <- cbind(out, round(r$detail, 4))
  out
}

# A complete page for a forecasting method:
# data input + the module's own controls on the left, results on the right.
forecast_page <- function(ns, explanation, ...) {
  sidebarLayout(
    sidebarPanel(
      formula_box(explanation),
      series_input_ui(ns("data")),
      ...,
      horizon_ui(ns)
    ),
    mainPanel(forecast_results_ui(ns))
  )
}
