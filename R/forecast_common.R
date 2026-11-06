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

# Columns that hold period names / dates rather than demand:
# text or date columns, and numeric columns called e.g. "Period", "Month", "Date".
label_column_candidates <- function(df) {
  nums <- numeric_columns(df)
  named <- nums[grepl("^(period|date|time|t|month|week|year|quarter|day|ay|tarih|donem|hafta|yil)$",
                      tolower(trimws(nums)))]
  list(labels = c(setdiff(names(df), nums), named), series = setdiff(nums, named))
}

# Turns a label column into text (Excel dates -> "2025-01-31").
format_labels <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(format(as.Date(x)))
  as.character(x)
}

# Cleans the chosen column and records what was done in attr(, "notes"):
#   text cells -> treated as empty; empty rows at the start/end -> dropped;
#   empty cells inside the series -> linear interpolation; negative / zero values -> warning.
# The period names are kept in attr(, "labels").
clean_series <- function(raw, labels = NULL) {
  v <- as_number(raw)
  notes <- character(0)
  txt <- trimws(as.character(raw))
  bad_text <- sum(!is.na(raw) & !is.na(txt) & txt != "" & is.na(v))
  if (bad_text > 0) notes <- c(notes, paste(bad_text, "cell(s) contain text and were treated as empty."))
  ok <- which(!is.na(v))
  validate(need(length(ok) >= 3, "The series needs at least 3 numbers."))
  keep <- min(ok):max(ok)
  dropped <- length(v) - length(keep)
  if (dropped > 0) notes <- c(notes, paste(dropped, "empty row(s) at the start or end were ignored."))
  v <- v[keep]
  if (!is.null(labels)) labels <- labels[keep]
  gaps <- which(is.na(v))
  if (length(gaps) > 0) {
    v[gaps] <- stats::approx(seq_along(v), v, xout = gaps)$y
    where <- if (!is.null(labels)) labels[gaps] else gaps
    notes <- c(notes, paste0(length(gaps), " empty cell(s) inside the series were filled by linear interpolation (",
                             paste(head(where, 5), collapse = ", "), if (length(gaps) > 5) ", ..." else "", ")."))
  }
  if (any(v < 0)) notes <- c(notes, paste(sum(v < 0), "negative value(s) - please check the data."))
  if (any(v == 0)) notes <- c(notes, paste(sum(v == 0), "zero value(s): these periods are left out of MAPE."))
  attr(v, "labels") <- labels
  attr(v, "notes") <- notes
  v
}

# Name of period t (t > n: "+1", "+2", ... for future periods).
period_label <- function(d, t) {
  n <- length(d); lab <- attr(d, "labels")
  ifelse(t > n, paste0("+", t - n),
         if (is.null(lab)) as.character(t) else as.character(lab[pmin(t, n)]))
}

# An empty Excel file in the right format.
write_input_template <- function(file) {
  writexl::write_xlsx(list(
    Data = data.frame(Period = c("2025-01", "2025-02", "2025-03"),
                      Demand = c(NA_real_, NA_real_, NA_real_)),
    Instructions = data.frame(Instructions = c(
      "One row per period, oldest first.",
      "Column 'Period': period name or date (optional, used on the charts).",
      "Then one numeric column per series (e.g. one per product).",
      "Empty cells inside a series are filled by interpolation; text cells are ignored."))),
    file)
}

# Screen part: file upload + column choice + data check (a small module of its own).
series_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload data (.xlsx, .xls or .csv)",
              accept = c(".xlsx", ".xls", ".csv")),
    div(class = "data-actions", example_button(ns),
        downloadLink(ns("template"), "Empty Excel template", class = "template-link")),
    selectInput(ns("dataset"), "Or choose from the data library", choices = c("(none loaded yet)" = "")),
    selectInput(ns("column"), "Series (demand column)", choices = NULL),
    selectInput(ns("label_col"), "Period names / dates (optional)", choices = c("(none)" = "")),
    uiOutput(ns("check"))
  )
}

# Returns a reactive with the cleaned demand vector of the chosen column
# (period names in attr(, "labels"), data notes in attr(, "notes")).
# `example_column`: the column chosen when the example data is loaded.
series_input_server <- function(id, example_column = "Seasonal") {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    preferred <- reactiveVal(NULL)
    lib <- data_library(session)
    broadcast <- library_broadcast(session)

    # a file uploaded here goes into the library (all its sheets) and is selected
    observeEvent(input$file, {
      res <- tryCatch(library_add_files(session, input$file), error = function(e) e)
      if (inherits(res, "error") || length(res$added) == 0) {
        showNotification(if (inherits(res, "error")) conditionMessage(res) else "The file could not be read.",
                         type = "error")
        return()
      }
      preferred(NULL)
      data(lib()[[res$added[1]]])
      updateSelectInput(session, "dataset", choices = c("(choose a dataset)" = "", names(lib())),
                        selected = res$added[1])
    })
    observeEvent(input$example, {
      preferred(example_column)
      data(example_forecast_data())
      updateSelectInput(session, "dataset", selected = "")
    })

    # the library list follows the library
    observeEvent(lib(), {
      nms <- names(lib())
      sel <- if (isTruthy(input$dataset) && input$dataset %in% nms) input$dataset else ""
      updateSelectInput(session, "dataset",
                        choices = c(if (length(nms)) c("(choose a dataset)" = "") else c("(none loaded yet)" = ""), nms),
                        selected = sel)
    })
    observeEvent(input$dataset, {
      req(input$dataset, input$dataset %in% names(lib()))
      preferred(NULL)
      data(lib()[[input$dataset]])
    })
    # "Use in every forecasting tab" from the Data Library
    observeEvent(broadcast(), {
      b <- broadcast()
      req(b$name %in% names(lib()))
      updateSelectInput(session, "dataset", selected = b$name)
      data(lib()[[b$name]])
    }, ignoreInit = TRUE)

    observeEvent(data(), {
      cand <- label_column_candidates(data())
      cols <- cand$series
      sel <- if (!is.null(preferred()) && preferred() %in% cols) preferred() else tail(cols, 1)
      updateSelectInput(session, "column", choices = cols, selected = sel)
      updateSelectInput(session, "label_col", choices = c("(none)" = "", cand$labels),
                        selected = if (length(cand$labels)) cand$labels[1] else "")
    })

    series <- reactive({
      req(input$column, input$column %in% names(data()))
      df <- data()
      lab <- if (isTruthy(input$label_col) && input$label_col %in% names(df))
        format_labels(df[[input$label_col]]) else NULL
      clean_series(df[[input$column]], lab)
    })

    output$check <- renderUI({
      d <- tryCatch(series(), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      n <- length(d)
      notes <- attr(d, "notes")
      div(class = if (length(notes)) "data-check warn" else "data-check",
          div(icon(if (length(notes)) "triangle-exclamation" else "circle-check"),
              paste0(" ", n, " periods loaded (", period_label(d, 1), " - ", period_label(d, n), ")")),
          if (length(notes)) tags$ul(lapply(notes, tags$li)))
    })

    output$template <- downloadHandler(
      filename = function() "forecast_input_template.xlsx",
      content = write_input_template)

    series
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

# Hold-out test: the last k periods are hidden, the method is fitted again on the
# remaining periods (including its parameter search) and its forecasts for the
# hidden periods are compared with what really happened.
holdout_ui <- function(ns) {
  numericInput(ns("test"), "Hold-out test periods (0 = none)", value = 0, min = 0, step = 1)
}

holdout_k <- function(input) {
  k <- input$test
  if (is.null(k) || is.na(k) || k < 1) 0L else as.integer(k)
}

# `compute(d, h)` is the module's whole calculation; adds r$test.
add_holdout <- function(r, input, compute) {
  k <- holdout_k(input)
  if (k == 0) return(r)
  d <- r$d; n <- length(d)
  if (n - k < 4) {
    r$test <- list(message = "Too many hold-out periods: at least 4 periods must remain for fitting.")
    return(r)
  }
  fc <- tryCatch(compute(as.numeric(d[1:(n - k)]), k)$future, error = function(e) e)
  if (inherits(fc, "error")) {
    r$test <- list(message = paste("The method could not be fitted on the first", n - k,
                                 "periods:", conditionMessage(fc)))
    return(r)
  }
  actual <- as.numeric(d[(n - k + 1):n])
  e <- fc - actual; nz <- actual != 0
  r$test <- list(k = k, periods = (n - k + 1):n, actual = actual, forecast = fc,
                 errors = c(MAD = mean(abs(e)), MSE = mean(e^2),
                            MAPE = if (any(nz)) mean(abs(e[nz] / actual[nz])) * 100 else NA))
  r
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
    p(class = "result-note", textOutput(ns("test_note"), inline = TRUE)),
    extra,
    fluidRow(
      column(6, h4("Error measures"), tableOutput(ns("error_table"))),
      column(6, h4("Parameters used"), tableOutput(ns("param_table")))
    ),
    h4(textOutput(ns("test_title"))),
    tableOutput(ns("test_table")),
    h4(textOutput(ns("search_title"))),
    tableOutput(ns("search_table")),
    h4("Forecasts for future periods"),
    tableOutput(ns("future_table")),
    div(class = "table-head", h4("Calculations by period"),
        downloadButton(ns("download"), "Download Excel", class = "btn-sm btn-outline-secondary")),
    tableOutput(ns("period_table"))
  )
}

# x axis with period names when they exist.
axis_periods <- function(d, h) {
  n <- length(d)
  if (is.null(attr(d, "labels"))) {
    at <- pretty(c(1, n + h)); at <- at[at >= 1 & at <= n + h]
    axis(1, at = at, labels = period_label(d, at), lwd = 0, lwd.ticks = 1)
  } else {
    at <- seq(1, n, by = max(1, ceiling(n / 7)))
    axis(1, at = at, labels = period_label(d, at), lwd = 0, lwd.ticks = 1, cex.axis = 0.78)
  }
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
    tst <- if (!is.null(r$test$errors)) r$test$errors else NULL
    sub <- function(m) if (is.null(tst)) "in-sample" else
      paste0("in-sample  |  test: ", fmt_num(tst[[m]]), if (m == "MAPE") " %" else "")
    div(class = "kpi-row",
        kpi_tile("Next-period forecast", fmt_num(r$future[1]),
                 paste("Period", period_label(r$d, length(r$d) + 1))),
        kpi_tile("MAD", fmt_num(err[["MAD"]]), sub("MAD")),
        kpi_tile("MSE", fmt_num(err[["MSE"]]), sub("MSE")),
        kpi_tile("MAPE", paste0(fmt_num(err[["MAPE"]]), " %"), sub("MAPE")))
  })

  output$forecast_plot <- renderPlot({
    r <- result()
    n <- length(r$d); h <- length(r$future)
    cols <- app_colors
    yr <- range(c(r$d, r$fitted, r$future, r$future_lower, r$future_upper, r$test$forecast),
                na.rm = TRUE)
    yr <- yr + c(-0.10, 0.22) * diff(yr)               # room for the legend and area labels
    plot_setup()
    plot_frame(c(1, n + h), yr, xlab = "Period", ylab = "Demand", xaxt = "n")
    axis_periods(r$d, h)
    big <- diff(yr)
    rect(n + 0.5, yr[1] - big, n + h + 1, yr[2] + big, col = "#f4f8fd", border = NA)   # future
    if (!is.null(r$test$forecast)) {
      rect(min(r$test$periods) - 0.5, yr[1] - big, n + 0.5, yr[2] + big,
           col = "#edf8f3", border = NA)                                                # hold-out
    }
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
    labs <- c("Actual demand", "One-step forecast", "Future forecast")
    lc <- c(cols$ink_2, cols$series[1], cols$series[2]); lt <- c(1, 2, 1)
    if (!is.null(r$test$forecast)) {
      lines(r$test$periods, r$test$forecast, col = cols$series[3], lwd = 2.5)
      points(r$test$periods, r$test$forecast, pch = 21, bg = cols$series[3], col = "white", cex = 1.3)
      text(min(r$test$periods) - 0.4, yr[1], "Test", adj = c(0, 0), cex = 0.8, col = cols$ink_2)
      labs <- c(labs, "Hold-out forecast"); lc <- c(lc, cols$series[3]); lt <- c(lt, 1)
    }
    text(n + 0.6, yr[1], "Forecast", adj = c(0, 0), cex = 0.8, col = cols$ink_2)
    plot_legend(labs, lc, lty = lt, ncol = if (length(labs) > 3) 2 else NULL)
  }, bg = "white", res = 96)

  output$test_note <- renderText({
    t <- result()$test
    if (is.null(t)) return("")
    if (!is.null(t[["message"]])) return(t[["message"]])
    paste0("Hold-out test: the method was fitted on the first ", min(t$periods) - 1,
           " periods only and forecast the last ", t$k, " (green area).")
  })

  output$error_table <- renderTable({
    r <- result()
    err <- forecast_errors(r$d, r$fitted, r$periods)
    out <- data.frame(Measure = c("MAD", "MSE", "MAPE (%)"),
                      "In-sample" = round(unname(err), 3),
                      "Periods" = paste0(period_label(r$d, min(r$periods)), " - ",
                                         period_label(r$d, max(r$periods))),
                      check.names = FALSE)
    if (!is.null(r$test$errors)) out[[paste0("Test (last ", r$test$k, ")")]] <- round(unname(r$test$errors), 3)
    out
  })

  output$param_table <- renderTable({
    r <- result()
    r$params
  }, digits = 4)

  output$test_title <- renderText({
    t <- result()$test
    if (is.null(t$errors)) "" else paste0("Hold-out test: last ", t$k, " periods")
  })

  output$test_table <- renderTable({
    t <- result()$test
    if (is.null(t$errors)) return(NULL)
    forecast_test_table(result())
  }, digits = 3)

  output$search_title <- renderText({
    if (is.null(result()$search)) "" else "Parameter search (best 10)"
  })

  output$search_table <- renderTable({
    s <- result()$search
    if (is.null(s)) return(NULL)
    head(s, 10)
  }, digits = 3)

  output$future_table <- renderTable(forecast_future_table(result()))

  output$period_table <- renderTable(forecast_period_table(result()), na = "")

  output$download <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", result()$method), ".xlsx"),
    content = function(file) {
      r <- result()
      err <- forecast_errors(r$d, r$fitted, r$periods)
      summary <- data.frame(Item = c("Method", "MAD (in-sample)", "MSE (in-sample)", "MAPE % (in-sample)",
                                     "Next-period forecast"),
                            Value = c(r$method, round(unname(err), 4), round(r$future[1], 4)))
      if (!is.null(r$test$errors)) {
        summary <- rbind(summary, data.frame(
          Item = paste0(c("MAD", "MSE", "MAPE %"), " (test, last ", r$test$k, ")"),
          Value = round(unname(r$test$errors), 4)))
      }
      sheets <- list(Summary = summary,
                     Parameters = if (is.null(r$params)) data.frame() else r$params,
                     Future = forecast_future_table(r),
                     Calculations = forecast_period_table(r))
      if (!is.null(r$test$errors)) sheets$`Hold-out test` <- forecast_test_table(r)
      if (!is.null(r$search)) sheets$`Parameter search` <- r$search
      writexl::write_xlsx(sheets, file)
    })
}

# Period-by-period table (screen and download).
forecast_period_table <- function(r) {
  n <- length(r$d)
  out <- data.frame(Period = 1:n, check.names = FALSE)
  if (!is.null(attr(r$d, "labels"))) out$Name <- attr(r$d, "labels")
  out <- cbind(out, data.frame("Demand D_t" = as.numeric(r$d),
                               "Forecast F_t" = round(r$fitted, 3),
                               "Error e_t = F_t - D_t" = round(r$fitted - as.numeric(r$d), 3),
                               check.names = FALSE))
  if (!is.null(r$detail)) out <- cbind(out, round(r$detail, 4))
  out
}

forecast_future_table <- function(r) {
  n <- length(r$d); h <- length(r$future)
  out <- data.frame(Period = period_label(r$d, (n + 1):(n + h)), Forecast = round(r$future, 3))
  if (!is.null(r$future_lower)) {
    out[["Lower 95%"]] <- round(r$future_lower, 3)
    out[["Upper 95%"]] <- round(r$future_upper, 3)
  }
  out
}

forecast_test_table <- function(r) {
  t <- r$test
  data.frame(Period = period_label(r$d, t$periods), Actual = t$actual,
             "Hold-out forecast" = t$forecast, Error = t$forecast - t$actual,
             check.names = FALSE)
}

# A complete page for a forecasting method:
# data input + the module's own controls on the left, results on the right.
forecast_page <- function(ns, explanation, ...) {
  sidebarLayout(
    sidebarPanel(
      formula_box(explanation),
      series_input_ui(ns("data")),
      ...,
      horizon_ui(ns),
      holdout_ui(ns)
    ),
    mainPanel(forecast_results_ui(ns))
  )
}
