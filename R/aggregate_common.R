# =====================================================================
# Shared tools for all Aggregate Planning modules
# Nahmias, Chapter 3
#
# Notation:
#   D_t   demand forecast of period t          n_t  working days
#   K     units produced by one worker in one day
#   K n_t units one worker produces in period t
#   W_t   workers, H_t hired, F_t fired
#   I_0   starting inventory, I_T required ending inventory
#   Net demand:  D'_1 = D_1 - I_0,  D'_T = D_T + I_T,  D'_t = D_t otherwise
#   Inventory:   I_t = cumulative production - cumulative net demand
#                (inventory above the required ending inventory)
#
# Costs of a plan:
#   Hiring         c_H x sum(H_t)
#   Firing         c_F x sum(F_t)
#   Holding        c_I x sum(I_t)
#   Regular time   c_R x units made on regular time
#   Overtime       c_O x units made on overtime
#   Idle time      c_U x unused regular-time capacity (units)
#   Subcontracting c_S x units bought from subcontractors
#
# This file contains:
#   1) the "Data and Costs" tab (aggregate_data_ui / aggregate_data_server);
#      its server returns one reactive that every other module uses
#   2) building a plan table and computing its costs
#   3) the common outputs (cost table, cumulative plot, plan tables)
# =====================================================================


# ---------------------------------------------------------------------
# 1) DATA AND COSTS TAB
# ---------------------------------------------------------------------

# First column whose name contains one of the patterns.
guess_column <- function(cols, patterns, exclude = character(0)) {
  cand <- setdiff(cols, exclude)
  hit <- cand[grepl(paste(patterns, collapse = "|"), tolower(cand))]
  if (length(hit) > 0) hit[1] else if (length(cand) > 0) cand[1] else NULL
}

aggregate_data_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      helpText("Enter the data once here; every Aggregate Planning tab uses it."),
      h4("1. Demand data"),
      fileInput(ns("file"), "Upload data (.xlsx, .xls or .csv)",
                accept = c(".xlsx", ".xls", ".csv")),
      div(class = "data-actions", example_button(ns),
          downloadLink(ns("template"), "Empty Excel template", class = "template-link")),
      selectInput(ns("dataset"), "Or choose from the data library", choices = c("(none loaded yet)" = "")),
      selectInput(ns("name_col"), "Period names column", choices = NULL),
      selectInput(ns("demand_col"), "Demand forecast column", choices = NULL),
      selectInput(ns("days_col"), "Working days column", choices = NULL),
      textInput(ns("names"), "Period names (comma separated, you can edit them)"),

      h4("2. Costs"),
      numericInput(ns("c_hire"), "Hiring cost per worker (c_H)", value = 500, min = 0),
      numericInput(ns("c_fire"), "Firing cost per worker (c_F)", value = 1000, min = 0),
      numericInput(ns("c_hold"), "Holding cost per unit per period (c_I)", value = 80, min = 0),
      numericInput(ns("c_reg"), "Regular time cost per unit (c_R)", value = 0, min = 0),
      numericInput(ns("c_ot"), "Overtime cost per unit (c_O)", value = 0, min = 0),
      numericInput(ns("c_idle"), "Idle time cost per unit (c_U)", value = 0, min = 0),
      numericInput(ns("c_sub"), "Subcontracting cost per unit (c_S)", value = 0, min = 0),

      h4("3. Plant data"),
      numericInput(ns("W0"), "Starting workforce W_0", value = 300, min = 0),
      numericInput(ns("I0"), "Starting inventory I_0", value = 500, min = 0),
      numericInput(ns("IT"), "Required ending inventory I_T", value = 600, min = 0),

      h4("4. Productivity K (units per worker per day)"),
      radioButtons(ns("k_mode"), NULL,
                   choices = c("Enter K directly" = "direct",
                               "Compute K from past data" = "past")),
      conditionalPanel("input.k_mode == 'direct'", ns = ns,
        numericInput(ns("K"), "K", value = 0.14653, min = 0, step = 0.01)
      ),
      conditionalPanel("input.k_mode == 'past'", ns = ns,
        numericInput(ns("past_units"), "Units produced", value = 245, min = 0),
        numericInput(ns("past_workers"), "Number of workers", value = 76, min = 1),
        numericInput(ns("past_days"), "Working days", value = 22, min = 1),
        helpText("K = units produced / (workers x working days)")
      )
    ),
    mainPanel(
      div(class = "result-title", "Data and costs"),
      uiOutput(ns("data_kpis")),
      plotOutput(ns("demand_plot"), height = "300px"),
      div(class = "table-head", h4("Data used by the Aggregate Planning tabs"),
          downloadButton(ns("download"), "Download Excel", class = "btn-sm btn-outline-secondary")),
      tableOutput(ns("data_table"))
    )
  )
}

aggregate_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    num <- function(x) if (is.null(x) || is.na(x)) 0 else x

    data_rv <- reactiveVal(NULL)
    lib <- data_library(session)

    # a file uploaded here goes into the library (all its sheets) and is selected
    observeEvent(input$file, {
      res <- tryCatch(library_add_files(session, input$file), error = function(e) e)
      if (inherits(res, "error") || length(res$added) == 0) {
        showNotification(if (inherits(res, "error")) conditionMessage(res) else "The file could not be read.",
                         type = "error")
        return()
      }
      data_rv(lib()[[res$added[1]]])
      updateSelectInput(session, "dataset", choices = c("(choose a dataset)" = "", names(lib())),
                        selected = res$added[1])
    })
    observeEvent(input$example, {
      data_rv(example_aggregate_data())
      updateRadioButtons(session, "k_mode", selected = "past")
      updateSelectInput(session, "dataset", selected = "")
    })
    observeEvent(lib(), {
      nms <- names(lib())
      sel <- if (isTruthy(input$dataset) && input$dataset %in% nms) input$dataset else ""
      updateSelectInput(session, "dataset",
                        choices = c(if (length(nms)) c("(choose a dataset)" = "") else c("(none loaded yet)" = ""), nms),
                        selected = sel)
    })
    observeEvent(input$dataset, {
      req(input$dataset, input$dataset %in% names(lib()))
      data_rv(lib()[[input$dataset]])
    })
    # "Send to a method" from the Data Library
    send <- library_send(session)
    observeEvent(send(), {
      s <- send()
      req(s$target == "aggregate_data", s$name %in% names(lib()))
      updateSelectInput(session, "dataset", selected = s$name)
      data_rv(NULL); data_rv(lib()[[s$name]])
    }, ignoreInit = TRUE)
    data <- reactive({ req(data_rv()); data_rv() })

    output$template <- downloadHandler(
      filename = function() "aggregate_planning_template.xlsx",
      content = function(file) writexl::write_xlsx(list(
        Data = data.frame(Period = c("January", "February", "March"),
                          Demand = c(NA_real_, NA_real_, NA_real_),
                          "Working days" = c(NA_real_, NA_real_, NA_real_), check.names = FALSE),
        Instructions = data.frame(Instructions = c(
          "One row per period, in time order.",
          "Period: name of the period (month, week, ...).",
          "Demand: demand forecast of the period (aggregate units).",
          "Working days: number of working days in the period."))), file))

    # Guess the columns when a file is uploaded
    observeEvent(data(), {
      df <- data()
      nums <- numeric_columns(df)
      texts <- setdiff(names(df), nums)
      demand <- guess_column(nums, c("demand", "forecast", "talep"))
      days <- guess_column(nums, c("day", "gun"), exclude = demand)
      name <- if (length(texts) > 0) texts[1] else names(df)[1]
      updateSelectInput(session, "name_col", choices = names(df), selected = name)
      updateSelectInput(session, "demand_col", choices = nums, selected = demand)
      updateSelectInput(session, "days_col", choices = nums, selected = days)
    })

    # Fill the editable period names from the chosen column
    observeEvent(list(data(), input$name_col), {
      req(input$name_col %in% names(data()))
      updateTextInput(session, "names",
                      value = paste(as.character(data()[[input$name_col]]), collapse = ", "))
    })

    K <- reactive({
      if (input$k_mode == "direct") num(input$K)
      else num(input$past_units) / (max(num(input$past_workers), 1e-9) * max(num(input$past_days), 1e-9))
    })

    ap <- reactive({
      df <- data()
      req(input$demand_col %in% names(df), input$days_col %in% names(df))
      D <- as_number(df[[input$demand_col]])
      n <- as_number(df[[input$days_col]])
      keep <- !is.na(D) & !is.na(n)
      D <- D[keep]; n <- n[keep]
      Tn <- length(D)
      validate(need(Tn >= 2, "At least 2 periods are needed."),
               need(all(n > 0), "Working days must be positive."),
               need(K() > 0, "K must be positive."))
      nm <- trimws(strsplit(input$names, ",")[[1]])
      if (length(nm) != Tn) {
        nm <- if (input$name_col %in% names(df)) as.character(df[[input$name_col]])[keep]
              else paste("Period", 1:Tn)
      }
      I0 <- num(input$I0); IT <- num(input$IT)
      Dnet <- D
      Dnet[1] <- Dnet[1] - I0
      Dnet[Tn] <- Dnet[Tn] + IT
      list(periods = nm, D = D, days = n, Dnet = Dnet, K = K(),
           W0 = num(input$W0), I0 = I0, IT = IT,
           costs = list(hire = num(input$c_hire), fire = num(input$c_fire),
                        hold = num(input$c_hold), reg = num(input$c_reg),
                        ot = num(input$c_ot), idle = num(input$c_idle),
                        sub = num(input$c_sub)))
    })

    output$data_kpis <- renderUI({
      a <- tryCatch(ap(), error = function(e) NULL)
      div(class = "kpi-row",
          kpi_tile("K (units / worker / day)", fmt_num(K(), 5)),
          kpi_tile("Periods", if (is.null(a)) "-" else length(a$Dnet)),
          kpi_tile("Total net demand", if (is.null(a)) "-" else fmt_num(sum(a$Dnet), 0)),
          kpi_tile("Starting workforce", fmt_num(num(input$W0), 0)))
    })

    output$download <- downloadHandler(
      filename = function() "aggregate_planning_data.xlsx",
      content = function(file) {
        a <- ap()
        k <- a$costs
        writexl::write_xlsx(list(
          Data = data.frame(Period = a$periods, "Demand forecast" = a$D, "Working days" = a$days,
                            "Net demand D'" = a$Dnet, "Units per worker (K n_t)" = a$K * a$days,
                            "Cum. net demand" = cumsum(a$Dnet), check.names = FALSE),
          `Costs and plant` = data.frame(
            Item = c("Hiring cost per worker", "Firing cost per worker", "Holding cost per unit per period",
                     "Regular time cost per unit", "Overtime cost per unit", "Idle time cost per unit",
                     "Subcontracting cost per unit", "Starting workforce W0", "Starting inventory I0",
                     "Required ending inventory IT", "K (units per worker per day)"),
            Value = c(k$hire, k$fire, k$hold, k$reg, k$ot, k$idle, k$sub, a$W0, a$I0, a$IT, a$K))), file)
      })

    output$data_table <- renderTable({
      a <- ap()
      data.frame(Period = a$periods, "Demand forecast" = a$D,
                 "Working days" = a$days, "Net demand D'" = a$Dnet,
                 "Units per worker (K n_t)" = round(a$K * a$days, 3),
                 "Cum. net demand" = cumsum(a$Dnet), check.names = FALSE)
    })

    output$demand_plot <- renderPlot({
      a <- ap()
      plot_setup(mar = c(3, 5.2, 2.6, 1))
      yr <- c(min(0, a$Dnet), max(a$Dnet) * 1.08)
      bp <- barplot(a$Dnet, plot = FALSE)
      plot_frame(range(bp) + c(-0.7, 0.7), yr, ylab = "Units", xaxt = "n",
                 main = "Net demand by period")
      barplot(a$Dnet, names.arg = a$periods, col = app_colors$series[1], border = app_colors$plot_bg,
              add = TRUE, axes = FALSE, width = 1, space = 0.2, cex.names = 0.85,
              col.axis = app_colors$ink_2)
      text(bp, a$Dnet, fmt_num(a$Dnet, 0), pos = 3, cex = 0.78, col = app_colors$ink_2, xpd = TRUE)
    }, bg = app_colors$plot_bg, res = 96)

    ap
  })
}

# Stops a module with a friendly message when no data has been entered,
# or shows the problem found in the 'Data and Costs' tab.
ap_require <- function(ap) {
  res <- tryCatch(ap(), error = function(e) e)
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    validate(need(FALSE,
      if (nzchar(msg)) paste0("Problem in the 'Data and Costs' tab: ", msg)
      else "First upload the demand data in the 'Data and Costs' tab."))
  }
  res
}

# Total units a plan makes available in each period (own production + subcontracting).
ap_plan_production <- function(plan) {
  plan[["Regular production"]] + plan$Overtime + plan$Subcontract
}


# ---------------------------------------------------------------------
# 2) PLAN TABLE AND COSTS
# ---------------------------------------------------------------------

# Builds the plan table from the workforce W and (optionally) overtime O,
# idle units U and subcontracted units S of every period.
ap_build_plan <- function(ap, W, O = 0, U = 0, S = 0) {
  Tn <- length(ap$Dnet)
  full <- function(x) if (length(x) == 1) rep(x, Tn) else x
  W <- full(W); O <- full(O); U <- full(U); S <- full(S)
  # remove round-off from the LP solver (e.g. 2.8e-11 instead of 0, 300.0000001 instead of 300)
  tidy <- function(x) { x[abs(x) < 1e-6] <- 0; r <- round(x); x[abs(x - r) < 1e-6] <- r[abs(x - r) < 1e-6]; x }
  W <- tidy(W); O <- tidy(O); U <- tidy(U); S <- tidy(S)
  per_worker <- ap$K * ap$days
  regular <- per_worker * W - U
  produced <- regular + O + S
  dW <- diff(c(ap$W0, W))
  data.frame(Period = ap$periods,
             "Net demand" = ap$Dnet,
             "Units per worker" = per_worker,
             Workers = W,
             Hired = pmax(dW, 0),
             Fired = pmax(-dW, 0),
             "Regular production" = regular,
             Overtime = O,
             "Idle (units)" = U,
             Subcontract = S,
             "Cum. production" = cumsum(produced),
             "Cum. net demand" = cumsum(ap$Dnet),
             Inventory = (function(x) { x[abs(x) < 1e-6] <- 0; x })(cumsum(produced - ap$Dnet)),
             check.names = FALSE)
}

# Cost of each item and the total.
ap_costs <- function(plan, ap) {
  k <- ap$costs
  items <- c("Hiring"         = k$hire * sum(plan$Hired),
             "Firing"         = k$fire * sum(plan$Fired),
             "Holding"        = k$hold * sum(pmax(plan$Inventory, 0)),
             "Regular time"   = k$reg  * sum(plan[["Regular production"]]),
             "Overtime"       = k$ot   * sum(plan$Overtime),
             "Idle time"      = k$idle * sum(plan[["Idle (units)"]]),
             "Subcontracting" = k$sub  * sum(plan$Subcontract))
  c(items, "Total" = sum(items))
}

is_whole <- function(x) all(abs(x - round(x)) < 1e-6)


# ---------------------------------------------------------------------
# 3) COMMON OUTPUTS
# ---------------------------------------------------------------------
# Every module builds a result list:
#   title  text
#   plans  named list of 1 or 2 plan tables, e.g.
#          list("Original" = ..., "Whole workers" = ...)
#   note   extra text (optional)

ap_results_ui <- function(ns) {
  tagList(
    div(class = "result-title", textOutput(ns("headline"), inline = TRUE)),
    uiOutput(ns("kpis")),
    p(class = "result-note", textOutput(ns("note"), inline = TRUE)),
    plotOutput(ns("cum_plot"), height = "340px"),
    h4("Cost of each item"),
    tableOutput(ns("cost_table")),
    div(class = "table-head", h4(textOutput(ns("plan_title_1"))),
        downloadButton(ns("download"), "Download Excel", class = "btn-sm btn-outline-secondary")),
    tableOutput(ns("plan_table_1")),
    h4(textOutput(ns("plan_title_2"))),
    tableOutput(ns("plan_table_2"))
  )
}

# Hides the overtime / idle / subcontract columns when they are all zero.
ap_display_plan <- function(plan) {
  for (col in c("Overtime", "Idle (units)", "Subcontract")) {
    if (all(abs(plan[[col]]) < 1e-9)) plan[[col]] <- NULL
  }
  num <- vapply(plan, is.numeric, logical(1))
  plan[num] <- lapply(plan[num], function(x) round(x, 2))
  plan
}

# `extra_sheets`: optional function returning a named list of extra Excel sheets.
render_ap_results <- function(output, ap, result, extra_sheets = NULL) {
  output$headline <- renderText(result()$title)

  output$kpis <- renderUI({
    a <- tryCatch(ap(), error = function(e) NULL)
    if (is.null(a)) {
      return(empty_state("Drop the demand file here",
                         "or click to choose it - it is loaded into the 'Data and Costs' tab.",
                         aggregate = TRUE))
    }
    r <- tryCatch(result(), error = function(e) NULL)
    if (is.null(r)) return(NULL)                          # the message is shown below
    tiles <- lapply(names(r$plans), function(nm) {
      p <- r$plans[[nm]]
      kpi_tile(paste("Total cost -", nm), fmt_num(ap_costs(p, a)[["Total"]]),
               paste0("Workers ", fmt_num(min(p$Workers), 1), " - ", fmt_num(max(p$Workers), 1),
                      "  |  hired ", fmt_num(sum(p$Hired), 1),
                      ", fired ", fmt_num(sum(p$Fired), 1)))
    })
    div(class = "kpi-row", tiles)
  })

  output$note <- renderText({
    r <- result()
    short <- vapply(r$plans, function(p) any(p$Inventory < -1e-6), logical(1))
    paste(c(r$note,
            if (any(short)) "Warning: inventory becomes negative (shortage) in some periods."),
          collapse = " ")
  })

  output$cum_plot <- renderPlot({
    r <- result(); a <- ap()
    Tn <- length(a$Dnet)
    cols <- app_colors$series[1:2]
    ys <- c(0, cumsum(a$Dnet), unlist(lapply(r$plans, function(p) p[["Cum. production"]])))
    yr <- range(ys); yr[2] <- yr[2] + 0.05 * diff(yr)
    plot_setup(mar = c(4, 5.2, 2.6, 1))
    plot_frame(c(0, Tn), yr, xlab = "", ylab = "Cumulative units", xaxt = "n")
    axis(1, at = 0:Tn, labels = c("Start", a$periods), lwd = 0, lwd.ticks = 1)
    lines(0:Tn, c(0, cumsum(a$Dnet)), col = app_colors$ink_2, lwd = 2.5)
    points(0:Tn, c(0, cumsum(a$Dnet)), pch = 16, col = app_colors$ink_2, cex = 0.9)
    for (i in seq_along(r$plans)) {
      y <- c(0, r$plans[[i]][["Cum. production"]])
      lines(0:Tn, y, col = cols[i], lwd = 2, lty = if (i == 1) 1 else 2)
      points(0:Tn, y, pch = 21, bg = cols[i], col = app_colors$plot_bg, cex = 1.2, lwd = 1.2)
    }
    plot_legend(c("Cumulative net demand", paste("Cumulative production -", names(r$plans))),
                c(app_colors$ink_2, cols[seq_along(r$plans)]),
                lty = c(1, 1, 2)[seq_len(length(r$plans) + 1)], horiz = FALSE)
  }, bg = app_colors$plot_bg, res = 96)

  output$cost_table <- renderTable({
    r <- result(); a <- ap()
    costs <- lapply(r$plans, function(p) ap_costs(p, a))
    out <- data.frame(Item = names(costs[[1]]))
    for (nm in names(costs)) out[[nm]] <- round(unname(costs[[nm]]), 2)
    out
  })

  output$plan_title_1 <- renderText(paste("Plan -", names(result()$plans)[1]))
  output$plan_table_1 <- renderTable(ap_display_plan(result()$plans[[1]]))
  output$plan_title_2 <- renderText({
    p <- result()$plans
    if (length(p) >= 2) paste("Plan -", names(p)[2]) else ""
  })
  output$download <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", result()$title), ".xlsx"),
    content = function(file) {
      r <- result(); a <- ap()
      costs <- lapply(r$plans, function(p) ap_costs(p, a))
      cost_sheet <- data.frame(Item = names(costs[[1]]))
      for (nm in names(costs)) cost_sheet[[nm]] <- round(unname(costs[[nm]]), 2)
      plan_sheets <- lapply(r$plans, function(p) {
        num <- vapply(p, is.numeric, logical(1))
        p[num] <- lapply(p[num], function(x) round(x, 3))
        p
      })
      names(plan_sheets) <- substr(gsub("[^A-Za-z0-9 ]", "", names(plan_sheets)), 1, 31)
      sheets <- c(list(Costs = cost_sheet), plan_sheets)
      if (!is.null(extra_sheets)) sheets <- c(sheets, extra_sheets())
      writexl::write_xlsx(sheets, file)
    })

  output$plan_table_2 <- renderTable({
    p <- result()$plans
    if (length(p) >= 2) ap_display_plan(p[[2]]) else NULL
  })
}

# The original plan and, if workers are fractional, the whole-worker plan.
ap_with_rounding <- function(original, rounded_fn) {
  W <- original$Workers
  if (is_whole(W)) {
    list(plans = list("Original" = original),
         note = "The number of workers is already a whole number in every period.")
  } else {
    list(plans = list("Original (fractional workers)" = original,
                      "Whole workers (rounded up)" = rounded_fn()),
         note = "Workers were fractional, so a second plan with the workers rounded up is shown.")
  }
}

# A complete page: the module's own controls on the left, results on the right.
ap_page <- function(ns, explanation, ..., extra_main = NULL) {
  sidebarLayout(
    sidebarPanel(formula_box(explanation), ...,
                 helpText("Data, costs, K and plant data come from the 'Data and Costs' tab.")),
    mainPanel(ap_results_ui(ns), extra_main)
  )
}
