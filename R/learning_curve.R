# =====================================================================
# Topic: Learning curve
# Nahmias, Chapter 1
#
# Time to produce unit u:  T(u) = T_1 * u^(-b),   b = -log(L) / log(2)
# L = learning rate: every time the cumulative output doubles, the time
#     per unit is multiplied by L (e.g. L = 0.8 -> 80% learning curve).
# (Same formula as the original template module; only the screen was extended.)
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
learning_curve_b <- function(L) -log(L) / log(2)
learning_curve_time <- function(u, T1, L) T1 * u^(-learning_curve_b(L))


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
learning_curve_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      formula_box(c("\\(T(u) = T_1\\, u^{-b}\\)",
                    "\\(b = -\\dfrac{\\log L}{\\log 2}\\)")),
      sliderInput(ns("L"), "Learning rate L", 0.6, 0.99, 0.8),
      numericInput(ns("T1"), "Time for the first unit T_1", value = 100, min = 0.001),
      numericInput(ns("units"), "Number of units", value = 50, min = 2, step = 1)
    ),
    mainPanel(
      div(class = "result-title", textOutput(ns("headline"), inline = TRUE)),
      uiOutput(ns("kpis")),
      plotOutput(ns("plot"), height = "340px"),
      div(class = "table-head", h4("Time per unit when the output doubles"),
          downloadButton(ns("download"), "Download Excel", class = "btn-sm btn-outline-secondary")),
      tableOutput(ns("doubling_table"))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
learning_curve_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    curve <- reactive({
      req(input$L, input$T1, input$units)
      validate(need(input$T1 > 0, "T_1 must be positive."),
               need(input$units >= 2, "At least 2 units."))
      u <- 1:as.integer(input$units)
      list(u = u, time = learning_curve_time(u, input$T1, input$L),
           b = learning_curve_b(input$L))
    })

    output$headline <- renderText(paste0(round(100 * input$L), "% learning curve"))

    output$kpis <- renderUI({
      cv <- curve(); n <- length(cv$u)
      div(class = "kpi-row",
          kpi_tile("Exponent b", fmt_num(cv$b, 4)),
          kpi_tile(paste("Time of unit", n), fmt_num(cv$time[n])),
          kpi_tile(paste("Total time, units 1 -", n), fmt_num(sum(cv$time))),
          kpi_tile("Average time per unit", fmt_num(mean(cv$time))))
    })

    output$plot <- renderPlot({
      cv <- curve()
      yr <- c(0, max(cv$time) * 1.3)                    # room for the legend
      plot_setup()
      plot_frame(range(cv$u), yr, xlab = "Unit", ylab = "Time per unit")
      lines(cv$u, cv$time, col = app_colors$series[1], lwd = 2.5)
      dbl <- 2^(0:floor(log2(max(cv$u))))
      points(dbl, cv$time[dbl], pch = 21, bg = app_colors$series[2], col = "white", cex = 1.4)
      text(dbl, cv$time[dbl], fmt_num(cv$time[dbl], 1), pos = 3, cex = 0.75,
           col = app_colors$ink_2, xpd = TRUE)
      plot_legend(c("Time per unit", "Doubling points (1, 2, 4, 8, ...)"),
                  c(app_colors$series[1], app_colors$series[2]), pch = c(NA, 16), lty = c(1, NA))
    }, bg = "white", res = 96)

    output$download <- downloadHandler(
      filename = function() paste0("learning_curve_", round(100 * input$L), ".xlsx"),
      content = function(file) {
        cv <- curve()
        writexl::write_xlsx(list(
          Summary = data.frame(Item = c("Learning rate L", "Exponent b", "Time for the first unit T1",
                                        "Number of units", "Total time", "Average time per unit"),
                               Value = c(input$L, cv$b, input$T1, length(cv$u), sum(cv$time), mean(cv$time))),
          `Time per unit` = data.frame(Unit = cv$u, "Time per unit" = cv$time,
                                       "Cumulative time" = cumsum(cv$time), check.names = FALSE)), file)
      })

    output$doubling_table <- renderTable({
      cv <- curve()
      dbl <- 2^(0:floor(log2(max(cv$u))))
      data.frame(Unit = as.integer(dbl), "Time per unit" = cv$time[dbl],
                 "Ratio to previous" = c(NA, cv$time[dbl][-1] / head(cv$time[dbl], -1)),
                 check.names = FALSE)
    }, digits = 3, na = "")
  })
}
