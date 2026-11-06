# =====================================================================
# Topic: Disaggregating the aggregate plan - square-root rule
# Nahmias, Chapter 3 (Bitran and Hax)
#
# The aggregate production X of a period is split among product families
# j = 1..J with setup cost K_j and demand lambda_j:
#   minimize  sum_j K_j lambda_j / Y_j      (setup cost of the runs)
#   s.t.      sum_j Y_j = X,   a_j <= Y_j <= b_j
# Without bounds the solution is the square-root rule:
#   Y_j = X * sqrt(K_j lambda_j) / sum_i sqrt(K_i lambda_i)
# With bounds: Y_j = min(b_j, max(a_j, c * sqrt(K_j lambda_j))), where the
# constant c is chosen so that the Y_j add up to X (found by bisection).
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
disaggregation_split <- function(X, setup, demand, lower, upper) {
  r <- sqrt(setup * demand)
  y_of <- function(c) pmin(upper, pmax(lower, c * r))
  if (X <= sum(lower)) return(lower)
  if (X >= sum(upper)) return(upper)
  lo <- 0; hi <- 1
  while (sum(y_of(hi)) < X) hi <- hi * 2
  for (i in 1:200) {
    mid <- (lo + hi) / 2
    if (sum(y_of(mid)) < X) lo <- mid else hi <- mid
  }
  y_of(hi)
}

# Reads the family table typed in the text box.
parse_families <- function(text) {
  df <- utils::read.csv(text = text, strip.white = TRUE, check.names = FALSE)
  validate(need(ncol(df) >= 3, "Give at least: Family, Setup cost, Demand."))
  out <- data.frame(Family = as.character(df[[1]]),
                    Setup = as_number(df[[2]]),
                    Demand = as_number(df[[3]]),
                    Lower = if (ncol(df) >= 4) as_number(df[[4]]) else 0,
                    Upper = if (ncol(df) >= 5) as_number(df[[5]]) else Inf)
  out$Lower[is.na(out$Lower)] <- 0
  out$Upper[is.na(out$Upper)] <- Inf
  validate(need(all(!is.na(out$Setup) & !is.na(out$Demand)),
                "Setup cost and demand must be numbers."),
           need(all(out$Setup >= 0 & out$Demand >= 0), "Values cannot be negative."),
           need(all(out$Lower <= out$Upper), "A lower bound is larger than its upper bound."))
  out
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
disaggregation_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      helpText("Y_j = X * sqrt(K_j lambda_j) / sum sqrt(K_i lambda_i), kept within the bounds a_j <= Y_j <= b_j."),
      textAreaInput(ns("families"),
                    "Product families (one per line: Family, Setup cost, Demand, Lower bound, Upper bound)",
                    value = paste("Family, Setup cost, Demand, Lower bound, Upper bound",
                                  "A, 1200, 400, 0, 5000",
                                  "B, 800, 250, 0, 5000",
                                  "C, 2000, 600, 100, 5000", sep = "\n"),
                    rows = 6),
      radioButtons(ns("source"), "Aggregate production X of each period",
                   choices = c("Type the values" = "manual",
                               "Use the net demand from 'Data and Costs'" = "data")),
      conditionalPanel("input.source == 'manual'", ns = ns,
        textInput(ns("X"), "X values (comma separated, one per period)",
                  value = "1000, 1200, 900"))
    ),
    mainPanel(
      h4("Square-root weights"),
      tableOutput(ns("weight_table")),
      plotOutput(ns("split_plot"), height = "320px"),
      h4("Production of each family by period"),
      tableOutput(ns("split_table"))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
disaggregation_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {
    families <- reactive(parse_families(input$families))

    X <- reactive({
      if (input$source == "data") {
        a <- ap_require(ap)
        setNames(pmax(a$Dnet, 0), a$periods)
      } else {
        x <- as_number(trimws(strsplit(input$X, ",")[[1]]))
        validate(need(length(x) >= 1 && all(!is.na(x) & x >= 0),
                      "Type the X values as non-negative numbers separated by commas."))
        setNames(x, paste("Period", seq_along(x)))
      }
    })

    split <- reactive({
      f <- families(); x <- X()
      Y <- t(vapply(x, function(v) disaggregation_split(v, f$Setup, f$Demand, f$Lower, f$Upper),
                    numeric(nrow(f))))
      if (nrow(f) == 1) Y <- matrix(Y, ncol = 1)
      colnames(Y) <- f$Family
      Y
    })

    output$weight_table <- renderTable({
      f <- families()
      r <- sqrt(f$Setup * f$Demand)
      data.frame(Family = f$Family, "Setup cost K_j" = f$Setup, "Demand lambda_j" = f$Demand,
                 "sqrt(K_j lambda_j)" = round(r, 3), "Share (no bounds)" = round(r / sum(r), 4),
                 "Lower a_j" = f$Lower, "Upper b_j" = f$Upper, check.names = FALSE)
    })

    output$split_table <- renderTable({
      Y <- split(); x <- X(); f <- families()
      out <- data.frame(Period = names(x), "Aggregate X" = x, round(Y, 2), check.names = FALSE)
      out$Check <- ifelse(abs(rowSums(Y) - x) < 1e-6, "sum = X",
                          ifelse(x < sum(f$Lower), "X below sum of lower bounds",
                                 "X above sum of upper bounds"))
      out
    })

    output$split_plot <- renderPlot({
      Y <- split()
      barplot(t(Y), names.arg = names(X()), col = hcl.colors(ncol(Y), "Blues 3"),
              border = NA, legend.text = colnames(Y), ylab = "Units",
              main = "Aggregate production split among families",
              args.legend = list(bty = "n", x = "topright"))
    })
  })
}
