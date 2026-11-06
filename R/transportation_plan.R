# =====================================================================
# Topic: Aggregate planning with the transportation method
# Nahmias, Chapter 3
#
# Sources (rows):  starting inventory, and for every period t
#                  regular time RT_t, overtime OT_t, subcontracting SC_t
# Destinations:    the demand of every period (the last period also needs
#                  the required ending inventory) + "Unused capacity"
# Capacities:      RT_t = K n_t W     (W = workforce, constant)
#                  OT_t = overtime % x RT_t
#                  SC_t = subcontracting capacity per period
# Cell cost:       cost of the source + c_I x (periods held)
#                  = c_R / c_O / c_S + c_I (j - t);  j < t is not allowed
#                  (no backorders). Unused RT capacity costs c_U per unit.
# Solutions:
#   North-west corner: start at the top-left cell, give as much as
#     possible, then move right (demand met) or down (capacity used up).
#   Cheapest cell first (least-cost method): repeatedly give as much as
#     possible to the cheapest allowed cell. The result is checked with an
#     exact transportation LP (lpSolve::lp.transport).
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------

# Builds the transportation table: sources, demands and cell costs.
transportation_setup <- function(ap, W, ot_pct, sub_cap) {
  Tn <- length(ap$D); k <- ap$costs
  demand <- ap$D; demand[Tn] <- demand[Tn] + ap$IT
  src <- data.frame(label = character(0), type = character(0), period = integer(0),
                    capacity = numeric(0), cost = numeric(0), unused_cost = numeric(0))
  add <- function(label, type, t, cap, cost, unused) {
    src[nrow(src) + 1, ] <<- list(label, type, t, cap, cost, unused)
  }
  if (ap$I0 > 0) add("Starting inventory", "INV", 1L, ap$I0, 0, 0)
  for (t in 1:Tn) {
    rt <- ap$K * ap$days[t] * W
    add(paste("RT", ap$periods[t]), "RT", t, rt, k$reg, k$idle)
    if (ot_pct > 0) add(paste("OT", ap$periods[t]), "OT", t, ot_pct / 100 * rt, k$ot, 0)
    if (sub_cap > 0) add(paste("SC", ap$periods[t]), "SC", t, sub_cap, k$sub, 0)
  }
  cost <- matrix(NA_real_, nrow(src), Tn)
  for (i in seq_len(nrow(src))) {
    j <- src$period[i]:Tn
    cost[i, j] <- src$cost[i] + k$hold * (j - src$period[i])
  }
  list(src = src, demand = demand, cost = cost,
       unused = max(0, sum(src$capacity) - sum(demand)))
}

# Every period's demand must be covered by sources of that period or earlier.
transportation_feasible <- function(tp) {
  Tn <- length(tp$demand)
  cap_by_t <- vapply(1:Tn, function(t) sum(tp$src$capacity[tp$src$period == t]), numeric(1))
  all(cumsum(cap_by_t) >= cumsum(tp$demand) - 1e-6)
}

# North-west corner rule (the last column is "Unused capacity").
transportation_northwest <- function(tp) {
  supply <- tp$src$capacity
  dem <- c(tp$demand, tp$unused)
  X <- matrix(0, length(supply), length(dem))
  i <- 1; j <- 1
  while (i <= length(supply) && j <= length(dem)) {
    q <- min(supply[i], dem[j])
    X[i, j] <- q
    supply[i] <- supply[i] - q; dem[j] <- dem[j] - q
    if (supply[i] <= 1e-9) i <- i + 1 else j <- j + 1
  }
  X
}

# Cheapest cell first; leftover capacity goes to "Unused capacity".
transportation_least_cost <- function(tp) {
  supply <- tp$src$capacity
  dem <- tp$demand
  Tn <- length(dem)
  X <- matrix(0, length(supply), Tn + 1)
  cells <- which(!is.na(tp$cost), arr.ind = TRUE)
  cells <- cells[order(tp$cost[cells], cells[, "col"], cells[, "row"]), , drop = FALSE]
  for (k in seq_len(nrow(cells))) {
    i <- cells[k, "row"]; j <- cells[k, "col"]
    q <- min(supply[i], dem[j])
    if (q > 0) {
      X[i, j] <- X[i, j] + q
      supply[i] <- supply[i] - q; dem[j] <- dem[j] - q
    }
  }
  X[, Tn + 1] <- supply
  list(X = X, unmet = sum(dem))
}

# Exact optimum of the same transportation problem (for checking).
# One LP variable per allowed cell (cells with backorders are left out).
transportation_optimal <- function(tp) {
  C <- cbind(tp$cost, tp$src$unused_cost)
  cells <- which(!is.na(C), arr.ind = TRUE)
  nv <- nrow(cells)
  m <- nrow(C); n <- ncol(C)
  A <- matrix(0, m + n, nv)
  A[cbind(cells[, "row"], 1:nv)] <- 1            # each source ships its capacity
  A[cbind(m + cells[, "col"], 1:nv)] <- 1        # each destination receives its demand
  sol <- lpSolve::lp("min", C[cells], A, rep("=", m + n),
                     c(tp$src$capacity, tp$demand, tp$unused))
  if (sol$status != 0) return(NULL)
  X <- matrix(0, m, n)
  X[cells] <- sol$solution
  X
}

# Total cost of an allocation (cells used x their cost).
transportation_cost <- function(tp, X) {
  C <- cbind(tp$cost, tp$src$unused_cost)
  C[is.na(C)] <- 1e9
  sum(X * C)
}

# Turns an allocation into the usual plan table.
transportation_plan_table <- function(ap, tp, X, W) {
  Tn <- length(ap$D)
  used <- function(type) vapply(1:Tn, function(t) {
    rows <- which(tp$src$type == type & tp$src$period == t)
    if (length(rows) == 0) 0 else sum(X[rows, 1:Tn])
  }, numeric(1))
  rt_cap <- ap$K * ap$days * W
  ap_build_plan(ap, W, O = used("OT"), U = rt_cap - used("RT"), S = used("SC"))
}

# Allocation table for the screen.
transportation_show <- function(tp, X, ap) {
  Tn <- length(tp$demand)
  out <- as.data.frame(matrix("", nrow(X), Tn + 1), stringsAsFactors = FALSE)
  names(out) <- c(ap$periods, "Unused")
  for (i in seq_len(nrow(X))) for (j in 1:(Tn + 1)) {
    if (j <= Tn && is.na(tp$cost[i, j])) out[i, j] <- "x"
    else if (X[i, j] > 1e-9) out[i, j] <- format(round(X[i, j], 2), big.mark = ",")
  }
  cbind(Source = tp$src$label, out,
        Capacity = round(tp$src$capacity, 2))
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
transportation_plan_ui <- function(id) {
  ns <- NS(id)
  ap_page(ns,
    c("Sources: RT, OT and SC of every period; destinations: the demands.", "Cell cost \\(= c_{source} + c_I\\,(j - t), \\quad j \\ge t\\)"),
    numericInput(ns("W"), "Workforce W (constant, used for regular-time capacity)",
                 value = 300, min = 0, step = 1),
    helpText("Default: the level-plan workforce (the smallest constant workforce",
             "with no shortage on regular time). A smaller W can work with overtime",
             "or subcontracting."),
    numericInput(ns("ot_pct"), "Overtime capacity (% of regular-time capacity, 0 = none)",
                 value = 20, min = 0),
    numericInput(ns("sub_cap"), "Subcontracting capacity per period (units, 0 = none)",
                 value = 0, min = 0),
    extra_main = tagList(
      h4("Cell costs (x = not allowed: no backorders)"),
      tableOutput(ns("cost_matrix")),
      h4(textOutput(ns("nw_title"))),
      tableOutput(ns("nw_table")),
      h4(textOutput(ns("lc_title"))),
      tableOutput(ns("lc_table"))
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
transportation_plan_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {
    num <- function(x) if (is.null(x) || is.na(x)) 0 else x

    # Default workforce: the level-plan workforce, i.e. the smallest constant
    # workforce that meets every period's demand on regular time alone
    # (so the transportation problem is always feasible). It is recomputed
    # whenever the demand, working days or K change; the user can edit it.
    observeEvent(tryCatch(list(ap()$Dnet, ap()$days, ap()$K), error = function(e) NULL), {
      updateNumericInput(session, "W", value = ceiling(level_plan_workers(ap()) - 1e-9))
    })

    solved <- reactive({
      a <- ap_require(ap)
      W <- ceiling(num(input$W) - 1e-9)
      tp <- transportation_setup(a, W, num(input$ot_pct), num(input$sub_cap))
      validate(need(transportation_feasible(tp),
                    "Not enough capacity: some period's demand cannot be met without backorders. Increase W, overtime or subcontracting."))
      nw <- transportation_northwest(tp)
      lc <- transportation_least_cost(tp)
      opt <- transportation_optimal(tp)
      best <- lc$X
      note <- NULL
      if (lc$unmet > 1e-6 && !is.null(opt)) {
        best <- opt
        note <- "The cheapest-cell-first rule left some demand unmet, so the exact LP optimum is shown instead."
      } else if (!is.null(opt) && transportation_cost(tp, lc$X) > transportation_cost(tp, opt) + 1e-6) {
        note <- paste0("Note: the exact optimum (", round(transportation_cost(tp, opt), 2),
                       ") is cheaper than the cheapest-cell-first solution.")
      } else {
        note <- "The cheapest-cell-first solution is optimal (checked with an exact LP)."
      }
      list(a = a, W = W, tp = tp, nw = nw, best = best, note = note)
    })

    result <- reactive({
      s <- solved()
      list(title = paste0("Transportation method, W = ", s$W),
           plans = list("North-west corner" = transportation_plan_table(s$a, s$tp, s$nw, s$W),
                        "Cheapest cell first (optimal)" = transportation_plan_table(s$a, s$tp, s$best, s$W)),
           note = s$note)
    })

    output$cost_matrix <- renderTable({
      s <- solved()
      C <- s$tp$cost
      out <- as.data.frame(ifelse(is.na(C), "x", format(round(C, 2))), stringsAsFactors = FALSE)
      names(out) <- s$a$periods
      cbind(Source = s$tp$src$label, out, Unused = s$tp$src$unused_cost)
    })
    output$nw_title <- renderText({
      s <- solved()
      paste0("North-west corner solution (transportation cost ",
             format(round(transportation_cost(s$tp, s$nw), 2), big.mark = ","), ")")
    })
    output$nw_table <- renderTable({ s <- solved(); transportation_show(s$tp, s$nw, s$a) })
    output$lc_title <- renderText({
      s <- solved()
      paste0("Cheapest cell first solution (transportation cost ",
             format(round(transportation_cost(s$tp, s$best), 2), big.mark = ","), ")")
    })
    output$lc_table <- renderTable({ s <- solved(); transportation_show(s$tp, s$best, s$a) })

    render_ap_results(output, ap, result)
    result      # returned so that the Compare Plans tab can use this plan
  })
}
