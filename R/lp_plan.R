# =====================================================================
# Topic: Aggregate planning with linear programming
# Nahmias, Chapter 3
#
# Decision variables for every period t:
#   W_t workers, H_t hired, F_t fired, I_t inventory,
#   O_t overtime units, U_t idle units, S_t subcontracted units
#
# Minimize  sum_t [ c_H H_t + c_F F_t + c_I I_t + c_R (K n_t W_t - U_t)
#                   + c_O O_t + c_U U_t + c_S S_t ]
# subject to
#   W_t = W_{t-1} + H_t - F_t                          (workforce balance)
#   I_t = I_{t-1} + K n_t W_t + O_t - U_t + S_t - D'_t   (inventory balance)
#   U_t <= K n_t W_t                                    (idle <= capacity)
#   O_t <= (overtime limit %) x K n_t W_t               (if a limit is given)
#   S_t <= subcontracting limit                          (if a limit is given)
#   O_t = 0 / U_t = 0 / S_t = 0  when that option is switched off
#   all variables >= 0, I_0 = 0 (I_0 is already inside D'_1)
# Solved with the lpSolve package.
# Whole workers: W_t is rounded up, then the LP is solved again with the
# workforce fixed at those values (the other variables are re-optimized).
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
lp_plan_solve <- function(ap, allow_ot, ot_limit, allow_idle, allow_sub, sub_limit,
                          fixed_W = NULL) {
  Tn <- length(ap$Dnet)
  k <- ap$costs
  pw <- ap$K * ap$days                    # units per worker in each period
  blocks <- c("W", "H", "F", "I", "O", "U", "S")
  col <- function(v, t) (match(v, blocks) - 1) * Tn + t
  nv <- length(blocks) * Tn

  obj <- numeric(nv)
  for (t in 1:Tn) {
    obj[col("H", t)] <- k$hire
    obj[col("F", t)] <- k$fire
    obj[col("I", t)] <- k$hold
    obj[col("W", t)] <- k$reg * pw[t]
    obj[col("O", t)] <- k$ot
    obj[col("U", t)] <- k$idle - k$reg
    obj[col("S", t)] <- k$sub
  }

  A <- list(); dir <- character(0); rhs <- numeric(0)
  add <- function(coefs, d, r) {
    row <- numeric(nv); row[as.integer(names(coefs))] <- coefs
    A[[length(A) + 1]] <<- row; dir <<- c(dir, d); rhs <<- c(rhs, r)
  }
  term <- function(...) {               # term("W", t, 1, "H", t, -1, ...)
    a <- list(...); out <- c()
    for (i in seq(1, length(a), by = 3)) {
      out[as.character(col(a[[i]], a[[i + 1]]))] <- a[[i + 2]]
    }
    out
  }

  for (t in 1:Tn) {
    # workforce balance:  W_t - W_{t-1} - H_t + F_t = 0   (W_0 goes to the right side)
    co <- term("W", t, 1, "H", t, -1, "F", t, 1)
    if (t > 1) co <- c(co, term("W", t - 1, -1))
    add(co, "=", if (t == 1) ap$W0 else 0)
    # inventory balance:  I_t - I_{t-1} - K n_t W_t - O_t + U_t - S_t = -D'_t
    co <- term("I", t, 1, "W", t, -pw[t], "O", t, -1, "U", t, 1, "S", t, -1)
    if (t > 1) co <- c(co, term("I", t - 1, -1))
    add(co, "=", -ap$Dnet[t])
    # idle units cannot exceed regular capacity
    add(term("U", t, 1, "W", t, -pw[t]), "<=", 0)
    # options
    if (!allow_ot) add(term("O", t, 1), "=", 0)
    else if (!is.na(ot_limit) && ot_limit > 0) add(term("O", t, 1, "W", t, -ot_limit / 100 * pw[t]), "<=", 0)
    if (!allow_idle) add(term("U", t, 1), "=", 0)
    if (!allow_sub) add(term("S", t, 1), "=", 0)
    else if (!is.na(sub_limit) && sub_limit > 0) add(term("S", t, 1), "<=", sub_limit)
    if (!is.null(fixed_W)) add(term("W", t, 1), "=", fixed_W[t])
  }

  sol <- lpSolve::lp("min", obj, do.call(rbind, A), dir, rhs)
  if (sol$status != 0) return(NULL)
  x <- sol$solution
  get <- function(v) x[col(v, 1:Tn)]
  list(W = get("W"), O = get("O"), U = get("U"), S = get("S"),
       objective = sol$objval)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
lp_plan_ui <- function(id) {
  ns <- NS(id)
  ap_page(ns,
    c("\\(\\min \\sum_t \\big(c_H H_t + c_F F_t + c_I I_t\\)", "\\(\\qquad + c_R R_t + c_O O_t + c_U U_t + c_S S_t\\big)\\)", "Choose which options the model may use."),
    checkboxInput(ns("allow_ot"), "Allow overtime", value = FALSE),
    conditionalPanel("input.allow_ot", ns = ns,
      numericInput(ns("ot_limit"), "Overtime limit (% of regular capacity, 0 = no limit)",
                   value = 0, min = 0)),
    checkboxInput(ns("allow_idle"), "Allow idle time", value = TRUE),
    checkboxInput(ns("allow_sub"), "Allow subcontracting", value = FALSE),
    conditionalPanel("input.allow_sub", ns = ns,
      numericInput(ns("sub_limit"), "Subcontracting limit per period (units, 0 = no limit)",
                   value = 0, min = 0)),
    helpText("If an option has no cost entered (cost 0), the LP may use it for free."),
    example = "lp"
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
lp_plan_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {
    ap_example_observe(input, session, "lp")
    # every example also sets the LP options (so Compare Plans uses the right ones)
    example <- ap_example_signal(session)
    observeEvent(example(), {
      o <- example_aggregate_scenarios()[[example()$key]]$lp
      req(o)
      updateCheckboxInput(session, "allow_ot", value = o$allow_ot)
      updateNumericInput(session, "ot_limit", value = o$ot_limit)
      updateCheckboxInput(session, "allow_idle", value = o$allow_idle)
      updateCheckboxInput(session, "allow_sub", value = o$allow_sub)
      updateNumericInput(session, "sub_limit", value = o$sub_limit)
    }, ignoreInit = TRUE)
    num <- function(x) if (is.null(x) || is.na(x)) 0 else x

    result <- reactive({
      a <- ap_require(ap)
      solve <- function(fixed = NULL) {
        lp_plan_solve(a, isTRUE(input$allow_ot), num(input$ot_limit),
                      isTRUE(input$allow_idle), isTRUE(input$allow_sub),
                      num(input$sub_limit), fixed)
      }
      s <- solve()
      validate(need(!is.null(s), "The LP has no feasible solution with these options."))
      original <- ap_build_plan(a, s$W, s$O, s$U, s$S)
      r <- ap_with_rounding(original, function() {
        s2 <- solve(ceiling(s$W - 1e-6))
        validate(need(!is.null(s2), "No feasible plan with the rounded workforce."))
        ap_build_plan(a, ceiling(s$W - 1e-6), s2$O, s2$U, s2$S)
      })
      r$title <- "Linear programming plan"
      k <- a$costs
      free <- c("overtime" = isTRUE(input$allow_ot) && k$ot == 0,
                "subcontracting" = isTRUE(input$allow_sub) && k$sub == 0)
      if (any(free)) {
        r$note <- paste0("Warning: the cost of ", paste(names(free)[free], collapse = " and "),
                         " is 0 in 'Data and Costs', so the LP can use it for free. ", r$note)
      }
      r
    })
    render_ap_results(output, ap, result)
    result      # returned so that the Disaggregation tab can use this plan
  })
}
