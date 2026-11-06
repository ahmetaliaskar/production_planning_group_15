# =====================================================================
# Topic: Mixed plan                                     (heuristic plan)
# Nahmias, Chapter 3
#
# The user chooses the periods in which the workforce may change.
# Between two change points the workforce is constant (a small level plan):
# for a block of periods a ... b
#   W = max over t in a..b of
#       (D'_a + ... + D'_t - I_{a-1}) / (K n_a + ... + K n_t)
# where I_{a-1} is the inventory left from the previous block.
# Period 1 is always a change point (the plan starts from W_0).
# Whole workers: each block's W is rounded up and the next blocks are
# recomputed with the inventory that results.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
mixed_plan_workers <- function(ap, change_periods, round_up = FALSE) {
  Tn <- length(ap$Dnet)
  starts <- sort(unique(c(1, change_periods[change_periods >= 1 & change_periods <= Tn])))
  ends <- c(starts[-1] - 1, Tn)
  per_worker <- ap$K * ap$days
  W <- numeric(Tn)
  inv <- 0
  for (b in seq_along(starts)) {
    idx <- starts[b]:ends[b]
    need <- (cumsum(ap$Dnet[idx]) - inv) / cumsum(per_worker[idx])
    w <- max(0, need)
    if (round_up) w <- ceiling(w - 1e-9)
    W[idx] <- w
    inv <- inv + sum(per_worker[idx] * w - ap$Dnet[idx])
  }
  W
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
mixed_plan_ui <- function(id) {
  ns <- NS(id)
  ap_page(ns,
    "The workforce stays constant between the periods you choose and changes only in those periods.",
    checkboxGroupInput(ns("changes"),
                       "Periods in which the workforce changes (period 1 always changes)",
                       choices = NULL)
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
mixed_plan_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {

    # List the periods as check boxes when the data changes
    observeEvent(tryCatch(ap()$periods, error = function(e) NULL), {
      p <- ap()$periods
      Tn <- length(p)
      updateCheckboxGroupInput(session, "changes",
                               choices = setNames(as.character(1:Tn), p),
                               selected = as.character(unique(c(1, ceiling(Tn / 2) + 1))))
    })

    result <- reactive({
      a <- ap_require(ap)
      ch <- as.integer(input$changes)
      W <- mixed_plan_workers(a, ch)
      r <- ap_with_rounding(ap_build_plan(a, W),
                            function() ap_build_plan(a, mixed_plan_workers(a, ch, round_up = TRUE)))
      starts <- sort(unique(c(1, ch)))
      r$title <- paste0("Mixed plan, workforce changes in: ",
                        paste(a$periods[starts], collapse = ", "))
      r
    })
    render_ap_results(output, ap, result)
    result      # returned so that the Disaggregation tab can use this plan
  })
}
