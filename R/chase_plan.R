# =====================================================================
# Topic: Zero-inventory (chase) plan                    (heuristic plan)
# Nahmias, Chapter 3
#
# The workforce follows demand so that no inventory is carried:
#   W_t = D'_t / (K n_t)       (workers needed in period t)
#   H_t = max(W_t - W_{t-1}, 0),  F_t = max(W_{t-1} - W_t, 0)
# Whole workers: W_t is rounded up, ceil(D'_t / (K n_t)); the extra units
# produced are carried as inventory (as in the textbook table).
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
chase_plan_workers <- function(ap) pmax(ap$Dnet, 0) / (ap$K * ap$days)


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
chase_plan_ui <- function(id) {
  ns <- NS(id)
  ap_page(ns, c("\\(W_t = \\dfrac{D'_t}{K\\, n_t}\\)", "Hire and fire every period; production = net demand."))
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
chase_plan_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {
    result <- reactive({
      a <- ap_require(ap)
      W <- chase_plan_workers(a)
      r <- ap_with_rounding(ap_build_plan(a, W),
                            function() ap_build_plan(a, ceiling(W - 1e-9)))
      r$title <- "Zero-inventory (chase) plan"
      r
    })
    render_ap_results(output, ap, result)
    result      # returned so that the Disaggregation tab can use this plan
  })
}
