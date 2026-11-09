# =====================================================================
# Topic: Constant-workforce (level) plan                (heuristic plan)
# Nahmias, Chapter 3
#
# One workforce level for all periods, the smallest one with no shortage:
#   W = max over t of  (cumulative net demand up to t) /
#                      (cumulative units per worker up to t)
#     = max_t  (D'_1 + ... + D'_t) / (K n_1 + ... + K n_t)
# Workers change only once, in period 1 (from W_0 to W).
# Whole workers: W is rounded up.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS
# ---------------------------------------------------------------------
level_plan_ratios <- function(ap) cumsum(ap$Dnet) / cumsum(ap$K * ap$days)
level_plan_workers <- function(ap) max(0, level_plan_ratios(ap))


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
level_plan_ui <- function(id) {
  ns <- NS(id)
  ap_page(ns, c("\\(W = \\max_t \\dfrac{\\sum_{k \\le t} D'_k}{\\sum_{k \\le t} K\\, n_k}\\)", "One workforce level, no shortages."),
          example = "level",
          extra_main = tagList(h4("Workers needed to cover cumulative net demand"),
                               tableOutput(ns("ratio_table"))))
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
level_plan_server <- function(id, ap) {
  moduleServer(id, function(input, output, session) {
    ap_example_observe(input, session, "level")
    result <- reactive({
      a <- ap_require(ap)
      W <- level_plan_workers(a)
      r <- ap_with_rounding(ap_build_plan(a, W),
                            function() ap_build_plan(a, ceiling(W - 1e-9)))
      r$title <- paste0("Constant-workforce (level) plan, W = ", round(W, 3))
      r
    })

    output$ratio_table <- renderTable({
      a <- ap_require(ap)
      ratio <- level_plan_ratios(a)
      data.frame(Period = a$periods,
                 "Cum. net demand" = cumsum(a$Dnet),
                 "Cum. units per worker" = round(cumsum(a$K * a$days), 3),
                 "Ratio" = round(ratio, 3),
                 "Largest" = ifelse(seq_along(ratio) == which.max(ratio), "<- W", ""),
                 check.names = FALSE)
    })

    render_ap_results(output, ap, result)
    result      # returned so that the Disaggregation tab can use this plan
  })
}
