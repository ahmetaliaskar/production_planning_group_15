library(shiny)

ui <- navbarPage("Group 15 - Production Planning",
                 theme = app_theme(),
                 header = app_css(),
                 tabPanel("Home", home_ui()),
                 tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
                 navbarMenu("Forecasting",
                   "Stationary series",
                   tabPanel("Moving Average", moving_average_ui("moving_average")),
                   tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average")),
                   tabPanel("Simple Exponential Smoothing", exponential_smoothing_ui("exponential_smoothing")),
                   tabPanel("Seasonal Factors (Quick and Dirty)", seasonal_factors_ui("seasonal_factors")),
                   "----",
                   "Trend series",
                   tabPanel("Holt's Method", holt_ui("holt")),
                   "----",
                   "Seasonal series",
                   tabPanel("Seasonal Decomposition", seasonal_decomposition_ui("seasonal_decomposition")),
                   tabPanel("Winters' Method", winters_ui("winters")),
                   "----",
                   "Box-Jenkins",
                   tabPanel("AR", box_jenkins_ui("bj_ar", "AR")),
                   tabPanel("MA", box_jenkins_ui("bj_ma", "MA")),
                   tabPanel("ARMA", box_jenkins_ui("bj_arma", "ARMA")),
                   tabPanel("ARIMA", box_jenkins_ui("bj_arima", "ARIMA"))
                 ),
                 navbarMenu("Aggregate Planning",
                   tabPanel("Data and Costs", aggregate_data_ui("aggregate_data")),
                   "----",
                   "Heuristic plans",
                   tabPanel("Zero-Inventory (Chase) Plan", chase_plan_ui("chase_plan")),
                   tabPanel("Constant-Workforce (Level) Plan", level_plan_ui("level_plan")),
                   tabPanel("Mixed Plan", mixed_plan_ui("mixed_plan")),
                   "----",
                   "Optimization",
                   tabPanel("Linear Programming", lp_plan_ui("lp_plan")),
                   tabPanel("Transportation Method", transportation_plan_ui("transportation_plan")),
                   "----",
                   tabPanel("Disaggregation (Square-Root Rule)", disaggregation_ui("disaggregation"))
                 )
)

server <- function(input, output, session) {
  learning_curve_server("learning_curve")
  moving_average_server("moving_average")
  weighted_moving_average_server("weighted_moving_average")
  exponential_smoothing_server("exponential_smoothing")
  seasonal_factors_server("seasonal_factors")
  holt_server("holt")
  seasonal_decomposition_server("seasonal_decomposition")
  winters_server("winters")
  box_jenkins_server("bj_ar", "AR")
  box_jenkins_server("bj_ma", "MA")
  box_jenkins_server("bj_arma", "ARMA")
  box_jenkins_server("bj_arima", "ARIMA")

  # Aggregate planning: the data entered once in "Data and Costs" (ap)
  # is passed to every aggregate planning module
  ap <- aggregate_data_server("aggregate_data")
  chase <- chase_plan_server("chase_plan", ap)
  level <- level_plan_server("level_plan", ap)
  mixed <- mixed_plan_server("mixed_plan", ap)
  lp    <- lp_plan_server("lp_plan", ap)
  transportation_plan_server("transportation_plan", ap)
  disaggregation_server("disaggregation", ap,
                        plans = list("Zero-inventory (chase) plan" = chase,
                                     "Constant-workforce (level) plan" = level,
                                     "Mixed plan" = mixed,
                                     "Linear programming plan" = lp))
}

shinyApp(ui, server)
