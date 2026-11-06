# =====================================================================
# Help texts: a (?) sign is added next to the label of every input below.
# Keys are input ids. A key can be the full id ("moving_average-N") or only
# the last part of it ("alpha" = every input whose id ends in "-alpha").
# The full id wins. Edit the texts here; nothing else needs to change.
# =====================================================================

help_texts <- list(
  # --- data ----------------------------------------------------------
  file = "Excel (.xlsx, .xls) or .csv. One row per period, oldest first; one column per series. You can also drag a file onto this panel.",
  column = "The numeric column to forecast.",
  label_col = "Column with the period names or dates. Only used as labels on the charts and tables.",

  # --- forecasting ---------------------------------------------------
  mode = "Enter the parameters yourself, or let the app try many values and keep the ones with the smallest error.",
  criterion = "MAD: average absolute error. MSE: average squared error (punishes large errors). MAPE: average percentage error.",
  h = "How many periods after the last data point to forecast.",
  test = "Hide the last k periods, fit the method on the rest and check how well it forecasts the hidden periods. This shows the real accuracy. 0 = off.",
  init = "Starting level S0 (and slope G0): from a regression line through the data, or typed by you.",
  S0 = "Starting level: the smoothed demand just before period 1.",
  G0 = "Starting slope: the trend per period just before period 1.",
  alpha = "Smoothing constant for the level (0 < alpha <= 1). Small: smooth, reacts slowly. Large: follows the latest demand closely.",
  beta = "Smoothing constant for the trend (0 < beta <= 1). Small: stable trend. Large: the trend changes quickly.",
  gamma = "Smoothing constant for the seasonal factors (0 < gamma <= 1).",
  N = "Number of periods in one season: 4 for quarterly, 12 for monthly, 52 for weekly data.",
  `moving_average-N` = "How many past periods are averaged. Larger N: smoother, but slower to react to changes.",
  Nmax = "The search tries every N from 1 up to this value.",
  weights = "Weights for the most recent period first, separated by commas, e.g. 0.5, 0.3, 0.2. They must add up to 1.",
  `weighted_moving_average-N` = "Number of weights to search for (2 to 5).",
  p = "AR order: how many past demand values are used. Hint: where the PACF cuts off.",
  d = "Number of differences needed to remove the trend (usually 0, 1 or 2).",
  q = "MA order: how many past random errors are used. Hint: where the ACF cuts off.",
  pmax = "Largest AR order p to try.",
  dmax = "Largest number of differences d to try.",
  qmax = "Largest MA order q to try.",
  mean = "Include a constant (the mean level) in the model.",

  # --- aggregate planning: data and costs ----------------------------
  name_col = "Column with the period names.",
  demand_col = "Column with the demand forecast of each period (aggregate units).",
  days_col = "Column with the number of working days in each period.",
  names = "Period names separated by commas. You can rename the periods here.",
  c_hire = "Cost of hiring one worker.",
  c_fire = "Cost of laying off one worker.",
  c_hold = "Cost of keeping one unit in stock for one period.",
  c_reg = "Cost of making one unit on regular time. Use 0 if wages are not part of the comparison.",
  c_ot = "Cost of making one unit on overtime.",
  c_idle = "Cost of one unit of unused regular-time capacity (workers paid but idle).",
  c_sub = "Cost of buying one unit from a subcontractor.",
  W0 = "Number of workers just before the first period.",
  I0 = "Inventory on hand at the start of the first period.",
  IT = "Inventory that must be left at the end of the last period.",
  k_mode = "Enter K directly, or compute it from a past production record.",
  K = "Units one worker produces in one working day.",
  past_units = "Units produced in the past record (K = units / (workers x days)).",
  past_workers = "Number of workers in the past record.",
  past_days = "Number of working days in the past record.",

  # --- aggregate planning: methods ------------------------------------
  changes = "Tick the periods in which the workforce may change. Between them it stays constant.",
  allow_ot = "Let the LP use overtime (at the overtime cost).",
  ot_limit = "Overtime cannot exceed this percentage of regular-time capacity. 0 = no limit.",
  allow_idle = "Let the LP leave regular-time capacity unused (at the idle cost).",
  allow_sub = "Let the LP buy units from subcontractors (at the subcontracting cost).",
  sub_limit = "Most units that can be subcontracted in one period. 0 = no limit.",
  `transportation_plan-W` = "Constant workforce that sets the regular-time capacity K x n_t x W of every period.",
  ot_pct = "Overtime capacity of each period as a percentage of its regular-time capacity. 0 = no overtime.",
  sub_cap = "Units that can be subcontracted in each period. 0 = no subcontracting.",
  families = "One family per line: name, setup cost K_j, demand lambda_j, lower bound a_j, upper bound b_j.",
  source = "Split the production of one of the aggregate plans, or type the aggregate quantities yourself.",
  plan = "Which aggregate plan to split (its whole-worker version is used).",
  X = "Aggregate production of each period, separated by commas.",

  # --- learning curve -------------------------------------------------
  L = "Learning rate: each time the cumulative output doubles, the time per unit is multiplied by L.",
  T1 = "Time needed for the first unit.",
  units = "How many units to show."
)

# Adds the (?) signs in the browser (Bootstrap tooltips).
app_help_js <- function() {
  tags$script(HTML(paste0("
    (function () {
      var HELP = ", jsonlite::toJSON(help_texts, auto_unbox = TRUE), ";
      function textFor(id) {
        if (!id) return null;
        if (HELP[id]) return HELP[id];
        var parts = id.split('-');
        for (var i = 1; i < parts.length; i++) {           // longest suffix first
          var key = parts.slice(i).join('-');
          if (HELP[key]) return HELP[key];
        }
        return null;
      }
      function icon(text) {
        var s = document.createElement('span');
        s.className = 'help-icon'; s.tabIndex = 0; s.textContent = '?';
        s.setAttribute('title', text); s.setAttribute('data-bs-toggle', 'tooltip');
        if (window.bootstrap && bootstrap.Tooltip) new bootstrap.Tooltip(s, { placement: 'right' });
        return s;
      }
      function addHelp() {
        document.querySelectorAll('label.control-label, label.form-label').forEach(function (l) {
          if (l.querySelector('.help-icon')) return;
          var t = textFor(l.getAttribute('for') || (l.id || '').replace(/-label$/, ''));
          if (t) l.appendChild(icon(t));
        });
        document.querySelectorAll('.checkbox input[type=checkbox], .form-check input[type=checkbox]').forEach(function (c) {
          var span = c.parentElement && c.parentElement.querySelector('span');
          if (!span || span.querySelector('.help-icon') || !c.id) return;
          var t = textFor(c.id);
          if (t) span.appendChild(icon(t));
        });
      }
      document.addEventListener('DOMContentLoaded', addHelp);
      document.addEventListener('shiny:connected', function () { setTimeout(addHelp, 300); });
    })();
  ")))
}
