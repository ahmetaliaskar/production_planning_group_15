# Production Planning - Group 15

Shiny application for the Production Planning course (Nahmias).

**Live app:** https://01a1b063-ba5d-86b5-f942-aaaca58bdcaa.share.connect.posit.cloud

## Group members

- Ahmet Ali Askar
- Eren Yalçınkaya
- Bulut Sakallıoğlu
- Bilal İskender

## Contents of the application

| Menu | Tabs |
|---|---|
| Home | Overview and group members |
| Learning Curve | Learning curve |
| Forecasting (Assignment 2) | Moving Average, Weighted Moving Average, Simple Exponential Smoothing, Seasonal Factors (Quick and Dirty), Holt's Method, Seasonal Decomposition, Winters' Method, AR, MA, ARMA, ARIMA, **Compare Methods** |
| Aggregate Planning (Assignment 3) | Data and Costs, Zero-Inventory (Chase) Plan, Constant-Workforce (Level) Plan, Mixed Plan, Linear Programming, Transportation Method, Disaggregation (Square-Root Rule), **Compare Plans** |

Every data tab has a **Load example data** button, and results can be downloaded as .csv.

## Repository structure

```
app.R              main menu (navbarPage)
manifest.json      package list for Posit Connect Cloud
R/
  forecast_common.R     shared tools of the forecasting modules
  aggregate_common.R    "Data and Costs" tab and shared tools of aggregate planning
  app_theme.R           theme, colors, chart style and the Home page
  example_data.R        built-in example data
  <method>.R            one module per method (<method>_ui / <method>_server)
```

Each method is a Shiny module: its UI function wraps every input and output id in `ns()`,
and its server function uses `moduleServer()`. `app.R` adds one `tabPanel` per module.

## Data files

Example files are in the `data/` folder:

| File | Use it in | Contents |
|---|---|---|
| `forecast_example.xlsx` / `.csv` | Forecasting tabs | 24 quarters; columns `Stationary`, `Trend`, `Seasonal` |
| `aggregate_example.xlsx` / `.csv` | Aggregate Planning - Data and Costs | 6 months; `Month`, `Demand`, `Working days` |

- Forecasting: an Excel (.xlsx, .xls) or .csv file with one or more numeric columns;
  the series is chosen in the app.
- Aggregate planning: period names, demand forecasts and working days.
- csv files saved by a Turkish Excel (";" separator, "," decimals) are also accepted.
