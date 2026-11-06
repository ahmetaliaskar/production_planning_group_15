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
| Data Library | Load many files or a folder once; preview the datasets |
| Learning Curve | Learning curve |
| Forecasting (Assignment 2) | Moving Average, Weighted Moving Average, Simple Exponential Smoothing, Seasonal Factors (Quick and Dirty), Holt's Method, Seasonal Decomposition, Winters' Method, AR, MA, ARMA, ARIMA, **Compare Methods** |
| Aggregate Planning (Assignment 3) | Data and Costs, Zero-Inventory (Chase) Plan, Constant-Workforce (Level) Plan, Mixed Plan, Linear Programming, Transportation Method, Disaggregation (Square-Root Rule), **Compare Plans** |

Features for everyday use:

- **Data Library**: load many files or a whole folder at once (every Excel sheet becomes a dataset),
  then pick any dataset in any tab from its "data library" list
- **Load example data** button and an **empty Excel template** in every data panel
- Drag and drop a file onto the left panel or onto the "Drop a file here" card
- Data check: text cells, empty rows, gaps (filled by interpolation), negative and zero values are reported
- Period names / dates from the file are used on the charts and tables
- **Hold-out test**: hide the last k periods to measure the real forecast accuracy (also in Compare Methods)
- Results download as **Excel (.xlsx)** with several sheets
- A (?) help sign next to every input; the texts are in `R/help_texts.R`

## Repository structure

```
app.R              main menu (navbarPage)
manifest.json      package list for Posit Connect Cloud
R/
  forecast_common.R     shared tools of the forecasting modules
  aggregate_common.R    "Data and Costs" tab and shared tools of aggregate planning
  app_theme.R           theme, colors, chart style and the Home page
  example_data.R        built-in example data
  data_library.R        the Data Library (many files / a folder, every sheet a dataset)
  help_texts.R          help texts of the (?) signs
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
