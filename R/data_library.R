# =====================================================================
# Data Library
# Load many files (or a whole folder) once; every Excel sheet and every
# csv file becomes a "dataset" that any tab can pick from its
# "Or choose from the data library" list. A file uploaded in any tab is
# added to the library too.
# The library lives in session$userData (one per browser session); it is
# not stored on the server after the page is closed.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPERS
# ---------------------------------------------------------------------

# Every sheet of an Excel file (or the csv file) as a named list of data frames.
# Empty sheets are skipped. Names: "file.xlsx > Sheet1" or "file.csv".
read_workbook <- function(path, file_name) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(path)
    out <- lapply(sheets, function(sh) as.data.frame(readxl::read_excel(path, sheet = sh)))
    names(out) <- if (length(sheets) == 1) file_name else paste(file_name, ">", sheets)
    out[vapply(out, function(df) nrow(df) > 0 && ncol(df) > 0, logical(1))]
  } else {
    setNames(list(read_data_file(path, file_name)), file_name)
  }
}

# The library of this browser session (created on first use).
data_library <- function(session) {
  if (is.null(session$userData$library)) session$userData$library <- reactiveVal(list())
  session$userData$library
}

# "Use in every forecasting tab" signal: list(name, time).
library_broadcast <- function(session) {
  if (is.null(session$userData$broadcast)) session$userData$broadcast <- reactiveVal(NULL)
  session$userData$broadcast
}

# Adds the files of a fileInput value to the library; returns the new names.
# Files that are not .xlsx / .xls / .csv are ignored.
library_add_files <- function(session, files) {
  lib <- data_library(session)
  ok <- tolower(tools::file_ext(files$name)) %in% c("xlsx", "xls", "csv")
  current <- lib()
  added <- character(0); failed <- character(0)
  for (i in which(ok)) {
    sets <- tryCatch(read_workbook(files$datapath[i], files$name[i]), error = function(e) NULL)
    if (is.null(sets)) { failed <- c(failed, files$name[i]); next }
    for (nm in names(sets)) {
      key <- nm; k <- 2
      while (key %in% names(current)) { key <- paste0(nm, " (", k, ")"); k <- k + 1 }
      current[[key]] <- sets[[nm]]
      added <- c(added, key)
    }
  }
  lib(current)
  list(added = added, skipped = files$name[!ok], failed = failed)
}

# Short description of a dataset: rows, columns, numeric columns.
describe_dataset <- function(df) {
  nums <- numeric_columns(df)
  data.frame(Rows = nrow(df), Columns = ncol(df),
             "Numeric columns" = paste(nums, collapse = ", "), check.names = FALSE)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------
data_library_ui <- function(id) {
  ns <- NS(id)
  folder_input <- fileInput(ns("folder"), "Add a whole folder", multiple = TRUE)
  folder_input <- htmltools::tagQuery(folder_input)$find("input")$
    addAttrs(webkitdirectory = NA, directory = NA)$allTags()
  sidebarLayout(
    sidebarPanel(
      formula_box(c("Load your data once here.",
                    "Every tab can then pick a dataset from its 'data library' list.")),
      fileInput(ns("files"), "Add files (.xlsx, .xls, .csv - you can select several)",
                multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
      folder_input,
      helpText("Every sheet of an Excel file becomes a separate dataset.",
               "Other file types in a folder are skipped."),
      hr(),
      selectInput(ns("selected"), "Dataset", choices = NULL),
      div(class = "data-actions",
          actionButton(ns("use_all"), "Use in every forecasting tab", icon = icon("share-nodes"),
                       class = "btn-sm btn-outline-primary"),
          actionButton(ns("remove"), "Remove", icon = icon("trash"), class = "btn-sm btn-outline-secondary")),
      actionLink(ns("clear"), "Remove all datasets")
    ),
    mainPanel(
      div(class = "result-title", "Data library"),
      uiOutput(ns("kpis")),
      h4("Datasets"),
      tableOutput(ns("list")),
      h4(textOutput(ns("preview_title"))),
      tableOutput(ns("preview"))
    )
  )
}


# ---------------------------------------------------------------------
# 3) SERVER
# ---------------------------------------------------------------------
data_library_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    lib <- data_library(session)
    broadcast <- library_broadcast(session)

    report <- function(res) {
      msg <- paste0(length(res$added), " dataset(s) added.")
      if (length(res$skipped)) msg <- paste0(msg, " Skipped (not Excel/csv): ", length(res$skipped), ".")
      if (length(res$failed)) msg <- paste0(msg, " Could not read: ", paste(res$failed, collapse = ", "), ".")
      showNotification(msg, type = if (length(res$failed)) "warning" else "message")
    }
    observeEvent(input$files, report(library_add_files(session, input$files)))
    observeEvent(input$folder, report(library_add_files(session, input$folder)))

    observeEvent(lib(), {
      nms <- names(lib())
      sel <- if (isTruthy(input$selected) && input$selected %in% nms) input$selected else tail(nms, 1)
      updateSelectInput(session, "selected", choices = nms, selected = sel)
    })

    observeEvent(input$remove, {
      req(input$selected)
      cur <- lib(); cur[[input$selected]] <- NULL; lib(cur)
    })
    observeEvent(input$clear, lib(list()))
    observeEvent(input$use_all, {
      req(input$selected)
      broadcast(list(name = input$selected, time = Sys.time()))
      showNotification(paste0("'", input$selected, "' is now selected in every forecasting tab."),
                       type = "message")
    })

    output$kpis <- renderUI({
      l <- lib()
      if (length(l) == 0) {
        return(empty_state("Drop Excel or .csv files here",
                           "or click to choose them - or use 'Add a whole folder' on the left."))
      }
      div(class = "kpi-row",
          kpi_tile("Datasets", length(l)),
          kpi_tile("Files", length(unique(sub(" > .*$", "", names(l))))),
          kpi_tile("Rows in total", fmt_num(sum(vapply(l, nrow, numeric(1))), 0)))
    })

    output$list <- renderTable({
      l <- lib()
      if (length(l) == 0) return(NULL)
      cbind(Dataset = names(l), do.call(rbind, lapply(l, describe_dataset)))
    })

    output$preview_title <- renderText({
      if (isTruthy(input$selected) && input$selected %in% names(lib()))
        paste("Preview:", input$selected, "(first 15 rows)") else ""
    })
    output$preview <- renderTable({
      req(input$selected, input$selected %in% names(lib()))
      df <- head(lib()[[input$selected]], 15)
      dates <- vapply(df, function(x) inherits(x, c("Date", "POSIXt")), logical(1))
      df[dates] <- lapply(df[dates], function(x) format(as.Date(x)))
      df
    })
  })
}
