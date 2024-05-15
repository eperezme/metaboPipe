library(shiny)
library(DT)
library(shinyFiles)
# Define UI
ui <- navbarPage(
  id = "DataInitializer",
  title = "MetaboPipe Data Initializer",
  tabPanel(
    "Data Upload",
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Select Dataset:",
          choices = c("MTBLS79", "ST000284", "Upload data"),
          selected = "Upload data"
        ),
        conditionalPanel(
          condition = "input.dataset == 'Upload data'",
          fileInput("dataMatrix", "Choose DataMatrix file",
            accept = c(".csv")
          ),
          radioButtons("dataSep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";"
            ),
            selected = ","
          ),
          tags$hr(),
          fileInput("sampleMetadata", "Choose Sample Metadata file",
            accept = c(".csv")
          ),
          radioButtons("sampleSep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";"
            ),
            selected = ","
          ),
          tags$hr(),
          fileInput("variableMetadata", "Choose Variable Metadata file",
            accept = c(".csv")
          ),
          radioButtons("variableSep", "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";"
            ),
            selected = ","
          )
        ),
        actionButton("loadData", "Load Data", style = "color: #fff; background-color: #28a745; border-color: #28a745;")
      ),
      mainPanel()
    )
  ),
  tabPanel(
    "DataConfig",
    sidebarLayout(
      sidebarPanel(
        selectizeInput("factorCols", "Select columns as factor variables:",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput("sampleIdCol", "Select sample ID column:",
          choices = NULL
        ),
        selectizeInput("sampleTypeCol", "Select sample Type column (if its QC, Blank or Sample):",
          choices = NULL
        ),
        selectizeInput("groupCol", "Select the Group column (the study variable):",
          choices = NULL
        ),
        actionButton("setSettings", "Set Settings", style = "color: #fff; background-color: #007bff; border-color: #007bff;")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Summary", DT::dataTableOutput("dataMatrixTable")),
          tabPanel("Sample Metadata Summary", DT::dataTableOutput("sampleMetadataTable")),
          tabPanel("Variable Metadata Summary", DT::dataTableOutput("variableMetadataTable"))
        )
      )
    )
  ),
  tabPanel(
    "Process Selector",
    sidebarLayout(
      sidebarPanel(
        selectInput("process", "Select the first step to perform:",
          choices = c("Filter", "Impute", "Normalize", "Batch Correct")
        ),
        conditionalPanel(
          condition = "input.process == 'Filter'",
          numericInput("naThreshold", "Select the threshold for missing values (between 0 and 1):",
            min = 0, max = 1, value = 0.80, step = 0.01
          ),
          checkboxInput("filterOutliers", "Filter Outliers"),
          conditionalPanel(
            condition = "input.filterOutliers == true",
            selectInput("confLimit", "Select the confidence limit for outlier removal:",
              choices = c("0.95", "0.99")
            )
          )
        ),
        conditionalPanel(
          condition = "input.process == 'Impute'",
          selectInput("imputeMethod", "Select the imputation method:",
            choices = c("mean", "median", "RF", "QRILC", "kNN", "SVD", "bpca", "ppca"),
            selected = "RF"
          ),
          conditionalPanel(
            condition = "input.imputeMethod == 'kNN'",
            numericInput("k", "Select the number of neighbors for kNN imputation:",
              min = 1, value = 5
            )
          ),
          conditionalPanel(
            condition = "input.imputeMethod == 'SVD' || input.imputeMethod == 'BPCA' || input.imputeMethod == 'PPCA'",
            numericInput("k", "Select the number of principal components for imputation:",
              min = 1, value = 5
            )
          )
        ),
        conditionalPanel(
          condition = "input.process == 'Normalize'",
          selectInput("rowNormMethod", "Select the row-wise normalization method:",
            choices = c("QuantileNorm", "CompNorm", "SumNorm", "MedianNorm", "SpecNorm", "NULL"),
            selected = "NULL"
          ),
          conditionalPanel(
            condition = "input.rowNormMethod == 'CompNorm'",
            selectizeInput("ref", "Enter the compound of reference for CompNorm normalization:",
              choices = NULL
            )
          ),
          selectInput("transNormMethod", "Select the transformation method:",
            choices = c("LogNorm", "CrNorm", "NULL"),
            selected = "NULL"
          ),
          selectInput("scaleNormMethod", "Select the scaling method:",
            choices = c("MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm", "NULL"),
            selected = "NULL"
          )
        ),
        conditionalPanel(
          condition = "input.process == 'Batch Correct'",
          selectInput("batchCorrectMethod", "Select the batch correction method:",
            choices = c("ComBat", "SVA", "RUV", "NULL"),
            selected = "NULL"
          ),
          selectizeInput("orderCol", "Select the Order column (the injection order):",
            choices = NULL
          ),
          selectizeInput("batchCol", "Select the batch column:",
            choices = NULL
          ),
        ),
        actionButton("removeStep", "Remove Step", style = "color: #fff; background-color: #dc3545; border-color: #dc3545;"),
        actionButton("addStep", "Add Step", style = "color: #fff; background-color: #007bff; border-color: #007bff;")
      ),
      mainPanel(
        h3("Added Steps"),
        verbatimTextOutput("addedSteps"),
        shinyDirButton("outDir", "Output Directory", "Select a directory", "Choose", icon = icon("folder", class = "solid", lib = "font-awesome")),
        actionButton("writeSteps", "Write Steps", style = "color: #fff; background-color: #28a745; border-color: #28a745;"),
        actionButton("runPipeline", "Run Pipeline", style = "color: #fff; background-color: #007bff; border-color: #007bff;")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  data <- reactiveValues(
    dataMatrixPath = NULL,
    sampleMetadataPath = NULL,
    variableMetadataPath = NULL,
    dataSeparator = ",",
    sampleSeparator = ",",
    variableSeparator = ",",
    factorCols = NULL,
    sampleIdCol = NULL,
    sampleTypeCol = NULL,
    groupCol = NULL,
    orderCol = NULL,
    batchCol = NULL,
    rowNormMethod = NULL,
    transNormMethod = NULL,
    scaleNormMethod = NULL,
    confLimit = NULL,
    naThreshold = 0.80,
    filterOutliers = FALSE,
    imputeMethod = "Random Forest",
    k = 5,
    batchCorrectMethod = NULL,
    ref = NULL,
    outDir = "output"
  )

  observeEvent(input$loadData, {
    if (input$dataset == "Upload data") {
      data$dataMatrixPath <- input$dataMatrix$datapath
      data$sampleMetadataPath <- input$sampleMetadata$datapath
      data$variableMetadataPath <- input$variableMetadata$datapath
    } else {
      data$dataMatrixPath <- switch(input$dataset,
        "MTBLS79" = "data/MTBLS79/data.csv",
        "ST000284" = "data/ST000284/dataMatrix.csv"
      )
      data$sampleMetadataPath <- switch(input$dataset,
        "MTBLS79" = "data/MTBLS79/sample_meta.csv",
        "ST000284" = "data/ST000284/sampleMetadata.csv"
      )
      data$variableMetadataPath <- switch(input$dataset,
        "MTBLS79" = "data/MTBLS79/variable_meta.csv",
        "ST000284" = "data/ST000284/variableMetadata.csv"
      )
      data$dataSeparator <- ","
      data$sampleSeparator <- ","
      data$variableSeparator <- ","
    }
  })

  observeEvent(input$dataSep, {
    data$dataSeparator <- input$dataSep
  })
  observeEvent(input$sampleSep, {
    data$sampleSeparator <- input$sampleSep
  })
  observeEvent(input$variableSep, {
    data$variableSeparator <- input$variableSep
  })

  observeEvent(data$sampleMetadataPath, {
    req(data$sampleMetadataPath)
    df <- read.csv(data$sampleMetadataPath, sep = data$sampleSeparator, strip.white = TRUE)
    updateSelectizeInput(session, "factorCols", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "sampleIdCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "sampleTypeCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "groupCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "orderCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "batchCol", choices = colnames(df), server = TRUE)
  })

  observeEvent(input$setSettings, {
    data$factorCols <- input$factorCols
    data$sampleIdCol <- input$sampleIdCol
    data$sampleTypeCol <- input$sampleTypeCol
    data$groupCol <- input$groupCol
  })

  observeEvent(input$rowNormMethod, {
    if (input$rowNormMethod == "CompNorm") {
      req(data$dataMatrixPath)
      df <- read.csv(data$dataMatrixPath, sep = data$dataSeparator, strip.white = TRUE)
      updateSelectizeInput(session, "ref", choices = colnames(df), server = TRUE)
    }
  })

  output$dataMatrixTable <- DT::renderDataTable({
    req(data$dataMatrixPath)
    df <- read.csv(data$dataMatrixPath, sep = data$dataSeparator)
    DT::datatable(df)
  })

  output$sampleMetadataTable <- DT::renderDataTable({
    req(data$sampleMetadataPath)
    df <- read.csv(data$sampleMetadataPath, sep = data$sampleSeparator)
    DT::datatable(df)
  })

  output$variableMetadataTable <- DT::renderDataTable({
    req(data$variableMetadataPath)
    df <- read.csv(data$variableMetadataPath, sep = data$variableSeparator)
    DT::datatable(df)
  })

  output$addedSteps <- renderPrint({
    step_lines$lines
  })



  # Reactive values to store lines to add
  step_lines <- reactiveValues(lines = character(0))
  previous_target_name <- reactiveValues(name = "factorized")
  step_counter <- reactiveValues(counter = 1)



  observeEvent(input$addStep, {
    # Set the targets file
    if (step_counter$counter == 1) {
      previous_target_name$name <- "factorized"
    }

    if (input$process == "Filter") {
      target_name <- paste0("filtered_", step_counter$counter)
      line_to_add <- paste0("filter_step(", target_name, ", ", previous_target_name$name, ", threshold = ", input$naThreshold, ", filter_outliers = ", input$filterOutliers, ", conf.limit = '", input$confLimit, "', out_dir = out_dir),")

      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }

    if (input$process == "Impute") {
      target_name <- paste0("imputed_", step_counter$counter)
      line_to_add <- paste0("impute(", target_name, ", ", previous_target_name$name, ", method = '", input$imputeMethod, "', k = ", input$k, "),")

      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }

    if (input$process == "Normalize") {
      target_name <- paste0("normalized_", step_counter$counter)
      line_to_add <- paste0("normalize(", target_name, ", ", previous_target_name$name, ", factor_col = group_col, sample_id_col = sample_id_col, rowNorm = '", input$rowNormMethod, "', transNorm = '", input$transNormMethod, "', scaleNorm = '", input$scaleNormMethod, "', ref = '", input$ref, "', out_dir = out_dir),")

      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }

    if (input$process == "Batch Correct") {
      target_name <- paste0("batch_corrected_", step_counter$counter)
      line_to_add <- paste0("batch_correct(", target_name, ", ", previous_target_name$name, ", method = '", input$batchCorrectMethod, "', out_dir = out_dir),")

      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }

    previous_target_name$name <- target_name
    step_counter$counter <- step_counter$counter + 1
  })

  # Remove last step
  observeEvent(input$removeStep, {
    # Check if there are steps to remove
    if (length(step_lines$lines) > 0) {
      # Remove the last step added
      step_lines$lines <- step_lines$lines[-length(step_lines$lines)]
      # Decrement the step counter if greater than 1
      if (step_counter$counter > 1) {
        step_counter$counter <- step_counter$counter - 1
      }
    }
  })

  observeEvent(input$writeSteps, {
    # Transform the outDir to a valid path
    out_dir <- parseDirPath(directories, input$outDir)

    # Print out the variables being concatenated

    # Write lines to file
    targets_file <- "_targets_template.R"
    template_lines <- readLines(targets_file)
    # Add the lines to the beginning of the file
    global_var_lines <- c(
      "#### Global variables #####",
      paste0("out_dir <- '", out_dir, "'"),
      paste0("dataMatrixPath <- '", data$dataMatrixPath, "'"),
      paste0("sampleMetadataPath <- '", data$sampleMetadataPath, "'"),
      paste0("variableMetadataPath <- '", data$variableMetadataPath, "'"),
      paste0("dataSep <- '", data$dataSeparator, "'"),
      paste0("sampleSep <- '", data$sampleSeparator, "'"),
      paste0("variableSep <- '", data$variableSeparator, "'"),
      "",
      "# Columns settings",
      paste0("factor_cols <- c(", paste0("'", data$factorCols, "'", collapse = ", "), ")"),
      paste0("sample_id_col <- '", data$sampleIdCol, "'"),
      paste0("sample_type_col <- '", data$sampleTypeCol, "'"),
      paste0("group_col <- '", data$groupCol, "'"),
      "",
      "",
      "dir.create(out_dir, showWarnings = FALSE)",
      "out_dir <- tools::file_path_as_absolute(out_dir)"
    )

    pipeline_load_lines <- c(
      "##### DEFINE THE PIPELINE ######",
      "list(",
      "# LOAD THE DATA",
      "load_data(data, dataMatrixPath, sampleMetadataPath, dataSep = dataSep, sampleSep = sampleSep, variableSep = variableSep),",
      "",
      "# Create a DatasetExperiment object",
      "createExperiment(experiment, data),",
      "",
      "# Factorize the cols",
      "factorize_cols(factorized, experiment, factor_cols),",
      "",
      "#SHINY STEPS",
      ""
    )

    pipeline_ending_lines <- c(
      "#### EXTRACTION ####",
      "# Extract the data",
      paste0("tar_target(extract_data, export_data(", previous_target_name$name, ", out_dir = out_dir, out_name = 'Processed')),"),
      "",
      "",
      "#### Cleaning ####",
      "tar_target(clean, withr::with_dir(out_dir, unlink('TempData', recursive = TRUE)))",
      ")"
    )

    lines <- c(template_lines, global_var_lines, pipeline_load_lines, step_lines$lines, pipeline_ending_lines)

    writeLines(lines, "_targets.R")
  })

  volumes <- getVolumes()()
  directories <- c(wd = ".", dirUp = "..", volumes)
  # Function to choose output directory
  shinyDirChoose(input, "outDir", roots = directories, filetypes = c("", "txt"))


  observeEvent(input$runPipeline, {
    validate(
      need(data$dataMatrixPath != "", "Please upload the data matrix file."),
      need(data$sampleMetadataPath != "", "Please upload the sample metadata file."),
      need(input$outDir != "", "Please select an output directory."),
      need(data$factorCols != "", "Please select the factor columns."),
      need(data$sampleIdCol != "", "Please select the sample ID column."),
      need(data$sampleTypeCol != "", "Please select the sample type column."),
      need(data$groupCol != "", "Please select the group column.")
    )
    # Run the pipeline with error handling
    tryCatch(
      {
        library(targets)
        tar_make()
      },
      error = function(e) {
        # Print the error message
        print(paste("Error:", e$message))
        # Display a notification or message to the user
        showModal(modalDialog(
          title = "Error",
          "An error occurred. Please check your inputs and try again.",
          easyClose = TRUE
        ))
      }
    )
  })
}
# Run the application
shinyApp(ui = ui, server = server)
