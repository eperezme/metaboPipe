library(shiny)
library(DT)

# Define UI
ui <- navbarPage(
  id = "DataInitializer",
  title = "MetaboPipe Data Initializer",
  
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               selectInput("dataset", "Select Dataset:",
                           choices = c("MTBLS79", "ST000284", "Upload data"),
                           selected = "Upload data"),
               
               conditionalPanel(
                 condition = "input.dataset == 'Upload data'",
                 fileInput("dataMatrix", "Choose DataMatrix file",
                           accept = c(".csv")),
                 radioButtons("dataSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ","),
                 tags$hr(),
                 fileInput("sampleMetadata", "Choose Sample Metadata file",
                           accept = c(".csv")),
                 radioButtons("sampleSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ","),
                 tags$hr(),
                 fileInput("variableMetadata", "Choose Variable Metadata file",
                           accept = c(".csv")),
                 radioButtons("variableSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ",")
               ),
               actionButton("loadData", "Load Data", style = "color: #fff; background-color: #28a745; border-color: #28a745;")
             ),
             mainPanel()
           )
  ),
  
  tabPanel("DataConfig",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("factorCols", "Select columns as factor variables:",
                              choices = NULL,
                              multiple = TRUE),
               selectizeInput("sampleIdCol", "Select sample ID column:",
                              choices = NULL),
               selectizeInput("sampleTypeCol", "Select sample Type column (if its QC, Blank or Sample):",
                              choices = NULL),
               selectizeInput("groupCol", "Select the Group column (the study variable):",
                              choices = NULL),
               selectizeInput("orderCol", "Select the Order column (the injection order):",
                              choices = NULL),
               selectizeInput("batchCol", "Select the batch column:",
                              choices = NULL),
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
  
  tabPanel("Process Selector",
           sidebarLayout(
             sidebarPanel(
               selectInput("process", "Select the first step to perform:",
                           choices = c("Filter", "Impute", "Normalize", "Batch Correct")),
               conditionalPanel(
                 condition = "input.process == 'Filter'",
                 numericInput("naThreshold", "Select the threshold for missing values (between 0 and 1):",
                              min = 0, max = 1, value = 0.80, step = 0.01),
                 checkboxInput("filterOutliers", "Filter Outliers"),
                 conditionalPanel(
                   condition = "input.filterOutliers == true",
                   selectInput("confLimit", "Select the confidence limit for outlier removal:",
                               choices = c("0.95", "0.99"))
                 )
               ),
               conditionalPanel(
                 condition = "input.process == 'Impute'",
                 selectInput("imputeMethod", "Select the imputation method:",
                             choices = c("Mean", "Median", "Random Forest", "QRILC", "kNN", "SVD", "BPCA", "PPCA"),
                             selected = "Random Forest"),
                 conditionalPanel(
                   condition = "input.imputeMethod == 'kNN'",
                   numericInput("k", "Select the number of neighbors for kNN imputation:",
                                min = 1, value = 5)
                 ),
                 conditionalPanel(
                   condition = "input.imputeMethod == 'SVD' || input.imputeMethod == 'BPCA' || input.imputeMethod == 'PPCA'",
                   numericInput("k", "Select the number of principal components for imputation:",
                                min = 1, value = 5)
                 )
               ),
               conditionalPanel(
                 condition = "input.process == 'Normalize'",
                 selectInput("rowNormMethod", "Select the row-wise normalization method:",
                             choices = c("QuantileNorm", "CompNorm", "SumNorm", "MedianNorm", "SpecNorm", "NULL"),
                             selected = "NULL"),
                 conditionalPanel(
                   condition = "input.rowNormMethod == 'CompNorm'",
                   selectizeInput("ref", "Enter the compound of reference for CompNorm normalization:",
                                  choices = NULL)
                 ),
                 selectInput("transNormMethod", "Select the transformation method:",
                             choices = c("LogNorm", "CrNorm", "NULL"),
                             selected = "NULL"),
                 selectInput("scaleNormMethod", "Select the scaling method:",
                             choices = c("MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm", "NULL"),
                             selected = "NULL")
               ),
               conditionalPanel(
                 condition = "input.process == 'Batch Correct'",
                 selectInput("batchCorrectMethod", "Select the batch correction method:",
                             choices = c("ComBat", "SVA", "RUV", "NULL"),
                             selected = "NULL")
               ),
               actionButton("addStep", "Add Step", style = "color: #fff; background-color: #dc3545; border-color: #dc3545;"),
               actionButton("writeSteps", "Write Steps", style = "color: #fff; background-color: #dc3545; border-color: #dc3545;")
             ),
             mainPanel()
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
                                    "ST000284" = "data/ST000284/dataMatrix.csv")
      data$sampleMetadataPath <- switch(input$dataset,
                                        "MTBLS79" = "data/MTBLS79/sample_meta.csv",
                                        "ST000284" = "data/ST000284/sampleMetadata.csv")
      data$variableMetadataPath <- switch(input$dataset,
                                          "MTBLS79" = "data/MTBLS79/variable_meta.csv",
                                          "ST000284" = "data/ST000284/variableMetadata.csv")
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
    data$orderCol <- input$orderCol
    data$batchCol <- input$batchCol
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
      line_to_add <- paste0("filter_step(", target_name, ", ", previous_target_name$name, ", threshold = ", input$naThreshold, ", filter_outliers = ", input$filterOutliers, ", conf.limit = '", input$confLimit ,"', out_dir = out_dir),")
      
      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }
    
    if (input$process == "Impute") {
      target_name <- paste0("imputed_", step_counter$counter)
      line_to_add <- paste0("impute(", target_name, ", ", previous_target_name$name, ", method = '", input$imputeMethod, "', k = ", input$k, ", out_dir = out_dir),")
      
      # Append line to reactiveValues
      step_lines$lines <- c(step_lines$lines, line_to_add)
    }
    
    if (input$process == "Normalize") {
      target_name <- paste0("normalized_", step_counter$counter)
      line_to_add <- paste0("normalize(", target_name, ", ", previous_target_name$name, ", rowNorm = '", input$rowNormMethod, "', transNorm = '", input$transNormMethod, "', scaleNorm = '", input$scaleNormMethod, "', ref = '", input$ref, "', out_dir = out_dir),")
    
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
  
  observeEvent(input$writeSteps, {
    # Write lines to file
    targets_file <- "_targets_template.R"
    lines <- readLines(targets_file)
    insert_line <- 99
    lines <- c(head(lines, insert_line - 1), step_lines$lines, tail(lines, -insert_line + 1))
    
    writeLines(lines, "_targets.R")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
