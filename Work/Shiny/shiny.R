# Define UI
ui <- navbarPage(
  id = "Data Initializer",
  # Navbar title
  title = "MetaboPipe Data Initializer",
  
  # Define tabs in the navbar
  tabPanel("Data Upload",
           # Sidebar layout with input and output definitions
           sidebarLayout(
             # Sidebar panel for inputs
             sidebarPanel(
               # Dataset selection
               selectInput("dataset", "Select Dataset:",
                           choices = c("MTBLS79", "ST000284", "Upload data"),
                           selected = "Upload data"),
               
               # Conditional panels for uploading data files
               conditionalPanel(
                 condition = "input.dataset == 'Upload data'",
                 ### DATA ###
                 fileInput("dataMatrix", "Choose DataMatrix file",
                           accept = c(".csv")
                 ),
                 # Input: Select separator ----
                 radioButtons("dataSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ","),
                 # Horizontal line ----
                 tags$hr(),
                 ### SAMPLE ###
                 fileInput("sampleMetadata", "Choose Sample Metadata file",
                           accept = c(".csv")
                 ),
                 radioButtons("sampleSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ","),
                 # Horizontal line ----
                 tags$hr(),
                 ### VARIABLE ###
                 fileInput("variableMetadata", "Choose Variable Metadata file",
                           accept = c(".csv")
                 ),
                 radioButtons("variableSep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";"),
                              selected = ",")
               ),
               # Add a button to load data
               actionButton("loadData", "Load Data", style = "color: #fff; background-color: #28a745; border-color: #28a745;")
             ),
             
             # Main panel for displaying outputs
             mainPanel()
           )
  ),
  tabPanel("DataConfig",
           sidebarLayout(
             sidebarPanel(
               # Input: Select factor column ----
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
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Data Summary", DT::dataTableOutput("dataMatrixTable")),
                 tabPanel("Sample Metadata Summary", DT::dataTableOutput("sampleMetadataTable")),
                 tabPanel("Variable Metadata Summary", DT::dataTableOutput("variableMetadataTable"))
               )
               # Placeholder for displaying outputs
               # Add outputs here based on user input
             )
           )
  ),
  tabPanel("Process Selector",
           sidebarLayout(
             sidebarPanel(
               # Placeholder for user inputs
               # Add inputs here
             ),
             mainPanel(
               # Placeholder for displaying outputs
               # Add outputs here based on user input
             )
           )
  ),
)




# Define server logic
server <- function(input, output, session) {
  
  # Define reactive values to store data paths, separator, and factor columns
  data <- reactiveValues(
    dataMatrixPath = NULL,
    sampleMetadataPath = NULL,
    variableMetadataPath = NULL,
    separator = ",",
    factor_cols = NULL,
    sample_id_col = NULL,
    sample_type_col = NULL,
    group_col = NULL,
    na_threshold = 0.80,
    filter_outliers = NULL,
    blank_label = NULL,
    qc_label = NULL,
    fold_change = 20,
    rowNorm = NULL,
    ref = NULL,
    transNorm = NULL,
    scaleNorm = NULL
  )
  
  
  # Update data paths, separator, and factor_cols based on user input
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
  
  # Update separator based on user input
  observeEvent(input$dataSep, {
    data$separator <- input$dataSep
  })
  
  # Update factor_cols, sample_id_col, sample_type_col, and group_col based on user selection
  observeEvent(data$sampleMetadataPath, {
    req(data$sampleMetadataPath)
    df <- read.csv(data$sampleMetadataPath, sep = data$separator, strip.white = TRUE)  # Strip leading/trailing spaces
    updateSelectizeInput(session, "factorCols", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "sampleIdCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "sampleTypeCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "groupCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "orderCol", choices = colnames(df), server = TRUE)
    updateSelectizeInput(session, "batchCol", choices = colnames(df), server = TRUE)
  })
  
  # Store selected factor columns, sample_id_col, sample_type_col, and group_col
  observeEvent(input$factorCols, {
    data$factor_cols <- input$factorCols
    data$sample_id_col <- input$sampleIdCol
    data$sample_type_col <- input$sampleTypeCol
    data$group_col <- input$groupCol
    data$order_col <- input$orderCol
    data$batch_col <- input$batchCol
  })
  
  # Proceed to the next panel after loading the data

  
  
  # Render data tables based on user input
  output$dataMatrixTable <- DT::renderDataTable({
    req(data$dataMatrixPath)
    df <- read.csv(data$dataMatrixPath, sep = data$separator)
    DT::datatable(df)
  })
  
  output$sampleMetadataTable <- DT::renderDataTable({
    req(data$sampleMetadataPath)
    df <- read.csv(data$sampleMetadataPath, sep = data$separator)
    DT::datatable(df)
  })
  
  output$variableMetadataTable <- DT::renderDataTable({
    req(data$variableMetadataPath)
    df <- read.csv(data$variableMetadataPath, sep = data$separator)
    DT::datatable(df)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
