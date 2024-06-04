library(shiny)
library(DT)
library(shinyFiles)

navbarPage(
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
                conditionalPanel(
                    condition = "input.sampleTypeCol != 'NULL'",
                    selectizeInput("qcLabel", "Select the QC label:",
                        choices = NULL
                    )
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
                        min = 0, max = 1, value = 0.20, step = 0.01
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
                        choices = c("ComBat", "QCRSC", "NULL"),
                        selected = "NULL"
                    ),
                    conditionalPanel(
                        condition = "input.batchCorrectMethod == 'QCRSC'",
                        selectizeInput("orderCol", "Select the Order column (the injection order):",
                            choices = NULL
                        )
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
