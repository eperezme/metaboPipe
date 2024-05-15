# _targets.R file:
library(targets)
library(tarchetypes)
library(crew)
# library(doParallel)
# Load all R scripts in the R/ directory.
# file.sources <- list.files("R", pattern = "*.R", full.names = TRUE)
# invisible(sapply(file.sources, source, .GlobalEnv))

# Declare libraries
# These are the libraries that the pipeline depends on.

# To create a list with all the libraries to copy-paste:
# cat(paste(shQuote(unique(renv::dependencies(path = "R")$Package), type="cmd"), collapse=", "))

tar_option_set(
  packages = c(
    "structToolbox", "SummarizedExperiment", "VIM", "impute", "imputeLCMD",
    "missForest", "caret", "pcaMethods", "tidyverse", "MetaboAnalystR", "tinytex",
    "HotellingEllipse", "ggforce", "tools", "cowplot", "metaboPipe"
  )
)
# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
# controller <- crew::crew_controller_local(
#   name = "Controller",
#   workers = 2,
#   seconds_idle = 3
# )
# tar_option_set(controller = controller)
#### Global variables #####
out_dir <- ''
dataMatrixPath <- 'C:\Users\eduar\AppData\Local\Temp\RtmpecffkZ/d8950fe574d75594a5b569b5/0.csv'
sampleMetadataPath <- ''
variableMetadataPath <- ''
dataSep <- ','
sampleSep <- ','
variableSep <- ','

# Columns settings
factor_cols <- c('')
sample_id_col <- ''
sample_type_col <- ''
group_col <- ''


dir.create(out_dir, showWarnings = FALSE)
out_dir <- tools::file_path_as_absolute(out_dir)
##### DEFINE THE PIPELINE ######
list(
# LOAD THE DATA
load_data(data, dataMatrixPath, sampleMetadataPath, dataSep = dataSep, sampleSep = sampleSep, variableSep = variableSep),

# Create a DatasetExperiment object
createExperiment(experiment, data),

# Factorize the cols
factorize_cols(factorized, experiment, factor_cols),

#SHINY STEPS

#### EXTRACTION ####
# Extract the data
tar_target(extract_data, export_data(factorized, out_dir = out_dir, out_name = 'Processed')),


#### Cleaning ####
tar_target(clean, withr::with_dir(out_dir, unlink('TempData', recursive = TRUE)))
)
