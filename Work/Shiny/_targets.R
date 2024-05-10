# _targets.R file:
library(targets)
library(tarchetypes)
library(crew)
# library(doParallel)
# Load all R scripts in the R/ directory.
file.sources <- list.files("R", pattern = "*.R", full.names = TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))

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
out_dir <- './Output'
dataMatrixPath <- 'data/ST000284/dataMatrix.csv'
sampleMetadataPath <- 'data/ST000284/sampleMetadata.csv'
variableMetadataPath <- 'data/ST000284/variableMetadata.csv'
dataSep <- ','
sampleSep <- ','
variableSep <- ','

# Columns settings
factor_cols <- c('Groups', 'Age', 'Gender', 'Smoking')
sample_id_col <- 'sample_id'
sample_type_col <- 'sample_type'
group_col <- 'Groups'


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

filter_step(filtered_1, factorized, threshold = 0.8, filter_outliers = TRUE, conf.limit = '0.95', out_dir = out_dir),
impute(imputed_2, filtered_1, method = 'RF', k = 5),
normalize(normalized_3, imputed_2, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'QuantileNorm', transNorm = 'NULL', scaleNorm = 'NULL', ref = '', out_dir = out_dir),
#### EXTRACTION ####
# Extract the data
tar_target(extract_data, export_data(normalized_3, out_dir = out_dir, out_name = 'Processed')),


#### Cleaning ####
tar_target(clean, withr::with_dir(out_dir, unlink('TempData', recursive = TRUE)))
)
