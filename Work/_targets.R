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
    "structToolbox", "SummarizedExperiment",
    "VIM", "impute", "imputeLCMD", "missForest", "caret", "pcaMethods", "tidyverse", "MetaboAnalystR", "tinytex", 
    "HotellingEllipse", "ggforce", "tools", "cowplot"
  )
)
# #### GLOBAL VARIABLES ####
out_dir <- "Out"



# # MTBLS79 DATASET
# dataMatrixPath <- "data/MTBLS79/data.csv"
# sampleMetadataPath <- "data/MTBLS79/sample_meta.csv"
# variableMetadataPath <- "data/MTBLS79/variable_meta.csv"
# 
# 
# na_threshold <- 0.8
# 
# # Columns setting
# factor_cols <- c("Type", "Batch", "Class", "batch_qc")
# sample_type_col <- 'Type'
# order_col <- 'run_order'
# batch_col <- 'Batch'
# internal_standard_col <- 'IS'
# 
# 
# # blank filter
# blank_label <- NA
# qc_label <- 'QC'
# fold_change <- 20
# 

#### ST000284 DATASET #####
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
separator <- ","



# Columns setting
factor_cols <- c("Groups", "Gender", "Smoking", "Alcohol", "Diagnosis", "Stage")
sample_id_col <- "sample_id"
sample_type_col <- "sample_type"
group_col <- "Groups"

# order_col <- 'Order'
# batch_col <- 'Batch'
# internal_standard_col <- NA

# Filter Missing values threshold
na_threshold <- 0.80

# blank filter
filter_blank <- TRUE
blank_label <- 'Blank'
qc_label <- 'QC'
fold_change <- 20


# NORMALIZATION
# rowNorm = c("QuantileNorm", "CompNorm", "SumNorm", "MedianNorm", "SpecNorm", 'NULL')
rowNorm <-  "CompNorm"
ref <- "Creatinine (114.1 / 44.0)"

#transNorm = c("LogNorm", "CrNorm", "NULL")
transNorm <- "LogNorm"

# scaleNorm = c("MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm", "NULL")
scaleNorm <- "AutoNorm"


# # OTHER DATASET
# dataMatrixPath <- "data/dataMatrix.csv"
# sampleMetadataPath <- "data/sampleMetadata.csv"
# variableMetadataPath <- "data/variableMetadata.csv"
# factor_cols <- c("sample_type", "biol.batch")
# sample_type_col <- 'sample_type'
# order_col <- 'order'
# batch_col <- NA
# 
# 
# # blank filter
# blank_label <- NA
# qc_label <- 'QC'
# fold_change <- 20


# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
# controller <- crew::crew_controller_local(
#   name = "Controller",
#   workers = 2,
#   seconds_idle = 3
# )
# tar_option_set(controller = controller)


#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################


out_dir <- tools::file_path_as_absolute(out_dir)
dir.create(out_dir, showWarnings = FALSE)


# Define the pipeline.
list(
  # LOAD THE DATA
  load_data(data, dataMatrixPath, sampleMetadataPath, separator = sep),
  
  
  
  # Create a DatasetExperiment object
  createExperiment(experiment, data),
  
  # Factorize the cols
  factorize_cols(factorized, experiment, factor_cols),
  
  
  #### FILTERING ####
  # Filter missing values
  filter_step(filtered, factorized, threshold = na_threshold, filter_outliers = TRUE, conf.limit = "0.95", out_dir = out_dir),
  
  # #### IMPUTE ####
  # # impute missing values
  impute(imputed, filtered, method = "RF", 5), #out_dir = out_dir),
  
  
  #### NORMALIZATION ####
  normalize(normalized, imputed, 
            factor_col = group_col, sample_id_col = sample_id_col, 
            rowNorm = rowNorm, transNorm = transNorm, scaleNorm = scaleNorm, ref = ref, out_dir = out_dir),
  


  #### EXTRACTION #####
  # Extract the data
  tar_target(extract_data, export_data(normalized, out_dir = out_dir, out_name = "Processed")),
  
  
  
  # Deletes all the files but not the folder
  #### Cleaning ####
  tar_target(clean, withr::with_dir(out_dir, unlink("TempData",recursive=TRUE)))
)


