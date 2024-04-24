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
    "VIM", "impute", "imputeLCMD", "missForest", "caret", "pcaMethods", "tidyverse"
  )
)

#### GLOBAL VARIABLES ####
# MTBLS79 DATASET
dataMatrixPath <- "data/MTBLS79/data.csv"
sampleMetadataPath <- "data/MTBLS79/sample_meta.csv"
variableMetadataPath <- "data/MTBLS79/variable_meta.csv"


na_threshold <- 0.8

# Columns setting
factor_cols <- c("Type", "Batch", "Class", "batch_qc")
sample_type_col <- 'Type'
order_col <- 'run_order'
batch_col <- 'Batch'


# blank filter
blank_label <- NA
qc_label <- 'QC'
fold_change <- 20





# OTHER DATASET
dataMatrixPath <- "data/dataMatrix.csv"
sampleMetadataPath <- "data/sampleMetadata.csv"
variableMetadataPath <- "data/variableMetadata.csv"
factor_cols <- c("sample_type", "biol.batch")
sample_type_col <- 'sample_type'
order_col <- 'order'
batch_col <- NA


# blank filter
blank_label <- NA
qc_label <- 'QC'
fold_change <- 20


# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
# controller <- crew::crew_controller_local(
#   name = "Controller",
#   workers = 2,
#   seconds_idle = 3
# )
# tar_option_set(controller = controller)


# Define the pipeline.
list(
  # LOAD THE DATA
  tar_file_read(dataMatrix, dataMatrixPath, read.csv(!!.x, sep = ";")),
  tar_file_read(sampleMetadata, sampleMetadataPath, read.csv(!!.x)),
  tar_file_read(variableMetadata, variableMetadataPath, read.csv(!!.x)),

  
  # Create a DatasetExperiment object
  tar_target(raw_experiment, createExperiment(dataMatrix, sampleMetadata, variableMetadata)),

  tar_target(experiment, factor_sample_col(raw_experiment, factor_cols)),
  
  
  #### FILTERING ####
  # Filter missing values
  tar_target(na_experiment, zero_to_na(experiment)),
  tar_target(filtered_experiment, filter_MV(na_experiment, na_threshold)),
  
  # blank filter
  tar_skip(blank_filtered, filter_blanks(filtered_experiment, fold_change = fold_change, 
                                           blank_label = blank_label, qc_label = qc_label, 
                                           factor_name = sample_type, fraction_in_blank = 0 ),
           skip = is.na(blank_label)),
  
  
  
  
  #### BATCH CORRECTION ####

  tar_skip(batch_corrected, batch_correction(filtered_experiment, 
                                               order_col = order_col, 
                                               batch_col = batch_col, 
                                               qc_col = sample_type_col, 
                                               qc_label = qc_label),
           skip = is.na(batch_col) | is.na(order_col) | is.na(sample_type_col) | is.na(qc_label)),
  
  
  
  #### NORMALIZATION ####
  tar_target(normalized, normalize_pqn(filtered_experiment, qc_label, sample_type_col)),
  
  
  
  #### IMPUTE ####
  # impute missing values
  tar_target(mean_imputed, impute_mean(batch_corrected)),
  tar_target(median_imputed, impute_median(batch_corrected)),
  tar_target(RF_imputed, impute_RF(batch_corrected)),
  tar_target(QRILC_imputed, impute_QRILC(batch_corrected)),
  tar_target(knn_imputed, impute_kNN(batch_corrected, 5)),
  tar_target(svd_imputed, impute_SVD(batch_corrected, k = 5)),
  tar_target(bpca_imputed, impute_bpca(batch_corrected, nPCs = 5)),
  tar_target(ppca_imputed, impute_ppca(batch_corrected, nPCs = 5))
  
  # Remove outliers
  # somehow
  
  
  
  


  
  
  
  
  # The report summary
  # tar_quarto(
  #   processing_report,
  #   path = "processing_report.qmd",
  #   quiet = FALSE,
  #   packages = c("targets", "tidyverse")
  # )
)
