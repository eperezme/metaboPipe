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
    "VIM", "impute", "imputeLCMD", "missForest", "caret", "pcaMethods", "metabolomicsWorkbenchR", "tidyverse"
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

dataMatrixPath <- "data/dataMatrix.csv"
sampleMetadataPath <- "data/sampleMetadata.csv"
variableMetadataPath <- "data/variableMetadata.csv"

# Define the pipeline.
list(
  tar_file_read(dataMatrix, dataMatrixPath, read.csv(!!.x)),
  tar_file_read(sampleMetadata, sampleMetadataPath, read.csv(!!.x)),
  tar_file_read(variableMetadata, variableMetadataPath, read.csv(!!.x)),

  # Create a DatasetExperiment object
  # tar_target(experiment, createExperiment(dataMatrix, sampleMetadata, variableMetadata)),
  
  tar_target(experiment, metabolomicsWorkbenchR::do_query(context = "study", input_item = "analysis_id", input_value = "AN004436", output_item = "DatasetExperiment")),

  # Filter missing values
  tar_target(na_experiment, zero_to_na(experiment)),
  tar_target(filtered_experiment, filter_MV(na_experiment)),

  # impute missing values
  # tar_target(mai_imputed_experiment, impute_MAI(filtered_experiment, "random_forest", "Single"))
  tar_target(mean_imputed, impute_mean(filtered_experiment)),
  tar_target(median_imputed, impute_median(filtered_experiment)),
  tar_target(RF_imputed, impute_RF(filtered_experiment)),
  tar_target(QRILC_imputed, impute_QRILC(filtered_experiment)),
  tar_target(knn_imputed, impute_kNN(filtered_experiment, 5)),
  tar_target(svd_imputed, impute_SVD(filtered_experiment, k = 5)),
  tar_target(bpca_imputed, impute_bpca(filtered_experiment, nPCs = 5)),
  tar_target(ppca_imputed, impute_ppca(filtered_experiment, nPCs = 5))
  
  # The report summary
  # tar_quarto(
  #   processing_report,
  #   path = "processing_report.qmd",
  #   quiet = FALSE,
  #   packages = c("targets", "tidyverse")
  # )
)
