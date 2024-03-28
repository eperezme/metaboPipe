# _targets.R file:
library(targets)
library(crew)
# library(doParallel)
# Load all R scripts in the R/ directory.
file.sources <- list.files("R", pattern = "*.R", full.names=TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))

# Declare libraries
# These are the libraries that the pipeline depends on.

# To create a list with all the libraries to copy-paste:
# cat(paste(shQuote(unique(renv::dependencies(path = "R")$Package), type="cmd"), collapse=", "))

tar_option_set(
  packages = c("structToolbox", "SummarizedExperiment", 
               "VIM", "impute", "imputeLCMD", "missForest", "doParallel"))



# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
controller <- crew::crew_controller_local(
  name = "Controller",
  workers = 5,
  seconds_idle = 3
)

dataMatrixPath <- "data/dataMatrix.csv"
sampleMetadataPath <- "data/sampleMetadata.csv"
variableMetadataPath <- "data/variableMetadata.csv"

# Define the pipeline.
tar_option_set(controller = controller)
list(

  # Define the paths to the data files
  tar_target(dataMatrixFile, dataMatrixPath, format = "file"),
  tar_target(sampleMetadataFile, sampleMetadataPath, format = "file"),
  tar_target(variableMetadataFile, variableMetadataPath, format = "file"),
  
  # Read the data files
  tar_target(dataMatrix, read.csv(dataMatrixFile, ), format = "feather"),
  tar_target(sampleMetadata, read.csv(sampleMetadataFile), format = "feather"),
  tar_target(variableMetadata, read.csv(variableMetadataFile), format = "feather"),
  
  
  # Create a DatasetExperiment object
  tar_target(experiment, createExperiment(dataMatrix, sampleMetadata, variableMetadata)),

  # Filter missing values
  tar_target(filtered_experiment, filter_MV(experiment)),
  
  # impute missing values
  tar_target(QRILC_imputed_experiment, impute_QRILC(filtered_experiment)),
  tar_target(RF_imputed_experiment, impute_RF(filtered_experiment))
  # tar_target(knn_imputed_experiment, impute_kNN(filtered_experiment))
)
