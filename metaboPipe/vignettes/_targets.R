# _targets.R file:
library(targets)
library(tarchetypes)
library(crew)
library(metaboPipe)
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
    "HotellingEllipse", "ggforce", "tools", "cowplot", "metaboPipe", "sva", "data.table", "pcpr2"
  )
)

# tar_source()
# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
# controller <- crew::crew_controller_local(
#   name = "Controller",
#   workers = 2,
#   seconds_idle = 3
# )
# tar_option_set(controller = controller)
#### Global variables #####
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
factor_col = "Groups"
sample_id_col = "sample_id"


list(
  load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
  create_experiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
  impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5),
  normalize(normalized_experiment, imputed_experiment, factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', ref = 'Creatine (132.1 / 90.0)', out_dir = outdir),
  normalize(scaled_experiment, normalized_experiment,factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm", out_dir = outdir),
  normalize(transformed_experiment, scaled_experiment, factor_col = factor_col, sample_id_col = sample_id_col, transNorm = "LogNorm", out_dir= outdir),
  export_data(export, transformed_experiment, out_dir= outdir)
)
