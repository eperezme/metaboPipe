## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy.opts = list(width.cutoff = 60), 
  tidy = TRUE,
  dpi=96,fig.width=5,fig.height=5.5,fig.retina = 1,fig.small = TRUE
)

set.seed(57475)

## ----installation, eval=FALSE-------------------------------------------------
#  # Install the package
#  install.packages("devtools")
#  devtools::install_github("https://github.com/eperezme/metaboPipe", subdir = "Work")

## -----------------------------------------------------------------------------
DE <- metaboPipe::ST000284
DE

## ----eval=FALSE---------------------------------------------------------------
#  head(DE$data[, 1:4])
#  head(DE$sample_meta[, 1:4])
#  head(DE$variable_meta)

## ----eval=FALSE---------------------------------------------------------------
#  create_pipeline()

## ----eval=FALSE---------------------------------------------------------------
#  # Load packages required to define the pipeline:
#  library(targets)
#  library(tarchetypes)
#  library(metaboPipe)
#  
#  tar_option_set(
#    packages = c(
#      "structToolbox", "SummarizedExperiment", "VIM", "impute", "imputeLCMD",
#      "missForest", "caret", "pcaMethods", "tidyverse", "MetaboAnalystR", "tinytex",
#      "HotellingEllipse", "ggforce", "tools", "cowplot", "metaboPipe"
#    )
#  )

## ----eval=FALSE---------------------------------------------------------------
#  list(
#    tar_target(dataMatrix, read.csv("data/ST000284/dataMatrix.csv")),
#    tar_target(sampleMeta, read.csv("data/ST000284/sampleMetadata.csv")),
#    tar_target(variableMetadata, read.csv("data/ST000284/variableMetadata.csv")),
#    tar_target(experiment, warper_createExperiment(dataMatrix, sampleMeta, variableMetadata, experiment_name = "ST000284", experiment_description = "Example metabolomics dataset ST000284"))
#  )

## ----eval=FALSE---------------------------------------------------------------
#  tar_load(experiment)

## ----load_data_exp, eval=FALSE------------------------------------------------
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath),
#    createExperiment(experiment, data_loaded, experiment_name = "ST000284", experiment_description = "Example metabolomics dataset ST000284")
#  )

## ----load, eval=FALSE---------------------------------------------------------
#  tar_load(experiment)

## ----load_separators, eval=FALSE----------------------------------------------
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description)
#  )

## ----factorize, eval=FALSE----------------------------------------------------
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns)
#  )

## ----filter, eval=FALSE-------------------------------------------------------
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = "results")
#  )

## ----impute, eval=FALSE-------------------------------------------------------
#  # General config
#  outdir = "results"
#  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
#  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility
#  
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
#    impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5) # Impute using Random forest
#  )

## ----normalize, eval=FALSE----------------------------------------------------
#  # General config
#  outdir = "results"
#  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
#  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility
#  
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
#    impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5),
#    normalize(normalized_experiment, imputed_experiment, factor_col = "Groups", sample_id_col = "sample_id", rowNorm = 'CompNorm', ref = 'Creatine..132.1...90.0.', out_dir = outdir)
#  )
#  

## ----scaling, eval=FALSE------------------------------------------------------
#  # General config
#  outdir = "results"
#  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
#  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility
#  
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  factor_col = "Groups"
#  sample_id_col = "sample_id"
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
#    impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5),
#    normalize(normalized_experiment, imputed_experiment, factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', ref = 'Creatine..132.1...90.0.', out_dir = outdir),
#    normalize(scaled_experiment, normalized_experiment,factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm", out_dir = outdir)
#  )

## ----transform, eval=FALSE----------------------------------------------------
#  # General config
#  outdir = "results"
#  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
#  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility
#  
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  factor_col = "Groups"
#  sample_id_col = "sample_id"
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
#    impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5),
#    normalize(normalized_experiment, imputed_experiment, factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', ref = 'Creatine (132.1 / 90.0)', out_dir = outdir),
#    normalize(scaled_experiment, normalized_experiment,factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm", out_dir = outdir),
#    normalize(transformed_experiment, scaled_experiment, factor_col = factor_col, sample_id_col = sample_id_col, transNorm = "LogNorm", out_dir= outdir)
#  )

## ----export, eval=FALSE-------------------------------------------------------
#  # General config
#  outdir = "results"
#  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
#  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility
#  
#  # Load the Data
#  dataMatrixPath <- "data/ST000284/dataMatrix.csv"
#  sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
#  variableMetadataPath <- "data/ST000284/variableMetadata.csv"
#  dataSep <- ","
#  sampleSep <- ","
#  variableSep <- ","
#  
#  # Create the experiment
#  name <- "ST000284"
#  description <- "Example metabolomics dataset ST000284"
#  
#  # Setting up the columns
#  columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
#  factor_col = "Groups"
#  sample_id_col = "sample_id"
#  
#  
#  list(
#    load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath, dataSep, sampleSep, variableSep),
#    createExperiment(experiment, data_loaded, experiment_name = name, experiment_description = description),
#    factorize_cols(factorized_experiment, experiment, columns),
#    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
#    impute(imputed_experiment, filtered_experiment, method = 'RF', k = 5),
#    normalize(normalized_experiment, imputed_experiment, factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', ref = 'Creatine (132.1 / 90.0)', out_dir = outdir),
#    normalize(scaled_experiment, normalized_experiment,factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm", out_dir = outdir),
#    normalize(transformed_experiment, scaled_experiment, factor_col = factor_col, sample_id_col = sample_id_col, transNorm = "LogNorm", out_dir= outdir),
#    exportData(export, transformed_experiment, out_dir= outdir)
#  )

## ----tar_check, paged.print=FALSE---------------------------------------------
targets::tar_visnetwork(targets_only = TRUE)
targets::tar_manifest()

## ----run_pipeline, eval=FALSE-------------------------------------------------
#  targets::tar_make()

## ----shiny, eval=FALSE--------------------------------------------------------
#  metaboPipe::pipePliers()

## ----sessionInfo--------------------------------------------------------------
sessionInfo()

