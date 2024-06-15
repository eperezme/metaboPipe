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
# Declare controller
# Create a controller with 5 workers and a 3-second idle time.
# controller <- crew::crew_controller_local(
#   name = "Controller",
#   workers = 2,
#   seconds_idle = 3
# )
# tar_option_set(controller = controller)
#### Global variables #####

out_dir <- 'C:/Users/eduar/Desktop/TEST/Transformed'
dataMatrixPath <- 'C:/Users/eduar/AppData/Local/R/cache/R/renv/library/metaboPipe-e56bca86/R-4.3/x86_64-w64-mingw32/metaboPipe/data/ST000284/dataMatrix.csv'
sampleMetadataPath <- 'C:/Users/eduar/AppData/Local/R/cache/R/renv/library/metaboPipe-e56bca86/R-4.3/x86_64-w64-mingw32/metaboPipe/data/ST000284/sampleMetadata.csv'
variableMetadataPath <- 'C:/Users/eduar/AppData/Local/R/cache/R/renv/library/metaboPipe-e56bca86/R-4.3/x86_64-w64-mingw32/metaboPipe/data/ST000284/variableMetadata.csv'
dataSep <- ','
sampleSep <- ','
variableSep <- ','

# Columns settings
factor_cols <- c('sample_type', 'Groups', 'Age', 'Gender')
sample_id_col <- 'sample_id'
sample_type_col <- 'sample_type'
group_col <- 'Groups'
qc_label <- 'Sample'
order_col <- 'run_order'
batch_col <- 'Batch'


dir.create(out_dir, showWarnings = FALSE)
out_dir <- tools::file_path_as_absolute(out_dir)
##### DEFINE THE PIPELINE ######
list(
# LOAD THE DATA
load_data(data, dataMatrixPath, sampleMetadataPath, dataSep = dataSep, sampleSep = sampleSep, variableSep = variableSep),

# Create a DatasetExperiment object
create_experiment(experiment, data),

# Factorize the cols
factorize_cols(factorized, experiment, factor_cols),

filter_step(filtered_1, factorized, threshold = 0.2, filter_outliers = TRUE, conf.limit = '0.95', out_dir = out_dir),
# FIRST BRANCH
impute(imputed_2, filtered_1, method = 'kNN', k = 5),
batch_correct(batch_corrected_1, imputed_2,  method = 'ComBat', order_col = order_col, batch_col = batch_col, qc_col = sample_type_col, qc_label = qc_label),
normalize(normalized_3, batch_corrected_1, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', transNorm = 'LogNorm', scaleNorm = 'AutoNorm', ref = 'Creatine', out_dir = out_dir),

# FIRST SUB BRANCH
normalize(normalized_4, batch_corrected_1, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'NULL', transNorm = 'NULL', scaleNorm = 'AutoNorm', ref = '', out_dir = out_dir),

normalize(normalized_5, normalized_4, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', transNorm = 'LogNorm', scaleNorm = 'NULL', ref = 'Creatine', out_dir = out_dir),

# SECOND BRANCH
batch_correct(batch_corrected_2, filtered_1,  method = 'ComBat', order_col = order_col, batch_col = batch_col, qc_col = sample_type_col, qc_label = qc_label),
impute(imputed_after_batch_KNN, batch_corrected_2, method = 'kNN', k = 5),
normalize(normalized_KNN, batch_corrected_2, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', transNorm = 'LogNorm', scaleNorm = 'AutoNorm', ref = 'Creatine', out_dir = out_dir),

# SECOND SUB BRANCH
impute(imputed_after_batch_RF, batch_corrected_2, method = 'RF'),
normalize(normalized_RF, imputed_after_batch_RF, factor_col = group_col, sample_id_col = sample_id_col, rowNorm = 'CompNorm', transNorm = 'LogNorm', scaleNorm = 'AutoNorm', ref = 'Creatine', out_dir = out_dir),

#### EXTRACTION ####
# Extract the data
export_data(Branch1, normalized_3, out_dir = out_dir),
export_data(Branch1_1, normalized_5, out_dir = out_dir),
export_data(Branch2_knn, normalized_KNN, out_dir = out_dir),
export_data(Branch2_rf, normalized_RF, out_dir = out_dir),

#### Cleaning ####
tar_target(clean, withr::with_dir(out_dir, unlink('TempData', recursive = TRUE)))
)
