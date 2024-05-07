# Load data from files into data frames
load_data <- function(output_name, dataMatrixFile, sampleMetadataFile, variableMetadataFile = NULL, separator = ",") {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  name_data <- paste0(target_name, "_data") # Generate the name for the data target
  name_sample <- paste0(target_name, "_sample") # Generate the name for the sample target
  name_variable <- paste0(target_name, "_variable") # Generate the name for the variable target

  # Define targets to load and read data matrix and sample metadata
  list(
    # Target to load data matrix file
    tar_target_raw("matrixFile", dataMatrixFile, format = "file", deployment = "main"),
    # Target to read data matrix
    tar_target_raw(name_data, quote(read.csv(matrixFile, sep = separator, header = TRUE)), format = "fst_tbl", deployment = "main"),
    # Target to load sample metadata file
    tar_target_raw("sampleFile", sampleMetadataFile, format = "file", deployment = "main"),
    # Target to read sample metadata
    tar_target_raw(name_sample, quote(read.csv(sampleFile, sep = separator)), format = "fst_tbl", deployment = "main"),
    # If variable metadata file is provided
    if (!is.null(variableMetadataFile)) {
      list(
        # Target to load variable metadata file
        tar_target_raw("variableFile", variableMetadataFile, format = "file", deployment = "main"),
        # Target to read variable metadata
        tar_target_raw(name_variable, quote(read.csv(variableFile, sep = separator)), format = "fst_tbl", deployment = "main")
      )
    } else { # If variable metadata file is not provided
      list(
        # Target to read data matrix without header
        tar_target_raw("headDataMatrix", quote(read.csv(matrixFile, sep = separator, header = FALSE)), format = "fst_tbl", deployment = "main"),
        # Target to create variable metadata from data matrix
        tar_target_raw(name_variable, quote(extract_names(headDataMatrix)), format = "fst_tbl", deployment = "main")
      )
    }
  )
}

# Create DatasetExperiment object
createExperiment <- function(output_name, data,
                             experiment_name = "Name", experiment_description = "Description") {
  name_data <- deparse(substitute(data)) # Get the name of the input data
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  data_matrix <- paste0(name_data, "_data") # Generate the name for the data target
  data_sample <- paste0(name_data, "_sample") # Generate the name for the sample target
  data_variable <- paste0(name_data, "_variable") # Generate the name for the variable target
  # Define target to create DatasetExperiment object
  command_experiment <- substitute(warper_createExperiment(data_matrix, data_sample, data_variable, experiment_name, experiment_description), env = list(data_matrix = as.name(data_matrix), data_sample = as.name(data_sample), data_variable = as.name(data_variable), experiment_name = experiment_name, experiment_description = experiment_description))
  list(
    tar_target_raw(target_name, command_experiment, format = "qs", deployment = "main")
  )
}

# Factorize columns in sample metadata
factorize_cols <- function(output_name, input_name, cols) {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  data <- deparse(substitute(input_name)) # Get the name of the input data
  # Define target to factorize columns
  command_factor <- substitute(
    warper_factor_sample_col(data, col),
    env =
      list(data = as.name(data), col = cols)
  )
  list(
    tar_target_raw(target_name, command_factor, format = "qs", deployment = "main")
  )
}

# Filter data by missing value threshold
filter_step <- function(output_name, input_name, threshold, filter_outliers = TRUE, conf.limit = "0.95", out_dir) {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  input_target <- deparse(substitute(input_name)) # Get the name of the input data
  name_mv <- paste0(target_name, "_missingval") # Generate the name for the filtered data target
  name_na <- paste0(input_target, "_na") # Generate the name for the target with NAs replaced by zeros
  plot_before_name <- paste0("before_", target_name, "_plot") # Generate the name for the plot target before filtering
  plot_after_name <- paste0("after_", target_name, "_plot") # Generate the name for the plot target after filtering
  plot_outliers_name <- paste0(target_name, "_outliers_plot") # Generate the name for the outliers plot target
  
  # Define targets to save plots
  command_plot_mv_before <- substitute(missing_values_plot(dataset_experiment, out_dir, name), 
                                    env = list(dataset_experiment = as.name(input_target), 
                                               out_dir=out_dir, name = "before_filter_mv"))
  
  command_plot_mv_after <- substitute(missing_values_plot(dataset_experiment, out_dir, name), 
                                   env = list(dataset_experiment = as.name(target_name), 
                                              out_dir=out_dir, name = "after_filter_mv"))
  
  command_plot_outliers1 <- substitute(plot_outliers(dataset_experiment, nPCs = 5, out_dir, name),
                                          env = list(dataset_experiment = as.name(name_mv),
                                                     out_dir=out_dir, name = "outliers"))
  command_plot_outliers2 <- substitute(plot_outliers(dataset_experiment, nPCs = 5, out_dir, name),
                                          env = list(dataset_experiment = as.name(target_name),
                                                     out_dir=out_dir, name = "outliers"))
  
  command_na <- substitute(zero_to_na(dataset_experiment), 
                           env = list(dataset_experiment = as.name(input_target)))
  
  command_filter_mv <- substitute(filter_MV(dataset_experiment, threshold = threshold), 
                                  env = list(dataset_experiment = as.name(name_na), threshold = threshold))
  
  command_filter_outl <- substitute(filter_outliers(dataset_experiment, conf.limit = conf.limit), 
                                    env = list(dataset_experiment = as.name(name_mv), conf.limit = conf.limit, threshold = threshold
                                               ))
  
  
  
  # Define list of targets
  list(
    tar_target_raw(name_na, command_na, format = "qs", deployment = "main"),
    if(filter_outliers==TRUE) {
      list(
        tar_target_raw(name_mv, command_filter_mv, format = "qs", deployment = "main"),
        tar_target_raw(target_name, command_filter_outl, format = "qs", deployment = "main"),
        tar_target_raw(plot_outliers_name, command_plot_outliers1, format = "qs", deployment = "main", 
                       cue = tar_cue(mode = "always"))

      )
    } else {
      list(
      tar_target_raw(target_name, command_filter_mv, format = "qs", deployment = "main"),
      tar_target_raw(plot_outliers_name, command_plot_outliers2, format = "qs", deployment = "main", 
                     cue = tar_cue(mode = "always"))
      )
    },
    tar_target_raw(plot_before_name, command_plot_mv_before, format = "qs", deployment = "main", cue = tar_cue(mode = "always")),
    tar_target_raw(plot_after_name, command_plot_mv_after, format = "qs", deployment = "main", cue = tar_cue(mode = "always"))
  )
}

# Batch correction
batch_correct <- function(output_name, input_name, order_col, batch_col, qc_col, qc_label) {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  data <- deparse(substitute(input_name)) # Get the name of the input data
  # Define target to perform batch correction
  command_correct <- substitute(
    warper_batch_correction(data, order_col, batch_col, qc_col, qc_label),
    env =
      list(data = as.name(data), order_col = order_col, batch_col = batch_col, qc_col = qc_col, qc_label = qc_label)
  )
  list(
    tar_target_raw(target_name, command_correct, format = "qs", deployment = "main")
  )
}

# Impute missing values
impute <- function(output_name, input_name, method, k = 5) {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  data <- deparse(substitute(input_name)) # Get the name of the input data
  # Define target to impute missing values
  command <- substitute(impute_warper(data, method = method, k = k),
    env =
      list(data = as.name(data), method = method, k = k)
  )
  list(
    tar_target_raw(target_name, command, format = "qs", deployment = "main")
  )
}

# Normalize data
normalize <- function(output_name, input_name, factor_col, sample_id_col, rowNorm = NULL, transNorm = NULL, scaleNorm = NULL, ref = NULL, out_dir) {
  target_name <- deparse(substitute(output_name)) # Get the name of the output target
  data <- deparse(substitute(input_name)) # Get the name of the input data
  # Define target to normalize data
  command <- substitute(normalize_metab(data, factor_col, sample_id_col = sample_id_col,
                                        rowNorm, transNorm, scaleNorm, ref = ref, out_dir = out_dir),
                        env =
                          list(data = as.name(data), factor_col = factor_col, sample_id_col = sample_id_col, 
                               rowNorm = rowNorm, transNorm = transNorm, scaleNorm = scaleNorm, ref = ref, out_dir = out_dir)
  )
  list(
    tar_target_raw(target_name, command, format = "qs", deployment = "main"),
    
    tar_target_raw("Clean_normalization", substitute(withr::with_dir(out_dir, unlink("TempData",recursive=TRUE)), 
                                          env = list(out_dir = out_dir)), 
                   format = "qs", deployment = "main", cue = tar_cue(mode = "always"), deps = target_name)
  )
}
