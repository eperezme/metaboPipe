
load_data <- function(output_name, dataMatrixFile, sampleMetadataFile, variableMetadataFile = NULL, separator = ",") {
  target_name <- deparse(substitute(output_name))
  name_data <- paste0(target_name, "_data")
  name_sample <- paste0(target_name, "_sample")
  name_variable <- paste0(target_name, "_variable")
  
  list(
    
    tar_target_raw("matrixFile", dataMatrixFile, format = "file", deployment = "main"),
    tar_target_raw(name_data, quote(read.csv(matrixFile, sep = separator, header = TRUE)), format = "fst_tbl", deployment = "main"),
    
    
    tar_target_raw("sampleFile", sampleMetadataFile, format = "file", deployment = "main"),
    tar_target_raw(name_sample, quote(read.csv(sampleFile, sep = separator)), format = "fst_tbl", deployment = "main"),
    
    if(!is.null(variableMetadataFile)) {
      list(
        tar_target_raw("variableFile", variableMetadataFile, format = "file", deployment = "main"),
        tar_target_raw(name_variable, quote(read.csv(variableFile, sep = separator)), format = "fst_tbl", deployment = "main")
      )
    } else {
      list(
        tar_target_raw("headDataMatrix", quote(read.csv(matrixFile, sep = separator, header = FALSE)), 
                       format = "fst_tbl", deployment = "main"),
        tar_target_raw(name_variable, quote(data.frame('annotation' = t(headDataMatrix))), 
                       format = "fst_tbl", deployment = "main")
      )
    }
  )
}

createExperiment <- function(output_name, data,
                             experiment_name = "Name", experiment_description = "Description") {
  name_data <- deparse(substitute(data)) 
  target_name <- deparse(substitute(output_name))
  data_matrix <- paste0(name_data, "_data")
  data_sample <- paste0(name_data, "_sample")
  data_variable <- paste0(name_data, "_variable")
  command_experiment <- substitute(warper_createExperiment(data_matrix, data_sample, data_variable, experiment_name, experiment_description), env = list(data_matrix = as.name(data_matrix), data_sample = as.name(data_sample), data_variable = as.name(data_variable), experiment_name = experiment_name, experiment_description = experiment_description))
  list(
    tar_target_raw(target_name, command_experiment, format = "qs", deployment = "main")
  )
}

factorize_cols <- function(output_name, input_name, cols) {
  target_name <- deparse(substitute(output_name))
  data <- deparse(substitute(input_name))
  command_factor <- substitute(
    warper_factor_sample_col(data, col), env =
      list(data = as.name(data), col = cols)
  )
  list(
    tar_target_raw(target_name, command_factor, format = "qs", deployment = "main")
  )
}

filter <- function(output_name, input_name, threshold) {
  output_name <- deparse(substitute(output_name))
  input_name <- deparse(substitute(input_name))
  plot_before_name <- paste0("before_", output_name, "_plot")
  plot_after_name <- paste0("after_", output_name, "_plot")
  command_plot_before <- substitute(save_plot(input_name, plot_before_name), env = list(input_name = as.name(input_name), plot_before_name = plot_before_name))
  command_plot_after <- substitute(save_plot(output_name, plot_after_name), env = list(output_name = as.name(output_name), plot_after_name = plot_after_name))
  name_na <- paste0(input_name, "_na")
  command_na <- substitute(zero_to_na(input_name), env = list(input_name = as.name(input_name)))
  command_filter <- substitute(filter_MV(name_na, threshold), env = list(name_na = as.name(name_na), threshold = threshold))
  
  
  list(
    tar_target_raw(name_na, command_na, format = "qs", deployment = "main"),
    tar_target_raw(output_name, command_filter, format = "qs", deployment = "main"),
    tar_target_raw(plot_before_name, command_plot_before, format = "qs", deployment = "main", cue = tar_cue(mode="always")),
    tar_target_raw(plot_after_name, command_plot_after, format = "qs", deployment = "main", cue = tar_cue(mode="always"))
    )
  
}


batch_correct <- function(output_name, input_name, order_col, batch_col, qc_col, qc_label) {
  target_name <- deparse(substitute(output_name))
  data <- deparse(substitute(input_name))
  command_correct <- substitute(
    warper_batch_correction(data, order_col, batch_col, qc_col, qc_label), env =
      list(data = as.name(data), order_col = order_col, batch_col = batch_col, qc_col = qc_col, qc_label = qc_label)
  )
  list(
    tar_target_raw(target_name, command_correct, format = "qs", deployment = "main")
  )
}

#### IMPUTE ####
impute <- function(output_name, input_name, method, k = 5) {
  target_name <- deparse(substitute(output_name))
  data <- deparse(substitute(input_name))
  command <- substitute(impute_warper(data, method = method, k = k), env =
                          list(data = as.name(data), method = method, k = k))
  list(
    tar_target_raw(target_name, command, format = "qs", deployment = "main")
  )
}


#### Normalize ####
normalize <- function(output_name, input_name, factor_col, sample_id_col, rowNorm = NULL, transNorm = NULL, scaleNorm = NULL, ref = NULL) {
  target_name <- deparse(substitute(output_name))
  data <- deparse(substitute(input_name))
  command <- substitute(normalize_metab(data, factor_col, sample_id_col = sample_id_col, rowNorm, transNorm, scaleNorm, ref = ref), env =
                          list(data = as.name(data), factor_col=factor_col, sample_id_col = sample_id_col, rowNorm = rowNorm, transNorm = transNorm, scaleNorm = scaleNorm, ref = ref))
  list(
    tar_target_raw(target_name, command, format = "qs", deployment = "main")
  )
}
