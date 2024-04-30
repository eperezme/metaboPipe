# Filter Missing values

filter_MV <- function(dataset_exp, threshold = 0.8) {
  # IDEA
  # Filter SAMPLES by Blank
  # and filter Features by 
  
  # Check if threshold is specified and valid
  if (missing(threshold)) {
    threshold <- 0.8 # Default threshold
    cat("No threshold specified. Defaulting to 0.8.\n")
  } else {
    # Check if threshold is between 0 and 1
    if (threshold < 0 || threshold > 1) {
      stop("Threshold must be between 0 and 1.")
    }
    # Check if threshold is a numeric value
    if (!is.numeric(threshold)) {
      stop("Threshold must be a numeric value.")
    }
    cat(paste0("Threshold value: ", threshold, "\n"))
  }
  
  ncols <- ncol(SummarizedExperiment::assay(dataset_exp))
  
  
  M <- mv_sample_filter(mv_threshold = threshold*100) + mv_feature_filter(threshold = threshold*100, method = "across", factor_name = 'sample_type')
  # C <- mv_sample_filter_hist()
  M = model_apply(M, dataset_exp)
  # chart_plot(C, M[2])
  
  filtered_experiment <- predicted(M)
  
  # Calculate the number of rows and columns removed
  removed_rows <- nrow(SummarizedExperiment::assay(dataset_exp)) - nrow(SummarizedExperiment::assay(filtered_experiment))
  removed_cols <- ncol(SummarizedExperiment::assay(dataset_exp)) - ncol(SummarizedExperiment::assay(filtered_experiment))
  
  # Print information about removed rows and columns
  if (removed_rows == 0) {
    cat("No rows removed\n")
  } else {
    cat(paste0("Number of rows removed: ", removed_rows, "\n"))
  }
  
  if (removed_cols == 0) {
    cat("No columns removed\n")
  } else {
    cat(paste0("Number of columns removed: ", removed_cols, "\n"))
  }
  
  # 
  # # Plot before filtering
  # plot_before <- VIM::aggr(SummarizedExperiment::assay(dataset_exp), plot = plot)

  # # Calculate the threshold values based on the modified 80% rule
  # row_threshold <- ncols * threshold
  # col_threshold <- nrows * threshold
  # 
  # # Filter rows based on the modified 80% rule
  # row_counts <- rowSums(!is.na(SummarizedExperiment::assay(dataset_exp)))
  # dataset_exp <- dataset_exp[row_counts >= row_threshold, ]
  # 
  # # Filter columns based on the modified 80% rule
  # col_counts <- colSums(!is.na(SummarizedExperiment::assay(dataset_exp)))
  # dataset_exp <- dataset_exp[, col_counts >= col_threshold]
  # 
  # 
  # # Genereate after plot
  # plot_after <- VIM::aggr(SummarizedExperiment::assay(dataset_exp), plot = plot)
  # 
  # # Plot before and after
  # 
  # 

  # 
  # # par(mfrow=c(2,2))
  # # plot(plot_before)
  # # plot(plot_after)
  # # par(mfrow=c(1,1))

  return(filtered_experiment)
}
# Make 0 as NA
zero_to_na <- function(dataset_exp) {
  modified_de <- dataset_exp
  df <- SummarizedExperiment::assay(dataset_exp)
  df <- df %>% mutate_if(is.character, as.numeric)
  df[df == 0] <- NA
  SummarizedExperiment::assay(modified_de, withDimnames = FALSE) <- df
  return(modified_de)
}


filter_blanks <- function(dataset_experiment, fold_change = 20, blank_label = 'blank', qc_label = 'QC', factor_name = 'sample_type', fraction_in_blank = 0) {
  M <- blank_filter(
    fold_change = fold_change,
    blank_label = blank_label,
    qc_label = qc_label,
    factor_name = factor_name,
    fraction_in_blank = fraction_in_blank
  )
  M <- model_apply(M, dataset_experiment)
  filtered_experiment <- predicted(M)
  
  return(filtered_experiment)
}

#
#
# # HOW MANY FOLD CHANGES? DEPENDS ON SAMPLES? ON METABOLITES? ON PPM AND PEAK?
# M = blank_filter(
#   fold_change = 1,
#   blank_label = "Blank",
#   qc_label = "QC",
#   factor_name = "sample_type",
#   fraction_in_blank = 0)
#
# M = model_apply(M,filtered_experiment)
#
# test <- predicted(M)
#
# test
#
# blank_filter_hist(M)

# DE
# A <- rsd_filter(rsd_threshold = 20, qc_label= "QC", factor_name = "condition")
# A <- model_apply(A,DE)
#
# filtered <- predicted(A)
# filtered
# sb_corrected<- batch_correction(blank_filtered, 
#                  order_col = "order", 
#                  batch_col = "biol.batch", 
#                  qc_col = "sample_type", 
#                  qc_label = 'QC')


