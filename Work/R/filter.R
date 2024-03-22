filter_MV <- function(dataset_exp, threshold = 0.8, plot = TRUE) {
  # Plot before filtering
  plot_before <- VIM::aggr(SummarizedExperiment::assay(dataset_exp), plot = FALSE)
  
  # Check if threshold is specified and valid
  if (missing(threshold)) {
    threshold <- 0.8  # Default threshold
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
  
  # Calculate number of rows and columns in the dataset
  nrows <- nrow(SummarizedExperiment::assay(dataset_exp))
  ncols <- ncol(SummarizedExperiment::assay(dataset_exp))
  
  # Calculate the threshold values based on the modified 80% rule
  row_threshold <- ncols * threshold
  col_threshold <- nrows * threshold
  
  # Filter rows based on the modified 80% rule
  row_counts <- rowSums(!is.na(SummarizedExperiment::assay(dataset_exp)))
  dataset_exp <- dataset_exp[row_counts >= row_threshold, ]
  
  # Filter columns based on the modified 80% rule
  col_counts <- colSums(!is.na(SummarizedExperiment::assay(dataset_exp)))
  dataset_exp <- dataset_exp[, col_counts >= col_threshold]
  
  
  # Genereate after plot
  plot_after <- VIM::aggr(SummarizedExperiment::assay(dataset_exp), plot = FALSE)
  
  # Plot before and after
  
  
  # Calculate the number of rows and columns removed
  removed_rows <- nrows - sum(row_counts >= row_threshold)
  removed_cols <- ncols - sum(col_counts >= col_threshold)
  
  # Print information about removed rows and columns
  if (removed_rows == 0) {
    cat('No rows removed\n')
  } else {
    cat(paste0('Number of rows removed: ', removed_rows, '\n'))
  }
  
  if (removed_cols == 0) {
    cat('No columns removed\n')
  } else {
    cat(paste0('Number of columns removed: ', removed_cols, '\n'))
  }
  
  par(mfrow=c(2,2))
  plot(plot_before)
  plot(plot_after)
  par(mfrow=c(1,1))
  
  return(dataset_exp)
}

DE
A <- rsd_filter(rsd_threshold = 20, qc_label= "QC", factor_name = "condition")
A <- model_apply(A,DE)

filtered <- predicted(A)
filtered

