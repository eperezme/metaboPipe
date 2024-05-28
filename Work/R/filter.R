#' Filter Missing values
#' 
#' Filter the missing values in a dataset experiment.
#' 
#' @param dataset_exp The dataset experiment object to filter missing values from.
#' @param threshold The threshold for filtering missing values (default is 0.8).
#' 
#' @return A dataset experiment object with missing values filtered out.
#' 
#' @export
#' 
#' @examples
#' filtered_data <- filter_MV(dataset_exp, threshold = 0.8)
filter_MV <- function(dataset_exp, threshold = 0.8) {
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
  
  M <- mv_sample_filter(mv_threshold = threshold * 100) + mv_feature_filter(threshold = threshold * 100, method = "across", factor_name = "sample_type")
  M <- model_apply(M, dataset_exp)
  
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
  
  return(filtered_experiment)
}

#' Make 0 as NA
#' 
#' Replace 0 values with NA in a dataset experiment.
#' 
#' @param dataset_exp The dataset experiment object.
#' 
#' @return A dataset experiment object with 0 values replaced by NA.
#' 
#' @export
#' 
#' @examples
#' modified_dataset <- zero_to_na(dataset_exp)
zero_to_na <- function(dataset_exp) {
  modified_de <- dataset_exp
  df <- SummarizedExperiment::assay(dataset_exp)
  df <- df %>% mutate_if(is.character, as.numeric)
  df[df == 0] <- NA
  SummarizedExperiment::assay(modified_de, withDimnames = FALSE) <- df
  return(modified_de)
}

#' Filter Blanks
#' 
#' Filter blanks from the dataset experiment.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param fold_change The fold change threshold for blank filtering.
#' @param blank_label The label for blanks.
#' @param qc_label The label for quality control samples.
#' @param factor_name The factor column name.
#' @param fraction_in_blank The fraction of values in blank (default is 0).
#' 
#' @return A dataset experiment object with blanks filtered out.
#' 
#' @export
#' 
#' @examples
#' filtered_data <- filter_blanks(dataset_experiment, fold_change = 20, blank_label = "blank", qc_label = "QC", factor_name = "sample_type", fraction_in_blank = 0)
filter_blanks <- function(dataset_experiment, fold_change = 20, blank_label = "blank", qc_label = "QC", factor_name = "sample_type", fraction_in_blank = 0) {
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

#' Filter Outliers
#' 
#' Filter outliers from the dataset experiment using a Hotelling's T2 distribution ellipse.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for PCA.
#' @param conf.limit The confidence limit for outlier detection. Either 0.95 or 0.99.
#' 
#' @return A dataset experiment object with outliers filtered out.
#' 
#' @export
#' 
#' @examples
#' filtered_data <- filter_outliers(dataset_experiment, nPCs = 5, conf.limit = "0.95")
filter_outliers <- function(dataset_experiment, nPCs = 5, conf.limit = c("0.95", "0.99")) {
  # Check if dataset_experiment is a dataset_experiment object
  if (!inherits(dataset_experiment, "DatasetExperiment")) {
    stop("dataset_experiment must be a DatasetExperiment object")
  }
  # Check if nPCs is numeric
  if (!is.numeric(nPCs)) {
    stop("nPCs must be a numeric value")
  }
  # Check if conf.limit is either 0.95 or 0.99
  if (!conf.limit %in% c("0.95", "0.99")) {
    stop("conf.limit must be either 0.95 or 0.99")
  }
  
  # Perform PCA
  M <- structToolbox::knn_impute() + structToolbox::mean_centre() + structToolbox::PCA(number_components = nPCs)
  M <- structToolbox::model_apply(M, dataset_experiment)
  
  # Extract pca_scores
  pca_scores <- M[3]$scores$data %>% as_tibble()
  
  # Calculate Hotelling's T2 ellipse params
  res_PCs <- HotellingEllipse::ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
  # Extract Hotelling's T2 values
  T2 <- purrr::pluck(res_PCs, "Tsquare", "value")
  
  # Extract cutoff values for Hotelling's T2
  cutoff_99 <- purrr::pluck(res_PCs, "cutoff.99pct")
  cutoff_95 <- purrr::pluck(res_PCs, "cutoff.95pct")
  
  # Select Observations that are above the 99% confidence interval
  outliers_99 <- pca_scores %>%
    mutate(obs = rownames(pca_scores)) %>%
    filter(T2 > cutoff_99)
  
  # Select Observations that are above the 99% confidence interval
  outliers_95 <- pca_scores %>%
    mutate(obs = rownames(pca_scores)) %>%
    filter(T2 > cutoff_95)
  
  
  # Remove outliers from experiment
  if (conf.limit == "0.95") {
    FT <- structToolbox::filter_by_name(mode = "exclude", dimension = "sample", outliers_95$obs)
    Filtered <- structToolbox::model_apply(FT, dataset_experiment)
  }
  if (conf.limit == "0.99") {
    FT <- structToolbox::filter_by_name(mode = "exclude", dimension = "sample", outliers_99$obs)
    Filtered <- structToolbox::model_apply(FT, dataset_experiment)
  }
  
  return(predicted(Filtered))
}
