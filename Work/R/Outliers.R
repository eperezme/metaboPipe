outliers <- function(dataset_experiment, nPCs = 5, conf.limit = c("0.95", "0.99")) {
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
  T2 <- purr::pluck(res_PCs, "Tsquare", "value")

  # Extract cutoff values for Hotelling's T2
  cutoff_99 <- purr::pluck(res_PCs, "cutoff.99pct")
  cutoff_95 <- purr::pluck(res_PCs, "cutoff.95pct")

  # Select Observations that are avove the 99% confidence interval
  outliers_99 <- pca_scores %>%
    mutate(obs = rownames(pca_scores)) %>%
    filter(T2 > cutoff_99)

  # Select Observations that are avove the 99% confidence interval
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
