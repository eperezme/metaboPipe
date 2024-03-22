#### MEAN ####




#### MEDIAN ####




#### RANDOM FOREST ####
impute_RF <- function(dataset_experiment) {
  # Load required library
  require(missForest)
  
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  forest_result <- missForest(SummarizedExperiment::assay(dataset_experiment), maxiter = 10, ntree = 100)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp) <- forest_result$ximp
  
  return(DE_imp)
}

#### QRILC ####
impute_QRILC <- function(dataset_experiment) {
  # Load required library
  require(imputeLCMD)
  
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  imputed_data <- impute.QRILC(SummarizedExperiment::assay(dataset_experiment))
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp) <- imputed_data[[1]]
  
  return(DE_imp)
}


#### kNN ####


#### SVD ####


#### BPCA ####


#### PPCA ####


#### MAI #### (auto?)
















