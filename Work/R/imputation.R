#### MEAN ####




#### MEDIAN ####




#### RANDOM FOREST ####
impute_RF <- function(dataset_experiment) {

  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  forest_result <- missForest::missForest(SummarizedExperiment::assay(dataset_experiment), maxiter = 10, ntree = 100)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp) <- forest_result$ximp
  
  return(DE_imp)
}

#### QRILC ####
impute_QRILC <- function(dataset_experiment) {
  
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  imputed_data <- imputeLCMD::impute.QRILC(SummarizedExperiment::assay(dataset_experiment))
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp) <- imputed_data[[1]]
  
  return(DE_imp)
}


#### kNN ####
impute_kNN <- function(dataset_experiment) {
  # Load required library
  require(impute)
  
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  imputed_data <- impute.knn(SummarizedExperiment::assay(dataset_experiment))
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp) <- imputed_data
  
  return(DE_imp)
}



#### SVD ####


#### BPCA ####


#### PPCA ####


#### MAI #### (auto?)
















