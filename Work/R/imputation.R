#' Impute Missing Values
#' 
#' Impute missing values in a dataset experiment using various methods.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param method The imputation method to use.
#' @param k The parameter for some imputation methods (default: 5).
#' 
#' @return The dataset experiment object with missing values imputed.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_warper(DE, method = "mean")
#' summary(imputed)
impute_warper <- function(dataset_experiment, method, k = 5) {
  switch(method,
         "mean"    = impute_mean(dataset_experiment),
         "median"  = impute_median(dataset_experiment),
         'RF'      = impute_RF(dataset_experiment),
         "QRILC"   = impute_QRILC(dataset_experiment),
         "kNN"     = impute_kNN(dataset_experiment, k = k),
         "SVD"     = impute_SVD(dataset_experiment, nPCs = k),
         "bpca"    = impute_bpca(dataset_experiment, nPCs = k),
         "ppca"    = impute_ppca(dataset_experiment, nPCs = k)
  )
}

#' Impute Mean
#' 
#' Impute missing values in a dataset experiment using the mean.
#' 
#' @param dataset_experiment The dataset experiment object.
#' 
#' @return The dataset experiment object with missing values imputed using the mean.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_mean(DE)
#' summary(imputed)
impute_mean <- function(dataset_experiment) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Impute missing values column-wise
  data_matrix[] <- lapply(data_matrix, function(x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    x
  })
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- data_matrix
  
  return(DE_imp)
}

#' Impute Median
#' 
#' Impute missing values in a dataset experiment using the median.
#' 
#' @param dataset_experiment The dataset experiment object.
#' 
#' @return The dataset experiment object with missing values imputed using the median.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_median(DE)
#' summary(imputed)
impute_median <- function(dataset_experiment) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Impute missing values column-wise
  data_matrix[] <- lapply(data_matrix, function(x) {
    x[is.na(x)] <- median(x, na.rm = T)
    x
  })
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- data_matrix
  
  return(DE_imp)
}

#' Impute Random Forest
#' 
#' Impute missing values in a dataset experiment using random forest.
#' 
#' @param dataset_experiment The dataset experiment object.
#' 
#' @return The dataset experiment object with missing values imputed using random forest.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_RF(DE)
#' summary(imputed)
impute_RF <- function(dataset_experiment) {
  # doParallel::registerDoParallel(cores=4)
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  # forest_result <- missForest::missForest(SummarizedExperiment::assay(dataset_experiment), maxiter = 100, ntree = 1000, parallelize = "forests")
  forest_result <- missForest::missForest(SummarizedExperiment::assay(dataset_experiment), maxiter = 100, ntree = 1000)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- forest_result$ximp
  
  return(DE_imp)
}

#' Impute QRILC
#' 
#' Impute missing values in a dataset experiment using QRILC.
#' 
#' @param dataset_experiment The dataset experiment object.
#' 
#' @return The dataset experiment object with missing values imputed using QRILC.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_QRILC(DE)
#' summary(imputed)
impute_QRILC <- function(dataset_experiment) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Impute missing values
  imputed_data <- imputeLCMD::impute.QRILC(SummarizedExperiment::assay(dataset_experiment))
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data[[1]]
  
  return(DE_imp)
}


#' Impute kNN
#' 
#' Impute missing values in a dataset experiment using kNN.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param k The number of neighbors for kNN imputation.
#' 
#' @return The dataset experiment object with missing values imputed using kNN.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_kNN(DE, k = 5)
#' summary(imputed)
impute_kNN <- function(dataset_experiment, k = k) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Impute missing values
  imputed_data <- impute::impute.knn(as.matrix(data_matrix), k = k)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data$data
  
  return(DE_imp)
}



#' Impute SVD
#' 
#' Impute missing values in a dataset experiment using SVD.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for SVD.
#' 
#' @return The dataset experiment object with missing values imputed using SVD.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_SVD(DE, nPCs = 5)
#' summary(imputed)
impute_SVD <- function(dataset_experiment, nPCs = k, center = TRUE, ...) {
  library(pcaMethods)
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Call wrapper function to impute missing values
  impute_results <- pcaMethods::pca(data_matrix, method = "svdImpute", nPcs = nPCs, center = center, ...)
  plotPcs(impute_results, type = "scores")
  imputed_data <- pcaMethods::completeObs(impute_results)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data
  
  return(DE_imp)
}

#' Impute BPCA
#' 
#' Impute missing values in a dataset experiment using BPCA.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for BPCA.
#' 
#' @return The dataset experiment object with missing values imputed using BPCA.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_bpca(DE, nPCs = 5)
#' summary(imputed)
impute_bpca <- function(dataset_experiment, nPCs = k, ...) {
  library(pcaMethods)
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Call wrapper function to impute missing values
  pc <- pcaMethods::pca(data_matrix, method = "bpca", nPCs = nPCs, ...)
  
  imputed_data <- pcaMethods::completeObs(pc)
  
  slplot(pc)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data
  
  return(DE_imp)
}



#' Impute PPCA
#' 
#' Impute missing values in a dataset experiment using PPCA.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for PPCA.
#' 
#' @return The dataset experiment object with missing values imputed using PPCA.
#' 
#' @export
#' 
#' @examples
#' DE <- ST000284
#' imputed <- impute_ppca(DE, nPCs = 5)
#' summary(imputed)
impute_ppca <- function(dataset_experiment, nPCs = k, ...) {
  library(pcaMethods)
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment
  
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Call wrapper function to impute missing values
  impute_results <- pcaMethods::pca(data_matrix, method = "ppca", nPcs = nPCs, ...)
  pcaMethods::plotPcs(impute_results, type = "scores")
  imputed_data <- pcaMethods::completeObs(impute_results)
  
  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data
  
  return(DE_imp)
}
