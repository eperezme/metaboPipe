# #### MAI (AUTO) ####
# impute_MAI <- function(dataset_experiment, MCAR, MNAR, ncores = 1, ntree = 300, proximity = FALSE) {
#
#   # Create a copy of the DatasetExperiment object
#   DE_imp <- dataset_experiment
#
#   # Impute missing values
#   result <- MAI(t(SummarizedExperiment::assay(dataset_experiment)),
#                            MCAR_algorithm = MCAR,
#                            MNAR_algorithm= MNAR,
#                            n_cores = ncores,
#                            forest_list_args = list( # random forest arguments for training
#                              ntree = ntree,
#                              proximity = proximity
#                              ),
#                            verbose = TRUE # allows console message output
#                            )
#
#   # Replace missing values with imputed values
#   SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- t(result$Imputed_data)
#
#   return(DE_imp)
# }






#### MEAN ####

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


#### MEDIAN ####
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



#### RANDOM FOREST ####
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

#### QRILC ####
impute_QRILC <- function(dataset_experiment) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment

  # Impute missing values
  imputed_data <- imputeLCMD::impute.QRILC(SummarizedExperiment::assay(dataset_experiment))

  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data[[1]]

  return(DE_imp)
}


#### kNN ####
impute_kNN <- function(dataset_experiment, k = 5) {
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



#### SVD ####


impute_SVD <- function(dataset_experiment, K = 5, center = TRUE, ...) {
  # Create a copy of the DatasetExperiment object
  DE_imp <- dataset_experiment

  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)

  # Call wrapper function to impute missing values
  impute_results <- pcaMethods::pca(data_matrix, method = "svdImpute", nPcs = K, center = center, ...)
  plotPcs(impute_results, type = "scores")
  imputed_data <- pcaMethods::completeObs(impute_results)

  # Replace missing values with imputed values
  SummarizedExperiment::assay(DE_imp, withDimnames = FALSE) <- imputed_data

  return(DE_imp)
}

#### BPCA ####
impute_bpca <- function(dataset_experiment, nPCs = 5, ...) {
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



#### PPCA ####
impute_ppca <- function(dataset_experiment, nPCs = 5, ...) {
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
#
# #### nsKNN ####
# impute_nsKNN <- function(dataset_experiment, k = 5) {
#
#   # Create a copy of the DatasetExperiment object
#   DE_imp <- dataset_experiment
#
#   # Extract the data matrix
#   data_matrix <- SummarizedExperiment::assay(dataset_experiment)
#
#   # Call wrapper function to impute missing values
#   inputed_data <- nsKNN(missingData, k,
#         iters = 4, weighted = TRUE, scale = TRUE,
#         shuffle = TRUE
#         )
#
#   # Replace missing values with imputed values
#   SummarizedExperiment::assay(DE_imp) <- imputed_data
#
#   return(DE_imp)
# }
