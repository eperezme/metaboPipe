imputation_algorithms <- function(missingData,
                                  predictions,
                                  MCAR_algorithm,
                                  MNAR_algorithm,
                                  n_cores, ...) {
  # Imputation step
  numcols <- ncol(missingData)
  numrows <- nrow(missingData)
  predicted_missingData <- matrix(predictions,
    ncol = numcols, nrow = numrows, byrow = TRUE
  )
  MCAR_index <- which(predicted_missingData == "MCAR", arr.ind = TRUE)
  MNAR_index <- which(predicted_missingData == "MNAR", arr.ind = TRUE)
  non_missing <- which(predicted_missingData == "O", arr.ind = TRUE)
  Imputed_data <- matrix(NA, ncol = numcols, nrow = numrows)

  if (n_cores != 1) {
    plan(multisession, workers = 2)

    SingleAlgorithmImputations <- future_lapply(list(
      MNAR_algorithm,
      MCAR_algorithm
    ), function(l) {
      # nolint
      if (l == "Single") {
        MNAR_imputation <- kapImpute2(missingData) # nolint
      }

      if (l == "nsKNN") {
        k <- floor(sqrt(nrow(missingData)))
        if (k > numcols) {
          k <- numcols - 1
        }
        MNAR_imputation <- nsKNN(missingData, k, iters = 1)
      }

      if (l == "BPCA") {
        MCAR_imputation <- pca(missingData,
          method = "bpca",
          verbose = FALSE
        )@completeObs
      }
      if (l == "random_forest") {
        MCAR_imputation <- missForest(missingData)[["ximp"]]
      }
      if (l == "Multi_nsKNN") {
        k <- floor(sqrt(nrow(missingData)))
        if (k > numcols) {
          k <- numcols - 1
        }
        MCAR_imputation <- nsKNN(missingData, k,
          iters = 4, weighted = TRUE, scale = TRUE,
          shuffle = TRUE
        )
      }
      if (l == "mean") {
        MNAR_imputation <- colMeans(missingData, na.rm = TRUE)
      }
      if (l == "median") {
        MNAR_imputation <- apply(missingData, 2, median, na.rm = TRUE)
      }
      if (l == "QRILC") {
        MNAR_imputation <- apply(missingData, 2, function(x) quantile(x, .25, na.rm = TRUE))
      }
      if (l == "SVD") {
        MNAR_imputation <- svdImpute(missingData)$imputed
      }
      if (l == "BPCA") {
        MNAR_imputation <- pca(missingData, method = "bpca", verbose = FALSE)@completeObs
      }
      if (l == "PPCA") {
        MNAR_imputation <- ppcaImpute(missingData)$imputed
      }

      if (exists("MNAR_imputation")) {
        return(MNAR_imputation = MNAR_imputation)
      } else {
        return(MCAR_imputation = MCAR_imputation)
      }
    }, future.globals = c("kapImpute2", "nsKNN", "missForest", "pca", "svdImpute", "ppcaImpute"), future.seed = TRUE)

    plan(sequential)

    MNAR_imputation <- SingleAlgorithmImputations[[1]]
    MCAR_imputation <- SingleAlgorithmImputations[[2]]
  } else {
    if (MNAR_algorithm == "Single") {
      MNAR_imputation <- kapImpute2(missingData)
    }

    if (MNAR_algorithm == "nsKNN") {
      k <- floor(sqrt(nrow(missingData)))
      if (k > numcols) {
        k <- numcols - 1
      }
      MNAR_imputation <- nsKNN(missingData, k, iters = 1)
    }

    if (MCAR_algorithm == "BPCA") {
      MCAR_imputation <- pca(missingData,
        method = "bpca",
        verbose = FALSE
      )@completeObs
    }
    if (MCAR_algorithm == "random_forest") {
      MCAR_imputation <- missForest(missingData)[["ximp"]]
    }
    if (MCAR_algorithm == "Multi_nsKNN") {
      k <- floor(sqrt(nrow(missingData)))
      if (k > numcols) {
        k <- numcols - 1
      }
      MCAR_imputation <- nsKNN(missingData, k,
        iters = 4, weighted = TRUE, scale = TRUE,
        shuffle = TRUE
      )
    }
    if (MCAR_algorithm == "mean") {
      MCAR_imputation <- colMeans(missingData, na.rm = TRUE)
    }
    if (MCAR_algorithm == "median") {
      MCAR_imputation <- apply(missingData, 2, median, na.rm = TRUE)
    }
    if (MCAR_algorithm == "QRILC") {
      MCAR_imputation <- apply(missingData, 2, function(x) quantile(x, .25, na.rm = TRUE))
    }
    if (MCAR_algorithm == "SVD") {
      MCAR_imputation <- svdImpute(missingData, ...)$imputed
    }
    if (MCAR_algorithm == "BPCA") {
      MCAR_imputation <- pca(missingData, method = "bpca", verbose = FALSE)@completeObs
    }
    if (MCAR_algorithm == "PPCA") {
      MCAR_imputation <- ppcaImpute(missingData)$imputed
    }
  }

  Imputed_data[MCAR_index] <- MCAR_imputation[MCAR_index]
  Imputed_data[MNAR_index] <- MNAR_imputation[MNAR_index]
  Imputed_data[is.na(Imputed_data)] <- missingData[non_missing]
  return(list(
    MAI = Imputed_data,
    MCAR_imps = as.matrix(MCAR_imputation),
    MNAR_imps = as.matrix(MNAR_imputation)
  ))
}
