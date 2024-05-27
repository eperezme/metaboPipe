##### Probabilistic Quotient normalization (PQN) #####
#' Perform Probabilistic Quotient normalization (PQN)
#' 
#' @param dataset_experiment A `DatasetExperiment` object
#' @param qc_label Label for quality control samples
#' @param factor_name Name of the factor to use for normalization
#' 
#' @return Normalized A `DatasetExperiment` object
#' @export
#' 
#' @examples
#' normalize_pqn(dataset_experiment, qc_label, factor_name)
normalize_pqn <- function(dataset_experiment, qc_label, factor_name) {
  # It is not recommended to use pqn when the number of columns is greater than the number of rows
  if (ncol(dataset_experiment) > nrow(dataset_experiment)) {
    warning("The number of columns is greater than the number of rows. It is not recommended to use pqn when the number of Metabolites is greater than the number of Samples")
  }
  M <- pqn_norm(qc_label = qc_label, factor_name = factor_name)
  
  M <- model_apply(M, dataset_experiment)
  
  return(predicted(M))
}


##### Vector Length Normalization (VLN) #####
#' Perform Vector Length Normalization (VLN)
#' 
#' @param dataset_experiment A `DatasetExperiment` object
#' 
#' @return Normalized A `DatasetExperiment` object
#' @export
#' 
#' @examples
#' normalize_vln(dataset_experiment)
normalize_vln <- function(dataset_experiment) {
  M <- vec_norm()
  
  M <- model_apply(M, dataset_experiment)
  
  return(predicted(M))
}

#### Constant Sum Normalization (CSN) #### (Maybe should be discontinued)
#' Perform Constant Sum Normalization (CSN)
#' 
#' @param dataset_experiment A `DatasetExperiment` object
#' @param scaling_factor Scaling factor for normalization
#' 
#' @return Normalized A `DatasetExperiment` object
#' @export
#' 
#' @examples
#' normalize_csn(dataset_experiment, scaling_factor = 1)
normalize_csn <- function(dataset_experiment, scaling_factor = 1) {
  M <- constant_sum_norm(scaling_factor = scaling_factor)
  
  M <- model_apply(M, dataset_experiment)
  
  return(predicted(M))
}


##### Normalization with MetaboAnalystR
#' Normalize A `DatasetExperiment` object using MetaboAnalystR
#' 
#' @param dataset_experiment A `DatasetExperiment` object
#' @param factor_col Column containing factor information for normalization
#' @param sample_id_col Column containing sample IDs
#' @param rowNorm Type of row normalization (options: "QuantileNorm", "CompNorm", "SumNorm", "MedianNorm", "SpecNorm", or "NULL")
#' @param transNorm Type of transformation normalization (options: "LogNorm", "CrNorm", or "NULL")
#' @param scaleNorm Type of scaling normalization (options: "MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm", or "NULL")
#' @param ref Reference feature for 'CompNorm' normalization
#' @param ratio Boolean indicating whether to apply ratio normalization
#' @param ratioNum Number of samples for ratio normalization
#' @param out_dir Output directory for saving files
#'
#' @return Normalized A `DatasetExperiment` object
#' @export
#' 
#' @examples
#' normalize_metab(dataset_experiment, factor_col, sample_id_col, rowNorm = NULL, transNorm = NULL, scaleNorm = NULL, ref = NULL, ratio = FALSE, ratioNum = 20, out_dir)
normalize_metab <- function(dataset_experiment, factor_col, sample_id_col, rowNorm = "NULL", transNorm = "NULL", scaleNorm = "NULL", ref = NULL, ratio = FALSE, ratioNum = 20, out_dir) {
  # Check if the rowNorm argument is valid
  if (!is.null(rowNorm) && !rowNorm %in% c("QuantileNorm", "CompNorm", "SumNorm", "MedianNorm", "SpecNorm", "NULL")) {
    stop("Invalid rowNorm argument. Must be one of 'QuantileNorm', 'CompNorm', 'SumNorm', 'MedianNorm', 'SpecNorm', or NULL.")
  }
  # Check if the transNorm argument is valid
  if (!is.null(transNorm) && !transNorm %in% c("LogNorm", "CrNorm", "NULL")) {
    stop("Invalid transNorm argument. Must be one of 'LogNorm', 'CrNorm', or 'NULL'.")
  }
  # Check if the scaleNorm argument is valid
  if (!is.null(scaleNorm) && !scaleNorm %in% c("MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm", "NULL")) {
    stop("Invalid scaleNorm argument. Must be one of 'MeanCenter', 'AutoNorm', 'ParetoNorm', 'RangeNorm', or 'NULL'.")
  }
  if (rowNorm == "CompNorm" & is.null(ref)) {
    stop("Reference Feature must be specified for 'CompNorm' normalization.")
  }
  
  ref <- make.names(ref)
  
  withr::with_dir(out_dir, {
    dir.create("TempData", showWarnings = FALSE)
    
    
    # Create a metaboanalyst object
    toMetaboAnalyst(dataset_experiment, factor_col, sample_id_col)
    mSet <- MetaboAnalyst_load_data()
    mSet <- metaboNorm(mSet, rowNorm, transNorm, scaleNorm, ref, ratio, ratioNum, out_dir)
    save_metabo(mSet)
    
    # Now we have to rebuild the dataset_experiment object
    normalizedData <- read.csv("TempData/data_normalized.csv", header = F, row.names = 1) %>%
      t() %>%
      as.data.frame() %>%
      rename(sample_id = V1) %>%
      arrange(sample_id)
    
    rownames(normalizedData) <- normalizedData$sample_id
    
    if (rowNorm == "CompNorm" & !is.null(ref)) {
      Filt <- structToolbox::filter_by_name(
        mode = "exclude",
        dimension = "variable",
        ref
      )
      Filt <- model_apply(Filt, dataset_experiment)
      dataset_experiment <- predicted(Filt)
    }
    
    
    # Reorder the rows of df1 based on the row names from df2
    order <- assay(dataset_experiment) %>% rownames()
    sortedData <- normalizedData[order, , drop = FALSE]
    sortedData <- sortedData %>% select(-sample_id, -Label)
    # Make all the columns numeric
    numericData <- sortedData %>% mutate_all(as.numeric)
    
    
    # Modify the dataset_experiment object
    dataset_experiment <- data.modify(dataset_experiment, numericData)
  })
  
  return(dataset_experiment)
}
