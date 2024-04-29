# Signal drift and batch correction function
# This function performs signal drift and batch correction on the given experiment dataset QC-RSC method
# Arguments:
#   dataset_exp: Experiment dataset with samples and variables
#   order_col: Column indicating the order of samples
#   batch_col: Column indicating batch information
#   qc_col: Column indicating quality control information
#   qc_label: Label for quality control
# Returns:
#   Corrected experiment dataset
batch_correction <- function(dataset_exp, order_col, batch_col, qc_col, qc_label) {
  
  # Perform signal drift and batch correction using the sb_corr function
  M <- sb_corr(
    order_col = order_col,
    batch_col = batch_col,
    qc_col = qc_col,
    qc_label = qc_label,
    use_log = TRUE,  # Use logarithm for transformation
    spar_lim = c(-1.5, 1.5),  # Limit for signal drift correction
    min_qc = 4  # Minimum number of quality controls
  )
  
  # Apply the correction model to the experiment dataset
  M <- model_apply(M, dataset_exp)
  
  # Return the corrected experiment dataset
  return(predicted(M))
}

# Function to convert sample columns to factors
# This function converts specified columns in the sample metadata to factors
# Arguments:
#   dataset_exp: Experiment dataset with sample metadata
#   col: Column(s) to be converted to factors
# Returns:
#   Experiment dataset with specified columns converted to factors
factor_sample_col <- function(dataset_exp, col) {
  # Convert specified columns to factors using lapply
  dataset_exp$sample_meta[, col] <- lapply(dataset_exp$sample_meta[, col], factor)
  return(dataset_exp)
}

# Function to extract data matrix from experiment dataset
# This function extracts the data matrix from a SummarizedExperiment object
# Arguments:
#   dataset_exp: Experiment dataset
# Returns:
#   Data matrix
data.extract <- function(dataset_exp) {
  return(SummarizedExperiment::assay(dataset_exp))
}

# Function to modify data matrix of experiment dataset
# This function replaces the data matrix in a SummarizedExperiment object with new data
# Arguments:
#   dataset_exp: Experiment dataset
#   data: New data matrix
# Returns:
#   Experiment dataset with modified data matrix
data.modify <- function(dataset_exp, data) {
  # Replace data matrix in the experiment dataset
  SummarizedExperiment::assay(dataset_exp, withDimnames = FALSE) <- data
  return(dataset_exp)
}

# Function to extract sample metadata from experiment dataset
# This function extracts the sample metadata from a SummarizedExperiment object
# Arguments:
#   dataset_exp: Experiment dataset
# Returns:
#   Sample metadata
sample.data.extract <- function(dataset_exp) {
  return(dataset_exp$sample_meta)
}

# Function to modify sample metadata of experiment dataset
# This function replaces the sample metadata in a SummarizedExperiment object with new metadata
# Arguments:
#   dataset_exp: Experiment dataset
#   sample_meta: New sample metadata
# Returns:
#   Experiment dataset with modified sample metadata
sample.data.modify <- function(dataset_exp, sample_meta) {
  # Replace sample metadata in the experiment dataset
  dataset_exp$sample_meta <- sample_meta
  return(dataset_exp)
}

# Function to extract variable metadata from experiment dataset
# This function extracts the variable metadata from a SummarizedExperiment object
# Arguments:
#   dataset_exp: Experiment dataset
# Returns:
#   Variable metadata
variable.data.extract <- function(dataset_exp) {
  return(dataset_exp$variable_meta)
}

# Function to modify variable metadata of experiment dataset
# This function replaces the variable metadata in a SummarizedExperiment object with new metadata
# Arguments:
#   dataset_exp: Experiment dataset
#   variable_meta: New variable metadata
# Returns:
#   Experiment dataset with modified variable metadata
variable.data.modify <- function(dataset_exp, variable_meta) {
  # Replace variable metadata in the experiment dataset
  dataset_exp$variable_meta <- variable_meta
  return(dataset_exp)
}



# Function to create a dataSet of Metaboanalyst

toMetaboAnalyst <- function(dataset_exp, class_col = "sample_type") {
  # Extract data matrix
  dataMatrix_extracted <- SummarizedExperiment::assay(dataset_exp)
  sampleMetadata_extracted <- sample.data.extract(dataset_exp)
  
  # Extract relevant information using dplyr
  samples_name <- rownames(dataMatrix_extracted)
  classes <- dplyr::pull(sampleMetadata_extracted, {{ class_col }})
  
  # Create data frame for MetaboAnalyst
  MetaboDataMatrix <- data.frame(Sample = samples_name,
                                 Label = classes,
                                 dataMatrix_extracted)
  
  # Create a directory to save the processed data
  dir.create("Analysis", showWarnings = FALSE)
  
  # Save the data frame as a CSV file
  write.csv(MetaboDataMatrix, file = "Analysis/MetaboAnalystData.csv", row.names = FALSE)
  MetaboAnalyst_load_data()
  
}





MetaboAnalyst_load_data <- function() {
  library(MetaboAnalystR)
  withr::with_dir("Analysis", {
  # Initialize MetaboAnalyst data objects
  mSet <- InitDataObjects("conc", "stat", FALSE)
  
  # Read text data
  mSet <- Read.TextData(mSet, "MetaboAnalystData.csv", "rowu", "disc")
  
  # Print read message
  print(mSet$msgSet$read.msg)
  # Perform sanity check on data
  tryCatch({
    mSet <- SanityCheckData(mSet)
  }, error = function(e) {
    cat("Error occurred during data processing:", conditionMessage(e), "\n")
  })
  
  # Reset working directory
})
  return(mSet)
}
  


metaboNorm <- function(mSet, rowNorm = "NULL", transNorm = "NULL", scaleNorm = "NULL", ref=NULL, ratio=FALSE, ratioNum=20 ) {
  withr::with_dir("Analysis", {
    # file.copy("data_orig.qs", "data_proc.qs", overwrite = TRUE)
    mSet <- ReplaceMin(mSet)
  # Perform data normalization
    mSet<-PreparePrenormData(mSet);
    mSet<-Normalization(mSet, rowNorm, transNorm, scaleNorm, ref, ratio, ratioNum);
  
    # View feature normalization
    tryCatch({
    mSet<-PlotNormSummary(mSet, "feature_norm", format="png", dpi=300, width=NA)
    
    # View sample normalization
    mSet<-PlotSampleNormSummary(mSet, "sample_norm", format="png", dpi=300, width=NA)
    }, error = function(e) {
      cat("Error occurred during plot:", conditionMessage(e), "\n")
    })
  })
  
  return(mSet)
}
  
save_metabo <- function(mSet) {
  withr::with_dir("Analysis", {
  SaveTransformedData(mSet)
  })
}

