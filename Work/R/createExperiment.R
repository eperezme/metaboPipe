
# Sort by sample_id
sort_by_sample_id <- function(df) {
  # sort by sample_id
  df=df[order(df$sample_id),]
  return(df)
}


# Process the dataset to create the DatasetExperiment object
createExperiment <- function(dataMatrix, sampleMetadata, variableMetadata, 
                             experiment_name = "Name", experiment_description = "Description") {
  # dataMatrix should have a row for each sample and a column for each feature
  # Check dimensions
  if (nrow(dataMatrix) != nrow(sampleMetadata)) {
    stop("Number of rows in dataMatrix and sampleMetadata do not match.")
  }
  
  if (ncol(dataMatrix)-1 != nrow(variableMetadata)) {
    stop("Number of (columns - 1) in dataMatrix and rows in variableMetadata do not match.")
  }
  
  # sort by sample_id
  dataMatrix=sort_by_sample_id(dataMatrix)
  sampleMetadata=sort_by_sample_id(sampleMetadata)
  
  # Drop dataMatrix$sample_id
  dataMatrix$sample_id=NULL

  
  # Create new annotation col for variableMetadata == colnames of dataMatrix
  variableMetadata$annotation=colnames(dataMatrix)
  
  # Make metabolites underscore for compatibility
  colnames(dataMatrix)=gsub("-", "_", colnames(dataMatrix))
  
  
  # convert 0 to NA
  dataMatrix[dataMatrix==0]=NA
  dataMatrix=data.frame(lapply(dataMatrix,as.numeric),check.names = FALSE)
  
  
  # Check row/col names match
  rownames(dataMatrix)=sampleMetadata$sample_id
  rownames(sampleMetadata)=sampleMetadata$sample_id
  rownames(variableMetadata)=colnames(dataMatrix)
  
  
  # Create a DatasetExperiment object
  # require(structToolbox)
  DE <- DatasetExperiment(data = dataMatrix,
                          sample_meta = sampleMetadata,
                          variable_meta = variableMetadata,
                          name = experiment_name,
                          description = experiment_description)
  
  return(DE)
}
