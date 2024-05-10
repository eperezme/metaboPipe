prepare_dataMatrix <- function(data, first_col) {
  # Select the columns for the reads
  X <- data[, first_col:ncol(data)]
  X$sample_id <- data$label

  return(X)
}

prepare_sampleMetadata <- function(data, first_col, last_col) {
  # Create SampleMetadata dataframe
  SM <- data[, first_col:last_col]
  SM$sample_id <- data$label

  # Define QC and blank samples
  blanks <- c(1, 2, 33, 34, 65, 66)
  QCs <- c(3, 4, 11, 18, 25, 32, 35, 36, 43, 50, 57, 64)

  # Set sample_type based on sample id
  SM$sample_type <- ifelse(data$label %in% blanks, "Blank",
    ifelse(data$label %in% QCs, "QC", "Sample")
  )

  return(SM)
}

prepare_variableMetadata <- function(data, first_col) {
  # Create variableMetadata object
  VM <- data.frame(annotation = colnames(data)[first_col:ncol(data)])

  return(VM)
}
