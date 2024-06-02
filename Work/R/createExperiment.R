#' Sort by sample_id
#'
#' @param df A dataframe with a sample_id column.
#'
#' @return A data frame sorted by sample_id.
#' @export
#'
#' @examples
#' sort_by_sample_id(data)
sort_by_sample_id <- function(df) {
  # sort by sample_id
  df <- df[order(df$sample_id), ]
  return(df)
}


#' Process the dataset to create the DatasetExperiment object
#'
#' @param dataMatrix A matrix with samples as rows and features as columns.
#' @param sampleMetadata A data frame with the sample metadata.
#' @param variableMetadata A data frame with the variable metadata.
#' @param experiment_name The name for the experiment.
#' @param experiment_description The description for the experiment.
#'
#' @return A \code{DatasetExperiment} object.
#' @export
#'
#' @examples
#' warper_createExperiment(dataMatrix, sampleMetadata, variableMetadata, experiment_name = "Name", experiment_description = "Description")
warper_createExperiment <- function(dataMatrix, sampleMetadata, variableMetadata,
                                    experiment_name = "Name", experiment_description = "Description") {
  # Convert tibbles to data frames
  dataMatrix <- as.data.frame(dataMatrix)
  sampleMetadata <- as.data.frame(sampleMetadata)
  variableMetadata <- as.data.frame(variableMetadata)

  # dataMatrix should have a row for each sample and a column for each feature
  # Check dimensions
  if (nrow(dataMatrix) != nrow(sampleMetadata)) {
    stop("Number of rows in dataMatrix and sampleMetadata do not match.")
  }

  if (ncol(dataMatrix) != nrow(variableMetadata)) {
    stop("Number of columns in dataMatrix and rows in variableMetadata do not match.")
  }

  # convert 0 to NA
  dataMatrix <- dataMatrix %>% mutate_if(is.character, as.numeric)
  dataMatrix[dataMatrix <= 0] <- NA
  dataMatrix <- data.frame(lapply(dataMatrix, as.numeric), check.names = FALSE)


  # Drop dataMatrix$sample_id
  dataMatrix$sample_id <- NULL


  # Check row/col names match
  rownames(dataMatrix) <- sampleMetadata$sample_id
  rownames(sampleMetadata) <- sampleMetadata$sample_id
  rownames(variableMetadata) <- colnames(dataMatrix)


  # Create a DatasetExperiment object
  # require(structToolbox)
  DE <- DatasetExperiment(
    data = dataMatrix,
    sample_meta = sampleMetadata,
    variable_meta = variableMetadata,
    name = experiment_name,
    description = experiment_description
  )

  # Make the metabolite names correct
  # DE@colData@listData$annotation <- variableMetadata$annotation

  return(DE)
}
