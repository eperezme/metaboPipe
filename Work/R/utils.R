#' Signal drift and batch correction function
#' 
#' This function performs signal drift and batch correction on the given A `DatasetExperiment` object QC-RSC method
#' 
#' @param dataset_exp A `DatasetExperiment` object with samples and variables
#' @param order_col Column indicating the order of samples
#' @param batch_col Column indicating batch information
#' @param qc_col Column indicating quality control information
#' @param qc_label Label for quality control
#' 
#' @return Corrected A `DatasetExperiment` objectExperiment object
#' @export
#' 
#' @examples
#' warper_batch_correction(dataset_exp, order_col, batch_col, qc_col, qc_label)
warper_batch_correction <- function(dataset_exp, order_col, batch_col, qc_col, qc_label) {
  # Perform signal drift and batch correction using the sb_corr function
  M <- sb_corr(
    order_col = order_col,
    batch_col = batch_col,
    qc_col = qc_col,
    qc_label = qc_label,
    use_log = TRUE, # Use logarithm for transformation
    spar_lim = c(-1.5, 1.5), # Limit for signal drift correction
    min_qc = 4 # Minimum number of quality controls
  )
  
  # Apply the correction model to the A `DatasetExperiment` object
  M <- model_apply(M, dataset_exp)
  
  # Return the corrected A `DatasetExperiment` object
  return(predicted(M))
}

#' Function to convert sample columns to factors
#' 
#' This function converts specified columns in the sample metadata to factors
#' 
#' @param dataset_exp A `DatasetExperiment` object with sample metadata
#' @param col Column(s) to be converted to factors
#' 
#' @return A DatasetExperiment with specified columns converted to factors
#' @export
#' 
#' @examples
#' warper_factor_sample_col(dataset_exp, col)
warper_factor_sample_col <- function(dataset_exp, col) {
  # Convert specified columns to factors using lapply
  mod_dataset <- dataset_exp$sample_meta
  mod_dataset[, col] <- lapply(mod_dataset[, col], factor)
  
  dataset_exp$sample_meta <- mod_dataset
  
  return(dataset_exp)
}

#' Function to extract data matrix from A `DatasetExperiment` object
#' 
#' This function extracts the data matrix from a SummarizedExperiment object
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' 
#' @return Data matrix
#' @export
#' 
#' @examples
#' data.extract(dataset_exp)
data.extract <- function(dataset_exp) {
  return(SummarizedExperiment::assay(dataset_exp))
}

#' Function to modify data matrix of A `DatasetExperiment` object
#' 
#' This function replaces the data matrix in a SummarizedExperiment object with new data
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' @param data New data matrix
#' 
#' @return A `DatasetExperiment` object with modified data matrix
#' @export
#' 
#' @examples
#' data.modify(dataset_exp, data)
data.modify <- function(dataset_exp, data) {
  # Replace data matrix in the A `DatasetExperiment` object
  SummarizedExperiment::assay(dataset_exp, withDimnames = FALSE) <- data
  return(dataset_exp)
}

#' Function to extract sample metadata from A `DatasetExperiment` object
#' 
#' This function extracts the sample metadata from a SummarizedExperiment object
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' 
#' @return Sample metadata dataframe
#' @export
#' 
#' @examples
#' sample.data.extract(dataset_exp)
sample.data.extract <- function(dataset_exp) {
  return(dataset_exp$sample_meta)
}

#' Function to modify sample metadata of A `DatasetExperiment` object
#' 
#' This function replaces the sample metadata in a SummarizedExperiment object with new metadata
#' 
#' @param dataset_exp DatasetExperiment object
#' @param sample_meta New sample metadata dataframe
#' 
#' @return A `DatasetExperiment` object with modified sample metadata
#' @export
#' 
#' @examples
#' sample.data.modify(dataset_exp, sample_meta)
sample.data.modify <- function(dataset_exp, sample_meta) {
  # Replace sample metadata in the experiment dataframe
  dataset_exp$sample_meta <- sample_meta
  return(dataset_exp)
}

#' Function to extract variable metadata from A `DatasetExperiment` object
#' 
#' This function extracts the variable metadata from a SummarizedExperiment object
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' 
#' @return Variable metadata dataframe
#' @export
#' 
#' @examples
#' variable.data.extract(dataset_exp)
variable.data.extract <- function(dataset_exp) {
  return(dataset_exp$variable_meta)
}

#' Function to modify variable metadata of A `DatasetExperiment` object
#' 
#' This function replaces the variable metadata in a SummarizedExperiment object with new metadata
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' @param variable_meta New variable metadata
#' 
#' @return A `DatasetExperiment` object with modified variable metadata
#' @export
#' 
#' @examples
#' variable.data.modify(dataset_exp, variable_meta)
variable.data.modify <- function(dataset_exp, variable_meta) {
  # Replace variable metadata in the A `DatasetExperiment` object
  dataset_exp$variable_meta <- variable_meta
  return(dataset_exp)
}

#' Function to create a dataSet for Metaboanalyst
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' @param class_col Column to be used as class
#' @param sample_id Column to be used as sample ID
#' 
#' @return Nothing
#' @export
#' 
#' @examples
#' toMetaboAnalyst(dataset_exp, class_col = "sample_type", sample_id = "sample_id")
toMetaboAnalyst <- function(dataset_exp, class_col = "sample_type", sample_id = "sample_id") {
  # Extract data matrix
  dataMatrix_extracted <- SummarizedExperiment::assay(dataset_exp)
  sampleMetadata_extracted <- sample.data.extract(dataset_exp)
  
  # Extract relevant information using dplyr
  samples_name <- dplyr::pull(sampleMetadata_extracted, {{ sample_id }})
  classes <- dplyr::pull(sampleMetadata_extracted, {{ class_col }})
  
  # Create data frame for MetaboAnalyst
  MetaboDataMatrix <- data.frame(
    Sample = samples_name,
    Label = classes,
    dataMatrix_extracted
  )
  
  # Save the data frame as a CSV file
  write.csv(MetaboDataMatrix, file = "TempData/MetaboAnalystData.csv", row.names = FALSE)
  MetaboAnalyst_load_data()
}

#' Function to load the previously saved `MetaboAnalystData.csv` data into MetaboAnalyst from the TempData directory
#' 
#' @return MetaboAnalyst data object (mSet)
#' @export
#' 
#' @examples
#' MetaboAnalyst_load_data()
MetaboAnalyst_load_data <- function() {
  library(MetaboAnalystR)
  withr::with_dir("TempData", {
    # Initialize MetaboAnalyst data objects
    mSet <- InitDataObjects("conc", "stat", FALSE)
    
    # Read text data
    mSet <- Read.TextData(mSet, "MetaboAnalystData.csv", "rowu", "disc")
    
    # Print read message
    print(mSet$msgSet$read.msg)
    # Perform sanity check on data
    tryCatch(
      {
        mSet <- SanityCheckData(mSet)
      },
      error = function(e) {
        cat("Error occurred during data processing:", conditionMessage(e), "\n")
      }
    )
    
    #Reset working directory
  })
  return(mSet)
}

#' Function to normalize MetaboAnalyst data. This function performs row-wise normalization, transformation, and scaling of the metabolomic data.
#' 
#' @param mSet The MetaboAnalyst data object
#' @param rowNorm The row normalization method
#' @param transNorm The transformation normalization method
#' @param scaleNorm The scaling normalization method
#' @param ref Input the name of the reference sample or the reference feature, use " " around the name.
#' @param ratio This option is only for biomarker analysis.
#' @param ratioNum Relevant only for biomarker analysis.
#' @param out_dir The output directory for the plots
#' 
#' @return The normalized MetaboAnalyst data object
#' @export
#' 
#' @examples
#' metaboNorm(mSet, rowNorm = "NULL", transNorm = "NULL", scaleNorm = "NULL", ref = NULL, ratio = FALSE, ratioNum = 20, out_dir)
metaboNorm <- function(mSet, rowNorm = "NULL", transNorm = "NULL", scaleNorm = "NULL", ref = NULL, ratio = FALSE, ratioNum = 20, out_dir) {
  withr::with_dir("TempData", {
    # file.copy("data_orig.qs", "data_proc.qs", overwrite = TRUE)
    mSet <- ReplaceMin(mSet)
    # Perform data normalization
    mSet <- PreparePrenormData(mSet)
    mSet <- Normalization(mSet, rowNorm, transNorm, scaleNorm, ref, ratio, ratioNum)
    
    # Save plots
    # View feature normalization
    tryCatch(
      {
        dir.create("Plots", showWarnings = FALSE)
        mSet <- PlotNormSummary(mSet, paste0(out_dir, "/Plots/Normalization_features"), format = "png", dpi = 300, width = NA)
        
        # View sample normalization
        mSet <- PlotSampleNormSummary(mSet, paste0(out_dir, "Plots/Normalization_samples"), format = "png", dpi = 300, width = NA)
      },
      error = function(e) {
        cat("Error occurred during plot:", conditionMessage(e), "\n")
      }
    )
  })
  
  return(mSet)
}

#' Function to export MetaboAnalyst data
#' 
#' @param mSet The MetaboAnalyst data object
#' 
#' @return Nothing
#' @export
#' 
#' @examples
#' save_metabo(mSet)
save_metabo <- function(mSet) {
  withr::with_dir("TempData", {
    SaveTransformedData(mSet)
  })
}

#' Function to save plots
#'
#' @param plt The plot object
#' @param output_dir The output directory
#' @param output_name The output name for the plot file
#' 
#' @return Nothing
#' @export
#' 
#' @examples
#' save_plot(plt, output_dir, output_name)
save_plot <- function(plt, output_dir, output_name) {
  withr::with_dir(output_dir, {
    dir.create("Plots", showWarnings = FALSE)
    withr::with_dir("Plots", {
      # Save PNG
      png_name <- paste0(output_name, ".png")
      png(filename = png_name, width = 1080, height = 450, res = 100)
      plot(plt)
      dev.off()
      
      # Save SVG
      svg_name <- paste0(output_name, ".svg")
      svg(filename = svg_name, width = 12)
      plot(plt)
      dev.off()
    })
  })
}

#' Function to export data
#' 
#' @param dataset_exp A `DatasetExperiment` object
#' @param out_dir The output directory
#' @param out_name The output name of the files
#' 
#' @return Nothing
#' @export
#' 
#' @examples
#' export_data(dataset_exp, out_dir, out_name)
export_data <- function(dataset_exp, out_dir, out_name) {
  dir.create(out_dir, showWarnings = FALSE)
  withr::with_dir(out_dir, {
    # Extract data matrix
    dataMatrix <- data.extract(dataset_exp)
    # Extract sample metadata
    sampleMetadata <- sample.data.extract(dataset_exp)
    # Extract variable metadata
    variableMetadata <- variable.data.extract(dataset_exp)
    
    # Write data matrix to CSV
    write.csv(dataMatrix, file = paste0(out_name, "_data.csv"), row.names = TRUE)
    # Write sample metadata to CSV
    write.csv(sampleMetadata, file = paste0(out_name, "_sample_metadata.csv"), row.names = TRUE)
    # Write variable metadata to CSV
    write.csv(variableMetadata, file = paste0(out_name, "_variable_metadata.csv"), row.names = TRUE)
  })
}

#' Function to extract names
#' 
#' @param data 
#' 
#' @return The variableMetadata dataset for the DatasetExperiment object
#' @export
#' 
#' @examples
#' extract_names(data)
extract_names <- function(data) {
  variableData <- t(data) %>% as_tibble() %>%  select(V1) %>% rename("annotation" = V1) %>% as.data.frame()
  variableData$annotation <- as.character(variableData$annotation) 
  return(variableData)
}