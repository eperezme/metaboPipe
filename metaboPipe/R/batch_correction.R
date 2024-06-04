#' Signal drift and batch correction function
#'
#' This function performs signal drift and batch correction on a given DatasetExperiment object using the specified method.
#'
#' @param dataset_exp A DatasetExperiment object with samples and variables.
#' @param method The batch correction method to use: ComBat, QCRSC.
#' @param order_col Column indicating the order of samples.
#' @param batch_col Column indicating batch information.
#' @param qc_col Column indicating quality control information.
#' @param qc_label Label for quality control.
#'
#' @return Corrected DatasetExperiment object.
#' @export
#'
#' @examples
#' DE <- metaboPipe::MTBLS79
#' warper_batch_correction(MTBLS79, "QCRSC", "run_order", "Batch", "Type", "QC")
warper_batch_correction <- function(dataset_exp, method, order_col, batch_col, qc_col, qc_label) {
    switch(method,
        "ComBat" = combat_correction(dataset_exp, batch_col),
        "QCRSC" = qcrsc_correction(dataset_exp, order_col, batch_col, qc_col, qc_label)
    )
}

#' Signal drift and batch correction function
#'
#' This function performs signal drift and batch correction on a given DatasetExperiment object using the QC-RSC method.
#'
#' @param dataset_exp A DatasetExperiment object with samples and variables.
#' @param order_col Column indicating the order of samples.
#' @param batch_col Column indicating batch information.
#' @param qc_col Column indicating quality control information.
#' @param qc_label Label for quality control.
#'
#' @return Corrected DatasetExperiment object.
#' @export
#'
#' @examples
#' #' DE <- metaboPipe::MTBLS79
#' qcrsc_correction(MTBLS79, "run_order", "Batch", "Type", "QC")
qcrsc_correction <- function(dataset_exp, order_col, batch_col, qc_col, qc_label) {
    DE <- dataset_exp
    M <- sb_corr(
        order_col = order_col,
        batch_col = batch_col,
        qc_col = qc_col,
        qc_label = qc_label,
        use_log = TRUE, # Use logarithm for transformation
        spar_lim = c(-1.5, 1.5), # Limit for signal drift correction
        min_qc = 4 # Minimum number of quality controls
    )

    # Apply the correction model to the DatasetExperiment object
    M <- structToolbox::model_apply(M, DE)

    corrected_dataset_exp <- predicted(M)

    # Return the corrected DatasetExperiment object
    return(corrected_dataset_exp)
}


#' Correct Batch Effects in Metabolomics Data Using ComBat
#'
#' This function applies the ComBat method to correct for batch effects in a metabolomics dataset.
#'
#' @param dataset_exp A list containing two elements:
#'   \describe{
#'     \item{data}{A data frame or matrix of metabolite intensities with samples as rows and metabolites as columns.}
#'     \item{sample_meta}{A data frame containing sample metadata, with rows corresponding to samples and a column for batch information.}
#'   }
#' @param batch_col A string specifying the column name in \code{sample_meta} that contains the batch information.
#'
#' @return A matrix of corrected metabolite intensities with batch effects removed.
#'
#' @examples
#' # Example usage:
#' DE <- metaboPipe::MTBLS79
#' corrected_data <- combat_correction(DE, "Batch")
#'
#' @export
combat_correction <- function(dataset_exp, batch_col) {
    # Transpose the data as ComBat needs samples in columns
    data_matrix <- data.table::transpose(dataset_exp$data)

    batch <- dataset_exp$sample_meta[, batch_col]

    combat_data <- sva::ComBat(dat = data_matrix, batch = batch, par.prior = TRUE, prior.plots = FALSE)

    combat_data <- data.table::transpose(data.table::as.data.table(combat_data))

    corrected_dataset_exp <- data.modify(dataset_exp, combat_data)
    return(corrected_dataset_exp)
}
