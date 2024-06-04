#' Run PCPR2 analysis on metabolomic data
#'
#' This function performs PCPR2 analysis on metabolomic data to assess the variability explained by covariates.
#'
#' @param dataset_experiment The metabolomic dataset as a SummarizedExperiment object.
#' @param variables The variables (covariates) to include in the analysis. Default is all variables in the sample metadata.
#' @param log_transform Logical indicating whether to perform log transformation and autoscaling on the data. Default is TRUE.
#' @param pct_threshold The threshold for percentage of variability explained by covariates. Default is 0.8.
#' @param out_dir The output directory to save the plot. If NULL, the plot will not be saved. Default is NULL.
#' @param out_name The name of the output plot file. Default is "pcpr2".
#'
#' @return The pcpr2 output object.
#'
#' @importFrom structToolbox log_transform autoscale model_apply
#' @importFrom struct predicted
#' @importFrom SummarizedExperiment assay
#' @importFrom caret findLinearCombos
#' @importFrom pcpr2 runPCPR2
#' @importFrom metaboPipe save_plot
#'
#' @examples
#' # Load the metabolomic dataset
#' data("metabolomic_dataset")
#'
#' # Run PCPR2 analysis
#' pcpr2_result <- pcpr2(dataset_experiment = metabolomic_dataset)
#'
#' # Plot the result
#' pcpr2_result
#'
#' @export
pcpr2 <- function(dataset_experiment, variables = colnames(dataset_experiment$sample_meta), log_transform = TRUE, pct_threshold = 0.8, out_dir = NULL, out_name = "pcpr2") {
    # Check if the dataset_experiment is a DatasetExperiment object
    if (!inherits(dataset_experiment, "DatasetExperiment")) {
        stop("The dataset_experiment is not a DatasetExperiment object.")
    }

    # Extract the data

    if (log_transform) {
        M <- structToolbox::log_transform() + structToolbox::autoscale()
        M <- struct::model_apply(M, dataset_experiment)
        transformed <- struct::predicted(M)
        data <- SummarizedExperiment::assay(transformed)
    } else {
        data <- SummarizedExperiment::assay(dataset_experiment)
    }

    # transform it to a matrix
    data_matrix <- as.matrix(data)
    sample_meta <- dataset_experiment$sample_meta
    # Extract the variables
    Z_variables <- sample_meta[, variables]

    # Search for linearCombinations
    design_matrix <- model.matrix(~., data = Z_variables)
    combos <- caret::findLinearCombos(design_matrix)
    if (length(combos$linearCombos) > 0) {
        stop("Seems there are linearly dependent variables. Please remove the linearly dependent variables.")
    }

    output <- pcpr2::runPCPR2(X = data_matrix, Z = Z_variables, pct.threshold = pct_threshold)


    plt <- plot(output, col = "red", main = "Variability in metabolomic data explained by covariates")
    if (!is.null(out_dir)) {
        metaboPipe::save_plot(plt, out_dir, out_name)
        return(output)
    } else {
        print(plt)
        return(output)
    }
}
