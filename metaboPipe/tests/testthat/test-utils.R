# Sample data setup for testing
mock_dataset <- metaboPipe::MTBLS79


test_that("warper_factor_sample_col converts specified columns to factors", {
    # Create a mock dataset
    dataset_exp <- mock_dataset

    # Specify the columns to be converted to factors
    col <- c("Batch", "Class")

    # Call the warper_factor_sample_col function
    result <- warper_factor_sample_col(dataset_exp, col)

    # Check if the specified columns are converted to factors
    expect_is(result$sample_meta$Batch, "factor")
    expect_is(result$sample_meta$Class, "factor")
})

test_that("data.extract extracts the data matrix from a DatasetExperiment object", {
    # Create a mock dataset
    dataset_exp <- mock_dataset

    # Call the data.extract function
    result <- data.extract(dataset_exp)

    # Check if the result is a data matrix
    expect_is(result, "data.frame")
})


test_that("data.modify replaces the data matrix in a DatasetExperiment object with new data", {
    # change the data
    dataset_exp <- mock_dataset

    imputed <- impute_median(dataset_exp)

    imputed_dataset <- imputed$data

    # Call the data.modify function
    result <- data.modify(dataset_exp, imputed_dataset)

    # Check if the data matrix is replaced with new data
    expect_equal(result$data, imputed_dataset)
})



# Test sample.data.extract function
test_that("sample.data.extract extracts the sample metadata from a DatasetExperiment object", {
    dataset_exp <- mock_dataset

    result <- sample.data.extract(dataset_exp)

    expect_equal(result, dataset_exp$sample_meta)
})

# Test sample.data.modify function
test_that("sample.data.modify replaces the sample metadata in a DatasetExperiment object with new metadata", {
    dataset_exp <- mock_dataset

    new_sample_meta <- dataset_exp$sample_meta %>% dplyr::select(-Class)

    result <- sample.data.modify(dataset_exp, new_sample_meta)

    expect_equal(result$sample_meta, new_sample_meta)
})

# Test variable.data.extract function
test_that("variable.data.extract extracts the variable metadata from a DatasetExperiment object", {
    dataset_exp <- mock_dataset

    result <- variable.data.extract(dataset_exp)

    expect_equal(result, dataset_exp$variable_meta)
})

# Test variable.data.modify function
test_that("variable.data.modify replaces the variable metadata in a DatasetExperiment object with new metadata", {
    dataset_exp <- mock_dataset
    coln <- colnames(dataset_exp$data)
    new_variable_meta <- data.frame(annotation = coln)

    result <- variable.data.modify(dataset_exp, new_variable_meta)

    expect_equal(result$variable_meta, new_variable_meta)
})


# Test export_data function
test_that("export_data exports the data matrix and metadata to CSV files", {
    dataset_exp <- mock_dataset
    out_dir <- "Export"
    out_name <- "data"

    expect_silent(export_data(dataset_exp, out_dir, out_name))
})


# Test pipePliers function
test_that("pipePliers launches the Shiny app included with the package", {
    expect_silent(pipePliers())
})
