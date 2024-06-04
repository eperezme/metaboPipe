library(testthat)
library(structToolbox)
library(SummarizedExperiment)
library(caret)
library(pcpr2)
library(metaboPipe)
library(dplyr)

# Sample data setup for testing
mock_dataset <- metaboPipe::MTBLS79
mock_dataset <- impute_kNN(mock_dataset, k = 5)

mock_dataset$sample_meta <- mock_dataset$sample_meta %>% dplyr::select(Batch, Class, run_order)

test_that("PCPR2 works with typical dataset", {
    result <- pcpr2(dataset_experiment = mock_dataset)
    expect_is(result, "list")
})

test_that("PCPR2 works with log_transform set to FALSE", {
    result <- pcpr2(dataset_experiment = mock_dataset, log_transform = FALSE)
    expect_is(result, "list")
})

test_that("PCPR2 works with custom variables", {
    custom_vars <- c("Batch", "Class")
    result <- pcpr2(dataset_experiment = mock_dataset, variables = custom_vars)
    expect_is(result, "list")
})

test_that("PCPR2 handles multicollinearity error", {
    mock_dataset$sample_meta$Batch <- as.factor(mock_dataset$sample_meta$run_order) # Introduce perfect multicollinearity
    expect_error(pcpr2(dataset_experiment = mock_dataset), "linearly dependent variables")
})

test_that("PCPR2 handles invalid inputs", {
    expect_error(pcpr2(dataset_experiment = list()), "is not a DatasetExperiment object")
})
