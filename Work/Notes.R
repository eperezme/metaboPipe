library(targets)
library(tidyverse)


# Load the experiment
tar_load("experiment")
# dataexperiment <-


as_tibble() %>% bind_cols(rowData(experiment) %>% as_tibble() %>% select(sample_id))

rowData(experiment) %>%
  as_tibble() %>%
  select(sample_id)

# Create the metadata table
# Metadata table containing multiple factors and covariates
# This is a general table containing various descriptors for the data to be analyzed
#
# The sample IDs must be identical to the metabolomics data;
# The column after sample IDs should be the primary metadata of interest;
# The metadata can contain either categorical (with at least three replicates per group) or continuous values (covariates);
# Missing values are not allowed - you will be asked to manually "fix" the missing values if detected
# A screenshot of a metadata table is shown below.

library(SummarizedExperiment)
metabData <- rowData(experiment) %>%
  as.tibble() %>%
  select(sample_id, condition, sample_type, time.point, biol.batch) %>%
  left_join(
    SummarizedExperiment::assay(experiment) %>%
      mutate(
        sample_id = rownames(.)
      ),
    by = "sample_id"
  )


#### Metabolomics Workbench ####
# BiocManager::install("metabolomicsWorkbenchR")
library(metabolomicsWorkbenchR)
names(metabolomicsWorkbenchR::context)

# View Inputs
context_inputs('study')

# View Outputs
context_outputs('study')

# Query a study summary
S <- do_query('study','study_id','ST000336','summary')
t(S)

# View the analysis
print(
  t(
    do_query(context = "study", input_item = "study_id", input_value = "ST000336", output_item = "analysis")
    )
  )

SE <- do_query(context = "study", input_item = "analysis_id", input_value = "AN004436", output_item = "SummarizedExperiment")
DE <- metabolomicsWorkbenchR::do_query(context = "study", input_item = "analysis_id", input_value = "AN004436", output_item = "DatasetExperiment")

# Zero_to_na
SummarizedExperiment::assay(DE) <- SummarizedExperiment::assay(DE) %>% mutate_all(as.numeric) %>% replace(.==0, NA)

colData(DE)
View(rowData(DE))
assay(DE)

