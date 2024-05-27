---
title: "Data Processing of metabolomics datasets using metaboPipe"
author: "Perez Mendez, Eduard"
date: "2024-05-27"
output: 
  BiocStyle::html_document:
      toc: true
      toc_float: true
      toc_depth: 2
vignette: >
  %\VignetteIndexEntry{Data Processing of metabolomics datasets using metaboPipe}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
link-citations: true
---




# Introduction

Metabolomics, the study of small molecules in biological systems, plays a crucial role in understanding various physiological processes and disease mechanisms. As the volume and complexity of metabolomics data continue to grow, there is an increasing demand for robust and efficient data analysis pipelines.

`metaboPipe` is a comprehensive R package designed to streamline metabolomics analysis workflows. Built with the aim of simplifying data preprocessing tasks. `metaboPipe` offers a suite of tools tailored specifically for metabolomics researchers.

In this vignette, we will explore the key functionalities of `metaboPipe` and demonstrate how it can be used to perform common metabolomics preprocessing tasks. From loading the data, filtering, imputing, normalizing, and scaling, to extracting the processed data, `metaboPipe` provides a user-friendly interface to facilitate seamless integration into your metabolomics workflow.


## Installation

Clone the `metaboPipe` repository from GitHub and install the package using the following commands:

``` r
# Install the package
install.packages("devtools")
devtools::install_github("https://github.com/eperezme/metaboPipe",
    subdir = "Work")
```

## Introduction to `metaboPipe` and its functionalities
For our example we will load the ST000284 dataset. You can view the description of the dataset by running the following command:


``` r
DE <- metaboPipe::ST000284
DE
#> Loading required package: struct
#> A "DatasetExperiment" object
#> ----------------------------
#> name:          Colorectal Cancer Detection Using Targeted Serum Metabolic Profiling
#> description:   Colorectal cancer (CRC) is one of the most prevalent and deadly cancers in the world. Despite
#>                  an expanding knowledge of its molecular pathogenesis during the past two
#>                  decades, robust biomarkers to enable screening, surveillance, and therapy
#>                  monitoring of CRC are still lacking. In this study, we present a targeted
#>                  liquid chromatography-tandem mass spectrometry-based metabolic profiling
#>                  approach for identifying biomarker candidates that could enable highly
#>                  sensitive and specific CRC detection using human serum samples. In this
#>                  targeted approach, 158 metabolites from 25 metabolic pathways of potential
#>                  significance were monitored in 234 serum samples from three groups of
#>                  patients (66 CRC patients, 76 polyp patients, and 92 healthy controls).
#>                  Partial least squares-discriminant analysis (PLS-DA) models were established,
#>                  which proved to be powerful for distinguishing CRC patients from both healthy
#>                  controls and polyp patients. Receiver operating characteristic curves
#>                  generated based on these PLS-DA models showed high sensitivities (0.96 and
#>                  0.89, respectively, for differentiating CRC patients from healthy controls or
#>                  polyp patients); good specificities (0.80 and 0.88), and excellent areas
#>                  under the curve (0.93 and 0.95) were also obtained. Monte Carlo cross
#>                  validation (MCCV) was also applied, demonstrating the robust diagnostic power
#>                  of this metabolic profiling approach.
#> data:          234 rows x 113 columns
#> sample_meta:   234 rows x 13 columns
#> variable_meta: 113 rows x 1 columns
```


`metaboPipe` uses the `DatasetExperiment` object from the `structToolbox` package as the main **data structure** for storing and manipulating metabolomics data. The `DatasetExperiment` object is a list of data frames that contains the following components: 


- `data`: This encapsulates a data frame housing the measured data pertaining to each sample.
- `sample_meta`: This comprises a data frame furnishing supplementary sample-related information, such as group labels, often referred to as *phenoData.*
- `variable_meta`: This includes a data frame presenting additional details concerning variables (features), such as annotations.

Similar to all `struct` entities, it encompasses `name` and `description` fields, referred to as "slots" within the R language.

A notable distinction between `DatasetExperiment` and `SummarizedExperiment` structures lies in the data orientation. In `DatasetExperiment` instances, **samples are arranged in rows** while **features occupy columns**, contrasting with the arrangement in `SummarizedExperiment` structures.

All slots are accessible using dollar notation.


``` r
head(DE$data[, 1:4])
head(DE$sample_meta[, 1:4])
head(DE$variable_meta)
```



# Creating a pipeline 

`metaboPipe` uses the `targets` package to define and execute data processing pipelines. The pipeline is defined as a list of targets, each representing a step in the data processing workflow. Each target is a function call that takes input data and produces output data. The `targets` package ensures that each target is executed only when its dependencies are met, allowing for efficient and reproducible data processing.

The `metaboPipe` pipeline functions consists of several functions that create targets to execute specific processing steps, including data loading, filtering, batch correction, imputation and normalization (that includes tranformation and scaling). Each step is defined as a single or multiple targets in the pipeline, and the targets are executed sequentially to process the data.

In order to use the pipeline functions in `metaboPipe`, we need to create a targets file. You can use the `use_targets()` function to create a new `_targets.R` file in the working directory or use the `metaboPipe` template `create_pipeline()` function:

``` r
create_pipeline()
```

An example `_targets.R` file is created and saved in the working directory. The `_targets.R` file contains some example code, which we will modify to define the pipeline for our data processing workflow.
First of all we will load the required packages for `metaboPipe` and set the target options in the `_targets.R` file.


``` r
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(metaboPipe)

tar_option_set(packages = c("structToolbox", "SummarizedExperiment",
    "VIM", "impute", "imputeLCMD", "missForest", "caret", "pcaMethods",
    "tidyverse", "MetaboAnalystR", "tinytex", "HotellingEllipse",
    "ggforce", "tools", "cowplot", "metaboPipe"))
```

Now that the packages for our pipeline are set up, we can define the pipeline workflow. The pipeline consists of several steps, including data loading, filtering, batch correction, imputation, and normalization. Each step is defined as a function in the script but those functions create multiple targets, and the targets are executed sequentially to process the data.

The targets should be defined inside a `list()` function. 
We can load the data using the `tar_target()` function, which defines a target that loads the data from the specified file path.
We first will load the data using the `tar_target()` function, which defines a target that reads the data from the specified file path. And with the 3 dataframes loaded we can create another target to merge them into a `DatasetExperiment` object.

The first variable on the `tar_target()` function is the name of the target, that will be used for refering the objects on subsequent targets and the second variable is the code that will be executed to generate the target.

``` r
list(tar_target(dataMatrix, read.csv("data/ST000284/dataMatrix.csv")),
    tar_target(sampleMeta, read.csv("data/ST000284/sampleMetadata.csv")),
    tar_target(variableMetadata, read.csv("data/ST000284/variableMetadata.csv")),
    tar_target(experiment, warper_createExperiment(dataMatrix,
        sampleMeta, variableMetadata, experiment_name = "ST000284",
        experiment_description = "Example metabolomics dataset ST000284")))
```

If then we execute `tar_make()` the pipeline will be executed and the data will be loaded and merged into a `DatasetExperiment` object. To load the object into the R environment we can use the `tar_load()` function.


``` r
tar_load(experiment)
```


Even though the pipeline is correctly defined, `metaboPipe` provides a set of functions to make the pipeline definition easier.

## Using metaboPipe functions to define the pipeline
### Loading the data

Instead of writing 3 targets to load the 3 dataframes we can define the paths to the files before the targets list and then use the `load_data()` function to load the 3 dataframes. And then we can use the `createExperiment()` function to merge the 3 dataframes into a `DatasetExperiment` object.

The `load_data()` function takes the paths to the data files as arguments and returns a list with the loaded dataframes. The `createExperiment()` function takes the loaded dataframes as arguments and returns a `DatasetExperiment` object.

Same as before, the first variable on the functions is the name of the target.

``` r
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"

list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath), createExperiment(experiment, data_loaded,
    experiment_name = "ST000284", experiment_description = "Example metabolomics dataset ST000284"))
```

Now if we run `tar_make()` the pipeline will be executed and the data will be loaded and merged into a `DatasetExperiment` object. As before to load the object into the R environment we can use the `tar_load()` function.

``` r
tar_load(experiment)
```

By default, `load_data()` uses the "," as the separator for the data files. If the data files use a different separator, we can define for each file the separator:

``` r
# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description))
```

## Factorizing the columns of the experiment
There are functions that assume that specific columns are factors, such as the `normalize()` function. To factorize the columns of the experiment we can use the `factorize_cols()` function. This function takes the `DatasetExperiment` object, and a vector of column names as an argument and returns the object with the columns factorized.

``` r
# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns))
```

## Filtering the data
After the setup of the experiment, we can start the data manipulation steps. The first step we will perform is to filter the data. The `filter_step()` function encompases 2 filtering steps: 1) filtering by % of missing values and 2) filtering the outliers. The threshold is defined as the minimum percentage of data explained. By default the `threshold` is `0.8`, meaning that each row and collumn must have at least 80% of the values or get removed. The `filter_outliers` argument is a boolean that defines if the outliers should be removed or not and the `conf.limit` argument specify the confidence interval (either `'0.95'` or `'0.99'`) to discriminate the observation as an outlier or not. 


``` r
# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = "results"))
```

The `out_dir` option is where the plots will be exported under the path: `out_dir/plots`, in this case: `results/plots`.

## Imputing
The next step is to impute the missing values that remain after the filter step. To perform that we use the `impute()` function specifying the impute method we want to use and if it needs a special parameter like knn we specify it using `k`. In this example we will use a `Random forest` approach.

``` r
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE)  # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir)  # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
    impute(imputed_experiment, filtered_experiment, method = "RF",
        k = 5)  # Impute using Random forest
)
```


## Normalizing
Next step is to normalize the data. To do so we use the `normalize()` function, that uses MetaboAnalystR to normalize the data. To normalize the data we need to specify the factor study column and the column with the sample id using `factor_col`  and the `sample_id_col` arguments. Also the method of normalization (by rows) with the `rowNorm` wich in this case is by internal compound `CompNorm`. We specify the internal compound name with `ref`. 


``` r
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE)  # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir)  # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
    impute(imputed_experiment, filtered_experiment, method = "RF",
        k = 5), normalize(normalized_experiment, imputed_experiment,
        factor_col = "Groups", sample_id_col = "sample_id", rowNorm = "CompNorm",
        ref = "Creatine..132.1...90.0.", out_dir = outdir))

```


## Scaling
Scaling is also applied using the `normalize()` function. But to do that, instead of the `rowNorm` we specify it using the `scaleNorm` argument. We defined in the preamble the factor_col and sample_id_col for simplicity.


``` r
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE)  # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir)  # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
factor_col = "Groups"
sample_id_col = "sample_id"


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
    impute(imputed_experiment, filtered_experiment, method = "RF",
        k = 5), normalize(normalized_experiment, imputed_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        rowNorm = "CompNorm", ref = "Creatine..132.1...90.0.",
        out_dir = outdir), normalize(scaled_experiment, normalized_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        scaleNorm = "AutoNorm", out_dir = outdir))
```

## Transformation
Transforming the data is also made with the `normalize()` function under the argument `transNorm`.


``` r
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE)  # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir)  # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
factor_col = "Groups"
sample_id_col = "sample_id"


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
    impute(imputed_experiment, filtered_experiment, method = "RF",
        k = 5), normalize(normalized_experiment, imputed_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        rowNorm = "CompNorm", ref = "Creatine (132.1 / 90.0)",
        out_dir = outdir), normalize(scaled_experiment, normalized_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        scaleNorm = "AutoNorm", out_dir = outdir), normalize(transformed_experiment,
        scaled_experiment, factor_col = factor_col, sample_id_col = sample_id_col,
        transNorm = "LogNorm", out_dir = outdir))
```

## Extracting the data
Now that we have defined all the steps that we want the pipeline to perform we should tell them to extract the data processed. This can be achieved using the `export_data()` function:


``` r
# General config
outdir = "results"
dir.create(outdir, showWarnings = FALSE)  # We create the outdir in case there its not created yet
outdir <- tools::file_path_as_absolute(outdir)  # We get the absolute path of the dir for compatibility

# Load the Data
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"
dataSep <- ","
sampleSep <- ","
variableSep <- ","

# Create the experiment
name <- "ST000284"
description <- "Example metabolomics dataset ST000284"

# Setting up the columns
columns <- c("Groups", "Age", "Gender", "Smoking", "Alcohol")
factor_col = "Groups"
sample_id_col = "sample_id"


list(load_data(data_loaded, dataMatrixPath, sampleMetadataPath,
    variableMetadataPath, dataSep, sampleSep, variableSep), createExperiment(experiment,
    data_loaded, experiment_name = name, experiment_description = description),
    factorize_cols(factorized_experiment, experiment, columns),
    filter_step(filtered_experiment, factorized_experiment, threshold = 0.8,
        filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir),
    impute(imputed_experiment, filtered_experiment, method = "RF",
        k = 5), normalize(normalized_experiment, imputed_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        rowNorm = "CompNorm", ref = "Creatine (132.1 / 90.0)",
        out_dir = outdir), normalize(scaled_experiment, normalized_experiment,
        factor_col = factor_col, sample_id_col = sample_id_col,
        scaleNorm = "AutoNorm", out_dir = outdir), normalize(transformed_experiment,
        scaled_experiment, factor_col = factor_col, sample_id_col = sample_id_col,
        transNorm = "LogNorm", out_dir = outdir), exportData(export,
        transformed_experiment, out_dir = outdir))
```

## Running the pipeline

Now we are ready to run the pipeline. We can view a node map of the steps if we use the `tar_visnetwork()` function of the `targets` package. And we can view a full list of the steps with the function using `tar_manifest()`.

``` r
targets::tar_visnetwork(targets_only = TRUE)
#> You are running {targets} version 1.7.0, and the pipeline was last run with
#> version 1.6.0. Just after version 1.6.0, {targets} made changes that cause the
#> targets in old pipelines to rerun. For details, please see
#> https://github.com/ropensci/targets/blob/main/NEWS.md. Sorry for the
#> inconvenience. As a workaround, you can either rerun this pipeline from
#> scratch, or you can stop/interrupt the pipeline and downgrade to {targets}
#> version 1.6.0 to keep your work up to date in the short term.
```


```{=html}
<div class="visNetwork html-widget html-fill-item" id="htmlwidget-1898b961be2c3b35ceb4" style="width:100%;height:528px;"></div>
<script type="application/json" data-for="htmlwidget-1898b961be2c3b35ceb4">{"x":{"nodes":{"name":["after_filtered_experiment_plot","before_filtered_experiment_plot","data_loaded_data","data_loaded_matrixFile","data_loaded_sample","data_loaded_sampleFile","data_loaded_variable","data_loaded_variableFile","experiment","export","factorized_experiment","factorized_experiment_na","filtered_experiment","filtered_experiment_missingval","filtered_experiment_outliers_plot","imputed_experiment","normalized_experiment","scaled_experiment","transformed_experiment"],"type":["stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem","stem"],"description":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"status":["outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated"],"seconds":[0.25,0.375,0.015,0,0,5.344,0,0,0.156,0.047,0.016,0.047,0.265,0.829,3.25,21.984,2.219,1.922,1.938],"bytes":[54,54,88293,191608,14078,16515,3646,3294,93710,38,93647,93647,87882,93644,54,88241,134043,152663,141511],"branches":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"label":["after_filtered_experiment_plot","before_filtered_experiment_plot","data_loaded_data","data_loaded_matrixFile","data_loaded_sample","data_loaded_sampleFile","data_loaded_variable","data_loaded_variableFile","experiment","export","factorized_experiment","factorized_experiment_na","filtered_experiment","filtered_experiment_missingval","filtered_experiment_outliers_plot","imputed_experiment","normalized_experiment","scaled_experiment","transformed_experiment"],"color":["#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5","#78B7C5"],"id":["after_filtered_experiment_plot","before_filtered_experiment_plot","data_loaded_data","data_loaded_matrixFile","data_loaded_sample","data_loaded_sampleFile","data_loaded_variable","data_loaded_variableFile","experiment","export","factorized_experiment","factorized_experiment_na","filtered_experiment","filtered_experiment_missingval","filtered_experiment_outliers_plot","imputed_experiment","normalized_experiment","scaled_experiment","transformed_experiment"],"level":[8,5,2,1,2,1,2,1,3,12,4,5,7,6,7,8,9,10,11],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"]},"edges":{"from":["filtered_experiment","factorized_experiment","filtered_experiment_missingval","imputed_experiment","filtered_experiment","data_loaded_variableFile","transformed_experiment","experiment","data_loaded_matrixFile","scaled_experiment","factorized_experiment_na","filtered_experiment_missingval","normalized_experiment","data_loaded_sampleFile","data_loaded_data","data_loaded_sample","data_loaded_variable","factorized_experiment"],"to":["imputed_experiment","factorized_experiment_na","filtered_experiment","normalized_experiment","after_filtered_experiment_plot","data_loaded_variable","export","factorized_experiment","data_loaded_data","transformed_experiment","filtered_experiment_missingval","filtered_experiment_outliers_plot","scaled_experiment","data_loaded_sample","experiment","experiment","experiment","before_filtered_experiment_plot"],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"edges":{"smooth":{"type":"cubicBezier","forceDirection":"horizontal"}},"physics":{"stabilization":false},"interaction":{"zoomSpeed":1},"layout":{"hierarchical":{"enabled":true,"direction":"LR"}}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":{"text":"","style":"font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;"},"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":{"from":1,"to":1},"algorithm":"hierarchical","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":true,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"},"legend":{"width":0.2,"useGroups":false,"position":"right","ncol":1,"stepX":100,"stepY":100,"zoom":true,"nodes":{"label":["Outdated","Stem"],"color":["#78B7C5","#899DA4"],"shape":["dot","dot"]},"nodesToDataframe":true},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);"},"evals":[],"jsHooks":[]}</script>
```


``` r
targets::tar_manifest()
#> # A tibble: 19 × 2
#>    name                              command                                    
#>    <chr>                             <chr>                                      
#>  1 data_loaded_sampleFile            "\"data/ST000284/sampleMetadata.csv\""     
#>  2 data_loaded_variableFile          "\"data/ST000284/variableMetadata.csv\""   
#>  3 data_loaded_matrixFile            "\"data/ST000284/dataMatrix.csv\""         
#>  4 data_loaded_sample                "read.csv(data_loaded_sampleFile, sep = \"…
#>  5 data_loaded_variable              "read.csv(data_loaded_variableFile, sep = …
#>  6 data_loaded_data                  "read.csv(data_loaded_matrixFile, sep = \"…
#>  7 experiment                        "warper_createExperiment(data_loaded_data,…
#>  8 factorized_experiment             "warper_factor_sample_col(experiment, c(\"…
#>  9 factorized_experiment_na          "zero_to_na(factorized_experiment)"        
#> 10 before_filtered_experiment_plot   "missing_values_plot(factorized_experiment…
#> 11 filtered_experiment_missingval    "filter_MV(factorized_experiment_na, thres…
#> 12 filtered_experiment               "filter_outliers(filtered_experiment_missi…
#> 13 filtered_experiment_outliers_plot "plot_outliers(filtered_experiment_missing…
#> 14 imputed_experiment                "impute_warper(filtered_experiment, method…
#> 15 after_filtered_experiment_plot    "missing_values_plot(filtered_experiment, …
#> 16 normalized_experiment             "normalize_metab(imputed_experiment, \"Gro…
#> 17 scaled_experiment                 "normalize_metab(normalized_experiment, \"…
#> 18 transformed_experiment            "normalize_metab(scaled_experiment, \"Grou…
#> 19 export                            "export_data(dataset_exp = transformed_exp…
```


An now, to run the pipeline we just have to:

``` r
targets::tar_make()
```



# Using the Pilers 

Now that you know the core functions of `metaboPipe`, you can initialize and create the pipeline using the Shiny app for easiness.
In order to call the app:

``` r
metaboPipe::pipePliers()
```




# Session Info

``` r
sessionInfo()
#> R version 4.3.3 (2024-02-29 ucrt)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 11 x64 (build 22631)
#> 
#> Matrix products: default
#> 
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: Europe/Madrid
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] struct_1.14.1    BiocStyle_2.30.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] SummarizedExperiment_1.32.0 xfun_0.44                  
#>  [3] bslib_0.7.0                 htmlwidgets_1.6.4          
#>  [5] visNetwork_2.1.2            processx_3.8.4             
#>  [7] Biobase_2.62.0              lattice_0.22-6             
#>  [9] callr_3.7.6                 vctrs_0.6.5                
#> [11] tools_4.3.3                 ps_1.7.6                   
#> [13] bitops_1.0-7                stats4_4.3.3               
#> [15] base64url_1.4               tibble_3.2.1               
#> [17] fansi_1.0.6                 pkgconfig_2.0.3            
#> [19] Matrix_1.6-5                data.table_1.15.4          
#> [21] secretbase_0.5.0            S4Vectors_0.40.2           
#> [23] lifecycle_1.0.4             GenomeInfoDbData_1.2.11    
#> [25] compiler_4.3.3              codetools_0.2-20           
#> [27] GenomeInfoDb_1.38.8         htmltools_0.5.8.1          
#> [29] usethis_2.2.3               sass_0.4.9.9000            
#> [31] RCurl_1.98-1.14             yaml_2.3.8                 
#> [33] pillar_1.9.0                crayon_1.5.2               
#> [35] jquerylib_0.1.4             cachem_1.1.0               
#> [37] DelayedArray_0.28.0         abind_1.4-5                
#> [39] tidyselect_1.2.1            digest_0.6.35              
#> [41] purrr_1.0.2                 bookdown_0.39              
#> [43] fastmap_1.2.0               grid_4.3.3                 
#> [45] cli_3.6.2                   SparseArray_1.2.4          
#> [47] magrittr_2.0.3              S4Arrays_1.2.1             
#> [49] utf8_1.2.4                  withr_3.0.0                
#> [51] backports_1.4.1             metaboPipe_0.0.0.9000      
#> [53] rmarkdown_2.27              XVector_0.42.0             
#> [55] matrixStats_1.3.0           igraph_2.0.3               
#> [57] evaluate_0.23               knitr_1.46                 
#> [59] GenomicRanges_1.54.1        IRanges_2.36.0             
#> [61] rlang_1.1.3                 ontologyIndex_2.12         
#> [63] glue_1.7.0                  BiocManager_1.30.23        
#> [65] formatR_1.14                BiocGenerics_0.48.1        
#> [67] rstudioapi_0.16.0           jsonlite_1.8.8             
#> [69] R6_2.5.1                    MatrixGenerics_1.14.0      
#> [71] targets_1.7.0               fs_1.6.4                   
#> [73] zlibbioc_1.48.2
```










