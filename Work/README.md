
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaboPipe

Shield: [![CC BY-NC-SA
4.0](https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-nc-sa/4.0/)

This work is licensed under a [Creative Commons
Attribution-NonCommercial-ShareAlike 4.0 International
License](http://creativecommons.org/licenses/by-nc-sa/4.0/).

[![CC BY-NC-SA
4.0](https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc-sa/4.0/)

<!-- badges: start -->
<!-- badges: end -->

The goal of metaboPipe is to facilitate the analysis of metabolomics
data by the use of pipelines to preprocess the targeted metabolomics
data. The package includes functions for data filtering, imputation,
normalization, scaling, transformation, data visualization and more.

## Installation

You can install the development version of metaboPipe from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eperezme/metaboPipe", subdir = "Work")
```

## Introduction to `metaboPipe` and its functionalities

For our example we will load the ST000284 dataset. You can view the
description of the dataset by running the following command:

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

`metaboPipe` uses the `DatasetExperiment` object from the
`structToolbox` package as the main **data structure** for storing and
manipulating metabolomics data. The `DatasetExperiment` object is a list
of data frames that contains the following components:

- `data`: This encapsulates a data frame housing the measured data
  pertaining to each sample.
- `sample_meta`: This comprises a data frame furnishing supplementary
  sample-related information, such as group labels, often referred to as
  *phenoData.*
- `variable_meta`: This includes a data frame presenting additional
  details concerning variables (features), such as annotations.

Similar to all `struct` entities, it encompasses `name` and
`description` fields, referred to as “slots” within the R language.

A notable distinction between `DatasetExperiment` and
`SummarizedExperiment` structures lies in the data orientation. In
`DatasetExperiment` instances, **samples are arranged in rows** while
**features occupy columns**, contrasting with the arrangement in
`SummarizedExperiment` structures.

All slots are accessible using dollar notation.

``` r
head(DE$data[, 1:4])
head(DE$sample_meta[, 1:4])
head(DE$variable_meta)
```

# Creating a pipeline

`metaboPipe` uses the `targets` package to define and execute data
processing pipelines. The pipeline is defined as a list of targets, each
representing a step in the data processing workflow. Each target is a
function call that takes input data and produces output data. The
`targets` package ensures that each target is executed only when its
dependencies are met, allowing for efficient and reproducible data
processing.

The `metaboPipe` pipeline functions consists of several functions that
create targets to execute specific processing steps, including data
loading, filtering, batch correction, imputation and normalization (that
includes tranformation and scaling). Each step is defined as a single or
multiple targets in the pipeline, and the targets are executed
sequentially to process the data.

In order to use the pipeline functions in `metaboPipe`, we need to
create a targets file. You can use the `use_targets()` function to
create a new `_targets.R` file in the working directory or use the
`metaboPipe` template `create_pipeline()` function:

``` r
create_pipeline()
```

An example `_targets.R` file is created and saved in the working
directory. The `_targets.R` file contains some example code, which we
will modify to define the pipeline for our data processing workflow.
First of all we will load the required packages for `metaboPipe` and set
the target options in the `_targets.R` file.

``` r
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(metaboPipe)

tar_option_set(
  packages = c(
    "structToolbox", "SummarizedExperiment", "VIM", "impute", "imputeLCMD",
    "missForest", "caret", "pcaMethods", "tidyverse", "MetaboAnalystR", "tinytex",
    "HotellingEllipse", "ggforce", "tools", "cowplot", "metaboPipe"
  )
)
```

Now that the packages for our pipeline are set up, we can define the
pipeline workflow. The pipeline consists of several steps, including
data loading, filtering, batch correction, imputation, and
normalization. Each step is defined as a function in the script but
those functions create multiple targets, and the targets are executed
sequentially to process the data.

The targets should be defined inside a `list()` function. We can load
the data using the `tar_target()` function, which defines a target that
loads the data from the specified file path. We first will load the data
using the `tar_target()` function, which defines a target that reads the
data from the specified file path. And with the 3 dataframes loaded we
can create another target to merge them into a `DatasetExperiment`
object.

The first variable on the `tar_target()` function is the name of the
target, that will be used for refering the objects on subsequent targets
and the second variable is the code that will be executed to generate
the target.

``` r
list(
  tar_target(dataMatrix, read.csv("data/ST000284/dataMatrix.csv")),
  tar_target(sampleMeta, read.csv("data/ST000284/sampleMetadata.csv")),
  tar_target(variableMetadata, read.csv("data/ST000284/variableMetadata.csv")),
  tar_target(experiment, warper_create_experiment(dataMatrix, sampleMeta, variableMetadata,
    experiment_name = "ST000284", experiment_description = "Example metabolomics dataset ST000284"
  ))
)
```

If then we execute `tar_make()` the pipeline will be executed and the
data will be loaded and merged into a `DatasetExperiment` object. To
load the object into the R environment we can use the `tar_load()`
function.

``` r
tar_load(experiment)
```

Even though the pipeline is correctly defined, `metaboPipe` provides a
set of functions to make the pipeline definition easier.

## Using metaboPipe functions to define the pipeline

### Loading the data

Instead of writing 3 targets to load the 3 dataframes we can define the
paths to the files before the targets list and then use the
`load_data()` function to load the 3 dataframes. And then we can use the
`create_experiment()` function to merge the 3 dataframes into a
`DatasetExperiment` object.

The `load_data()` function takes the paths to the data files as
arguments and returns a list with the loaded dataframes. The
`create_experiment()` function takes the loaded dataframes as arguments
and returns a `DatasetExperiment` object.

Same as before, the first variable on the functions is the name of the
target.

``` r
dataMatrixPath <- "data/ST000284/dataMatrix.csv"
sampleMetadataPath <- "data/ST000284/sampleMetadata.csv"
variableMetadataPath <- "data/ST000284/variableMetadata.csv"

list(
  load_data(data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath),
  create_experiment(experiment, data_loaded,
    experiment_name = "ST000284",
    experiment_description = "Example metabolomics dataset ST000284"
  )
)
```

Now if we run `tar_make()` the pipeline will be executed and the data
will be loaded and merged into a `DatasetExperiment` object. As before
to load the object into the R environment we can use the `tar_load()`
function.

``` r
tar_load(experiment)
```

By default, `load_data()` uses the “,” as the separator for the data
files. If the data files use a different separator, we can define for
each file the separator:

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

list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  )
)
```

## Factorizing the columns of the experiment

There are functions that assume that specific columns are factors, such
as the `normalize()` function. To factorize the columns of the
experiment we can use the `factorize_cols()` function. This function
takes the `DatasetExperiment` object, and a vector of column names as an
argument and returns the object with the columns factorized.

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


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns)
)
```

## Filtering the data

After the setup of the experiment, we can start the data manipulation
steps. The first step we will perform is to filter the data. The
`filter_step()` function encompases 2 filtering steps: 1) filtering by %
of missing values and 2) filtering the outliers. The threshold is
defined as the minimum percentage of data explained. By default the
`threshold` is `0.8`, meaning that each row and collumn must have at
least 80% of the values or get removed. The `filter_outliers` argument
is a boolean that defines if the outliers should be removed or not and
the `conf.limit` argument specify the confidence interval (either
`'0.95'` or `'0.99'`) to discriminate the observation as an outlier or
not.

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


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8,
    filter_outliers = TRUE, conf.limit = "0.95", out_dir = "results"
  )
)
```

The `out_dir` option is where the plots will be exported under the path:
`out_dir/plots`, in this case: `results/plots`.

## Imputing

The next step is to impute the missing values that remain after the
filter step. To perform that we use the `impute()` function specifying
the impute method we want to use and if it needs a special parameter
like knn we specify it using `k`. In this example we will use a
`Random forest` approach.

``` r
# General config
outdir <- "results"
# We create the outdir in case there its not created yet
dir.create(outdir, showWarnings = FALSE)
# We get the absolute path of the dir for compatibility
outdir <- tools::file_path_as_absolute(outdir)

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


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8,
    filter_outliers = TRUE, conf.limit = "0.95", out_dir = outdir
  ),
  # Impute using Random forest
  impute(imputed_experiment, filtered_experiment, method = "RF", k = 5)
)
```

## Normalizing

Next step is to normalize the data. To do so we use the `normalize()`
function, that uses MetaboAnalystR to normalize the data. To normalize
the data we need to specify the factor study column and the column with
the sample id using `factor_col` and the `sample_id_col` arguments. Also
the method of normalization (by rows) with the `rowNorm` wich in this
case is by internal compound `CompNorm`. We specify the internal
compound name with `ref`.

``` r
# General config
outdir <- "results"
dir.create(outdir, showWarnings = FALSE)
outdir <- tools::file_path_as_absolute(outdir)

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


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95",
    out_dir = outdir
  ),
  impute(imputed_experiment, filtered_experiment, method = "RF", k = 5),
  normalize(normalized_experiment, imputed_experiment,
    factor_col = "Groups", sample_id_col = "sample_id", rowNorm = "CompNorm", ref = "Creatine..132.1...90.0.",
    out_dir = outdir
  )
)
```

## Scaling

Scaling is also applied using the `normalize()` function. But to do
that, instead of the `rowNorm` we specify it using the `scaleNorm`
argument. We defined in the preamble the factor_col and sample_id_col
for simplicity.

``` r
# General config
outdir <- "results"
dir.create(outdir, showWarnings = FALSE)
outdir <- tools::file_path_as_absolute(outdir)

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
factor_col <- "Groups"
sample_id_col <- "sample_id"


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95",
    out_dir = outdir
  ),
  impute(imputed_experiment, filtered_experiment, method = "RF", k = 5),
  normalize(normalized_experiment, imputed_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = "CompNorm",
    ref = "Creatine..132.1...90.0.",
    out_dir = outdir
  ),
  normalize(scaled_experiment, normalized_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm",
    out_dir = outdir
  )
)
```

## Transformation

Transforming the data is also made with the `normalize()` function under
the argument `transNorm`.

``` r
# General config
outdir <- "results"
dir.create(outdir, showWarnings = FALSE)
outdir <- tools::file_path_as_absolute(outdir)

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
factor_col <- "Groups"
sample_id_col <- "sample_id"


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95",
    out_dir = outdir
  ),
  impute(imputed_experiment, filtered_experiment, method = "RF", k = 5),
  normalize(normalized_experiment, imputed_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = "CompNorm",
    ref = "Creatine..132.1...90.0.",
    out_dir = outdir
  ),
  normalize(scaled_experiment, normalized_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm",
    out_dir = outdir
  ),
  normalize(transformed_experiment, scaled_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, transNorm = "LogNorm",
    out_dir = outdir
  )
)
```

## Extracting the data

Now that we have defined all the steps that we want the pipeline to
perform we should tell them to extract the data processed. This can be
achieved using the `export_data()` function:

``` r
# General config
outdir <- "results"
dir.create(outdir, showWarnings = FALSE)
outdir <- tools::file_path_as_absolute(outdir)

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
factor_col <- "Groups"
sample_id_col <- "sample_id"


list(
  load_data(
    data_loaded, dataMatrixPath, sampleMetadataPath, variableMetadataPath,
    dataSep, sampleSep, variableSep
  ),
  create_experiment(experiment, data_loaded,
    experiment_name = name,
    experiment_description = description
  ),
  factorize_cols(factorized_experiment, experiment, columns),
  filter_step(filtered_experiment, factorized_experiment,
    threshold = 0.8, filter_outliers = TRUE, conf.limit = "0.95",
    out_dir = outdir
  ),
  impute(imputed_experiment, filtered_experiment, method = "RF", k = 5),
  normalize(normalized_experiment, imputed_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, rowNorm = "CompNorm",
    ref = "Creatine..132.1...90.0.",
    out_dir = outdir
  ),
  normalize(scaled_experiment, normalized_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, scaleNorm = "AutoNorm",
    out_dir = outdir
  ),
  normalize(transformed_experiment, scaled_experiment,
    factor_col = factor_col, sample_id_col = sample_id_col, transNorm = "LogNorm",
    out_dir = outdir
  ),
  export_data(export, transformed_experiment, out_dir = outdir)
)
```

## Running the pipeline

Now we are ready to run the pipeline. We can view a node map of the
steps if we use the `tar_visnetwork()` function of the `targets`
package. And we can view a full list of the steps with the function
using `tar_manifest()`.

``` r
targets::tar_visnetwork(targets_only = TRUE)
targets::tar_manifest()
```

An now, to run the pipeline we just have to:

``` r
targets::tar_make()
```

# Using the Pilers

Now that you know the core functions of `metaboPipe`, you can initialize
and create the pipeline using the Shiny app for easiness. In order to
call the app:

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
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> other attached packages:
#> [1] struct_1.14.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] utf8_1.2.4                  renv_1.0.7                 
#>  [3] SparseArray_1.2.4           bitops_1.0-7               
#>  [5] lattice_0.22-6              digest_0.6.35              
#>  [7] magrittr_2.0.3              grid_4.3.3                 
#>  [9] evaluate_0.23               fastmap_1.2.0              
#> [11] Matrix_1.6-5                ontologyIndex_2.12         
#> [13] processx_3.8.4              backports_1.4.1            
#> [15] GenomeInfoDb_1.38.8         secretbase_0.5.0           
#> [17] ps_1.7.6                    BiocManager_1.30.23        
#> [19] purrr_1.0.2                 fansi_1.0.6                
#> [21] codetools_0.2-20            abind_1.4-5                
#> [23] cli_3.6.2                   crayon_1.5.2               
#> [25] rlang_1.1.3                 XVector_0.42.0             
#> [27] metaboPipe_0.0.0.9000       Biobase_2.62.0             
#> [29] DelayedArray_0.28.0         yaml_2.3.8                 
#> [31] S4Arrays_1.2.1              tools_4.3.3                
#> [33] base64url_1.4               GenomeInfoDbData_1.2.11    
#> [35] SummarizedExperiment_1.32.0 BiocGenerics_0.48.1        
#> [37] vctrs_0.6.5                 R6_2.5.1                   
#> [39] matrixStats_1.3.0           stats4_4.3.3               
#> [41] lifecycle_1.0.4             zlibbioc_1.48.2            
#> [43] S4Vectors_0.40.2            fs_1.6.4                   
#> [45] IRanges_2.36.0              usethis_2.2.3              
#> [47] targets_1.7.0               pkgconfig_2.0.3            
#> [49] callr_3.7.6                 pillar_1.9.0               
#> [51] data.table_1.15.4           glue_1.7.0                 
#> [53] xfun_0.44                   tibble_3.2.1               
#> [55] GenomicRanges_1.54.1        tidyselect_1.2.1           
#> [57] rstudioapi_0.16.0           MatrixGenerics_1.14.0      
#> [59] knitr_1.46                  htmltools_0.5.8.1          
#> [61] igraph_2.0.3                rmarkdown_2.27             
#> [63] compiler_4.3.3              RCurl_1.98-1.14
```
