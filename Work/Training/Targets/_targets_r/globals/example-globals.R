options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("structToolbox", "SummarizedExperiment",
    "VIM", "impute", "imputeLCMD", "missForest", "caret", "pcaMethods", 
    "metabolomicsWorkbenchR", "tidyverse", "tarchetypes"))
file.sources <- list.files("R", pattern = "*.R", full.names = TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))
