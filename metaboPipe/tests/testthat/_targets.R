
  library(targets)
  library(tarchetypes)
  library(metaboPipe)

  tar_option_set(
    packages = c(
      'structToolbox', 'SummarizedExperiment', 'VIM', 'impute', 'imputeLCMD',
      'missForest', 'caret', 'pcaMethods', 'tidyverse', 'MetaboAnalystR', 'tinytex',
      'HotellingEllipse', 'ggforce', 'tools', 'cowplot', 'metaboPipe', 'sva', 'data.table'
    )
  )


  #### Global variables #####
  # General config
  outdir = 'Results'
  dir.create(outdir, showWarnings = FALSE) # We create the outdir in case there its not created yet
  outdir <- tools::file_path_as_absolute(outdir) # We get the absolute path of the dir for compatibility

  list(





  )

