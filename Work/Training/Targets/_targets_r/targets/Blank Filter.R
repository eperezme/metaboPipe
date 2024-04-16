
list( 
  tar_target(blank_filtered, filter_blanks(knn_imputed, fold_change = 20, 
                                           blank_label = 'Blank', qc_label = 'QC', 
                                           factor_name = 'sample_type', fraction_in_blank = 0 ))
  )

