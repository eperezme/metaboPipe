# Signal drift and batch correction
batch_correction <- function(dataset_exp, order_col, batch_col, qc_col, qc_label) {
  
  M <- sb_corr(
    order_col = order_col,
    batch_col = batch_col,
    qc_col = qc_col,
    qc_label = qc_label,
    # spar_lim = c(0.6,0.8)
    )
  
  M <- model_apply(M, dataset_exp)
  
  return(predicted(M))
  
  
  
  ### THIS IS FOR PLOTING THE SIGNAL DRIFT AND BATCH CORRECTION #### \
  # BUT WE COULD USE A PCA PLOT INSTEAD
  
  # C = feature_profile(
  #   run_order=order_col,
  #   qc_label=qc_label,
  #   qc_column=qc_col,
  #   colour_by='batch_qc',
  #   feature_to_plot=,
  #   plot_sd=FALSE
  # )
  
}
