list(
  tar_target(mean_imputed, impute_mean(filtered_experiment)),
  tar_target(median_imputed, impute_median(filtered_experiment)),
  tar_target(RF_imputed, impute_RF(filtered_experiment)),
  tar_target(QRILC_imputed, impute_QRILC(filtered_experiment)),
  tar_target(knn_imputed, impute_kNN(filtered_experiment, 5)),
  tar_target(svd_imputed, impute_SVD(filtered_experiment, k = 5)),
  tar_target(bpca_imputed, impute_bpca(filtered_experiment, nPCs = 5)),
  tar_target(ppca_imputed, impute_ppca(filtered_experiment, nPCs = 5))
)

