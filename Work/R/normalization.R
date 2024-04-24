##### Probabilistic Quotient normalization (PQN) #####
normalize_pqn <- function(dataset_experiment, qc_label,factor_name) {
  # It is not recomended to use pqn when the number of columns is greater than the number of rows
  if (ncol(dataset_experiment) > nrow(dataset_experiment)) {
    warning("The number of columns is greater than the number of rows. It is not recomended to use pqn when the number of Metabolites is greater than the number of Samples")
  }
  M = pqn_norm(qc_label=qc_label,factor_name=factor_name)
  
  M = model_apply(M, dataset_experiment)
  
  return(predicted(M))
}




#### Quantile Normalization (QN) ####
normalize_qn <- function(dataset_experiment, qc_label,factor_name) {
  if (nrow(dataset_experiment) <= 50) {
    warning("The number of rows is less than 50. It is not recomended to use qn when the number of Samples is less than 50")
  }
  
  return(predicted(M))
}


##### Vector lenght Normalization (VLN) #####
normalize_vln <- function(dataset_experiment) {
  M = vec_norm()
  
  M = model_apply(M, dataset_experiment)
  
  return(predicted(M))
}

#### Constant Sum Normalization (CSN) #### (Maybe should be discontinued)
normalize_csn <- function(dataset_experiment, scaling_factor = 1) {
  M = constant_sum_norm(scaling_factor = scaling_factor)
  
  M = model_apply(M, dataset_experiment)
  
  return(predicted(M))
}



#### Internal Standard Normalization (ISN) ####





# 