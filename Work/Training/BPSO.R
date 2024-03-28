# 1. generating particles structure ------------------------------------------------

gen_par_st = function(cand_var, N_particles = 10, value = NULL){
  
  particles = data.frame(matrix(nrow = N_particles, ncol = length(cand_var)))
  rownames(particles) = paste0("particle", seq(1:N_particles))
  colnames(particles) = cand_var
  if(!is.null(value)){
    particles[] = value
  }
  return(particles)
}

# 2. generating first particle ------------------------------------------------

gen_first_par = function(particles){
  for (i in 1:nrow(particles)){
    particles[i,] = sample(c(0,1), replace = TRUE, size = ncol(particles))
    while(sum(particles[i,]) < 1){
      particles[i,] = sample(c(0,1), replace = TRUE, size = ncol(particles))
    }
  }
  return(particles)
}


# 3. fitness_eval_CART -------------------------------------------

fitness_eval_CART = function(X, y, particles, Ncores){
  
  fitness_score = c()
  # trainControl Setting
  if(is.factor(y)){
    if(length(levels(y)) == 2){
      fitControl = trainControl(method = "cv",
                                number = 5,
                                summaryFunction = twoClassSummary,
                                classProbs = TRUE)
      metric = "ROC"
    } else {
      fitControl = trainControl(method = "cv",
                                number = 5,
                                summaryFunction = multiClassSummary,
                                classProbs = TRUE)
      metric = "AUC"}
  } else {
    fitControl = trainControl(method = "cv",
                              number = 5)
    metric = "RMSE"
  }
  
  Cores_fit = Ncores
  cl_fit = makeCluster(Cores_fit)
  doParallel::registerDoParallel(cl_fit)
  
  fitness_score = foreach(p = 1:nrow(particles), .packages =c("rpart", "caret"),
                          .verbose = FALSE) %dopar% {
                            
                            particle_p = colnames(X)[which(particles[p,] == 1)]
                            X_ = X[particle_p]
                            
                            # fitting model
                            fit_rf = train(x = X_,
                                           y = y,
                                           method = "rpart",
                                           trControl = fitControl,
                                           metric = metric
                            )
                            
                            score = mean(unlist(fit_rf$results[metric]))
                            if(metric == "RMSE"){
                              score = -score
                            }
                            return(score)
                          }
  # stopCluster(cl_fit)
  fitness_score = unlist(fitness_score)
  return(fitness_score)
}


# 4. update p_best -----------------------------------------------------------

update_p_best = function(old_pop, new_pop){
  
  p_best = old_pop
  for(i in 1:nrow(old_pop)){
    if(new_pop$fitness[i] > old_pop$fitness[i]){
      p_best[i,] = new_pop[i,]
    }
  }
  return(p_best)
}


# 5. update g_best -----------------------------------------------------------

update_g_best = function(old_pop, new_pop){
  
  g_best = old_pop[which.max(old_pop$fitness),]
  if(max(new_pop$fitness) > max(old_pop$fitness)){
    g_best = new_pop[which.max(new_pop$fitness),]
  }
  
  g_id = g_best
  g_id = g_id[rep(seq_len(nrow(g_id)), nrow(old_pop)), ] ; row.names(g_id) = NULL
  return(g_id)
}


# 6. calculate new velocity --------------------------------------------------

calculate_new_velocity = function(w, v_old, c1, c2, p_id, g_id, x_id){
  
  # set r1 and r2
  r1 = runif(1)
  r2 = runif(1)
  
  # remove fitness column
  p_id = p_id[colnames(x_id)]
  g_id = g_id[colnames(x_id)]
  
  # pb_x and gb_x
  pb_x = p_id - x_id
  gb_x = g_id - x_id
  
  # calculate new velocity
  v_new = w*v_old + c1*r1*(pb_x) + c2*r2*(gb_x)
  return(v_new)
}


# 7. get new particles -------------------------------------------------------

get_new_par = function(new_position){
  
  new_particles = lapply(list(new_position), function(mydata_df){
    rand = runif(1, 0.1, 1)
    mydata_df %>% mutate_all(function(x) ifelse(x > rand, 1, 0))})
  new_particles = new_particles[[1]]
  
  rownames(new_particles) = paste0("particle", seq(1:nrow(new_particles)))
  return(new_particles)
}


# 8. BPSO algorithm -------------------------------------------

BPSO = function(X, y, N_particles, Iteration, w, c1, c2, var_len, verbose = NULL, Ncores){
  
  if(is.null(N_particles)){N_particles = 10}
  if(is.null(Iteration)){Iteration = 3}
  if(is.null(w)){w = 0.3}
  if(is.null(c1)){c1 = 0.3}
  if(is.null(c2)){c2 = 0.6}
  if(is.null(var_len)){var_len = 20}
  if(is.null(verbose)){verbose = FALSE}
  
  filter_var = which(sapply(X, function(x){(length(unique(x)) > 1)}))
  filter_var_names = colnames(X)[filter_var]
  Cores_fit = Ncores
  cl_fit = makeCluster(Cores_fit)
  doParallel::registerDoParallel(cl_fit)
  fit_aic = foreach(i = filter_var, .packages =c("rpart", "caret"),
                    .export = c("fit_simple_model"),
                    .verbose = FALSE) %dopar% {
                      x = X[,i]
                      aic = fit_simple_model(X = x, y = y)
                      return(aic)
                    }
  stopCluster(cl_fit)
  fit_aic = unlist(fit_aic)
  
  # first twenty variables with low AIC  
  if(length(fit_aic) >= var_len){
    cand_var = filter_var_names[order(fit_aic)[1:var_len]] 
  } else {
    cand_var = filter_var_names[order(fit_aic)]
  }
  
  v_old = gen_par_st(cand_var = cand_var, N_particles = 10, value = 0.3)
  par_st = gen_par_st(cand_var = cand_var, N_particles = 10)
  
  for(Iter in 1:Iteration){
    
    if(Iter == 1){
      # generating particles and evaluate fitness
      particles = gen_first_par(particles = par_st)
    }
    if(verbose){cat(paste0("Iteration: ", Iter))}
    
    fitness = fitness_eval_CART(X = X, y = y, particles = particles, Ncores = Ncores)
    pop = cbind(particles, fitness)
    if(verbose){cat(paste0("Fitness evaluated in Iteration ", Iter))}
    
    if(Iter == 1){
      # update p_best value and position
      p_best = update_p_best(old_pop = pop, new_pop = pop)
      
      # update g_best value and position
      g_best = update_g_best(old_pop = pop, new_pop = pop)
    } else {
      # update p_best value and position
      p_best = update_p_best(old_pop = pop_Old, new_pop = pop)
      
      # update g_best value and position
      g_best = update_g_best(old_pop = pop_Old, new_pop = pop)
    }
    
    # calculate new velocity
    v_new = calculate_new_velocity(w = w, v_old = v_old, c1 = c1, c2 = c2, p_id = p_best,
                                   g_id = g_best, x_id = particles)
    
    # get new position
    new_pos = sigmoid(v_new) # library("e1071")
    particles = get_new_par(new_position = new_pos)
    while(any(apply(particles, 1, sum) < 1)){
      if(verbose){cat("getting new position")}
      particles = get_new_par(new_position = new_pos)
    }
    pop_Old = pop
  } # Iteration
  
  final_result = g_best[1,]
  return(final_result)
}

# 9. imputation ----------------------------------------------------------


imputation = function(mydata_true, 
                      mydata_ref, 
                      mydata_impu, 
                      var_cat, 
                      var_con, 
                      target_var_miss = NULL, 
                      Ncores, 
                      mt = "mf", 
                      mar = NULL, 
                      ntree = 100){
  
  
  # imputation_bpso ---------------------------------------------------------
  if(mt == "bp"){
    
    # BPSO + missforest
    
    if(!is.null(mydata_true)){mydata_true = mydata_true[colnames(mydata_ref)]}
    mydata_impu = mydata_impu[colnames(mydata_ref)]
    
    mydata_ref_org = mydata_ref
    mydata_impu_org = mydata_impu
    
    impu_var_idx = which(colnames(mydata_ref) %in% c(var_cat, var_con))
    if(!is.null(target_var_miss)){
      idx_target_var = which(colnames(mydata_ref) %in% target_var_miss)
      impu_var_idx = intersect(impu_var_idx, idx_target_var)
    }
    target_var = colnames(mydata_ref)[impu_var_idx]
    
    # numeric y
    num_y = intersect(which(sapply(mydata_ref, function(x){is.numeric(x)})), impu_var_idx)
    # categorical y(binary)
    binary_y = intersect(which(sapply(mydata_ref, function(x){is.factor(x) & length(levels(x)) == 2})), impu_var_idx)
    # categorical y(multiclass)
    multi_y = intersect(which(sapply(mydata_ref, function(x){is.factor(x) & length(levels(x)) > 2})), impu_var_idx)
    
    sprintf("imputation_start at: %s", Sys.time())
    pb <- progress_bar$new(total = length(impu_var_idx))
    
    for(i in impu_var_idx){
      pb$tick()
      X = mydata_ref[, -i]; y = mydata_ref[, i]
      if(i %in% c(binary_y, multi_y)) {y = paste0("class",y); y = as.factor(y)}
      
      feature = tryCatch(try(BPSO(X = X, y = y,
                                  N_particles = 20, Iteration = 3, w = 0.3, c1 = 0.3, c2 = 0.6, var_len = floor(sqrt(ncol(X))),
                                  verbose = FALSE, Ncores = Ncores)))
      if(class(feature) != "try-error"){
        var_sel_temp = setdiff(colnames(feature)[which(feature[1,] == 1)], "fitness") # select variable
        
        x_mis = cbind(mydata_impu[colnames(mydata_ref)[i]], mydata_ref[var_sel_temp])
        if(getDoParWorkers() == 1 | getDoParWorkers() < Ncores){
          if(Ncores > length(var_sel_temp)){
            Ncores = length(var_sel_temp)
          }
        } else if(getDoParWorkers() >= length(var_sel_temp)) {
          Ncores = length(var_sel_temp)
        }
        cl_inner = makeCluster(Ncores)
        doParallel::registerDoParallel(cl_inner)
        mf_rst = tryCatch(try(missForest::missForest(xmis = x_mis, verbose = TRUE,
                                                     parallelize = "variables",
                                                     maxiter = 2, ntree = ntree,
                                                     replace = FALSE)))
        if(class(mf_rst) == 'try-error'){
          sprintf("Error in missforest : %s ", mf_rst[[1]])
        } else {
          x_impu = mf_rst$ximp[colnames(mydata_ref)[i]]
          if(colnames(x_impu) == colnames(mydata_ref)[i]){
            mydata_ref[colnames(x_impu)] = x_impu
            mydata_impu[colnames(x_impu)] = x_impu
          }
        }
      } else { # class(feature) != "try-error"
        sprintf("Error in BPSO : %s ", feature[[1]])
      }  ; rm(X, y, feature, var_sel_temp, x_mis, mf_rst, x_impu)
      
    } # for loop 
    
    sprintf("imputation_completed at: %s", Sys.time())
    
    xtrue = mydata_true ; xref = mydata_ref_org ; xmis = mydata_impu_org
    x_impu_df = mydata_impu[target_var]
    xtrue = xtrue[target_var] ; xref = xref[target_var] ; xmis = xmis[target_var]
    result = list(ximp = x_impu_df, xtrue = xtrue, xref = xref, xmis = xmis,
                  target = target_var, numvar = colnames(mydata_ref)[num_y], bivar = colnames(mydata_ref)[binary_y], multivar = colnames(mydata_ref)[multi_y]
                  # var_selected
    )
    
  } else if(mt == "mf") {
    # browser()
    # imputation_missforest ---------------------------------------------------
    
    impu_var_idx = which(colnames(mydata_ref) %in% c(var_cat, var_con))
    if(!is.null(target_var_miss)){
      idx_target_var = which(colnames(mydata_ref) %in% target_var_miss)
      impu_var_idx = intersect(impu_var_idx, idx_target_var)
    }
    target_var = colnames(mydata_ref)[impu_var_idx]
    mydata_impu[setdiff(colnames(mydata_ref), target_var)] = mydata_ref[setdiff(colnames(mydata_ref), target_var)]
    
    # numeric y
    num_y = intersect(which(sapply(mydata_ref, function(x){is.numeric(x)})), impu_var_idx)
    # categorical y(binary)
    binary_y = intersect(which(sapply(mydata_ref, function(x){is.factor(x) & length(levels(x)) == 2})), impu_var_idx)
    # categorical y(multiclass)
    multi_y = intersect(which(sapply(mydata_ref, function(x){is.factor(x) & length(levels(x)) > 2})), impu_var_idx)
    
    cl_inner = makeCluster(Ncores)
    doParallel::registerDoParallel(cl_inner)
    sprintf("imputation_start at: %s", Sys.time())
    mf_rst = tryCatch(try(missForest::missForest(xmis = mydata_impu, verbose = TRUE,
                                                 parallelize = "no", maxiter = 5,
                                                 ntree = ntree,
                                                 replace = FALSE)))
    
    sprintf("imputation_completed at: %s", Sys.time())
    
    if(class(mf_rst) == 'try-error'){
      sprintf(
        "Error in missforest : %s ",
        mf_rst[[1]])
    } else {
      mydata_ref[target_var] = ximp = mf_rst$ximp
      result = mydata_ref
      # xtrue = mydata_true ; xref = mydata_ref ;  xmis = mydata_impu
      # xtrue = xtrue[target_var] ; xref = xref[target_var] ; xmis = xmis[target_var]
      # result = list(ximp = mf_rst$ximp, xtrue = xtrue, xref = xref, xmis = xmis,
      #               target = target_var, numvar = colnames(mydata_ref)[num_y], bivar = colnames(mydata_ref)[binary_y], multivar = colnames(mydata_ref)[multi_y])
      # result = "Test"
    }
  } # else if (mt == "mf")
  stopCluster(cl_inner)
  
  if(exists("result")){
    impu_result = result
    return(impu_result)
  }
  print(mydata_true)
}

## example 
require(sf)
require(doParallel)
doParallel::registerDoParallel(cores = 2)
library(car)
library(missForest)
# library(parallel)
data("iris")
iris.mis <- prodNA(iris, noNA = 0.1)
# mydata_true=NULL
# mydata_ref=mydata_impu=iris.mis
# var_cat (categorical variable)="Species"
# var_con (continuous variable)= c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width")
# mt (method)="mt"
iris.imp <- imputation(NULL, iris.mis, iris.mis, "Species", c("Sepal.Length", "Sepal.Width","Petal.Length", "Petal.Width"), mt="bp", Ncores=NULL)
iris.imp
