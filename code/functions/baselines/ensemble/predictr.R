# take in result from grader, produce prediction

predict.cvek <- 
  function(res, Z_new, X_new = NULL, effect_type = "full"){
    # res: output of grader function
    # X_new: new input with col names 
    # Z_new: new input with col names 
    
    # 1. preprocess input
    n <- nrow(Z_new[[1]])
    if (is.null(X_new)){
      X_new_row <- apply(res$X, 2, median)
      X_new <- 
        matrix(rep(X_new_row, each = n), 
               nrow = n) %>% 
        set_colnames(names(res$X))
    }
    
    # 2. build prediction for each active kernel
    k_idx_list <- which(res$weight > 0)
    K_list <- vector("list", length = length(k_idx_list))
    f_list <- vector("list", length = length(k_idx_list)) 
    
    f_pred <- matrix(0, nrow = n, ncol = 1)
    
    for (k_id in k_idx_list){
      # prediction matrix 
      kern_func <- res$kernfunc[[k_id]]
      K_list[[k_id]] <- 
        make_kernel_pred(
          Z_new, res$Z, kern_func, 
          effect_type = effect_type)
      
      # prediction function for each kernel
      f_list[[k_id]] <- 
        X_new %*% rowMeans(res$beta) +
        K_list[[k_id]] %*% res$alpha[, k_id]
      
      # overall prediction
      f_pred <-
        f_pred + f_list[[k_id]] * res$weight[k_id]
    }
    
    # 3. build interaction function
    f_pred
    
  }

