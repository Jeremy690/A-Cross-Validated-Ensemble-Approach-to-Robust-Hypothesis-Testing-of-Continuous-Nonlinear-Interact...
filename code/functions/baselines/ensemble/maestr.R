# given weights, and kernel name, find assembled kernel

maestr <- 
  function(ensemble_fit, type = c("main", "intr", "full"), 
           final_lambda = NULL){
    
    type <- match.arg(type)
    
    # take in prediction from ensemble
    # create representation for ensemble kernel
    n <- ensemble_fit$y %>% length
    lambda <- ensemble_fit$lambda
    kfunc_list <- ensemble_fit$kernfunc
    weight <- ensemble_fit$weight
    int_id <- ensemble_fit$int_id
    
    X <- ensemble_fit$X
    Z <- ensemble_fit$Z
    
    # 0. create ensemble beta coefficient
    beta <- ensemble_fit$beta
    final_beta <- beta %*% weight
    
    # 1. create sum of individual hat kernel matrix
    hatMat_ensemble <- 
      matrix(0, nrow = n, ncol = n)
    
    for (kern_id in which(weight>0)) {
      # kernel matrix
      kernfunc <- kfunc_list[[kern_id]]
      K_tr <- 
        make_kernel_cv(
          Z_list = Z, 
          int_id = int_id,
          kernfunc = kernfunc, 
          effect_type = type, 
          cv_idx = FALSE)$tr
      
      # cumulate hat matrix
      eigen_list <- eigen(K_tr)
      U <- eigen_list$vectors
      d <- eigen_list$values
      
      hatMat_ensemble <- 
        hatMat_ensemble + 
        weight[[kern_id]] * 
        U %*% diag(d/(d + lambda[[kern_id]])) %*% t(U)
    }
    
    # 2. create ensemble matrix
    eigen_list <- eigen(hatMat_ensemble)
    if (is.null(final_lambda)){
      final_lambda <- 1
      # final_lambda <- mean(lambda[which(weight>0)])
      # final_lambda <- 
      #   min((1 - eigen_list$values)/eigen_list$values)
    }
    
    U_K <- eigen_list$vectors
    d_K <- 
      final_lambda * 
      eigen_list$values/(1 - eigen_list$values)
    
    # return
    K <- U_K %*% diag(d_K) %*% t(U_K)
    list(K = K, U_K = U_K, d_K = d_K, 
         beta = final_beta,
         H = hatMat_ensemble, 
         lambda = final_lambda)
  }
