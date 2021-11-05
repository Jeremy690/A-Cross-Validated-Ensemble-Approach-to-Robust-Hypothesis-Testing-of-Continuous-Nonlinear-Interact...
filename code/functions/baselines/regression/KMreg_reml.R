library(kernlab)

kreg_reml <- 
  function(y, X, Z, K, int_id,
           # method to select hyper-parmeters
           hypr_type = c("cv", "ml"), 
           verbose = FALSE, tol = 1e-4)
  {
    # hypr_type: method to select kernel parameter
    # > ml: by maximizing model likelihood (only rbf supported)
    # > cv: by minimizing cv error
    
    if (is.null(int_id)) int_id <- 1:length(Z)
    hypr_type <- match.arg(hypr_type)
    if (hypr_type == "ml") 
      cat("\n use mle-calibrated rbf kernel \n")
    
    n <- length(y)
    p <- ncol(X)

    # for compatibility to lskm_ridge
    y_cv <- rep(0, n)
    X_cv <- matrix(0, nrow = n, ncol = p)
    K_cv <- matrix(0, nrow = n, ncol = n) 
    
    # training
    tau_cur <- 1
    sig_cur <- 1
    lik_old <- Inf; lik_cur <- 0 
    count <- 0
    
    while(abs(lik_old - lik_cur) > tol){
      if (count > 1000){
        break
      }
      
      # update regression parameters
      yfit <- 
        lskm_ridge(y_tr = y, X_tr = X, K_tr = K, 
                   y_cv = y_cv, X_cv = X_cv, K_cv = K_cv, 
                   lambda = sig_cur/tau_cur)
      
      resid_fix <- y - X %*% yfit$beta # resid from fixed eff estimates
      resid_all <- y - yfit$tr # resid from fixed+random eff estimates
      
      # update variance parameters
      sig_cur <- 
        update_sig(resid_all, X, K,
                   sig = sig_cur, tau = tau_cur)
      tau_obj <- 
        update_tau(resid_fix, X, K,
                   sig = sig_cur, tau = NULL)
      tau_cur <- tau_obj$tau
      
      if (hypr_type == "ml"){
        # if hyper par selection method is ml, 
        # then use Z to produce rbf kernel
        rho_cur <- 
          update_rho(resid_fix, X, Z = Z, 
                     sig = sig_cur, tau = tau_cur, 
                     int_id = int_id)
        K <- 
          make_kernel(
            Z, int_id, 
            kern_type = "rbf", 
            kern_par = 1/rho_cur$rho,
            effect_type = "main")
      }
      
      # likelihood inspection and wrap up
      lik_old <- lik_cur
      lik_cur <- tau_obj$logLik
      
      count <- count + 1
      if (verbose) cat(lik_cur, "..")
    }
    
    if (verbose) cat("\n")
    
    # return
    outList <-
      list(yhat = yfit$tr, beta = yfit$beta, 
           sigma = sig_cur, tau = tau_cur, rho = NULL)
    
    if (hypr_type == "ml"){
      outList$rho = rho_cur$rho
    }
    
    return(outList)
  }