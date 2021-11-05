library(kernlab)

# # for parallel
library(foreach)
library(doParallel)
# library(tcltk)

#### <<<< main function >>>> ####

kreg_ensemble <- 
  function(y = NULL, X = NULL, Z_list = NULL, 
           int_id = NULL,
           data_list = NULL, # altly, provide a list of data
           kern_name = NULL, kern_par = NULL, lambda = NULL,
           type = c("main", "full"), 
           fold = 10, 
           verbose = FALSE
  )
  {
    type <- match.arg(type)
    if (is.null(int_id)) 
      int_id <- 1:length(Z_list)
    
    warning_header <- "kreg_ensemble:"
    #### 0. default value for parameters ####
    # input data validation
    if (!is.null(data_list)){
      y <- data_list$y
      X <- data_list$X
      Z_list <- data_list$Z_list
    }
    
    if (is.null(y)|is.null(X)|is.null(Z_list)){
      "some input data is of type NULL" %>%
        paste(warning_header, .) %>% stop
    }
    
    if (!all(c(class(y), class(X), sapply(Z_list, class)) %in%
             c("matrix", "numeric"))){
      "some input data is not of type matrix/numeric" %>%
        paste(warning_header, .) %>% stop
    }
    
    #### 1. initiate var and container ####
    # get library of kernel functions
    kfunc_list <- kernfuncLib(kern_name, kern_par)
    
    # get sample/library size
    n <- length(y)
    n_k <- length(kfunc_list)
    
    # get cv index/output container
    cv_idx_list <- cvIndex(n, fold = fold)
    fit_container <- cv_container <- 
      matrix(NaN, nrow = n, ncol = n_k + 1, 
             dimnames = 
               list(NULL, c("y", names(kfunc_list)))
      )
    
    fit_container[, "y"] <- 
      cv_container[ , "y"] <- y
    
    alpha_container <- 
      matrix(NaN, nrow(X), n_k) %>% set_colnames(names(kfunc_list))
    beta_container <- 
      matrix(NaN, ncol(X), n_k) %>% set_colnames(names(kfunc_list))
    lambda_container <- 
      rep(NaN, n_k) %>% set_names(names(kfunc_list))
    
    #### 2. training ####
    # for each fold, train all kernels in library, then pred
    if (verbose){
      cat("Ensemble trainig initiated,", n_k, "kernels in total\n")
    } else if (n_k > 1) {
      pb_kern <- txtProgressBar(min = 1, max = n_k, style = 3)
    }
    
    # detect cores
    #no_cores <- parallel:::detectCores()
    #cl <- makeCluster(no_cores)
    #registerDoParallel(cl)  
    kreg_fit_list <- vector("list", n_k)
    for(kern_idx in 1:n_k){
      kernname <- names(kfunc_list)[kern_idx]
      kernfunc <- kfunc_list[[kern_idx]]
      
      if (verbose){
        cat(paste0("\n", kern_idx, ". ", kernname, ":\n"))
      } else if (n_k > 1) {
        setTxtProgressBar(pb_kern, kern_idx)
      }
      
      # kernel fitting procedure goes here
      kreg_fit_list[[kern_idx]] <- 
        kreg_cv(y, X, Z_list, 
                int_id = int_id,
                lambda = lambda,
                type = type, 
                kernfunc = kernfunc, 
                kern_idx = kern_idx, 
                cv_idx_list = cv_idx_list, 
                verbose = verbose)
    }
    
    # end parallel
    # stopCluster(cl)
    if (verbose) {
      cat("\n==================================\n\n")
    }
    
    # fill in fit
    for (kern_idx in 1:n_k){
      fit_container[, kern_idx+1] <- kreg_fit_list[[kern_idx]]$pred_tr
      cv_container[,  kern_idx+1] <- kreg_fit_list[[kern_idx]]$pred_cv
      lambda_container[kern_idx] <- kreg_fit_list[[kern_idx]]$lambda
      alpha_container[, kern_idx] <- kreg_fit_list[[kern_idx]]$alpha
      beta_container[, kern_idx] <- kreg_fit_list[[kern_idx]]$beta
    }
    
    
    if (verbose) {
      cat("#~~~~~~~~~~~~~~Done!~~~~~~~~~~~~~~#\n")
      cat("==================================\n\n")
    }
    
    # return y and y_pred from each kernel
    out <- 
      list(tr = fit_container, 
           cv = cv_container, 
           lambda = lambda_container, 
           beta = beta_container,
           alpha = alpha_container,
           kernfunc = kfunc_list, 
           kerntype = type,
           y = y, X = X, Z = Z_list, 
           int_id = int_id)
    
    attr(out, "class") <- "cvek" 
    out
  }

#### <<<< sub-routines >>>> ####
kreg_cv <- 
  function(y, X, Z_list, int_id, lambda, 
           type = c("main", "full"), 
           kernfunc, kern_idx, 
           cv_idx_list, verbose)
  {
    type <- match.arg(type)
    
    # range of lambda to try for this kernel
    if (is.null(lambda)){
      lambda <- exp(c(seq(-14, -5, 1), seq(-5, 8, 0.1)[-1]))
    }
    
    fold <- length(cv_idx_list)
    n <- length(y)
    
    # initiate procedure
    # kernel-specific containers for lambda selection
    cv_container_kern <-
      matrix(NaN, nrow = n, ncol = length(lambda), 
             dimnames = list(NULL, lambda))
    
    # detect cores
    no_cores <- parallel:::detectCores()
    # TODO: change envir from Global to package envir
    registerDoParallel(no_cores)
    
    yfit_list <- 
      #      foreach(fold_idx = 1:fold, .packages = "tcltk") %dopar% {
      foreach(fold_idx = 1:fold) %dopar% {
        # for each kernel, 10 fold cv 
        cv_idx <- cv_idx_list[[fold_idx]]
        
        kreg_cv_fold(
          y, X, Z_list, int_id, kernfunc, 
          cv_idx, lambda, type = type)
        
        # if (verbose){
        #   if(!exists("pb_cv")){
        #     pb_cv <- txtProgressBar(min=1, max=fold, style = 3)
        #     # pb_cv <- tkProgressBar("CV Progress", min=1, max=fold)
        #   }
        #   setTxtProgressBar(pb_cv, fold_idx)
        #   # setTkProgressBar(pb_cv, fold_idx)
        # }
      }
    
    # end parallel
    stopImplicitCluster()
    
    for (fold_idx in 1:fold){
      yfit <- yfit_list[[fold_idx]]
      cv_idx <- cv_idx_list[[fold_idx]]
      cv_container_kern[cv_idx, ] <-
        lapply(yfit, function(yfit_lambda) yfit_lambda$cv) %>% 
        do.call(cbind, .)
    }
    
    # choose optimal lambda
    cv_error <- 
      apply(
        cv_container_kern, 2, 
        function(yhat) mean((y - yhat)^2)
      )
    
    
    lambda_idx <- which.min(cv_error)
    lambda_opt <- lambda[lambda_idx]
    
    # plot_idx <- 1:20
    # plot(names(cv_error)[plot_idx], cv_error[plot_idx], type = "l", 
    #      ylim = c(0, 0.15))
    # lines(names(train_error)[plot_idx], train_error[plot_idx], col = 2)
    # lines(names(diff_error)[plot_idx], diff_error[plot_idx], lty = 2)
    
    # produce final fit
    pred_fit <-
      kreg_cv_fold(y, X, Z_list, int_id, kernfunc, 
                   numeric(0), lambda = lambda_opt)[[1]]
    pred_tr <- pred_fit$tr
    beta_tr <- pred_fit$beta
    alpha_tr <- pred_fit$alpha
    pred_cv <- cv_container_kern[, lambda_idx]
    
    # return
    list(pred_tr = pred_tr, pred_cv = pred_cv, 
         beta = beta_tr, alpha = alpha_tr,
         lambda = lambda_opt)
  }

kreg_cv_fold <- 
  function(y, X, Z_list, int_id, kernfunc, 
           cv_idx, lambda, 
           type = c("main", "full"))
  {
    type <- match.arg(type)
    
    # given cv_idx, do below:
    #### 1. build training and cv kernel ####
    if (length(cv_idx) > 0){
      y_tr <- y[-cv_idx]
      y_cv <- y[cv_idx]
      X_tr <- X[-cv_idx, ]
      X_cv <- X[cv_idx, ]
      
      K_list <- 
        make_kernel_cv(
          Z_list, int_id,
          kernfunc = kernfunc, 
          effect_type = type, 
          cv_idx = cv_idx)
      
      K_tr <- K_list$tr
      K_cv <- K_list$cv
    } else {
      n <- dim(X)[1]
      p <- dim(X)[2]
      y_tr <- y
      X_tr <- X
      K_tr <- 
        make_kernel_cv(
          Z_list, int_id,
          kernfunc = kernfunc, 
          effect_type = "full", 
          cv_idx = FALSE)$tr
      y_cv <- 0
      X_cv <- matrix(0, nrow = n, ncol = p)
      K_cv <- matrix(0, nrow = n, ncol = n)
    }
    
    #### 2. train & predict #### 
    # with automatic lambda selection
    yfit_cand <- 
      vector("list", length = length(lambda))
    yfit_score <- 
      vector("numeric", length = length(lambda))
    
    for (lmd_idx in 1:length(lambda)){
      yfit <- 
        tryCatch(
          lskm_ridge(y_tr, X_tr, K_tr, 
                     y_cv, X_cv, K_cv, 
                     lambda = lambda[lmd_idx]), 
          error = function(e) e
        )
      
      if ("error" %in% class(yfit)) 
        # if regression fail, then no recording
        yfit <- list(tr = NaN, cv = NaN)
      
      yfit_cand[[lmd_idx]] <- yfit
      
      yfit_score[lmd_idx] <- # difference in tr and cv error
        abs(var(y_tr - yfit$tr) - 
              var(y_cv - yfit$cv)
        )
    }
    
    #### 3. evaluate and record #### 
    yfit_final <- yfit_cand
    
    yfit_final
  }

lskm_ridge <- 
  function(y_tr, X_tr, K_tr, 
           y_cv, X_cv, K_cv, lambda = 1)
  {
    # # preprocessing
    n <- length(y_tr)
    V_inv <- solve(K_tr + lambda * diag(n))
    
    # 1. training
    beta <- solve(t(X_tr) %*% V_inv %*% X_tr, 
                  t(X_tr) %*% V_inv %*% y_tr)
    
    alpha <- V_inv %*% (y_tr - X_tr %*% beta)
    
    # 2. prediction
    yhat <- X_tr %*% beta + K_tr %*% alpha
    ypred <- X_cv %*% beta + K_cv %*% alpha
    
    # return 
    list(tr = yhat, cv = ypred, 
         alpha = alpha, beta = beta)
  }

lskm_ridge_archive <- 
  function(y_tr, K_tr, 
           y_cv, K_cv, lambda = 1)
  {
    # historical version that only allow kernel effects
    # # preprocessing
    n <- length(y_tr)
    
    # 1. training
    eig_fit <- eigen(K_tr)$value
    vec_fit <- eigen(K_tr)$vectors
    eig_P_fit <- (eig_fit)/(eig_fit + lambda)
    
    # projection matrix
    P_fit <- vec_fit %*% diag(eig_P_fit) %*% t(vec_fit)
    alpha <- solve(K_tr + lambda * diag(n), y_tr)
    yhat <- P_fit %*% y_tr
    
    # 2. prediction
    ypred <- K_cv %*% alpha
    
    # return 
    list(tr = yhat, cv = ypred)
  }