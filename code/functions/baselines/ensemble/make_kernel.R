library(survey)
library(kernlab)
library(mvtnorm)

#### 1. make_kernel functions ####

make_kernel <- 
  function(Z_list, int_id, 
           kern_type = "rbf", kern_par = 1, 
           effect_type = c("main", "intr", "full"), 
           cv_idx = NULL)
  {
    # effect_type: 
    # "main": main effect only, (exlucde int_id effect)
    # "intr": interaction only, (only int_id effect)
    # "full": main + intr
    
    # 1. ascertain effect type and kernel function 
    effect_type <- match.arg(effect_type)
    
    kern_func <- 
      paste0("kern_func_", kern_type) %>% 
      parse(text = .) %>% eval()
    
    # 2. produce list of Z dataframes for each 
    #     main/intr terms to generate, depending on the effect_type
    termid_list <- 
      get_termid(n_terms = length(Z_list), int_id, effect_type)
    
    # 3. produce kernel matrix
    K_list <- 
      lapply(Z_list, function(X) kern_func(X, par = kern_par))
    K_term_list <- 
      lapply(termid_list, function(termid) Reduce("*", K_list[termid]))
    
    K <- Reduce("+", K_term_list)/length(termid_list)
    K
  }

make_kernel_cv <- 
  function(Z_list, int_id,
           # provide a list of kernel functions to train
           kernfunc,
           # if no kernel func, provide kernel type and par instead
           kern_type = "rbf", kern_par = 1, 
           effect_type = c("main", "intr", "full"), 
           cv_idx = FALSE)
  {
    # effect_type: use main effect kernel with no between-group interaction
    #              otherwise use full kernel that includes interaction
    # cv_idx: index of observations that will be used for validation purpose
    #         if user want to use all data for training, set cv_idx to FALSE
    
    # 1. ascertain cv_index and kernel function
    no_cv <- FALSE
    if (class(cv_idx) == "logical"){
      if (cv_idx == FALSE){
        cv_idx <- -(1:nrow(Z_list[[1]]))
        no_cv <- TRUE
      }
    }
    
    if (is.null(kernfunc)){
      kernfunc <- 
        paste0("kern_func_", kern_type) %>% 
        parse(text = .) %>% eval()
    }
    
    # 2. produce id for each kernel terms based on effect_type:
    #     "+" for main effect, "*" for interaction
    effect_type <- match.arg(effect_type)
    termid_list <- 
      get_termid(n_terms = length(Z_list), int_id, effect_type)
    
    # 3. produce training and validation kernels
    K_list_tr <-
      lapply(Z_list, 
             function(X) 
               kernfunc(X[-cv_idx, ], Y = NULL))
    K_term_list_tr <-
      lapply(termid_list, 
             function(termid) Reduce("*", K_list_tr[termid]))
    
    if (no_cv){
      K_term_list_cv <- 
        lapply(termid_list, function(X) NULL)
    } else {
      K_list_cv <-
        lapply(Z_list, 
               function(X) 
                 kernfunc(X[cv_idx, ], X[-cv_idx, ]))
      K_term_list_cv <-
        lapply(termid_list, 
               function(termid) Reduce("*", K_list_cv[termid]))
    }
    
    K_tr <- Reduce("+", K_term_list_tr)/length(termid_list)
    K_cv <- Reduce("+", K_term_list_cv)/length(termid_list)
    
    # 4. return
    K_list <- list(tr = K_tr, cv = K_cv)
    K_list
  }

make_kernel_pred <-
  function(Z_new, Z_fit, 
           # provide a list of kernel functions to train
           kernfunc,
           # if no kernel func, provide kernel type and par instead
           kern_type = "rbf", kern_par = 1, 
           effect_type = c("main", "full", "intr"))
  {
    # effect_type: use main effect kernel with no between-group interaction
    #              otherwise use full kernel that includes interaction
    # cv_idx: index of observations that will be used for validation purpose
    #         if user want to use all data for training, set cv_idx to FALSE
    
    # effect_type: "+" for main effect, "*" for interaction
    effect_type <- match.arg(effect_type)
    if (is.null(kernfunc)){
      kernfunc <- 
        paste0("kern_func_", kern_type) %>% 
        parse(text = .) %>% eval()
    }
    
    K_list <-
      lapply(1:length(Z_fit), 
             function(i) 
               kernfunc(Z_new[[i]], Z_fit[[i]]))
    
    if (effect_type == "main") {
      K <- Reduce("+", K_list)
    } else if (effect_type == "full") {
      # X <- do.call("cbind", Z_list)
      # K_tr <- kernfunc(X[-cv_idx, ], Y = NULL)
      # K_cv <- kernfunc(X[cv_idx, ], X[-cv_idx, ])
      K <- Reduce("+", K_list) + Reduce("*", K_list)
    } else if (effect_type == "intr") {
      K <- Reduce("*", K_list)
    }
    
    K
  }

get_termid <- 
  function(n_terms, int_id, effect_type){
    # if int_id is already a list then directory return
    if (is.list(int_id)) return(int_id)
    
    # else compute term id
    group_id <- 1:n_terms
    termlist_full <- 
      # a list of vector, each vector contain index for one interaction term
      lapply(1:n_terms, 
             function(i) as.list(as.data.frame(combn(group_id, i)))
      ) %>% do.call(c, .)
    
    if (effect_type == "main"){
      # if main-effect only, then remove interaction
      interm_id <- 
        sapply(termlist_full, 
               function(term_id) all(int_id %in% term_id)) %>% which
      termid_list <- termlist_full[-interm_id]
    } else if (effect_type == "intr"){
      # if main-effect only, then keep only interaction 
      # TODO (jereliu): remove below comments in published version
      # interm_id <- 
      #   sapply(termlist_full, 
      #          function(term_id) all(int_id %in% term_id)) %>% which
      # termid_list <- termlist_full[interm_id]
      termid_list <- list(int_id)
    } else if (effect_type == "full") {
      # do nothin
      termid_list <- termlist_full
    }
    
    # assemble Z_terms to produce Kernel matrices
    termid_list
  }

#### 2. kernel functions ####
kern_func_lnr <- 
  function(X, Y = NULL, par = 1){
    if (is.null(Y)){
      X %*% t(X)
    } else {
      X %*% t(Y)
    }
  }

kern_func_exp <- 
  function(X, Y = NULL, par = 1){
    K_lnr <- kern_func_lnr(X, Y, par)
    exp(par * K_lnr)
  }

kern_func_rbf <- 
  function(X, Y = NULL, par = 1){
    rbfkernel <- rbfdot(sigma = par)
    kernelMatrix(rbfkernel, X, Y)
  }

kern_func_spline <- 
  function(X, Y = NULL, par = 1){
    splinekernel <- splinedot()
    kernelMatrix(splinekernel, X, Y)
  }


kern_func_taylor <- 
  function(X, Y = NULL, par = c(1, 1, 1)){
    K_lnr <- kern_func_lnr(X, Y, par)
    p <- length(par)
    
    K_taylor <- 
      lapply(1:p, function(i) par[i]* K_lnr^(i-1)) %>%
      Reduce("+", .)
    
    # stablize matrix with diagonal ridge
    if (nrow(K_taylor) == ncol(K_taylor)){
      K_taylor <- 
        K_taylor + diag(nrow(K_taylor)) * 1e-5
    }
    K_taylor
  }

kern_func_matern1 <- 
  function(X, Y = NULL, par = 1){
    # Matern 3/2 kernel 
    K_r <- kern_func_rbf(X, Y, par = par)
    log_Kr <- log(K_r); log_Kr[abs(log_Kr) < 1e-10] <- 0
    K_r <- sqrt(-log_Kr)
    
    K_matern <- exp(-K_r)
    
    K_matern
  }

kern_func_matern3 <- 
  function(X, Y = NULL, par = 1){
    # Matern 3/2 kernel 
    K_r <- kern_func_rbf(X, Y, par = par)
    log_Kr <- log(K_r); log_Kr[abs(log_Kr) < 1e-10] <- 0
    K_r <- sqrt(-log_Kr)
    
    K_matern <- 
      (1 + sqrt(3) * K_r) * exp(-sqrt(3) * K_r)
    
    K_matern
  }

kern_func_matern5 <- 
  function(X, Y = NULL, par = 1){
    # Matern 5/2 kernel 
    K_r <- kern_func_rbf(X, Y, par = par)
    log_Kr <- log(K_r); log_Kr[abs(log_Kr) < 1e-10] <- 0
    K_r <- sqrt(-log_Kr)
    
    K_matern <- 
      (1 + sqrt(5) * K_r + (5/3) * K_r^2) *
      exp(-sqrt(5) * K_r)
    
    K_matern
  }


kern_func_nn <- 
  function(X, Y = NULL, par = 1){
    # kernel function for 1-layer neural network with probit link
    # see Chapter 4 of Rasmussen & Williams (2010)
    par <- as.numeric(par)
    
    # assemble components
    X <- cbind(1, X)
    K_r_denum_1 <- (1 + 2 * diag(kern_func_lnr(X)) * par)
    
    if (!is.null(Y)){
      # if prediction
      Y <- cbind(1, Y)
      K_r_denum_2 <- (1 + 2 * diag(kern_func_lnr(Y)) * par)
      K_r_denum <- K_r_denum_1 %*% t(K_r_denum_2)
    } else {
      K_r_denum <- K_r_denum_1 %*% t(K_r_denum_1)
    }
    
    #
    K_r_num <- 2 * kern_func_lnr(X, Y) * par
    K_r <- K_r_num/sqrt(K_r_denum)
    
    #
    K_nn <- (2/pi) * asin(K_r)
    K_nn
  }

# construct interaction functions
kern_func_micheal_interact <- 
  function(K_full, K_main, print_idx = FALSE){
    # TODO: Fix this
    # projection of additive kernel
    eigen_M <- eigen(K_main)
    v_main <- eigen_M$values
    U_main <- eigen_M$vectors
    
    val_idx <- which(v_main < 1e-15) # remove near 0 values
    if (length(val_idx) > 0){
      v_main <- v_main[-val_idx]
      U_main <- U_main[, -val_idx]
    }
    
    # pick only important features (crucial), otherwise K_inter is null
    eigen_idx <- 
      kmeans(log(v_main), 2)$cluster %>% 
      (function(x) which(x != x[length(x)]))
    if (print_idx) print(eigen_idx)
    Phi <- U_main[, eigen_idx] %*% 
      diag(sqrt(v_main[eigen_idx]))
    
    # build projection matrix
    P_add <- Phi %*% solve(t(Phi) %*% Phi, t(Phi))
    P_res <- diag(nrow(K_main)) - P_add
    
    # standardize
    K_int <- P_res %*% K_full %*% P_res
    
    # return
    K_int
  }

kern_func_full_v_add <- 
  function(K_full, K_main){
    U_ful <- eigen(K_full)$vector %>% apply(2, function(x) (x-mean(x))/sd(x))
    U_add <- eigen(K_main)$vector %>% apply(2, function(x) (x-mean(x))/sd(x))
    dist_vec <- diag(t(U_ful) %*% U_add)/nrow(K_full)
    plot(dist_vec, type = "l")
  }
