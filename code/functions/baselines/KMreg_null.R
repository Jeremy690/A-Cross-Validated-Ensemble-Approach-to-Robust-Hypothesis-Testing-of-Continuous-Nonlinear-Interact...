#################
##### Kernel machine testing using ensemble
###################

kern_reg_null <-
  function(y, X, Z, int_id = NULL,
           kern_name = NULL, kern_par = NULL,
           hypr_type = c("cv", "ml"),
           verbose = FALSE, standardize = TRUE)
  {
    ### This code does kernel machine regression for gaussian kernel
    ## y: response n x 1
    ## X: matrix of background covariates, n x p
    ## Z: list of matrix n x q
    ## int_id: id for Z sub-groups whose interaction is being tested for
    ##          by default, the highest order of interaction is being tested
    ##          i.e. if Z = [Z1, Z2, Z3], then the interaction Z1*Z2*Z3 is being tested
    ##
    ## Estimates: h(z), beta and rho (the kernel parameter)
    
    hypr_type <- match.arg(hypr_type)
    
    if (is.null(int_id)){
      int_id <- 1:length(Z)
    }
      
    if (is.null(kern_name)) {
      kern_name <- c("taylor", "rbf")
      kern_par <- list(taylor = diag(3),
                       rbf = matrix(exp(-5:2), ncol = 1))
    }
    
    #### 1: estimation ####
    if (standardize){
      X[, -1] <- apply(X[, -1], 2, function(x) x/sqrt(sum(x^2)))
      Z <-
        lapply(Z,
               function(z_list)
                 apply(z_list, 2, function(z) z/sqrt(sum(z^2))))
    }
    
    #### 1.1 estimate ensemble K ####
    is_single_kernel <- 
      (length(kern_name) == 1) & (nrow(kern_par[[1]]) == 1)
    ensemble_init <- NULL
    
    if (hypr_type == "cv"){
      if (is_single_kernel) {
        K <- make_kernel(Z, int_id, 
                         kern_type = kern_name, 
                         kern_par = kern_par[[1]][1, ], 
                         effect_type = "main")
      } else {
        ensemble_init <-
          kreg_ensemble(
            y, X, Z, int_id,
            lambda = NULL, type = "main", 
            kern_name = kern_name, kern_par = kern_par, 
            verbose = verbose
          ) %>% grader
        
        K <- maestr(ensemble_init)$K
      }
    } else {
      # use a single specified kernel
      K <- make_kernel(Z, int_id, 
                       kern_type = kern_name[1], 
                       kern_par = kern_par[[1]], 
                       effect_type = "main")
    }
    
    #### 1.2 fit null model using REML ####
    ensemble_main <- 
      kreg_reml(y, X, Z, K, int_id,
                hypr_type = hypr_type,
                verbose = verbose)
    
    #### 2: assemble output ####
    outlist <- list()
    
    # fill in output container, exit
    outlist$y.hat <- ensemble_main$yhat
    outlist$sig <- ensemble_main$sigma
    outlist$lam <- ensemble_main$tau
    outlist$bet <- ensemble_main$beta
    outlist$rho <- ensemble_main$rho
    
    if (hypr_type == "ml") {
      outlist$K_main <- 
        make_kernel(
          Z, int_id = int_id,
          kern_type = "rbf", 
          kern_par = 1/ensemble_main$rho,
          effect_type = "main")
      outlist$K_intr <- 
        make_kernel(
          Z, int_id = int_id,
          kern_type = "rbf", 
          kern_par = 1/ensemble_main$rho,
          effect_type = "intr")
      outlist$K_full <- 
        make_kernel(
          Z, int_id = int_id,
          kern_type = "rbf", 
          kern_par = 1/ensemble_main$rho,
          effect_type = "full")
      outlist$opt_method <- "rbf_mle"
    } else if (hypr_type == "cv"){
      if (is_single_kernel) {
        par_single_kern <- kern_par[[kern_name]][1, ]
        outlist$K_main <- 
          make_kernel(Z, int_id = int_id,
                      kern_type = kern_name, 
                      kern_par = par_single_kern,
                      effect_type = "main")
        outlist$K_intr <- 
          make_kernel(Z, int_id = int_id,
                      kern_type = kern_name, 
                      kern_par = par_single_kern,
                      effect_type = "intr")
        outlist$K_full <- 
          make_kernel(Z, int_id = int_id,
                      kern_type = kern_name, 
                      kern_par = par_single_kern,
                      effect_type = "full")
        outlist$opt_method <- paste0(kern_name, "_cv")
      } else {
        outlist$K_main <- K
        outlist$K_intr <- maestr(ensemble_init, type = "intr")$K
        outlist$K_full <- maestr(ensemble_init, type = "full")$K
        outlist$ensemble <- ensemble_init
        # for compatibility reasons
        outlist$opt_method <- "cvek"
      }
    } 
    
    return(outlist)
  }