options(warn = -1)
random_seed <- 42

# install missing packages.
list.of.packages <- c("mgcv", "psych", 
                      "nlme", "kernlab", "CompQuadForm", 
                      "iterators", "parallel", 
                      "grid", "Matrix", "survival", "survey", 
                      "dplyr", "MASS", 
                      "limSolve", "foreach", "doParallel")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  print("Installing required packages..")
  install.packages(new.packages)
  print("Done")
}

# Utility function to source all files in ./functions folder.
source_directory <- function(path, trace = TRUE, ...) {
  nm_lst <- 
    list.files(path, pattern = "[.][R]$", recursive = TRUE)
  for (nm in nm_lst) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

# Helper functions for nuisance variables.
nuisance_nutrition_variable <- function(
  nutr_names, 
  use_pc = TRUE, pc_thres = 0.9, 
  use_residual_projection = FALSE)
{
  # Return data matrix for nuisance nutrient variables.
  nuis_nutrition_mat <- as.matrix(Z[[2]][, setdiff(varnames$z2, nutr_names)])
  nuis_group_variable <- nuis_nutrition_mat
  
  if (use_pc) {
    nuis_group_pc <- prcomp(nuis_nutrition_mat)
    
    pc_idx <- 
      summary(nuis_group_pc)$importance %>% 
      extract("Cumulative Proportion", ) %>%
      (function(x) which(x < pc_thres))
    
    nuis_group_variable <- nuis_group_pc$x[, pc_idx]
  }
  
  if (use_residual_projection) {
    targ_nutrition_mat <- as.matrix(Z[[2]][, nutr_names])
    
    nuis_group_variable <- residual_projection(nuis_group_variable, 
                                               targ_nutrition_mat)
  }
  
  nuis_group_variable
}

# define factory function for running tests
run_kern_test <- function(
  y, X, Z_test, int_id_null = NULL,
  test_type = c("cvek-rbf", "cvek-nn",
                "iskat", "gkm", "ge-spline"),
  verbose = TRUE){
  if (verbose) {
    cat(gettextf("Performing Test '%s'\n", test_type))
  }
  
  # define kernel library
  if (test_type == "cvek-rbf"){
    kern_name <- "rbf"
    # kern_par <- list(rbf = matrix(exp(seq(-3, 2, 1)), ncol = 1))
    kern_pars <- data.frame(method = rep("rbf", 7), 
                            l = exp(seq(-3, 3, 1)/2),
                            Sigma = 0, p = 0)
    hypr_type <- "cv"
    intk_type = "default"
  } else if (test_type == "cvek-nn"){
    kern_name <- "nn"
    # kern_par <- list(nn = matrix(exp(seq(-3, 2, 1)), ncol = 1))
    kern_pars <- data.frame(method = rep("nn", 7), 
                            Sigma = exp(seq(-3, 3, 1)), 
                            l = 0, p = 0)
    hypr_type <- "cv"
    intk_type = "default"
  } else if (test_type == "gkm") {
    kern_name <- "rbf"
    kern_par <- 1
    hypr_type <- "ml"
    intk_type <- "default"
  } else if (test_type == "iskat") {
    kern_name <- "lnr"
    kern_par <- list(lnr = matrix(1, ncol = 1))
    hypr_type <- "cv"
    intk_type = "default"
  } else if (test_type == "ge-spline") {
    res_test <- run_spline_test(y, X, Z_test)
    return(res_test)
  } else {
    stop(paste0("No match for test_type=", test_type))
  }
  
  repeat {
    if (grepl("cvek", test_type)) {
      # Prepare the kernel function library for CVEK ensemble members.
      kern_list <- list()
      for (d in 1:nrow(kern_pars)) {
        method_name <- as.character(kern_pars[d, ]$method)
        kern_list[[d]] <- generate_kernel(method_name,
                                          kern_pars[d, ]$Sigma,
                                          kern_pars[d, ]$l,
                                          kern_pars[d, ]$p)
      }
      
      # Run CVEK kernel test.
      res_test <- testing(formula_int = NULL, 
                          label_names = NULL,
                          Y = y, 
                          fixed_X = X, 
                          X1 = Z_test[[1]], 
                          X2 = Z_test[[2]], 
                          kern_list = kern_list)
      pvalue <- res_test$pvalue
    } else {
      # Run baseline kernel tests.
      res_test <-
        tryCatch(
          kern_test(
            y, X, Z_test,
            int_id_null = int_id_null,
            kern_name = kern_name,
            kern_par = kern_par,
            hypr_type = hypr_type,
            intk_type = intk_type,
            verbose = verbose
          ), error = function(e) e)
      pvalue <- res_test$pvalue[[intk_type]]
    }
    
    if (!("error" %in% class(res_test))) {
      break
    } else {
      print("Error encountered. Re-run analysis...")
    }
  }
  
  list(pvalue = pvalue, res_test = res_test)
}

# Function to execute GE interaction test based on spline model.
run_spline_test <- function(
  y, X, Z_test, 
  num_comp_nuisance=5, num_comp_test=3, k=3){
  # Remove intercept from background covariate.
  X <- X[, -1]
  num_data = nrow(X)
  
  # Extract data groups.
  Z_group_1 <- Z_test[[1]]
  Z_group_2 <- Z_test[[2]]
  Z_nuisance <- Z_test[[3]]
  
  # Extract principle components
  Z_group_1_pc <- svd(Z_group_1)$u[, 1:num_comp_test, drop=FALSE]
  Z_group_2_pc <- svd(Z_group_2)$u[, 1:num_comp_test, drop=FALSE]
  Z_nuisance_pc <- svd(Z_nuisance)$u[, 1:num_comp_nuisance, drop=FALSE]
  
  colnames(Z_group_1_pc) <- paste0("Z1_pc_", 1:num_comp_test)
  colnames(Z_group_2_pc) <- paste0("Z2_pc_", 1:num_comp_test)
  colnames(Z_nuisance_pc) <- paste0("Z_nuisancw_pc_", 1:num_comp_nuisance)
  
  # Assmeble data frame.
  data_mat <- cbind(y = y, X, Z_nuisance, Z_nuisance_pc, Z_group_1_pc, Z_group_2_pc)
  data <- data.frame(data_mat)
  
  # Make prediction formula.
  formula_str <- paste0(
    "y ~ ", 
    paste(varnames$x, collapse = " + "), " + ",
    paste(varnames$x_toxin, collapse = " + "), " + s(",
    paste(colnames(Z_nuisance_pc), collapse = ") + s("), ") + s(", 
    paste(colnames(Z_group_1_pc), collapse = ", "), ") + s(", 
    paste(colnames(Z_group_2_pc), collapse = ", "), ") + ti(", 
    paste(colnames(Z_group_1_pc), collapse = "+"), ", ",      
    paste(colnames(Z_group_2_pc), collapse = "+"), ", k=", k, ")")
  
  print(paste0("Fitting spline model..."))
  # print(formula_str)
  
  # Fit spline interaction model.
  formula <- formula_str %>% parse(text = .) %>% eval
  gam_obj <- gam(formula, data = data)
  gam_summary <- gam_obj %>% summary
  
  # Return pvalue.
  list(pvalue = gam_summary$s.pv[length(gam_summary$s.pv)], 
       res_test = gam_obj)
}