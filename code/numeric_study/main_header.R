
simWorker <- function(M = 200, n = 200, B = 200,
                      method = "rbf", int_effect = 0, 
                      Sigma = 0, l = 1, p = 2, eps = .01, 
                      mode = "loocv", strategy = "erm", 
                      beta = 1, test = "boot", d = 1, 
                      lambda = exp(seq(-10, 5, .5)), 
                      dis_type = 0, dim_in = 3) {
  
  K1 <- data.frame(method = "linear", Sigma = 0, l = 1, p = 1)
  K1$method <- as.character(K1$method)
  
  K2 <- data.frame(method = "polynomial", Sigma = 0, l = 1, p = 2)
  K2$method <- as.character(K2$method)
  
  K3 <- data.frame(method = "rbf", Sigma = 0, l = 1, p = 1)
  K3$method <- as.character(K3$method)
  
  K4 <- data.frame(method = "rbf", Sigma = 0, l = 1, p = 1)
  K4$method <- as.character(K4$method)
  
  K5 <- data.frame(method = "matern", Sigma = 0, l = 1, p = 0)
  K5$method <- as.character(K5$method)
  
  K6 <- data.frame(method = "matern", Sigma = 0, l = 1, p = 1)
  K6$method <- as.character(K6$method)
  
  K7 <- data.frame(method = "matern", Sigma = 0, l = 1, p = 2)
  K7$method <- as.character(K7$method)
  
  K8 <- data.frame(method = "nn", Sigma = 0.1, l = 1, p = 1)
  K8$method <- as.character(K8$method)
  
  K9 <- data.frame(method = "nn", Sigma = 1, l = 1, p = 1)
  K9$method <- as.character(K9$method)
  
  K10 <- data.frame(method = "nn", Sigma = 10, l = 1, p = 1)
  K10$method <- as.character(K10$method)
  
  K11 <- data.frame(method = rep("rbf", 5), 
                    Sigma = rep(0, 5), l = exp(seq(-2, 2, 1)), p = rep(1, 5))
  K11$method <- as.character(K11$method)
  
  K12 <- data.frame(method = rep("nn", 4), 
                    Sigma = c(0.1, 1, 10, 50), l = rep(1, 4),
                    p = rep(1, 4))
  K12$method <- as.character(K12$method)
  
  
  Kerns <- list()
  Kerns[[1]] <- K1
  Kerns[[2]] <- K2
  Kerns[[3]] <- K3
  Kerns[[4]] <- K4
  Kerns[[5]] <- K5
  Kerns[[6]] <- K6
  Kerns[[7]] <- K7
  Kerns[[8]] <- K8
  Kerns[[9]] <- K9
  Kerns[[10]] <- K10
  Kerns[[11]] <- K11
  Kerns[[12]] <- K12
  
  kern <- Kerns[[d]]
  
  res <- vector("list", length = M)
  filename <-
    paste0("method", method, 
           "_p", p,
           "_l", l,
           "_int", int_effect,
           "_disType", dis_type,
           "_dim", dim_in,
           "_K", d,
           ".RData")
  header <- 
    paste0("method", method, 
           "_p", p, 
           "_l", l,
           "_int", int_effect,
           "_disType", dis_type,
           "_dim", dim_in,
           ", K=", d, ":")
  
  pv_lst <- NULL
  for (i in 1:M){
    res[[i]] <- 
      tryCatch(
        data_all <-
          generate_data(n = n, label_names, method = as.character(method),
                        int_effect = int_effect, l = l, 
                        p = p, eps = eps, dis_type = dis_type),
        error = function(e) e
      )
    
    if ("error" %in% class(res[[i]])){
      print(paste0(header, "Iteration ", i, "/", M,
                   " | ", res[[i]]$message , " >=<"))
    } else {
      data.sim <- data_all
      if (d == 4) {
        mat_data <- as.matrix(data.sim[, -c(1:6)])
        l_hat <- median(dist(mat_data, method = "euclidean", diag = FALSE, upper = FALSE))
        kern <- data.frame(method = "rbf", Sigma = 0, l = l_hat, p = 1)
        kern$method <- as.character(kern$method)
      } 
      
      if (d == 3) {
        mat_data <- as.matrix(data.sim[, -c(1:6)])
        l_hat <- median(dist(mat_data, method = "euclidean", diag = FALSE, upper = FALSE))
        l_list <- exp(log(l_hat) + seq(-1, 4, 1))
        
        RMSE_l <- sapply(l_list, function(l_cand) {
          kern <- data.frame(method = "rbf", Sigma = 0, l = l_cand, p = 1)
          kern$method <- as.character(kern$method)
          fit_cand <- define_model(formula, label_names, data.sim, kern)
          res_tr <- testing(formula_int, label_names, fit_cand$Y, fit_cand$fixed_X,
                            fit_cand$X1, fit_cand$X2, 
                            fit_cand$kern_list, as.character(mode), as.character(strategy), 
                            beta, as.character(test), lambda, B)
          res_tr$train_RMSE
        })
        
        l_est <- l_list[which(RMSE_l == min(RMSE_l))]
        kern <- data.frame(method = "rbf", Sigma = 0, l = l_est, p = 1)
        kern$method <- as.character(kern$method)
      }
      
      if (d == 5) {
        mat_data <- as.matrix(data.sim[, -c(1:6)])
        l_hat <- median(dist(mat_data, method = "euclidean", diag = FALSE, upper = FALSE))
        kern <- data.frame(method = "matern", Sigma = 0, l = 1/l_hat, p = 0)
        kern$method <- as.character(kern$method)
      } 
      
      if (d == 6) {
        mat_data <- as.matrix(data.sim[, -c(1:6)])
        l_hat <- median(dist(mat_data, method = "euclidean", diag = FALSE, upper = FALSE))
        kern <- data.frame(method = "matern", Sigma = 0, l = 1/l_hat, p = 1)
        kern$method <- as.character(kern$method)
      }
      
      if (d == 7) {
        mat_data <- as.matrix(data.sim[, -c(1:6)])
        l_hat <- median(dist(mat_data, method = "euclidean", diag = FALSE, upper = FALSE))
        kern <- data.frame(method = "matern", Sigma = 0, l = 1/l_hat, p = 2)
        kern$method <- as.character(kern$method)
      }
      
      fit <- define_model(formula, label_names, data.sim, kern)
      # fit_test <- define_model(formula, label_names, data_test, kern)
      
      #### 2. Fitting ####
      # kern_true <- generate_kernel(method = as.character(method), 
      #                              Sigma = 0, l = l, p = p)
      res[[i]] <- tryCatch(
        res_tst <- 
          testing(formula_int, label_names, 
                  fit$Y, fit$fixed_X, fit$X1, fit$X2, 
                  fit$kern_list, as.character(mode), as.character(strategy), 
                  beta, as.character(test), lambda, B),
        error = function(e) e
      )
      
      if(all(class(res[[i]]) == "list")){
        print(paste0(header, "Iteration ", i, "/", M, 
                     ", pval = ", 
                     paste(sapply(res[[i]]$pvalue, 
                                  function(p) round(p, 3)), 
                           collapse = "/")
        )
        )
        pv_lst <- c(pv_lst, res[[i]]$pvalue)
        
      } else {
        print(paste0(header, "Iteration ", i, "/", M,
                     " | ", res[[i]]$message , " >_<"))
      }
    }
    
    flush.console()
  }
  
  power0 <- mean(pv_lst < .05, na.rm = TRUE)
  cat(c(as.character(method), p, l, 
        int_effect, as.character(mode), 
        as.character(strategy), dis_type, 
        dim_in, d, power0),
      file = "power_res.txt", append = T, "\n")
  
  return(filename)
}

# Utility function to source all .R file under path.
source_directory <- function(path, trace = TRUE, ...) {
  nm_lst <- 
    list.files(path, pattern = "[.][R]$", recursive = TRUE)
  for (nm in nm_lst) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
