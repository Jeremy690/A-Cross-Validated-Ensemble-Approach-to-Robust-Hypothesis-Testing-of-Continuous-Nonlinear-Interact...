library(kernlab)
library(limSolve)


grader <- function(ensemble_fit){
  # Take in prediction from ensemble and 
  # "grade" them using cross-validation criteria
  fit_container <- ensemble_fit$tr
  cv_container <- ensemble_fit$cv
  
  n <- nrow(cv_container)
  n_k <- ncol(cv_container) - 1
  
  w <- # weight for each model in ensemble
    lsei(
      # least square target
      A = cv_container[, -1], B = cv_container[, "y"],
      # simplex constraint
      E = matrix(1, ncol = n_k), F = 1,
      # nonnegative constraint
      G = diag(n_k), H = matrix(0, nrow = n_k)
    )$X %>% matrix(ncol = 1) %>%
    set_rownames(names(ensemble_fit$kernfunc))
  
  ensemble_pred <- 
    fit_container[, -1] %*% w
  
  ensemble_fit$pred$tr <- 
    fit_container[, -1] %*% w
  ensemble_fit$pred$cv <- 
    cv_container[, -1] %*% w
  
  ensemble_fit$weight <- w
  
  ensemble_fit
}

model_rmse_plot <- 
  function(ensemble_fit) {
    cv_container <- ensemble_fit$cv
    
    resid_container <- 
      cv_container[, -1] - cv_container[, 1]
    resid_list <- 
      apply(resid_container, 2, 
            function(x) log(sqrt(mean(x^2))))
    resid_list <- resid_list[resid_list < 10]
    par(mai = c(0.5, 1.2, 0.2, 0.5))
    barplot(resid_list, 
            beside=TRUE, horiz=TRUE, 
            las = 1)
    par(mai = rep(0.75, 4))
  }
