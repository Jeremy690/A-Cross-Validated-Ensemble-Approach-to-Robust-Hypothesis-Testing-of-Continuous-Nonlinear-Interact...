library(CompQuadForm)
###################### 
##### Testing using garrote kernel
#############################

kern_test <- 
  function(y, X, Z, 
           int_id_null = NULL, 
           int_id_intr = NULL,
           kern_name = "rbf", kern_par = 1,
           hypr_type = c("cv", "ml"), # hyper-parameter selection method
           intk_type = "default", # choice for interaction kernel
           verbose = FALSE, 
           null_method = c("Satterthwaite", "Davies"))
  {
    # test using kernel machine for the effect of the first row of zz
    # alt: alternative hypothesis alt = "All" means all interaction, 
    #                             alt = digit indicate order of max order of interaction
    
    # define global parameter 
    n <- length(y)
    null_method <- match.arg(null_method)
    hypr_type <- match.arg(hypr_type)
    intk_type <- 
      match.arg(
        intk_type, 
        choices = c("default", "michael", "adaptive", "identity"), 
        several.ok = TRUE)
    
    if (is.null(int_id_null)) int_id_null <- 1:length(Z)
    if (is.null(int_id_intr)) int_id_intr <- int_id_null
    
    #### 1. Estimation under H0 ####

    #### 1.1 Estimation ====
    est.h0 <- #additive model assuming K = K1 + K2 + ...
      kern_reg_null(
        y, X, Z, 
        int_id = int_id_null,
        kern_name = kern_name,
        kern_par = kern_par,
        hypr_type = hypr_type,
        verbose = verbose)
    
    # store result
    outList <- list(
      y.hat = est.h0$y.hat,
      h.hat = est.h0$h.hat,
      sig.hat = est.h0$sig,
      lam.hat = est.h0$lam,
      rho.hat = est.h0$rho,
      bet.hat = est.h0$bet,
      eps.hat = y - est.h0$y.hat,
      weight = est.h0$ensemble$weight, 
      lambda = est.h0$ensemble$lambda
    )
    
    #### 2 Estimation Result Extraction ====
    
    #### 3. Testing ####
    outList$m <- list()
    outList$d <- list()
    outList$test.stat <- list()
    outList$pvalue <- list()
    
    for (intk_type_name in intk_type){
      test_res <- 
        kern_test_internal(
          intk_type_name,
          y, X, Z, 
          int_id_intr,
          est.h0, 
          null_method = null_method    
        )
      
      outList$m[intk_type_name] <- test_res$m
      outList$d[intk_type_name] <- test_res$d
      outList$test.stat[intk_type_name] <- test_res$stat
      outList$pvalue[intk_type_name] <- test_res$pval
    }
    
    return(outList)
  }

kern_test_internal <- 
  function(intk_type,
           y, X, Z, int_id,
           est.h0, null_method = "Satterthwaite")
  {
    n <- length(y)
    bet.hat <- est.h0$bet
    sig.hat <- est.h0$sig
    lam.hat <- est.h0$lam
    Kmat <- est.h0$K_main
    
    #### 0. prepare interaction kernel ====
    if (intk_type == "identity"){
      Kmat_1 <- diag(n)
    } else if (intk_type == "default"){
      Kmat_1 <- # linear interaction kernel
        make_kernel(
          Z, int_id, 
          kern_type = "lnr", kern_par = 1, 
          effect_type = "intr")
    } else if (intk_type == "adaptive") {
      Kmat_1 <- est.h0$K_intr
    } else if (intk_type == "michael"){
      Kmat_1 <- # engineer interaction kernel
        kern_func_micheal_interact(
          K_full = est.h0$K_full, 
          K_main = est.h0$K_main)
    }
    
    #### 1. initialize & projection matrix ====
    # initialize 
    m.chi <- numeric(0)
    d.chi <- numeric(0)
    score.chi <- numeric(0)
    pval.chi <- numeric(0)
    
    # projection matrix 
    #P0 = invV - invV * X * inv(X' * invV * X) * X' * invV
    V_inv <- 
      (sig.hat * diag(n) + lam.hat * Kmat) %>% solve2
    P0.mat <- 
      V_inv - 
      V_inv %*% X %*% 
      solve(t(X) %*% V_inv %*% X) %*% 
      t(X) %*% V_inv
    
    #### 2 compute score statistic ====
    
    ## Score statistics
    score.chi <-
      lam.hat * t(y - X %*% bet.hat)  %*%  V_inv  %*%
      Kmat_1  %*%
      V_inv  %*%  (y - X %*% bet.hat)/2
    
    #### 3. compute null distribution ====
    if (null_method == "Davies") {
      acc <- 1e-6
      
      eig_val <- 
        eigen(P0.mat %*% Kmat_1, 
              only.values = TRUE)$values %>% Re
      eig_val <- eig_val[eig_val > 1e-6*eig_val[1]]
      
      pval.chi <- 
        davies(2 * score.chi/lam.hat, 
               eig_val, acc = acc)$Qq
      
    } else if (null_method == "Satterthwaite"){
      # compute information matrix 
      par_len <- 3
      ## derivatives of V = sig * I + lambda * (K + delta * K_1), eval at H0: delta = 0
      drV.lam <- Kmat                     #dV/d_lambda = K
      drV.sig <- diag(n)                  #dV/d_sigma2 = I
      drV.del <- lam.hat * Kmat_1         #dV/d_delta  = lambda * dK/d_delta 
      
      ##Info matrix
      #Info for theta = (delta, sigma2, lambda)
      I0 <- 
        info_mat(P0.mat,
                 mat.del = drV.del, mat.sig = drV.sig, 
                 mat.lam = drV.lam, mat.rho = NULL)
      
      #Effective Info for delta
      tot.dim <- ncol(I0)
      I.deldel <-  
        I0[1,1] - 
        I0[1,2:tot.dim] %*% ginv(I0[2:tot.dim,2:tot.dim]) %*% I0[2:tot.dim, 1] 
      
      #
      md <- lam.hat * Trace(Kmat_1 %*% P0.mat)/2
      
      m.chi <- I.deldel / (2*md)
      d.chi <- md / m.chi
      
      #### 4. compute p-value ====
      pval.chi <- 1 - pchisq(score.chi/m.chi, d.chi)
    }
    
    list(pvalue = pval.chi, 
         stat = score.chi,
         m = m.chi, d = d.chi)
  }
