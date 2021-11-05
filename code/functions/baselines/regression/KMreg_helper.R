#################
##### Helper functions for Kernel Machine regression with Gaussian Kernel
###################
library(magrittr)
library(dplyr)
library(MASS)
library(xtable)
##########
# 1. zz2Kmat edited
# 2. 
##########

#### 1. Kernel Calculation ####
kernel_raw <- function(zz, kern_type = "poly"){
  zz <- as.matrix(zz)
  if (kern_type == "rbf"){
    # every column is a sample
    Kmat_raw <- matrix(NA, nrow=ncol(zz), ncol=ncol(zz))
    for (ii in 1:ncol(zz)){
      Kmat_raw[ii,] <- diag(t(zz-zz[,ii]) %*% (zz-zz[,ii]))
    }
    return(Kmat_raw)
  } else if (kern_type == "poly"){
    Kmat_raw <- t(zz) %*% zz
  }
  return(Kmat_raw)
}

kernel.Gauss <- function(Kmat_raw, scl){
  return(exp(-Kmat_raw/scl))
}

kernel.Poly <- 
  function(zz, a = 1, d = 2, scale = FALSE){
    Kmat_raw <- abs((t(zz) %*% zz + a))^d
    if (scale){
      Kmat_scl <- diag(Kmat_raw) %>% 
        (function(x) x %o% x) %>% sqrt
    } else {
      Kmat_scl <- max(Kmat_raw)
    }
    Kmat <- Kmat_raw/Kmat_scl
    Kmat
  }

zz2Kmat <- function(
  zz, zz.set.size, kern_type = "rbf", 
  output = "All", normFactor = NULL,
  # the raw component to be assembled,
  Kmat_raw = NULL,
  # if kern_type == "rbf"
  rho.hat = 1, 
  # if kern_type == "poly"
  a = 1, d = 2){
  #zz: every col an observation
  if (sum(zz.set.size) != nrow(zz)) zz <- t(zz)
  if (is.null(normFactor)){
    normFactor <- length(zz.set.size)
  }
  
  zz.list <- #group zz into sets
    rbind(c(1, cumsum(zz.set.size)[-length(zz.set.size)] + 1), 
          cumsum(zz.set.size)) %>% 
    data.frame %>%
    lapply(function(i){
      out <- as.matrix((zz)[i[1]:i[2], ])
      if(ncol(out) != ncol(zz)) out <- t(out)
      out
    }
    )
  
  out_list <- list()
  
  if (kern_type == "rbf"){
    if (is.null(Kmat_raw)){
      Dmat <- 
        lapply(zz.list, kernel_raw, kern_type = "rbf") 
    } else {
      Dmat <- Kmat_raw
    }
    Kmat_sep <- get_Kmat_sep(Dmat, rho.hat)
    Kmat <- Reduce("+", Kmat_sep)
    Kmat <- Kmat/normFactor
    
    out_list[["Dmat"]] <- Dmat
    out_list[["Kmat"]] <- Kmat
    out_list[["Kmat_sep"]] <- Kmat_sep
  } else if (kern_type == "poly") {
    Kmat_sep <- 
      lapply(zz.list, kernel.Poly, 
             a=a, d=d) 
    Kmat <- Reduce("+", Kmat_sep)
    Kmat <- Kmat/normFactor
    
    out_list[["Kmat"]] <- Kmat
    out_list[["Kmat_sep"]] <- Kmat_sep    
  }
  if  (output != "All") return(out_list[[output]])
  else return(out_list)
}

get_Kmat_sep <- function(Dmat, rho.hat) {
  mapply(kernel.Gauss, Kmat = Dmat, scl = rho.hat, 
         SIMPLIFY = FALSE) 
}

get_Kmat_null <- function(Kmat_sep) Reduce("+", Kmat_sep)

get_dKmat_null <- function(Kmat_sep) Reduce("*", Kmat_sep)

#### 2. Information matrix ####
info_mat <- 
  function(P0.mat,
           mat.del = NULL, mat.sig = NULL, 
           mat.lam = NULL, mat.rho = NULL)
  {
    par_len <- ifelse(is.null(mat.rho), 3, 4)
    
    I0 <- matrix(NA, par_len, par_len)
    
    I0[1,1] <- Trace( P0.mat %*% mat.del %*% P0.mat %*% mat.del )/2  ##del.del
    #
    I0[1,2] <- Trace( P0.mat %*% mat.del %*% P0.mat %*% mat.sig )/2  ##del.sig
    I0[2,1] <- I0[1,2]
    #
    I0[1,3] <- Trace( P0.mat %*% mat.del %*% P0.mat %*% mat.lam )/2  ##del.lam
    I0[3,1] <- I0[1,3]
    #
    #
    I0[2,2] <- Trace( P0.mat %*% mat.sig %*% P0.mat %*% mat.sig )/2  ##sig.sig
    #
    I0[2,3] <-  Trace( P0.mat %*% mat.sig %*% P0.mat %*% mat.lam )/2  ##sig.lam
    I0[3,2] <- I0[2,3]
    #
    I0[3,3] <-  Trace( P0.mat %*% mat.lam %*% P0.mat %*% mat.lam )/2  ##lam.lam
    
    if (par_len == 4){
      # add entry for rho
      I0[1,4] <- Trace( P0.mat %*% mat.del %*% P0.mat %*% mat.rho )/2  ##del.rho
      I0[4,1] <- I0[1,4]
      I0[2,4] <-  Trace( P0.mat %*% mat.sig %*% P0.mat %*% mat.rho )/2  ##sig.rho
      I0[4,2] <- I0[2,4]
      #
      I0[3,4] <-  Trace( P0.mat %*% mat.lam %*% P0.mat %*% mat.rho )/2  ##lam.rho
      I0[4,3] <- I0[3,4]  
      #
      I0[4,4] <-  Trace( P0.mat %*% mat.rho %*% P0.mat %*% mat.rho )/2  ##lam.rho
    }
    return(I0)
  }


#### 3. REML update function ####
update_sig <- 
  function(resid, X, K, sig, tau){
    lambda <- sig/tau
    n <- length(resid)
    
    # calculate components
    V_inv <- solve(lambda * diag(n) + K)
    Px <- 
      X %*% solve(t(X) %*% V_inv %*% X, 
                  t(X) %*% V_inv)
    A <- Px + K %*% V_inv %*% (diag(n) - Px)
    
    # calculate sigma^2
    sum(resid^2)/(n - sum(diag(A)))
  }

update_tau <- 
  function(resid, X, K, sig, tau){
    obj <- optimize(logLik_tau, c(0, 1e4),
                    X = X, resid = resid, K = K, sig = sig)
    list(tau = obj$minimum, logLik = obj$objective[1, 1])
  }

update_rho <- 
  function(resid, X, Z, sig, tau, int_id){
    obj <- optimize(logLik_rho, c(0, 1e4),
                    X = X, resid = resid, Z = Z, 
                    sig = sig, tau = tau, int_id = int_id)
    list(rho = obj$minimum, logLik = obj$objective[1, 1])
  }

logLik_tau <- 
  function(tau, resid, X, K, sig){
    V <- sig * diag(length(resid)) + tau * K
    V_inv <- solve(V)
    
    determinant(V)$modulus + 
      determinant(t(X) %*% V_inv %*% X)$modulus + 
      t(resid) %*% V_inv %*% resid
  }


logLik_rho <- 
  function(rho, resid, X, Z, sig, tau, int_id){
    # produce K
    K <- 
      make_kernel(
        Z, int_id = int_id,
        kern_type = "rbf", kern_par = 1/rho,
        effect_type = "main"
      )
    
    # produce variance component
    V <- sig * diag(length(resid)) + tau * K
    V_inv <- solve(V)
    
    # produce likelihood
    determinant(V)$modulus + 
      determinant(t(X) %*% V_inv %*% X)$modulus + 
      t(resid) %*% V_inv %*% resid
}


#### 4. Matrix Operations ####
Trace <- function(M){sum(diag(M))}
TraceMult <- function(A, B) {sum(A * B)}
solve2 <- function(M) chol2inv(chol(M))