# Set working directory to `numeric_study` folder, e.g., 
# > setwd("~/code/numeric_study/")

library(magrittr)
library(MASS)
library(survey)
library(CompQuadForm)
library(mvtnorm)
library(dplyr)
library(limSolve)
rm(list = ls())

source('main_header.R')
source_directory('../functions/cvek/')


# config read in 
config_all <- read.csv("settings.txt")

# read config
args <- commandArgs(trailingOnly = TRUE)
config_idx <- as.numeric(args)
#eval(parse(text = args))
print(sprintf("Processing config index '%d'.", config_idx))

# extract config and execute
setTable <- config_all[config_idx, ]

label_names <- list(X1 = c("x1", "x2", "x3"), 
                    X2 = c("w1", "w2", "w3"))
formula <- Y ~ X1 + X2
formula_int <- Y ~ X1 * X2
scale_l <- length(label_names[[1]])
M <- 200
n <- 200
mode <- "loocv"
strategy <- "erm"
beta <- "min"
set.seed(0218)

for (i in 1:nrow(setTable)){
  # extract command
  method <- setTable$method[i]
  p <- setTable$p[i]
  l <- scale_l * setTable$l[i]
  int_effect <- setTable$int_effect[i]
  d <- setTable$d[i]
  disType <- setTable$disType[i]
  dim_in <- setTable$dim_in[i]
  
  if(dim_in == 6) {
    label_names <- list(X1 = c("x1", "x2", "x3", "x4", "x5", "x6"), 
                        X2 = c("w1", "w2", "w3", "w4", "w5", "w6"))
    formula <- Y ~ X1 + X2
    formula_int <- Y ~ X1 * X2
    scale_l <- length(label_names[[1]])
    l <- scale_l * setTable$l[i]
  }
  
  if(dim_in == 10) {
    label_names <- list(X1 = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10"), 
                        X2 = c("w1", "w2", "w3", "w4", "w5", "w6", "w7", "w8", "w9", "w10"))
    formula <- Y ~ X1 + X2
    formula_int <- Y ~ X1 * X2
    scale_l <- length(label_names[[1]])
    l <- scale_l * setTable$l[i]
  }
  
  # execute command
  taskname <-
    simWorker(M = M, n = n, B = 200,
      method = method, int_effect = int_effect, 
      Sigma = 0, l = l, p = p, eps = .01, 
      mode = mode, strategy = strategy, 
      beta = beta, test = "asym", d = d,
      lambda = exp(seq(-10, 5, .5)), 
      dis_type = disType, dim_in = dim_in)
  
  # sign out sheet
  write(
    paste(config_idx, taskname, Sys.time(), 
          collapse = "\t\t"), 
    file="sign_out.txt", append=TRUE)
  print("'sign_out.txt' recorded.")
}