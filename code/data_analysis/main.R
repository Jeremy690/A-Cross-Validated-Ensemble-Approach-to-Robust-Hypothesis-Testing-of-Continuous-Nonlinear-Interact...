# Set working directory to `CVEK-data-analysis` folder, e.g., 
# > setwd("~/code/data_analysis/")
# Afterwards, can execute the content of this script by running:
# > source("./main.R")

source("./main_header.R")
source_directory("../functions/baselines/") 
source_directory("../functions/cvek/")
library(magrittr)
library(dplyr)
library(mgcv)
library(psych)

# configure file directory
data_name <-  "./data/data_standardized.csv"
tabl_name <- "./result/table.txt"

if (!file.exists(data_name)){
  stop(
    paste0(
      "Data file '", 
      data_name, 
      "' doesn't exist. No analysis is performed"))
}
dir.create(dirname(tabl_name), showWarnings = FALSE)

#### 1. prepare data ####
# read data
data_stand <- read.csv(data_name)

# define variable names
varnames <- 
  list(y = c("y"),
       # background covariates
       x = paste0("X", seq(1, 21)),
       # pollutants
       z1 = c("pb_ln", "mn_ln", "as_ln"),
       # nutrients
       z2 = c(
         # macro
         "water", "prot", "fat", "carb", "fib", "ash", "calc", 
         # mineral
         "fe", "mg", "phos", "k", "na", "zn", "cu", 
         # active vit A
         "vita", "ret", 
         # pre-vita A  (i.e., carotene)
         "carbeta", "caralpha", "betacaro", "crypto", 
         # vit B-1,2,3 (niacin) 
         "thiamin", "ribo", "niap", 
         # vit B-6,9
         "vitb", "folate", 
         # vit C
         "ascl",
         # vit D & E
         "vitd", "vite"
       )
  )

nutr_group <- 
  list(macro = c("prot", "fat", "carb", "fib", "ash"), 
       metal = c("mg", "phos", "k", "zn", "cu", "calc", "fe"),
       vit_a = c("vita", "ret", "carbeta", "caralpha", "betacaro", "crypto"),
       vit_b = c("ribo", "niap", "vitb", "folate", "thiamin"),
       vit_o = c("ascl", "vitd", "vite"))
pol_mixture <- 
  list(all = c("pb_ln", "mn_ln", "as_ln"))

# make data matrix: background covariates with intercept
X_main <- data_stand[, varnames$x] %>% as.matrix
X <- cbind(1, X_main)

# make data matrix: nutrition and environment variables
Z <- list(data_stand[, varnames$z1], 
          data_stand[, varnames$z2]) %>% lapply(as.matrix)

# make data matrix: outcome
y <- data_stand[, varnames$y]

#### 2. perform kernel test for each nutrition group ####
# define test groups
test_names <- c(
  "cvek-nn",
  "cvek-rbf",
  "iskat",
  "gkm",
  "ge-spline"
)

res_list <- list()
start.time <- Sys.time()
for (nutr_group_name in names(nutr_group)) {
  print(gettextf("Testing for Nutrition Group '%s'", nutr_group_name))
  nutr_names <- nutr_group[[nutr_group_name]]
  
  # handle nuisance nutrition groups
  nuisance_group <- nuisance_nutrition_variable(
    nutr_names, use_pc = FALSE)
  
  Z_test <- 
    list(
      as.matrix(Z[[1]][, pol_mixture$all]), 
      as.matrix(Z[[2]][, nutr_names]),
      nuisance_group
    ) %>% lapply(as.matrix)
  
  # Fit model  
  test_res <- sapply(
    test_names, 
    function(test_name){
      set.seed(random_seed)
      run_kern_test(y, X, Z_test, 
                    int_id_null = c(1, 2),
                    test_type = test_name,
                    verbose = TRUE)$pvalue
    })
  
  res_list[[nutr_group_name]] <- test_res
}
time.taken <- Sys.time() - start.time

#### 3. summarize the result. ####
table_outcome <- t(do.call(rbind, res_list))
table_outcome_latex <- xtable(table_outcome, na.print = "", digits = 4)

# Save to file.
print(table_outcome_latex, type = "latex", file = tabl_name)

# Print results and total time taken.
table_outcome
time.taken
