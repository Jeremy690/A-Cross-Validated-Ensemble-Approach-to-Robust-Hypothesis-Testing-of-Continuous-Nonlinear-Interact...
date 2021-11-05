#### Helper funcs ####

#### 1. Kernel Library ####

kernfuncLib <- 
  function(kern_name = NULL, kern_par = NULL)
  {
    # a list of kernel functions input Z output Kmat
    # kern_par is a vector for regular kernel, 
    #     and a matrix for "taylor" kernel
    
    warning_header <- "kernfuncLib:"
    
    # default value
    if (is.null(kern_name)){
      kern_name = c("taylor", "exp", "rbf")
      kern_par = list(taylor = diag(5)[-1, ],
                      exp = matrix(exp(-5:2), ncol = 1),
                      rbf = matrix(exp(-5:2), ncol = 1))
    }
    
    # validation: kern_par should contain names same as kern_name
    is_match <- 
      sapply(kern_name, 
             function(name) 
               name %in% names(kern_par)) %>% all
    
    if (!is_match){
      "names supplied in 'kern_par' don't match 'kern_name'" %>%
        paste(warning_header, .) %>% stop
    }
    
    
    # formal procedure: produce name for individual kernels
    kernel_name <- 
      sapply(kern_name, 
             function(name)
               apply(
                 as.matrix(kern_par[[name]]), 1, 
                 function(row) 
                   paste(c(name, 
                           format(round(row, 4))
                   ), 
                   collapse = "_"))     
      ) %>% unlist
    
    
    # build list of individual kernel functions
    kernel_func <- 
      sapply(kernel_name, 
             function(name){
               kname <- strsplit(name, "_")[[1]][1]
               kern_par <- strsplit(name, "_")[[1]][-1] %>% 
                 as.numeric()
               function(Z, Y = NULL){
                 paste0("kern_func_", kname, "(Z, Y, par = kern_par)") %>% 
                   parse(text = .) %>% eval
               }
             }
      ) %>% set_names(kernel_name)
    kernel_func
  }

cvIndex <- 
  function(n, fold = 10){
    # create index for cross-validation
    index_full <- sample(n)
    part_len <- round(n/fold)
    index_fold <- 
      lapply(1:fold, function(i) 
        na.omit(index_full[((i-1)*part_len + 1):(i*part_len)])
      )
    
    # add additional elements to last fold
    if (fold*part_len < length(index_full)){
      index_fold[[fold]] <- 
        c(index_fold[[fold]], 
          index_full[(fold*part_len):length(index_full)])
    }
    
    index_fold
  }