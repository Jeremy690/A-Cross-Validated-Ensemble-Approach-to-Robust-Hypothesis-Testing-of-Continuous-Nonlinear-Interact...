# Set working directory to `~/code/numeric_study` folder, e.g., 
# > setwd("~/code/numeric_study/")

library(dplyr)
library(MASS)

file_raw <- read.table("./power_res.txt")

file_raw$p <- file_raw$p + .5
file_raw$l <- 1 / (file_raw$l / file_raw$dim_in)

file_all <- filter(file_raw, disType == 0, dim_in == 3)

## Matern kernel
file0 <- filter(file_all, method == "matern")

for (pp in c(1.5, 2.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p3/p", pp, "_l", ll, "ma.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    
    if(pp == 1.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Linear", "Quadratic", "RBF_MLE", "RBF_Median"),
             col = c("skyblue", "skyblue", "black", "black"), 
             lty = c("solid", "dashed", "solid", "dashed"), cex = .7)
    }
    if(pp == 2.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Matern 1/2", "Matern 3/2", "Matern 5/2"),
             col = c("darkblue", "darkblue", "darkblue"), 
             lty = c("solid", "dashed", "dotted"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5, 2.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p3/p", pp, "_l", 1.5, "ma.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}


## RBF kernel
file0 <- filter(file_all, method == "rbf")


for (pp in c(1.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p3/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(ll == .5) {
      legend(-0.06, 1.05, legend = c("NN 0.1", "NN 1", "NN 10", "CVEK_RBF", "CVEK_NN"),
             col = c("purple", "purple", "purple", "red", "red"),
             lty = c("solid", "dashed", "dotted", "solid", "dotdash"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p3/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}



############### dim = 6
file_all <- filter(file_raw, dim_in == 6)

## Matern kernel
file0 <- filter(file_all, method == "matern")

for (pp in c(1.5, 2.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p6/p", pp, "_l", ll, "ma.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(pp == 1.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Linear", "Quadratic", "RBF_MLE", "RBF_Median"),
             col = c("skyblue", "skyblue", "black", "black"), 
             lty = c("solid", "dashed", "solid", "dashed"), cex = .7)
    }
    if(pp == 2.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Matern 1/2", "Matern 3/2", "Matern 5/2"),
             col = c("darkblue", "darkblue", "darkblue"), 
             lty = c("solid", "dashed", "dotted"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5, 2.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p6/p", pp, "_l", 1.5, "ma.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}



## RBF kernel
file0 <- filter(file_all, method == "rbf")


for (pp in c(1.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p6/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(ll == .5) {
      legend(-0.06, 1.05, legend = c("NN 0.1", "NN 1", "NN 10", "CVEK_RBF", "CVEK_NN"),
             col = c("purple", "purple", "purple", "red", "red"),
             lty = c("solid", "dashed", "dotted", "solid", "dotdash"), cex = .7)
    }
    dev.off()
  }
}




for (pp in c(1.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p6/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}


############### dim = 10
file_all <- filter(file_raw, dim_in == 10)

## Matern kernel
file0 <- filter(file_all, method == "matern")

for (pp in c(1.5, 2.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p10/p", pp, "_l", ll, "ma.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(pp == 1.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Linear", "Quadratic", "RBF_MLE", "RBF_Median"),
             col = c("skyblue", "skyblue", "black", "black"), 
             lty = c("solid", "dashed", "solid", "dashed"), cex = .7)
    }
    if(pp == 2.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Matern 1/2", "Matern 3/2", "Matern 5/2"),
             col = c("darkblue", "darkblue", "darkblue"), 
             lty = c("solid", "dashed", "dotted"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5, 2.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p10/p", pp, "_l", 1.5, "ma.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}



## RBF kernel
file0 <- filter(file_all, method == "rbf")


for (pp in c(1.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/p10/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(ll == .5) {
      legend(-0.06, 1.05, legend = c("NN 0.1", "NN 1", "NN 10", "CVEK_RBF", "CVEK_NN"),
             col = c("purple", "purple", "purple", "red", "red"),
             lty = c("solid", "dashed", "dotted", "solid", "dotdash"), cex = .7)
    }
    dev.off()
  }
}




for (pp in c(1.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/p10/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}




############### t dist 5
file_all <- filter(file_raw, disType == 1)

## Matern kernel
file0 <- filter(file_all, method == "matern")

for (pp in c(1.5, 2.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/t5/p", pp, "_l", ll, "ma.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(pp == 1.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Linear", "Quadratic", "RBF_MLE", "RBF_Median"),
             col = c("skyblue", "skyblue", "black", "black"), 
             lty = c("solid", "dashed", "solid", "dashed"), cex = .7)
    }
    if(pp == 2.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Matern 1/2", "Matern 3/2", "Matern 5/2"),
             col = c("darkblue", "darkblue", "darkblue"), 
             lty = c("solid", "dashed", "dotted"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5, 2.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/t5/p", pp, "_l", 1.5, "ma.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}



## RBF kernel
file0 <- filter(file_all, method == "rbf")


for (pp in c(1.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/t5/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(ll == .5) {
      legend(-0.06, 1.05, legend = c("NN 0.1", "NN 1", "NN 10", "CVEK_RBF", "CVEK_NN"),
             col = c("purple", "purple", "purple", "red", "red"),
             lty = c("solid", "dashed", "dotted", "solid", "dotdash"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/t5/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}





############### t dist 10
file_all <- filter(file_raw, disType == 2)

## Matern kernel
file0 <- filter(file_all, method == "matern")

for (pp in c(1.5, 2.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/t10/p", pp, "_l", ll, "ma.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(pp == 1.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Linear", "Quadratic", "RBF_MLE", "RBF_Median"),
             col = c("skyblue", "skyblue", "black", "black"), 
             lty = c("solid", "dashed", "solid", "dashed"), cex = .7)
    }
    if(pp == 2.5 & ll == .5) {
      legend(-0.06, 1.05, legend = c("Matern 1/2", "Matern 3/2", "Matern 5/2"),
             col = c("darkblue", "darkblue", "darkblue"), 
             lty = c("solid", "dashed", "dotted"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5, 2.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/t10/p", pp, "_l", 1.5, "ma.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}



## RBF kernel
file0 <- filter(file_all, method == "rbf")

for (pp in c(1.5)) {
  for (ll in c(.5, 1)) {
    file <- filter(file0, p == pp, l == ll)
    file1 <- arrange(file[which(file$d == 1), ], int_effect)
    file2 <- arrange(file[which(file$d == 2), ], int_effect)
    file3 <- arrange(file[which(file$d == 3), ], int_effect)
    file4 <- arrange(file[which(file$d == 4), ], int_effect)
    file5 <- arrange(file[which(file$d == 5), ], int_effect)
    file6 <- arrange(file[which(file$d == 6), ], int_effect)
    file7 <- arrange(file[which(file$d == 7), ], int_effect)
    file8 <- arrange(file[which(file$d == 8), ], int_effect)
    file9 <- arrange(file[which(file$d == 9), ], int_effect)
    file10 <- arrange(file[which(file$d == 10), ], int_effect)
    file11 <- arrange(file[which(file$d == 11), ], int_effect)
    file12 <- arrange(file[which(file$d == 12), ], int_effect)
    
    pdf(file = paste0("./plot/t10/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
    
    if(ll == .5) {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    } else {
      plot(file1$int_effect, file1$power, type = "n", 
           ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      lines(smooth.spline(file1$int_effect, file1$power), 
            type = "l", col = "skyblue", lty = "solid")
    }
    lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
    lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
    lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
    lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
    lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
    lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
    lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
    lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
    lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
    lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
    lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
    abline(h = .05, col = "black")
    if(ll == .5) {
      legend(-0.06, 1.05, legend = c("NN 0.1", "NN 1", "NN 10", "CVEK_RBF", "CVEK_NN"),
             col = c("purple", "purple", "purple", "red", "red"),
             lty = c("solid", "dashed", "dotted", "solid", "dotdash"), cex = .7)
    }
    dev.off()
  }
}


for (pp in c(1.5)) {
  ll <- 1.5
  file <- filter(file0, p == pp, !l %in% c(.5, 1))
  file1 <- arrange(file[which(file$d == 1), ], int_effect)
  file2 <- arrange(file[which(file$d == 2), ], int_effect)
  file3 <- arrange(file[which(file$d == 3), ], int_effect)
  file4 <- arrange(file[which(file$d == 4), ], int_effect)
  file5 <- arrange(file[which(file$d == 5), ], int_effect)
  file6 <- arrange(file[which(file$d == 6), ], int_effect)
  file7 <- arrange(file[which(file$d == 7), ], int_effect)
  file8 <- arrange(file[which(file$d == 8), ], int_effect)
  file9 <- arrange(file[which(file$d == 9), ], int_effect)
  file10 <- arrange(file[which(file$d == 10), ], int_effect)
  file11 <- arrange(file[which(file$d == 11), ], int_effect)
  file12 <- arrange(file[which(file$d == 12), ], int_effect)
  
  pdf(file = paste0("./plot/t10/p", pp, "_l", ll, "rbf.pdf"), width = 4, height = 4)
  
  if(ll == .5) {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  } else {
    plot(file1$int_effect, file1$power, type = "n", 
         ylim = c(0, 1), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    lines(smooth.spline(file1$int_effect, file1$power), 
          type = "l", col = "skyblue", lty = "solid")
  }
  lines(smooth.spline(file2$int_effect, file2$power), type = "l", col = "skyblue", lty = "dashed")
  lines(smooth.spline(file3$int_effect, file3$power), type = "l", col = "black", lty = "solid")
  lines(smooth.spline(file4$int_effect, file4$power), type = "l", col = "black", lty = "dashed")
  lines(smooth.spline(file5$int_effect, file5$power), type = "l", col = "darkblue", lty = "solid")
  lines(smooth.spline(file6$int_effect, file6$power), type = "l", col = "darkblue", lty = "dashed")
  lines(smooth.spline(file7$int_effect, file7$power), type = "l", col = "darkblue", lty = "dotted")
  lines(smooth.spline(file8$int_effect, file8$power), type = "l", col = "purple", lty = "solid")
  lines(smooth.spline(file9$int_effect, file9$power), type = "l", col = "purple", lty = "dashed")
  lines(smooth.spline(file10$int_effect, file10$power), type = "l", col = "purple", lty = "dotted")
  lines(smooth.spline(file11$int_effect, file11$power), type = "l", col = "red", lty = "solid")
  lines(smooth.spline(file12$int_effect, file12$power), type = "l", col = "red", lty = "dotdash")
  abline(h = .05, col = "black")
  dev.off()
  
}
