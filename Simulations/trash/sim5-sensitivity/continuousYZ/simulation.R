setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity/continuousYZ")

library(dplyr)

source("./DGPs/DGPs.R")
source("methods.R")

# The sample sizes tried
sample_sizes <- c(200, 500, 1000, 1500, 2000, 5000, 20000)
num_trials <- 200 # Number of datasets to generate for each sample size

# The models, two of which satisfy front-door and two that don't
dgps <- c("fd_admg1", "fd_admg2", "nonfd_admg1", "nonfd_admg2")

alpha <- 0.05 # Significance level for hypothesis tests

# Set seeds
set.seed(7)

# level of A for dual weights and pseudo outcome approach
A.level <- 1

# Initialize lists to store results
dual_tprs.orig <- c()
dual_fprs.orig <- c()
dual_tprs.pseudo <- c()
dual_fprs.pseudo <- c()

primal_tprs.orig <- c()
primal_fprs.orig <- c()
primal_tprs.pseudo <- c()
primal_fprs.pseudo <- c()


# Go over each sample size
for (num_samples in sample_sizes) {
  
  print(paste0("Sample size:", num_samples))
  
  # Keep track of true positives, false positives, etc.
  
  # the original method in UAI paper
  dual_tps.orig <- 0
  dual_fps.orig <- 0
  dual_tns.orig <- 0
  dual_fns.orig <- 0
  primal_tps.orig <- 0
  primal_fps.orig <- 0
  primal_tns.orig <- 0
  primal_fns.orig <- 0
  
  # the proposed method using EIF based pseudo-outcome
  dual_tps.pseudo <- 0
  dual_fps.pseudo <- 0
  dual_tns.pseudo <- 0
  dual_fns.pseudo <- 0
  primal_tps.pseudo <- 0
  primal_fps.pseudo <- 0
  primal_tns.pseudo <- 0
  primal_fns.pseudo <- 0
  
  
  
  # Try num_trials number of models
  for (trial in 1:num_trials) {
    
    # Generate random data
    dgp <- sample(dgps, 1)
    dt <- get(dgp)(num_samples)
    satisfies_fd <- ifelse(dgp %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE)
    
    dual_weights <- parametric_dual_weights(dt, "M", c("Z", "A"), "A", a=A.level)
    dual_pval.orig <- weighted_lr_test(dt, Y="Y", Z="Z", cond_set=c("M"), weights=dual_weights, a=A.level, pseudo.outcome=F)
    dual_pval.pseudo <- weighted_lr_test(dt, Y="Y", Z="Z", cond_set=c("M"), weights=dual_weights, a=A.level, pseudo.outcome=T)
    
    primal_weights <- parametric_primal_weights(dt, "A", "Y", c("Z"), c("Z", "A", "M"))
    primal_pval.orig <- weighted_lr_test(dt, Y="Y", Z="Z", cond_set=c("M"), weights=primal_weights, a=A.level, pseudo.outcome=F)
    primal_pval.pseudo <- weighted_lr_test(dt, Y="Y", Z="Z", cond_set=c("M"), weights=primal_weights, a=A.level, pseudo.outcome=T)
    
    dual_fd.orig <- (dual_pval.orig > alpha)
    dual_fd.pseudo <- (dual_pval.pseudo > alpha)
    
    primal_fd.orig <- (primal_pval.orig > alpha)
    primal_fd.pseudo <- (primal_pval.pseudo > alpha)

    
    # the dual approach
    for (method in c("orig","pseudo")){
      
      if (get(paste0("dual_fd.",method)) && satisfies_fd) {
        
        assign(paste0("dual_tps.", method), get(paste0("dual_tps.", method)) + 1)
        
      } else if (get(paste0("dual_fd.",method)) && !satisfies_fd) {
        
        assign(paste0("dual_fps.",method), get(paste0("dual_fps.",method)) + 1)
        
      } else if (!get(paste0("dual_fd.",method)) && satisfies_fd) {
        
        assign(paste0("dual_fns.",method), get(paste0("dual_fns.",method)) + 1)
        
      } else {
        
        assign(paste0("dual_tns.",method), get(paste0("dual_tns.",method)) + 1)
        
      }
      
    }
    
    # the primal approach
    for (method in c("orig","pseudo")){
      
      if (get(paste0("primal_fd.",method)) && satisfies_fd) {
        
        assign(paste0("primal_tps.",method), get(paste0("primal_tps.",method)) + 1)
        
      } else if (get(paste0("primal_fd.",method)) && !satisfies_fd) {
        
        assign(paste0("primal_fps.",method), get(paste0("primal_fps.",method)) + 1)
        
      } else if (!get(paste0("primal_fd.",method)) && satisfies_fd) {
        
        assign(paste0("primal_fns.",method), get(paste0("primal_fns.",method)) + 1)
        
      } else {
        
        assign(paste0("primal_tns.",method), get(paste0("primal_tns.",method)) + 1)
        
      }
      
    }
    
    
  } # end of multiple simulation loop
  
  
  
  # Store and print outputs
  dual_tprs.orig <- c(dual_tprs.orig, round(dual_tps.orig / (dual_tps.orig + dual_fns.orig), 3))
  dual_fprs.orig <- c(dual_fprs.orig, round(dual_fps.orig / (dual_fps.orig + dual_tns.orig), 3))
  dual_tprs.pseudo <- c(dual_tprs.pseudo, round(dual_tps.pseudo / (dual_tps.pseudo + dual_fns.pseudo), 3))
  dual_fprs.pseudo <- c(dual_fprs.pseudo, round(dual_fps.pseudo / (dual_fps.pseudo + dual_tns.pseudo), 3))
  
  primal_tprs.orig <- c(primal_tprs.orig, round(primal_tps.orig / (primal_tps.orig + primal_fns.orig), 3))
  primal_fprs.orig <- c(primal_fprs.orig, round(primal_fps.orig / (primal_fps.orig + primal_tns.orig), 3))
  primal_tprs.pseudo <- c(primal_tprs.pseudo, round(primal_tps.pseudo / (primal_tps.pseudo + primal_fns.pseudo), 3))
  primal_fprs.pseudo <- c(primal_fprs.pseudo, round(primal_fps.pseudo / (primal_fps.pseudo + primal_tns.pseudo), 3))
  



} # end of sample size loop

orig.result <- data.frame(n=sample_sizes, dual_tpr=dual_tprs.orig, dual_fpr=dual_fprs.orig, primal_tpr=primal_tprs.orig, primal_fpr=primal_fprs.orig)
pseudo.result <- data.frame(n=sample_sizes, dual_tpr=dual_tprs.pseudo, dual_fpr=dual_fprs.pseudo, primal_tpr=primal_tprs.pseudo, primal_fpr=primal_fprs.pseudo)

save(list = c("orig.result","pseudo.result"),file = "results.RData")
