args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication
estimator=args[3] # IPW or AIPW
dgp.f.name=args[4] # dgp function name
R=as.integer(args[5]) # number of bootstrap samples

# # one example input
# n=10000
# seed=1
# m=0
# estimator='AIPW'
# dgp.f.name='nonfd_admg2'
# R=5


# library(dplyr)
library(boot)
library(tidyverse)
# library(tibble)
# library(tidyr)

set.seed(seed)
A.level=1
#################################################
# Functions
#################################################
source("DGPs/DGPs.R")
source("methods.R")

check_complete <- function(dt){
  
  # Check for missing combinations
  combo_counts <- dt %>%
    group_by(M, A, Z, X) %>%
    summarize(count = n(), .groups = "drop")
  
  # Check if all combinations exist
  return(nrow(combo_counts) == 16)
  
}

generate_complete_data <- function(dgp.f.name, n) {
  
  # Define all possible combinations
  all_combinations <- expand.grid(
    M = c(0, 1),
    A = c(0, 1),
    Z = c(0, 1),
    X = c(0, 1)
  )
  
  complete_data <- FALSE
  attempts <- 0
  
  while (!complete_data) {
    attempts <- attempts + 1
    
    # Generate data
    dt <- get(dgp.f.name)(n)
    
    # If all 16 combinations exist, we're done
    if (check_complete(dt)) {
      complete_data <- TRUE
      cat("Complete data generated on attempt", attempts, "\n")
    } else {
      cat("Missing combinations, regenerating data (attempt", attempts, ")\n")
    }
  }
  
  return(dt)
}

# function that compute E[Y(m)|Z=1,X] - E[Y(m)|Z=0,X]
f.CATE <- function(dt, i , m.level=NULL,x.level=NULL){ # estimator: np, AIPW, IPW, Gcomp
  
  # resampling
  dt <- dt[i,]
  
  M <- dt$M
  Y <- dt$Y
  Z <- dt$Z
  A <- dt$A
  X <- dt$X
  
  ## p(A=1|Z,X=x)
  dtA_a1 <- dt %>%
    filter(X==x.level) %>%
    group_by(Z) %>%
    summarize(EA = mean(A), .groups = "drop") %>%
    mutate(name = paste0("p.a1.z", Z, "x")) %>%
    column_to_rownames("name")
  
  dtA_a0 <- dtA_a1 %>%
    rownames_to_column("old_name") %>%  # First convert existing rownames to a column
    mutate(
      EA = 1 - EA,
      name = paste0("p.a0.z", Z, "x")
    ) %>%
    select(-old_name) %>%  # Remove the old name column
    column_to_rownames("name")
  
  ## p(m|A,Z,x)
  dtM_m1 <- dt %>%
    filter(X==x.level) %>%
    group_by(A, Z) %>%
    summarize(EM = mean(M), .groups = "drop") %>%
    mutate(name = paste0("p.m1", ".a", A, "z", Z, "x")) %>%
    column_to_rownames("name")
  
  p.m1.Az1x <- (A==1)*dtM_m1['p.m1.a1z1x','EM'] + (A==0)*dtM_m1['p.m1.a0z1x','EM']
  p.m1.Az0x <- (A==1)*dtM_m1['p.m1.a1z0x','EM'] + (A==0)*dtM_m1['p.m1.a0z0x','EM']
  
  p.m.Az1x <- m.level*p.m1.Az1x + (1-m.level)*(1-p.m1.Az1x) 
  p.m.Az0x <- m.level*p.m1.Az0x + (1-m.level)*(1-p.m1.Az0x)
  
  # E[Y|m,A,Z,x]
  dtY_m <- dt %>%
    filter(M == m.level,X==x.level) %>%
    group_by(A, Z) %>%
    summarize(EY = mean(Y), .groups = "drop") %>%
    mutate(name = paste0("E.Y.ma", A, "z", Z,"x")) %>%
    column_to_rownames("name")
  
  E.Y.mAz1x <- (A==1)*dtY_m['E.Y.ma1z1x','EY'] + (A==0)*dtY_m['E.Y.ma0z1x','EY']
  E.Y.mAz0x <- (A==1)*dtY_m['E.Y.ma1z0x','EY'] + (A==0)*dtY_m['E.Y.ma0z0x','EY']
  
  
  # p(Z)
  dtZX <- dt %>%
    group_by(Z, X) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(prob = count / sum(count)) %>%
    mutate(name = paste0("p.z", Z, "x", X)) %>%
    column_to_rownames("name")
    
  ## the AIPW estimates
  # Z=1
  est.z1 <- dtY_m['E.Y.ma1z1x','EY']*dtA_a1['p.a1.z1x','EA'] + dtY_m['E.Y.ma0z1x','EY']*dtA_a0['p.a0.z1x','EA']
  eif.z1 <- (Z==1)*(M==m.level)*(X==x.level)/(p.m.Az1x*dtZX[paste0('p.z1x',x.level),'prob'])*(Y-E.Y.mAz1x) + (Z==1)*(X==x.level)/dtZX[paste0('p.z1x',x.level),'prob']*E.Y.mAz1x - est.z1
  std.z1 <- sqrt(mean(eif.z1^2)/nrow(dt))
  
  # Z=0
  est.z0 <- dtY_m['E.Y.ma1z0x','EY']*dtA_a1['p.a1.z0x','EA'] + dtY_m['E.Y.ma0z0x','EY']*dtA_a0['p.a0.z0x','EA']
  eif.z0 <- (Z==0)*(M==m.level)*(X==x.level)/(p.m.Az0x*dtZX[paste0('p.z0x',x.level),'prob'])*(Y-E.Y.mAz0x) + (Z==0)*(X==x.level)/dtZX[paste0('p.z0x',x.level),'prob']*E.Y.mAz0x - est.z0
  std.z0 <- sqrt(mean(eif.z0^2)/nrow(dt))
  
  # diff: E[Y(m)|Z=1,x] - E[Y(m)|Z=0,x]
  est.diff <- est.z1 - est.z0 # <- output of the function
  std.diff <- sqrt(std.z1^2 + std.z0^2)
  lower.ci <- est.diff - 1.96*std.diff
  upper.ci <- est.diff + 1.96*std.diff

  
  return(list(est.diff=est.diff,std.diff=std.diff))
  
}



parametric_dual_weights <- function(dt, a=1){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  X <- dt$X
  
  # p(M|A,Z,X)
  model.M <- glm(M ~ A * Z * X, family = binomial(link = "logit"), data = dt)
  p.m1.AZX <- predict(model.M, type = "response")
  p.M.AZX <- p.m1.AZX*(M==1) + (1-p.m1.AZX)*(M==0)
  
  #p(M|a,ZX)
  p.m1.aZX <- predict(model.M, type = "response", newdata = dt %>%  mutate(A=a))
  p.M.aZX <- p.m1.aZX*(M==1) + (1-p.m1.aZX)*(M==0)
  
  # return(p.M.aZX/p.M.AZX)
  return(list(w=1/p.M.AZX, model.M=model.M))
}



parametric_primal_weights <- function(dt,Y.family){
  
  # E[Y|M,A,Z,X]
  model.Y <- glm(Y ~ M * A * Z * X, family = Y.family, data = dt)
  
  EY.a1 <- predict(model.Y, newdata = dt %>% mutate(A=1), type = "response")
  EY.a0 <- predict(model.Y, newdata = dt %>% mutate(A=0), type = "response")
  EY <- predict(model.Y, type = "response")
  
  
  pY.a1 <- dnorm(dt$Y, mean = EY.a1, sd = sd(dt$Y-EY))
  pY.a0 <- dnorm(dt$Y, mean = EY.a0, sd = sd(dt$Y-EY))
  pY <- dnorm(dt$Y, mean = EY, sd = sd(dt$Y-EY))
  
  # p(A|Z,X)
  model.A <- glm(A ~ Z * X, family = binomial(link = "logit"), data = dt)
  
  p.a1.ZX <- predict(model.A, type = "response")
  p.a0.ZX <- 1 - p.a1.ZX
  p.A.ZX <- dt$A*p.a1.ZX + (1-dt$A)*p.a0.ZX
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a1.ZX*pY.a1 + p.a0.ZX*pY.a0
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A.ZX*pY
  # denominator <- p.a1.ZX*pY.a1
  
  return(list(w=numerator/denominator,model.Y=model.Y, model.A=model.A))
  # return(1/p.a1.ZX)
  
}

#################################################
# CATE-based TEST
#################################################

satisfies_fd <- ifelse(dgp.f.name %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE) # record whether the DGP randomly selected satisfies the FD assumption

# generate data
dt = generate_complete_data(dgp.f.name, n)
# 
# # custom bootstrap to remove any incomplete samples
# boot_custom <- function(dt, statistic, R,...) {
#   
#   results <- numeric(R)
#   n <- nrow(dt)
#   i <- 1
#   
#   while (i <= R) {
#     idx <- sample(1:n, n, replace = TRUE)
#     boot_sample <- dt[idx, ]
#     
#     if (check_complete(boot_sample)) {
#       results[i] <- statistic(boot_sample,...)
#       i <- i + 1
#     }
#   }
#   return(results)
# }

# # testing
# test.m1x1.orig <- f.CATE(dt, m.level=1, x.level=1)
# test.m0x0.orig <- f.CATE(dt, m.level=0, x.level=0)
# test.m1x0.orig <- f.CATE(dt, m.level=1, x.level=0)
# test.m0x1.orig <- f.CATE(dt, m.level=0, x.level=1)
# test.m1x1 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=1,x.level=1)
# test.m0x0 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=0, x.level=0)
# test.m1x0 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=1, x.level=0)
# test.m0x1 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=0, x.level=1)
# 
# # compute CI controling for FWER
# lower.ci.m1x1 <- quantile(test.m1x1, 0.0125)
# upper.ci.m1x1 <- quantile(test.m1x1, 0.9875)
# 
# lower.ci.m0x0 <- quantile(test.m0x0, 0.0125)
# upper.ci.m0x0 <- quantile(test.m0x0, 0.9875)
# 
# lower.ci.m1x0 <- quantile(test.m1x0, 0.0125)
# upper.ci.m1x0 <- quantile(test.m1x0, 0.9875)
# 
# lower.ci.m0x1 <- quantile(test.m0x1, 0.0125)
# upper.ci.m0x1 <- quantile(test.m0x1, 0.9875)
# 
# cate.value.matrix <- cbind(colMeans(cbind(test.m1x1, test.m0x0,test.m1x0,test.m0x1)), 
#                            c(lower.ci.m1x1, lower.ci.m0x0, lower.ci.m1x0, lower.ci.m0x1), 
#                            c(upper.ci.m1x1, upper.ci.m0x0, upper.ci.m1x0, upper.ci.m0x1))
# 
# 
# # Wald test
# #wald.test <- cbind(mean(test.m1x1),mean(test.m0x0), mean(test.m1x0),mean(test.m0x1))%*%solve(cov(cbind(test.m1x1, test.m0x0,test.m1x0,test.m0x1)))%*%rbind(mean(test.m1x1),mean(test.m0x0), mean(test.m1x0),mean(test.m0x1)) %>% as.numeric()
# 
# wald.test <- cbind(test.m1x1.orig, test.m0x0.orig, test.m1x0.orig, test.m0x1.orig) %*% solve(cov(cbind(test.m1x1, test.m0x0,test.m1x0,test.m0x1))) %*% rbind(test.m1x1.orig, test.m0x0.orig, test.m1x0.orig, test.m0x1.orig) %>% as.numeric()
# 
# (1-pchisq(wald.test, df=4))

# testing
test.m1x1 <- f.CATE(dt, m.level=1, x.level=1)
test.m0x0 <- f.CATE(dt, m.level=0, x.level=0)
test.m1x0 <- f.CATE(dt, m.level=1, x.level=0)
test.m0x1 <- f.CATE(dt, m.level=0, x.level=1)

wald.test <- any(
  c(
    (test.m1x1$est.diff - qnorm(1-0.05/8)*test.m1x1$std.diff > 0) | (test.m1x1$est.diff + qnorm(1-0.05/8)*test.m1x1$std.diff < 0),
    (test.m0x0$est.diff - qnorm(1-0.05/8)*test.m0x0$std.diff > 0) | (test.m0x0$est.diff + qnorm(1-0.05/8)*test.m0x0$std.diff < 0),
    (test.m1x0$est.diff - qnorm(1-0.05/8)*test.m1x0$std.diff > 0) | (test.m1x0$est.diff + qnorm(1-0.05/8)*test.m1x0$std.diff < 0),
    (test.m0x1$est.diff - qnorm(1-0.05/8)*test.m0x1$std.diff > 0) | (test.m0x1$est.diff + qnorm(1-0.05/8)*test.m0x1$std.diff < 0)
  )
)


#################################################
# weight-based TEST
#################################################

# fit weighted regression of Y~M+A+Z
Y.family <- gaussian(link = "identity")

# derive primal and dual weights
dual_weights <- parametric_dual_weights(dt, a=A.level)
primal_weights <- parametric_primal_weights(dt,Y.family)

# Function to perform permutation test for conditional independence
permutation_test_YZ <- function(dt, n_permutations = 1000, w, Y.family) {
  
  
  # Function to fit models and calculate MSE difference
  fit_and_compare <- function(dt, permuted = FALSE) {
    
    # If permutation is requested, permute Z values
    if (permuted) {
      dt$Z <- sample(dt$Z, replace = FALSE)
    }
    
    model_with_Z <- glm(Y~M*Z*X, data=dt, family=Y.family, weights = w)
    model_without_Z <- glm(Y~M*X, data=dt, family=Y.family, weights = w)
    
    # Calculate MSE for both models
    mse_with_Z <- mean(residuals(model_with_Z)^2)
    mse_without_Z <- mean(residuals(model_without_Z)^2)
    
    # Return difference in MSE (without Z minus with Z)
    # Positive values indicate model with Z performs better
    return(mse_without_Z - mse_with_Z)
  }
  
  # Calculate observed MSE difference
  observed_diff <- fit_and_compare(dt, permuted = FALSE)
  
  # Perform permutation test
  permutation_diffs <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    permutation_diffs[i] <- fit_and_compare(dt, permuted = TRUE)
  }
  
  # Calculate p-value
  # The p-value is the proportion of permutations with a difference as extreme or more
  # extreme than the observed difference
  p_value <- mean(permutation_diffs >= observed_diff)
  
  # Visualize results
  hist(permutation_diffs, main = "Permutation Distribution", 
       xlab = "MSE Difference (without Z - with Z)", 
       col = "lightblue")
  abline(v = observed_diff, col = "red", lwd = 2)
  
  return(list(
    observed_diff = observed_diff,
    permutation_diffs = permutation_diffs,
    p.value = p_value
  ))
  
}

# Run the permutation test
p.test <- permutation_test_YZ(dt, 
                              n_permutations = 1000, 
                              w = primal_weights$w, 
                              Y.family = Y.family )

cat('## primal test###\n')
cat("Observed MSE difference:", p.test$observed_diff, "\n")
cat("P-value:", p.test$p.value, "\n")
cat("Interpretation: Y", ifelse(p.test$p.value < 0.05, "is NOT", "may be"), 
    "independent of Z given X and M\n")

d.test <- permutation_test_YZ(dt, 
                              n_permutations = 1000, 
                              w = dual_weights$w, 
                              Y.family = Y.family )


cat('## dual test###\n')
cat("Observed MSE difference:", d.test$observed_diff, "\n")
cat("P-value:", d.test$p.value, "\n")
cat("Interpretation: Y", ifelse(d.test$p.value < 0.05, "is NOT", "may be"), 
    "independent of Z given X and M\n")


save(list = c("test.m1x1","test.m1x0","test.m0x1","test.m0x0","wald.test","p.test","d.test"),file = paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_seed",seed,".Rdata"))