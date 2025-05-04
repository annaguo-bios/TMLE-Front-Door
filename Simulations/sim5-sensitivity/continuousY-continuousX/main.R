args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication
estimator=args[3] # IPW or AIPW
dgp.f.name=args[4] # dgp function name
R=as.integer(args[5]) # number of bootstrap samples

# # one example input
# n=1000
# seed=1
# m=0
# estimator='AIPW'
# dgp.f.name='nonfd_admg1'
# R=5


# library(dplyr)
library(boot)
library(tidyverse)
library(SuperLearner)
# library(tibble)
# library(tidyr)

set.seed(seed)

if(seed%%200==0){seed <- 200}else{seed <- seed%%200}

A.level=0
lib <- c('SL.glm','SL.ranger')
#################################################
# Functions
#################################################
source("DGPs/DGPs.R")
source("methods.R")

check_complete <- function(dt){
  
  # Check for missing combinations
  combo_counts <- dt %>%
    group_by(M, A, Z) %>%
    summarize(count = n(), .groups = "drop")
  
  # Check if all combinations exist
  return(nrow(combo_counts) == 8)
  
}

generate_complete_data <- function(dgp.f.name, n) {
  
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
f.CATE <- function(dt, i , m.level=NULL){ # estimator: np, AIPW, IPW, Gcomp
  
  if(dgp.f.name %in% c('fd_admg1','fd_admg2')){A.family <- binomial(link = "identity")}else{A.family <- binomial(link = "logit")}
  
  
  # resampling
  dt <- dt[i,]
  
  M <- dt$M
  Y <- dt$Y
  Z <- dt$Z
  A <- dt$A
  X <- dt$X
  
  ## p(Z|X)
  if(dgp.f.name %in% c('fd_admg2')){Z.family <- binomial(link = "identity")}else{Z.family <- binomial(link = "logit")}
  model_Z <- glm(Z ~ X, data = dt, family = Z.family)
  
  p.z1.X <- predict(model_Z, type = "response")
  p.z0.X <- 1- p.z1.X
  
  ## p(A|Z,X)
  model_A <- glm(A ~ Z*X, data = dt, family = A.family)
  
  p.a1.mz1X <- predict(model_A, type = "response", newdata = dt %>% mutate(Z=1,M=m.level))
  p.a1.mz0X <- predict(model_A, type = "response", newdata = dt %>% mutate(Z=0,M=m.level))
  p.a0.mz1X <- 1 - p.a1.mz1X
  p.a0.mz0X <- 1 - p.a1.mz0X
  
  ## p(M|A,Z,X)
  model_M <- glm(M ~ A*Z*X, data = dt, family = binomial(link = "logit"))
  
  p.m1.Az1X <- predict(model_M, type = "response", newdata = dt %>% mutate(Z=1))
  p.m1.Az0X <- predict(model_M, type = "response", newdata = dt %>% mutate(Z=0))
  p.m0.Az1X <- 1 - p.m1.Az1X
  p.m0.Az0X <- 1 - p.m1.Az0X
  
  p.m.Az1X <- m.level*p.m1.Az1X + (1-m.level)*p.m0.Az1X
  p.m.Az0X <- m.level*p.m1.Az0X + (1-m.level)*p.m0.Az0X
  
  ## E[Y|M,A,Z,X]
  model_Y <- glm(Y ~ M*A*Z*X, data = dt, family = gaussian(link = "identity"))
  
  EY.mAz1X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=1,M=m.level))
  EY.mAz0X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=0,M=m.level))
  EY.ma1z1X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=1,A=1,M=m.level))
  EY.ma0z1X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=1,A=0,M=m.level))
  EY.ma1z0X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=0,A=1,M=m.level))
  EY.ma0z0X <- predict(model_Y, type = "response", newdata = dt %>% mutate(Z=0,A=0,M=m.level))
  
    
  ## the AIPW estimates
  # Z=1
  est.z1 <- mean(EY.ma1z1X*p.a1.mz1X + EY.ma0z1X*p.a0.mz1X)
  eif.z1 <- (Z==1)*(M==m.level)/(p.m.Az1X*p.z1.X)*(Y-EY.mAz1X) + (Z==1)/p.z1.X*(EY.mAz1X - (EY.ma1z1X*p.a1.mz1X + EY.ma0z1X*p.a0.mz1X)) + EY.ma1z1X*p.a1.mz1X + EY.ma0z1X*p.a0.mz1X - est.z1
  std.z1 <- sqrt(mean(eif.z1^2)/nrow(dt))
  
  # Z=0
  est.z0 <- mean(EY.ma1z0X*p.a1.mz0X + EY.ma0z0X*p.a0.mz0X)
  eif.z0 <- (Z==0)*(M==m.level)/(p.m.Az0X*p.z0.X)*(Y-EY.mAz0X) + (Z==0)/p.z0.X*(EY.mAz0X - (EY.ma1z0X*p.a1.mz0X + EY.ma0z0X*p.a0.mz0X)) + EY.ma1z0X*p.a1.mz0X + EY.ma0z0X*p.a0.mz0X - est.z0
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
  model.M <- glm(M ~ A+Z+X, family = binomial(link = "logit"), data = dt)
  p.M.AZX <- M*predict(model.M, type = "response") + (1-M)*(1-predict(model.M, type = "response"))
  
  #p(M|a,ZX)
  # p.M.aZX <- predict(model.M, type = "response", newdata = dt %>%  mutate(A=a))
  
  # return(p.M.aZX/p.M.AZX)
  return(list(w=1/p.M.AZX,model.M=model.M))
}



parametric_primal_weights <- function(dt,Y.family){
  
  if(dgp.f.name %in% c('fd_admg1','fd_admg2')){A.family <- binomial(link = "identity")}else{A.family <- binomial(link = "logit")}
  
  # E[Y|M,A,Z,X]
  model.Y <- glm(Y ~ M * A * Z * X, family = Y.family, data = dt)
  
  EY.a1 <- predict(model.Y, newdata = dt %>% mutate(A=1), type = "response")
  EY.a0 <- predict(model.Y, newdata = dt %>% mutate(A=0), type = "response")
  EY <- predict(model.Y, type = "response")
  
  pY.a1 <- dnorm(dt$Y, mean = EY.a1, sd = sd(dt$Y-EY))
  pY.a0 <- dnorm(dt$Y, mean = EY.a0, sd = sd(dt$Y-EY))
  pY <- dnorm(dt$Y, mean = EY, sd = sd(dt$Y-EY))
  
  # p(A|Z,X)
  model.A <- glm(A ~ Z * X, family = A.family, data = dt)
  
  p.a1.ZX <- predict(model.A, type = "response")
  p.a0.ZX <- 1 - p.a1.ZX
  p.A.ZX <- dt$A*p.a1.ZX + (1-dt$A)*p.a0.ZX
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a1.ZX*pY.a1 + p.a0.ZX*pY.a0
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A.ZX*pY
  
  return(list(w=numerator/denominator, model.Y=model.Y, model.A=model.A))
  # return(1/p.a1.ZX)
  
}

#################################################
# CATE-based TEST
#################################################

satisfies_fd <- ifelse(dgp.f.name %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE) # record whether the DGP randomly selected satisfies the FD assumption

# generate data
dt = generate_complete_data(dgp.f.name, n)

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
# 
# # testing
# test.m1 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=1)
# test.m0 <- boot_custom(dt, statistic=f.CATE , R=R, m.level=0)
# 
# # compute CI controling for FWER
# lower.ci.m1 <- quantile(test.m1, 0.0125)
# upper.ci.m1 <- quantile(test.m1, 0.9875)
# 
# lower.ci.m0 <- quantile(test.m0, 0.0125)
# upper.ci.m0 <- quantile(test.m0, 0.9875)
# 
# 
# cate.value.matrix <- cbind(colMeans(cbind(test.m1, test.m0)), 
#                            c(lower.ci.m1, lower.ci.m0), 
#                            c(upper.ci.m1, upper.ci.m0))
# 
# 
# # Wald test
# wald.test <- cbind(mean(test.m1),mean(test.m0))%*%solve(cov(cbind(test.m1, test.m0)))%*%rbind(mean(test.m1),mean(test.m0)) %>% as.numeric()
# 
# p.value_wald.test <- (1-pchisq(wald.test, df=2));p.value_wald.test

# testing
test.m1 <- f.CATE(dt, m.level=1)
test.m0 <- f.CATE(dt, m.level=0)

wald.test <- any(
  c(
    (test.m1$est.diff - qnorm(1-0.05/4)*test.m1$std.diff > 0) | (test.m1$est.diff + qnorm(1-0.05/4)*test.m1$std.diff < 0),
    (test.m0$est.diff - qnorm(1-0.05/4)*test.m0$std.diff > 0) | (test.m0$est.diff + qnorm(1-0.05/4)*test.m0$std.diff < 0)
  )
)


cat('## CATE test###\n')
cat("Interpretation: Y", ifelse(wald.test, "is NOT", "may be"), 
    "independent of Z given X and M\n")


#################################################
# weight-based TEST
#################################################

# fit weighted regression of Y~M+A+Z
Y.family <- gaussian(link = "identity")

# derive primal and dual weights
dual_weights <- parametric_dual_weights(dt, a=A.level)
primal_weights <- parametric_primal_weights(dt,Y.family)

# p.m1 <- glm(Y~M*Z*X, data=dt, family=Y.family, weights = primal_weights)
# p.m0 <- glm(Y~M*X, data=dt, family=Y.family, weights = primal_weights)
# 
# d.m1 <- glm(Y~M*Z*X, data=dt, family=Y.family, weights = dual_weights)
# d.m0 <- glm(Y~M*X, data=dt, family=Y.family, weights = dual_weights)
# 
# p.pred1 <- predict(p.m1, newdata=dt)
# p.pred0 <- predict(p.m0, newdata=dt)
# d.pred1 <- predict(d.m1, newdata=dt)
# d.pred0 <- predict(d.m0, newdata=dt)

# p.m1 <- SuperLearner(Y=Y, X=dt[,c('M','Z','X')], family = Y.family,obsWeights =  primal_weights, SL.library = lib)
# p.m0 <- SuperLearner(Y=Y, X=dt[,c('M','X')], family = Y.family,obsWeights =  primal_weights, SL.library = lib)
# 
# d.m1 <- SuperLearner(Y=Y, X=dt[,c('M','Z','X')], family = Y.family,obsWeights = dual_weights, SL.library = lib)
# d.m0 <- SuperLearner(Y=Y, X=dt[,c('M','X')], family = Y.family,obsWeights = dual_weights, SL.library = lib)
# 
# p.pred1 <- predict(p.m1)[[1]] %>% as.vector()
# p.pred0 <- predict(p.m0)[[1]] %>% as.vector()
# 
# d.pred1 <- predict(d.m1)[[1]] %>% as.vector()
# d.pred0 <- predict(d.m0)[[1]] %>% as.vector()

# p.test <- t.test((p.pred1 - dt$Y), (p.pred0 - dt$Y), paired = TRUE);p.test
# d.test <- t.test((d.pred1-dt$Y), (d.pred0-dt$Y), paired = TRUE);d.test

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


save(list = c("wald.test","p.test","d.test"),file = paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_seed",seed,".Rdata"))