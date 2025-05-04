args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication
estimator=args[3] # IPW or AIPW
dgp.f.name=args[4] # dgp function name
R=as.integer(args[5]) # number of bootstrap samples
superlearner=as.logical(args[6]) # whether to use superlearner or not

# # one example input
# n=1000
# seed=1
# m=0
# estimator='AIPW'
# dgp.f.name='fd_admg1'
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

parametric_dual_weights <- function(dt, a=1){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  X <- dt$X
  
  # p(M|A,Z,X)
  model.M <- glm(M ~ A + Z + X, family = gaussian(link = "identity"), data = dt)
  E.M.AZX <- predict(model.M, type = "response")
  E.MaZX <- predict(model.M, type = "response", newdata = dt %>% mutate(A=a))
  
  p.M.AZX <- dnorm(M, mean = E.M.AZX, sd = sd(M - E.M.AZX))
  p.M.aZX <- dnorm(M, mean = E.MaZX, sd = sd(M - E.M.AZX))
  
  # den.M <- density(M)
  # 
  # p.M <- approx(den.M$x, den.M$y, xout = M)$y
  
  return(list(w=p.M.aZX/p.M.AZX,model.M=model.M))
  # return(1/p.M.AZX)
}



parametric_primal_weights <- function(dt,Y.family,SL=F){
  
  A.family <- binomial(link = "logit")
  
  # E[Y|M,A,Z,X]
  if(SL){
    or_fit <- SuperLearner(Y=dt$Y, X=dt[,c('M','A','Z','X')], family = Y.family,SL.library = lib)
    EY <- predict(or_fit)[[1]] %>% as.vector()
    EY.a1 <- predict(or_fit, newdata=dt %>% mutate(A=1))[[1]] %>% as.vector()
    EY.a0 <- predict(or_fit, newdata=dt %>% mutate(A=0))[[1]] %>% as.vector()
    model.Y <- or_fit
  }else{
    
    model.Y <- glm(Y ~ M+A+Z+X, family = Y.family, data = dt)
    
    EY.a1 <- predict(model.Y, newdata = dt %>% mutate(A=1), type = "response")
    EY.a0 <- predict(model.Y, newdata = dt %>% mutate(A=0), type = "response")
    EY <- predict(model.Y, type = "response")
    
  }

  pY.a1 <- dnorm(dt$Y, mean = EY.a1, sd = sd(dt$Y-EY))
  pY.a0 <- dnorm(dt$Y, mean = EY.a0, sd = sd(dt$Y-EY))
  pY <- dnorm(dt$Y, mean = EY, sd = sd(dt$Y-EY))
  
  # p(A|Z,X)
  model.A <- glm(A ~ Z + X, family = A.family, data = dt)
  
  p.a1.ZX <- predict(model.A, type = "response")
  p.a0.ZX <- 1 - p.a1.ZX
  p.A.ZX <- dt$A*p.a1.ZX + (1-dt$A)*p.a0.ZX

  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a1.ZX*pY.a1 + p.a0.ZX*pY.a0

  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A.ZX*pY
  
  return(list(w=numerator/denominator,model.Y=model.Y,model.A=model.A))
  # return(1/p.a1.ZX)
  
}

#################################################
# CATE-based TEST
#################################################

satisfies_fd <- ifelse(dgp.f.name %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE) # record whether the DGP randomly selected satisfies the FD assumption

# generate data
dt = get(dgp.f.name)(n)

#################################################
# weight-based TEST
#################################################

# fit weighted regression of Y~M+A+Z
Y.family <- gaussian(link = "identity")

# derive primal and dual weights
dual_weights <- parametric_dual_weights(dt, a=A.level)
primal_weights <- parametric_primal_weights(dt,Y.family,superlearner)

# Function to perform permutation test for conditional independence
permutation_test_YZ <- function(dt, n_permutations = 1000, w, Y.family,SL) {
  
  
  # Function to fit models and calculate MSE difference
  fit_and_compare <- function(dt, permuted = FALSE,SL) {
    
    # If permutation is requested, permute Z values
    if (permuted) {
      dt$Z <- sample(dt$Z, replace = FALSE)
    }
    
    if(SL){
      
      model_with_Z <- SuperLearner(Y=dt$Y, X=dt[,c('M','Z','X')], family = Y.family,SL.library = lib, obsWeights = w)
      model_without_Z <- SuperLearner(Y=dt$Y, X=dt[,c('M','X')], family = Y.family,SL.library = lib, obsWeights = w)
      
      EY_with_Z <- predict(model_with_Z)[[1]] %>% as.vector()
      EY_without_Z <- predict(model_without_Z)[[1]] %>% as.vector()
      
      # MSE
      mse_with_Z <- sum(w * (dt$Y - EY_with_Z)^2) / sum(w)
      mse_without_Z <- sum(w * (dt$Y - EY_without_Z)^2) / sum(w)
      
    }else{
      
      model_with_Z <- glm(Y~M+Z+X, data=dt, family=Y.family, weights = w)
      model_without_Z <- glm(Y~M+X, data=dt, family=Y.family, weights = w)
      
      # Calculate MSE for both models
      mse_with_Z <- mean(residuals(model_with_Z)^2)
      mse_without_Z <- mean(residuals(model_without_Z)^2)
      
    }
    
    
    
    # Return difference in MSE (without Z minus with Z)
    # Positive values indicate model with Z performs better
    return(mse_without_Z - mse_with_Z)
  }
  
  # Calculate observed MSE difference
  observed_diff <- fit_and_compare(dt, permuted = FALSE, SL)
  
  # Perform permutation test
  permutation_diffs <- numeric(n_permutations)
  
  for (i in 1:n_permutations) {
    
    cat('Permutation:', i,'/',n_permutations, '\n')
    
    permutation_diffs[i] <- fit_and_compare(dt, permuted = TRUE,SL)
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
                              n_permutations = 100, 
                              w = primal_weights$w, 
                              Y.family = Y.family,
                              SL=superlearner)

cat('## primal test###\n')
cat("Observed MSE difference:", p.test$observed_diff, "\n")
cat("P-value:", p.test$p.value, "\n")
cat("Interpretation: Y", ifelse(p.test$p.value < 0.05, "is NOT", "may be"), 
    "independent of Z given X and M\n")

d.test <- permutation_test_YZ(dt, 
                              n_permutations = 100, 
                              w = dual_weights$w, 
                              Y.family = Y.family,
                              SL=superlearner)


cat('## dual test###\n')
cat("Observed MSE difference:", d.test$observed_diff, "\n")
cat("P-value:", d.test$p.value, "\n")
cat("Interpretation: Y", ifelse(d.test$p.value < 0.05, "is NOT", "may be"), 
    "independent of Z given X and M\n")


save(list = c("p.test","d.test"),file = paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_",superlearner,"_seed",seed,".Rdata"))