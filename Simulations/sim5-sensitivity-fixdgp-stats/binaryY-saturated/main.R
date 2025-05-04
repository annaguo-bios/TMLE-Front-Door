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
# dgp.f.name='fd_admg1'
# R=500


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

# Derive sum_a p(Y|M,a,z) p(a|z) for fixed z
test.z <- function(dt, m.level, z,x){
  
  ## Y fit
  dtY <- dt %>%
    filter(Z == z, X == x) %>%
    group_by(M, A) %>%
    summarize(EY = mean(Y), .groups = "drop") %>%
    mutate(name = paste0("p.y1.m", M, "a", A, "zx")) %>%
    column_to_rownames("name")
  
  
  ## A fit
  p.a1.zx <- mean(dt$A[dt$Z==z & dt$X==x])
  p.a0.zx <- 1-p.a1.zx
  
  ## test statistic
  test.stat.m1 <- dtY["p.y1.m1a1zx","EY"]*p.a1.zx + dtY["p.y1.m1a0zx","EY"]*p.a0.zx
  test.stat.m0 <- dtY["p.y1.m0a1zx","EY"]*p.a1.zx + dtY["p.y1.m0a0zx","EY"]*p.a0.zx
  
  return(m.level*test.stat.m1 + (1-m.level)*test.stat.m0)
}


# function that compute E[Y(m)|Z=1,X] - E[Y(m)|Z=0,X]
test <- function(dt, i , m.level=NULL,x,estimator='verma'){ # estimator: np, AIPW, IPW, Gcomp
  
  # resampling
  dt <- dt[i,]
  
  M <- dt$M
  Y <- dt$Y
  Z <- dt$Z
  A <- dt$A
  X <- dt$X
  
  ## p(A|Z,X)
  dtA_a1 <- dt %>%
    group_by(Z, X) %>%
    summarize(EA = mean(A), .groups = "drop") %>%
    mutate(name = paste0("p.a1.z", Z, "x", X)) %>%
    column_to_rownames("name")
  
  dtA_a0 <- dtA_a1 %>%
    rownames_to_column("old_name") %>%  # First convert existing rownames to a column
    mutate(
      EA = 1 - EA,
      name = paste0("p.a0.z", Z, "x", X)
    ) %>%
    select(-old_name) %>%  # Remove the old name column
    column_to_rownames("name")
  
  ## p(m|A,z,x)
  dtM_m1 <- dt %>%
    group_by(A, Z, X) %>%
    summarize(EM = mean(M), .groups = "drop") %>%
    mutate(name = paste0("p.m1", ".a", A, "z", Z, "x", X)) %>%
    column_to_rownames("name")
  
  p.m1.Az1x1 <- (A==1)*dtM_m1['p.m1.a1z1x1','EM'] + (A==0)*dtM_m1['p.m1.a0z1x1','EM']
  p.m1.Az1x0 <- (A==1)*dtM_m1['p.m1.a1z1x0','EM'] + (A==0)*dtM_m1['p.m1.a0z1x0','EM']
  p.m1.Az0x1 <- (A==1)*dtM_m1['p.m1.a1z0x1','EM'] + (A==0)*dtM_m1['p.m1.a0z0x1','EM']
  p.m1.Az0x0 <- (A==1)*dtM_m1['p.m1.a1z0x0','EM'] + (A==0)*dtM_m1['p.m1.a0z0x0','EM']
  
  p.m.Az1x1 <- m.level*p.m1.Az1x1 + (1-m.level)*(1-p.m1.Az1x1)
  p.m.Az1x0 <- m.level*p.m1.Az1x0 + (1-m.level)*(1-p.m1.Az1x0)
  p.m.Az0x1 <- m.level*p.m1.Az0x1 + (1-m.level)*(1-p.m1.Az0x1)
  p.m.Az0x0 <- m.level*p.m1.Az0x0 + (1-m.level)*(1-p.m1.Az0x0)
  
  # E[Y|m,A,z,x]
  dtY_m <- dt %>%
    filter(M == m.level) %>%
    group_by(A, Z, X) %>%
    summarize(EY = mean(Y), .groups = "drop") %>%
    mutate(name = paste0("E.Y.ma", A, "z", Z, "x", X)) %>%
    column_to_rownames("name")
  
  E.Y.mAz1x1 <- (A==1)*dtY_m['E.Y.ma1z1x1','EY'] + (A==0)*dtY_m['E.Y.ma0z1x1','EY']
  E.Y.mAz1x0 <- (A==1)*dtY_m['E.Y.ma1z1x0','EY'] + (A==0)*dtY_m['E.Y.ma0z1x0','EY']
  E.Y.mAz0x1 <- (A==1)*dtY_m['E.Y.ma1z0x1','EY'] + (A==0)*dtY_m['E.Y.ma0z0x1','EY']
  E.Y.mAz0x0 <- (A==1)*dtY_m['E.Y.ma1z0x0','EY'] + (A==0)*dtY_m['E.Y.ma0z0x0','EY']

  
  # p(Z)
  p.z1 <- mean(Z==1)
  p.z0 <- mean(Z==0)
  
  if (estimator == 'np'){
    
    test.stat.z1x1 <- test.z(dt, m.level, z=1,x=1)
    test.stat.z0x1 <- test.z(dt, m.level, z=0,x=1)
    
    est.diff <- test.stat.z1 - test.stat.z0
    
  }else if (estimator == 'AIPW'){
    
    ## the AIPW estimates
    # Z=1
    est.z1 <- mean((Z==1)*(M==m.level)/(p.m.Az1*p.z1)*(Y-E.Y.mAz1) + (Z==1)/p.z1*E.Y.mAz1)
    eif.z1 <- (Z==1)*(M==m.level)/(p.m.Az1*p.z1)*(Y-E.Y.mAz1) + (Z==1)/p.z1*E.Y.mAz1 - est.z1
    std.z1 <- sqrt(mean(eif.z1^2)/nrow(dt))
    
    # Z=0
    est.z0 <- mean((Z==0)*(M==m.level)/(p.m.Az0*p.z0)*(Y-E.Y.mAz0) + (Z==0)/p.z0*E.Y.mAz0)
    eif.z0 <- (Z==0)*(M==m.level)/(p.m.Az0*p.z0)*(Y-E.Y.mAz0) + (Z==0)/p.z0*E.Y.mAz0 - est.z0
    std.z0 <- sqrt(mean(eif.z0^2)/nrow(dt))
    
    # diff: E[Y(m)|Z=1] - E[Y(m)|Z=0]
    est.diff <- est.z1 - est.z0 # <- output of the function
    std.diff <- sqrt(std.z1^2 + std.z0^2)
    lower.ci <- est.diff - 1.96*std.diff
    upper.ci <- est.diff + 1.96*std.diff
    
  }else if (estimator == 'IPW'){
    
    ## the IPW estimates
    # Z=1
    est.z1 <- mean((Z==1)*(M==m.level)/(p.m.Az1*p.z1)*Y)
    
    # Z=0
    est.z0 <- mean((Z==0)*(M==m.level)/(p.m.Az0*p.z0)*Y)
    
    # diff: E[Y(m)|Z=1] - E[Y(m)|Z=0]
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else if (estimator == 'Gcomp'){
    
    est.z1 <- mean((Z==1)/p.z1*E.Y.mAz1)
    est.z0 <- mean((Z==0)/p.z0*E.Y.mAz0)
    
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else if (estimator == 'verma'){
    
    est.z1 <- E.Y.ma1z1*p.a1.z1 + E.Y.ma0z1*p.a0.z1
    est.z0 <- E.Y.ma1z0*p.a1.z0 + E.Y.ma0z0*p.a0.z0
    
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else{
    
    stop("estimator not recognized")
    
  }
  
  return(est.diff)
  
}



parametric_dual_weights <- function(dt, a=1){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  
  # p(M|A,Z)
  p.m1.a1z1 <- mean(M[A==1 & Z==1])
  p.m1.a1z0 <- mean(M[A==1 & Z==0])
  p.m1.a0z1 <- mean(M[A==0 & Z==1])
  p.m1.a0z0 <- mean(M[A==0 & Z==0])
  
  # p(M|A,Z)
  p.M1.AZ <- p.m1.a1z1*(A==1 & Z==1) + p.m1.a1z0*(A==1 & Z==0) + p.m1.a0z1*(A==0 & Z==1) + p.m1.a0z0*(A==0 & Z==0)
  p.M.AZ <- M*p.M1.AZ + (1-M)*(1-p.M1.AZ)
  
  #p(M|a,Z)
  p.M1.aZ <-  p.m1.a1z1*(a==1 & Z==1) + p.m1.a1z0*(a==1 & Z==0) + p.m1.a0z1*(a==0 & Z==1) + p.m1.a0z0*(a==0 & Z==0)
  p.M.aZ <- M*p.M1.aZ + (1-M)*(1-p.M1.aZ)
  
  return(p.M.aZ/p.M.AZ)
}



parametric_primal_weights <- function(dt){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  Y <- dt$Y
  
  # p(A|Z)
  p.a1.z1 <- mean(A[Z==1])
  p.a1.z0 <- mean(A[Z==0])
  p.a1.Z <- p.a1.z1*(Z==1) + p.a1.z0*(Z==0)
  p.a0.Z <- 1-p.a1.Z
  p.A <- A*p.a1.Z + (1-A)*p.a0.Z
  
  # p(Y|M,A,Z)
  EY.m1a1z1 <- mean(Y[M==1 & A==1 & Z==1])
  EY.m1a1z0 <- mean(Y[M==1 & A==1 & Z==0])
  EY.m1a0z1 <- mean(Y[M==1 & A==0 & Z==1])
  EY.m1a0z0 <- mean(Y[M==1 & A==0 & Z==0])
  EY.m0a1z1 <- mean(Y[M==0 & A==1 & Z==1])
  EY.m0a1z0 <- mean(Y[M==0 & A==1 & Z==0])
  EY.m0a0z1 <- mean(Y[M==0 & A==0 & Z==1])
  EY.m0a0z0 <- mean(Y[M==0 & A==0 & Z==0])
  
  EY.MAZ <- EY.m1a1z1*(M==1 & A==1 & Z==1) + EY.m1a1z0*(M==1 & A==1 & Z==0) + EY.m1a0z1*(M==1 & A==0 & Z==1) + EY.m1a0z0*(M==1 & A==0 & Z==0) +
    EY.m0a1z1*(M==0 & A==1 & Z==1) + EY.m0a1z0*(M==0 & A==1 & Z==0) + EY.m0a0z1*(M==0 & A==0 & Z==1) + EY.m0a0z0*(M==0 & A==0 & Z==0)
  
  EY.Ma1Z <- EY.m0a1z0*(M==0 & Z==0) + EY.m0a1z1*(M==0 & Z==1) + EY.m1a1z1*(M==1  & Z==1) + EY.m1a1z0*(M==1 & Z==0)
  EY.Ma0Z <- EY.m0a0z0*(M==0 &  Z==0) + EY.m0a0z1*(M==0  & Z==1) + EY.m1a0z1*(M==1  & Z==1) + EY.m1a0z0*(M==1  & Z==0)
  
  p.Y <- dbinom(Y, size=1, prob = EY.MAZ)
  p.Y.a1 <- dbinom(Y, size=1, prob = EY.Ma1Z)
  p.Y.a0 <- dbinom(Y, size=1, prob = EY.Ma0Z)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a1.Z*p.Y.a1 + p.a0.Z*p.Y.a0
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y
  
  return(numerator/denominator)
  
}



weighted_lr_test <- function(dt, weights){
  
  modelY_null <- glm(formula = as.formula("Y~M"), data = dt, family = binomial(), weights = weights)
  modelY_alt <- glm(formula = as.formula("Y~M+Z+I(M*Z)"), data=dt, family = binomial(), weights = weights)
  
  chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
  
  
  return(1-pchisq(chi2_stat, df=2))
  
}


wald_test <- function(dt,i, A.level=1){
  
  # resampling
  dt <- dt[i,]
  
  # derive primal and dual weights
  dual_weights <- parametric_dual_weights(dt, a=A.level)
  primal_weights <- parametric_primal_weights(dt)
  
  # fit weighted regression of Y~M+A+Z
  Y.family <- binomial(link="logit")
  
  p.m1 <- glm(Y~M+Z+I(M*Z), data=dt, family=Y.family, weights = primal_weights)
  p.m0 <- glm(Y~M, data=dt, family=Y.family, weights = primal_weights)
  
  d.m1 <- glm(Y~M+Z+I(M*Z), data=dt, family=Y.family, weights = dual_weights)
  d.m0 <- glm(Y~M, data=dt, family=Y.family, weights = dual_weights)
  
  p.pred1 <- predict(p.m1, newdata=dt, type="response")
  p.pred0 <- predict(p.m0, newdata=dt, type="response")
  d.pred1 <- predict(d.m1, newdata=dt, type="response")
  d.pred0 <- predict(d.m0, newdata=dt, type="response")
  
  return(c(mean((p.pred1-p.pred0)^2), mean((d.pred1-d.pred0)^2)))
  
  
  
}


#################################################
# CATE-based TEST
#################################################

satisfies_fd <- ifelse(dgp.f.name %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE) # record whether the DGP randomly selected satisfies the FD assumption

# generate data
dt = get(dgp.f.name)(n)

# testing
test.m1 <- boot(dt, statistic=test , R=R, m.level=1, estimator=estimator)
test.m0 <- boot(dt, statistic=test , R=R, m.level=0, estimator=estimator)

# compute CI controling for FWER
lower.ci.m1 <- quantile(test.m1$t, 0.0125)
upper.ci.m1 <- quantile(test.m1$t, 0.9875)
lower.ci.m0 <- quantile(test.m0$t, 0.0125)
upper.ci.m0 <- quantile(test.m0$t, 0.9875)

# Wald test
wald.test <- cbind(mean(test.m1$t),mean(test.m0$t))%*%solve(cov(cbind(test.m1$t, test.m0$t)))%*%rbind(mean(test.m1$t) , mean(test.m0$t)) %>% as.numeric()

#################################################
# weight-based TEST
#################################################

# derive primal and dual weights
dual_weights <- parametric_dual_weights(dt, a=A.level)
primal_weights <- parametric_primal_weights(dt)

# fit weighted regression of Y~M+A+Z
Y.family <- binomial(link="logit")

p.m1 <- glm(Y~M+Z+I(M*Z), data=dt, family=Y.family, weights = primal_weights)
p.m0 <- glm(Y~M, data=dt, family=Y.family, weights = primal_weights)

d.m1 <- glm(Y~M+Z+I(M*Z), data=dt, family=Y.family, weights = dual_weights)
d.m0 <- glm(Y~M, data=dt, family=Y.family, weights = dual_weights)

p.pred1 <- predict(p.m1, newdata=dt, type="response")
p.pred0 <- predict(p.m0, newdata=dt, type="response")
d.pred1 <- predict(d.m1, newdata=dt, type="response")
d.pred0 <- predict(d.m0, newdata=dt, type="response")


# Wald test
wald.boot <- boot(dt, statistic=wald_test, R=R, A.level=A.level)

d2.wald.test <- cbind(mean(wald.boot$t[,1]),mean(wald.boot$t[,2]))%*%solve( cov(wald.boot$t) )%*%rbind(mean(wald.boot$t[,1]),mean(wald.boot$t[,2])) %>% as.numeric()
d1.primal.wald.test <- mean(wald.boot$t[,1])^2/var(wald.boot$t[,1])
d1.dual.wald.test <- mean(wald.boot$t[,2])^2/var(wald.boot$t[,2])

## Likelihood ratio test
dual_pval <- weighted_lr_test(dt, weights=dual_weights)
primal_pval <- weighted_lr_test(dt, weights=primal_weights)

## Wilcoxon test
dual_wilcoxon_pval <- wilcox.test(dt$Y - d.pred1,dt$Y - d.pred0, paired=F)$p.value
primal_wilcoxon_pval <- wilcox.test(dt$Y - p.pred1,dt$Y - p.pred0, paired=F)$p.value


save(list = c("lower.ci.m1","lower.ci.m0","upper.ci.m1","upper.ci.m0","wald.test","satisfies_fd","d2.wald.test","d1.primal.wald.test","d1.dual.wald.test","dual_pval","primal_pval","dual_wilcoxon_pval","primal_wilcoxon_pval"),file = paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_seed",seed,".Rdata"))