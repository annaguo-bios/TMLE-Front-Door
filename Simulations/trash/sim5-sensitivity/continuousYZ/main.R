args = commandArgs(trailingOnly=T)
n=as.integer(args[1]) # sample size for the simulation
seed=as.integer(args[2])  # seeds for replication
m=as.integer(args[3]) # level of m
estimator=args[4] # IPW or AIPW
R=as.integer(args[5]) # number of bootstrap samples

# # one example input
# n=1000
# seed=1
# m=0
# estimator='AIPW'
# R=500


library(dplyr)
library(boot)

set.seed(seed)
#################################################
# Functions
#################################################
source("DGPs/DGPs.R")

# function that compute E[Y(m)|Z=1] - E[Y(m)|Z=0]
test <- function(dt, i , m.level=NULL , estimator='AIPW'){
  
  # binarize Z
  dt$Z <- as.numeric(dt$Z<0)
  
  # resampling
  dt <- dt[i,]
  
  M <- dt$M
  Y <- dt$Y
  Z <- dt$Z
  
  ## p(A|Z)
  a.fit <- glm(formula = A ~ Z, data = dt, family = binomial())
  p.a1.z1 <- predict(a.fit, newdata = data.frame(Z=1), type = "response")
  p.a1.z0 <- predict(a.fit, newdata = data.frame(Z=0), type = "response")
  p.a0.z1 <- 1-p.a1.z1
  p.a0.z0 <- 1-p.a1.z0
  
  ## p(m|A,z)
  m.fit <- glm(formula = M ~ A + Z, data = dt, family = binomial())
  
  dt.z1 <- dt %>% mutate(Z=1)
  dt.z0 <- dt %>% mutate(Z=0)
  
  p.m1.Az1 <- predict(m.fit, newdata = dt.z1, type = "response")
  p.m1.Az0 <- predict(m.fit, newdata = dt.z0, type = "response")
  p.m1.AZ <- predict(m.fit, type = "response")
  
  p.m.Az1 <- m.level*p.m1.Az1 + (1-m.level)*(1-p.m1.Az1)
  p.m.Az0 <- m.level*p.m1.Az0 + (1-m.level)*(1-p.m1.Az0)
  p.m.AZ <- m.level*p.m1.AZ + (1-m.level)*(1-p.m1.AZ)
  
  # E[Y|m,A,z]
  y.family <- if(all(Y %in% c(0,1))){binomial()}else{gaussian()}
  y.fit <- glm(formula = Y ~ M + A + Z, data = dt, family = y.family)
  
  dt.mAz1 <- dt %>% mutate(M=m.level, Z=1)
  dt.mAz0 <- dt %>% mutate(M=m.level, Z=0)
  dt.mAZ <- dt %>% mutate(M=m.level)
  dt.ma1z1 <- data.frame(M=m.level, A=1, Z=1)
  dt.ma0z1 <- data.frame(M=m.level, A=0, Z=1)
  dt.ma1z0 <- data.frame(M=m.level, A=1, Z=0)
  dt.ma0z0 <- data.frame(M=m.level, A=0, Z=0)
  
  E.Y.mAz1 <- predict(y.fit, newdata = dt.mAz1, type = "response")
  E.Y.mAz0 <- predict(y.fit, newdata = dt.mAz0, type = "response")
  E.Y.mAZ <- predict(y.fit, newdata = dt.mAZ, type = "response")
  E.Y.ma1z1 <- predict(y.fit, newdata = dt.ma1z1, type = "response")
  E.Y.ma0z1 <- predict(y.fit, newdata = dt.ma0z1, type = "response")
  E.Y.ma1z0 <- predict(y.fit, newdata = dt.ma1z0, type = "response")
  E.Y.ma0z0 <- predict(y.fit, newdata = dt.ma0z0, type = "response")
  
  # p(Z)
  p.z1 <- mean(Z==1)
  p.z0 <- mean(Z==0)
  
  if(estimator=='AIPW'){
    
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
    
  }else if (estimator=='IPW'){
    
    ## the IPW estimates
    # Z=1
    est.z1 <- mean((Z==1)*(M==m.level)/(p.m.Az1*p.z1)*Y)
    
    # Z=0
    est.z0 <- mean((Z==0)*(M==m.level)/(p.m.Az0*p.z0)*Y)
    
    # diff: E[Y(m)|Z=1] - E[Y(m)|Z=0]
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else if (estimator =='Gcomp'){
    
    est.z1 <- mean((Z==1)/p.z1*E.Y.mAz1)
    est.z0 <- mean((Z==0)/p.z0*E.Y.mAz0)
    
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else if (estimator == 'g-null'){
    
    est.z1 <- E.Y.ma1z1*p.a1.z1 + E.Y.ma0z1*p.a0.z1
    est.z0 <- E.Y.ma1z0*p.a1.z0 + E.Y.ma0z0*p.a0.z0
    
    est.diff <- est.z1 - est.z0 # <- output of the function
    
  }else{
    
    stop("estimator not recognized")
    
  }
  
  
  return(est.diff)
  
}


#################################################
# Generate data
#################################################

dgps <- c("fd_admg1", "fd_admg2", "nonfd_admg1", "nonfd_admg2")

dgp <- sample(dgps, 1)

dt <- get(dgp)(n)

satisfies_fd <- ifelse(dgp %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE) # record whether the DGP randomly selected satisfies the FD assumption

#################################################
# Testing
#################################################

test.m <- boot(dt, statistic=test , R=R, m.level=m, estimator=estimator)


test.statistic <- test.m$t %>% mean()
test.statistic.sd <- test.m$t %>% sd()
test.statistic.lower.ci <- test.statistic - 1.96*test.statistic.sd
test.statistic.upper.ci <- test.statistic + 1.96*test.statistic.sd

test.fd <- ifelse(test.statistic.lower.ci < 0 & test.statistic.upper.ci > 0, TRUE, FALSE)

#################################################
# Store testing results
#################################################

test.res <- c(0,0,0,0) #TP, FP, FN, TN

if (test.fd && satisfies_fd) {
  
  test.res[1] <- 1
  
} else if (test.fd && !satisfies_fd) {
  
  test.res[2] <- 1
  
} else if (!test.fd && satisfies_fd) {
  
  test.res[3] <- 1
  
} else {
  
  test.res[4] <- 1
  
}

save(list = c("test.statistic","test.statistic.sd","test.statistic.lower.ci","test.statistic.upper.ci","satisfies_fd","test.res"),file = paste0("output/output_n",n,"_m",m,"_",estimator,"_seed",seed,".Rdata"))