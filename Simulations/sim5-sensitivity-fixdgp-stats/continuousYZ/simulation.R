setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity-fixdgp-stats/continuousYZ")

library(dplyr)

source("./DGPs/DGPs.R")
source("methods.R")

# The sample sizes tried
n.vec <- c(1000, 1500, 2000, 5000, 20000)
nsim <- 200 # Number of datasets to generate for each sample size

# The models, two of which satisfy front-door and two that don't
dgps <- c("fd_admg1", "fd_admg2", "nonfd_admg1", "nonfd_admg2")

alpha <- 0.05 # Significance level for hypothesis tests

# Set seeds
set.seed(7)

# level of A for dual weights and pseudo outcome approach
A.level <- 1


for (dgp in dgps){ # loop over dgps
  
  # Wald test
  wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                 type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  primal.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  primal.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                        type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  dual.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  dual.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                      type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  # LRT test
  primal.lrt.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  primal.lrt.rate_matrix <- data.frame(n=n.vec, # sample size
                                       type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  dual.lrt.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  dual.lrt.rate_matrix <- data.frame(n=n.vec, # sample size
                                     type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  # Wilcoxon test
  primal.wilcoxon.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  primal.wilcoxon.rate_matrix <- data.frame(n=n.vec, # sample size
                                            type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  dual.wilcoxon.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
  dual.wilcoxon.rate_matrix <- data.frame(n=n.vec, # sample size
                                          type12error=vector(mode="integer",length=length(n.vec))) # true positive rate
  
  for (n in n.vec){ # loop over sample sizes
    
    for (seed in 1:nsim){ # loop over multiple simulations
      
      dt <- get(dgp)(n)
      satisfies_fd <- ifelse(dgp %in% c('fd_admg1', 'fd_admg2'), TRUE, FALSE)
      
      # derive primal and dual weights
      dual_weights <- parametric_dual_weights(dt, a=A.level)
      primal_weights <- parametric_primal_weights(dt)
      
      ## Wald test
      # fit weighted regression of Y~M+A+Z
      Y.family <- gaussian()
      p.m1 <- glm(Y~M+Z, data=dt, family=Y.family, weights = primal_weights)
      p.m0 <- glm(Y~M, data=dt, family=Y.family, weights = primal_weights)
      d.m1 <- glm(Y~M+Z, data=dt, family=Y.family, weights = dual_weights)
      d.m0 <- glm(Y~M, data=dt, family=Y.family, weights = dual_weights)
      
      p.pred1 <- predict(p.m1, newdata=dt, type="response")
      p.pred0 <- predict(p.m0, newdata=dt, type="response")
      d.pred1 <- predict(d.m1, newdata=dt, type="response")
      d.pred0 <- predict(d.m0, newdata=dt, type="response")
      
      p.pred1.mean <- mean(p.pred1)
      p.pred0.mean <- mean(p.pred0)
      d.pred1.mean <- mean(d.pred1)
      d.pred0.mean <- mean(d.pred0)
      
      pd.cov <- cov(cbind(p.pred1 - p.pred0, d.pred1 - d.pred0))
      
      d2.wald.test <- cbind(p.pred1.mean - p.pred0.mean ,d.pred1.mean - d.pred0.mean)%*%solve(pd.cov/n)%*%rbind(p.pred1.mean - p.pred0.mean ,d.pred1.mean - d.pred0.mean) %>% as.numeric()
      d1.primal.wald.test <- (p.pred1.mean - p.pred0.mean)^2/(var(p.pred1 - p.pred0)/n)
      d1.dual.wald.test <- (d.pred1.mean - d.pred0.mean)^2/(var(d.pred1 - d.pred0)/n)
      
      
      wald.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){(1-pchisq(d2.wald.test, df=2))>alpha}else{(1-pchisq(d2.wald.test, df=2))<=alpha}
      primal.wald.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){(1-pchisq(d1.primal.wald.test, df=1))>alpha}else{(1-pchisq(d1.primal.wald.test, df=1))<=alpha}
      dual.wald.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){(1-pchisq(d1.dual.wald.test, df=1))>alpha}else{(1-pchisq(d1.dual.wald.test, df=1))<=alpha}
      
      
      ## Likelihood ratio test
      dual_pval <- weighted_lr_test(dt, weights=dual_weights)
      primal_pval <- weighted_lr_test(dt, weights=primal_weights)
      
      dual.lrt.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){dual_pval>alpha}else{dual_pval<=alpha}
      primal.lrt.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){primal_pval>alpha}else{primal_pval<=alpha}
      
      ## Wilcoxon test
      primal.wilcoxon.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){wilcox.test(p.pred1,p.pred0,paired = T)$p.value > alpha}else{wilcox.test(p.pred1,p.pred0,paired=T)$p.value <= alpha}
      dual.wilcoxon.matrix[seed,which(n.vec==n)] <- if(satisfies_fd){wilcox.test(d.pred1,d.pred0,paired=T)$p.value > alpha}else{wilcox.test(d.pred1,d.pred0,paired=T)$p.value <= alpha}
      
      
    }
    
    wald.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(wald.matrix[,which(n.vec==n)])
    primal.wald.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(primal.wald.matrix[,which(n.vec==n)])
    dual.wald.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(dual.wald.matrix[,which(n.vec==n)])
    
    dual.lrt.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(dual.lrt.matrix[,which(n.vec==n)])
    primal.lrt.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(primal.lrt.matrix[,which(n.vec==n)])
    
    primal.wilcoxon.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(primal.wilcoxon.matrix[,which(n.vec==n)])
    dual.wilcoxon.rate_matrix[which(n.vec==n),"type12error"] <- 1 - mean(dual.wilcoxon.matrix[,which(n.vec==n)])
    
    # save data
    save(list = c("wald.matrix","wald.rate_matrix","primal.wald.matrix","primal.wald.rate_matrix","dual.wald.matrix","dual.wald.rate_matrix",
                  "dual.lrt.matrix","dual.lrt.rate_matrix",'primal.lrt.matrix','primal.lrt.rate_matrix',
                  'primal.wilcoxon.matrix','primal.wilcoxon.rate_matrix','dual.wilcoxon.matrix','dual.wilcoxon.rate_matrix'),file = paste0(dgp,'/weighted_result.Rdata'))
    
    
  }
  
}
