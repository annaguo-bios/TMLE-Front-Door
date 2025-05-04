library(dplyr)
library(boot)

parametric_dual_weights <- function(data, M, mpM, A, a=1){
  
  modelM <- glm(formula = as.formula("M~."), data = data[,c(M,mpM), drop=F], family = binomial())
  
  # p(M|A,X)
  p.M1.AX <- predict(modelM, type = "response")
  p.M.AX <- data[[M]]*p.M1.AX + (1-data[[M]])*(1-p.M1.AX)
  
  #p(M|a,X)
  data.a <- data %>% mutate(!!A := a)
  p.M1.aX <- predict(modelM, newdata = data.a, type = "response")
  p.M.aX <- data[[M]]*p.M1.aX + (1-data[[M]])*(1-p.M1.aX)
  
  return(p.M.aX/p.M.AX)
}



parametric_primal_weights <- function(data, A, Y, mpA, mpY){
  
  model_A <- glm(formula = as.formula('A~.'), data = data[, c(A, mpA), drop=F], family = binomial())
  model_Y <- glm(formula = as.formula('Y~.'), data = data[, c(Y, mpY), drop=F], family = gaussian())
  
  p.A1 <- predict(model_A, type = "response")
  p.A <- data[[A]]*p.A1 + (1-data[[A]])*(1-p.A1)
  
  dataA1 <- data[, c(Y, mpY), drop=F] %>% mutate(!!A := 1)
  dataA0 <- data[, c(Y, mpY), drop=F] %>% mutate(!!A := 0)
  
  EY.A <- predict(model_Y)
  EY.A1 <- predict(model_Y, newdata = dataA1)
  EY.A0 <- predict(model_Y, newdata = dataA0)
  
  stdY.A <- sqrt(var(data[[Y]]-EY.A))
  stdY.A1 <- sqrt(var(data[[Y]]-EY.A1))
  stdY.A0 <- sqrt(var(data[[Y]]-EY.A0))
  
  p.Y.A <- dnorm(data[[Y]], EY.A, stdY.A)
  p.Y.A1 <- dnorm(data[[Y]], EY.A1, stdY.A1)
  p.Y.A0 <- dnorm(data[[Y]], EY.A0, stdY.A0)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- (1-p.A1)*p.Y.A0 + p.A1*p.Y.A1
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y.A
  
  return(numerator/denominator)
  
}



weighted_lr_test <- function(dt, Y, Z, cond_set, weights, state_space = 'continuous', a=1, pseudo.outcome=T){
  
  if (pseudo.outcome){
    
    ## creating pseudo-outcome
    
    # nuisance estimation
    M.value <- dt$M
    Y.value <- dt$Y
    Z.value <- dt$Z
    A.value <- dt$A
    
    ## p(m|A,z)
    m.fit <- glm(formula = M ~ A + Z, data = dt, family = binomial())
    
    dt.a <- dt %>% mutate(A=a)
    dt.a1 <- dt %>% mutate(A=1)
    dt.a0 <- dt %>% mutate(A=0)
    
    p.m1.AZ <- predict(m.fit, type = "response") # p(M=1|A,Z)
    p.m1.aZ <- predict(m.fit, newdata = dt.a, type = "response") # p(M=1|a,Z)
    p.m0.AZ <- 1-p.m1.AZ # p(M=0|A,Z)
    p.m0.aZ <- 1-p.m1.aZ # p(M=0|a,Z)
    
    p.M.aZ <- M.value*p.m1.aZ + (1-M.value)*(1-p.m1.aZ) # p(M|a,Z)
    p.M.AZ <- M.value*p.m1.AZ + (1-M.value)*(1-p.m1.AZ) # p(M|A,Z)
    
    # E[Y|M,A,Z]
    
    y.fit <- glm(formula = Y ~ M + A + Z, data = dt, family = gaussian())
    
    dt.m1a1Z <- dt %>% mutate(M=1, A=1)
    dt.m1a0Z <- dt %>% mutate(M=1, A=0)
    dt.m0a1Z <- dt %>% mutate(M=0, A=1)
    dt.m0a0Z <- dt %>% mutate(M=0, A=0)
    
    E.Y.MAZ <- predict(y.fit, type = "response") # E(Y|M,A,Z)
    E.Y.m1a1Z <- predict(y.fit, newdata = dt.m1a1Z, type = "response") # E(Y|M=1,A=1,Z)
    E.Y.m1a0Z <- predict(y.fit, newdata = dt.m1a0Z, type = "response") # E(Y|M=1,A=0,Z)
    E.Y.m0a1Z <- predict(y.fit, newdata = dt.m0a1Z, type = "response") # E(Y|M=0,A=1,Z)
    E.Y.m0a0Z <- predict(y.fit, newdata = dt.m0a0Z, type = "response") # E(Y|M=0,A=0,Z)
    E.Y.Ma1Z <- predict(y.fit, newdata = dt.a1, type = "response") # E(Y|M,A=1,Z)
    E.Y.Ma0Z <- predict(y.fit, newdata = dt.a0, type = "response") # E(Y|M,A=0,Z)
    
    
    # p(A|Z)
    a.fit <- glm(formula = A ~ Z, data = dt, family = binomial())
    p.a1.Z <- predict(a.fit, type = "response")
    p.a0.Z <- 1-p.a1.Z
    p.a.Z <- a*p.a1.Z + (1-a)*p.a0.Z
    
    # the pseudo-outcome
    Y_tilde <- (p.M.aZ/p.M.AZ)*(Y.value-E.Y.MAZ) + 
      {(A.value==a)/p.a.Z}*{E.Y.Ma1Z*p.a1.Z + E.Y.Ma0Z*p.a0.Z - (E.Y.m1a1Z*p.m1.aZ*p.a1.Z + E.Y.m1a0Z*p.m1.aZ*p.a0.Z + E.Y.m0a1Z*p.m0.aZ*p.a1.Z + E.Y.m0a0Z*p.m0.aZ*p.a0.Z)}+
      (E.Y.m1a1Z*p.m1.aZ*p.a1.Z + E.Y.m1a0Z*p.m1.aZ*p.a0.Z + E.Y.m0a1Z*p.m0.aZ*p.a1.Z + E.Y.m0a0Z*p.m0.aZ*p.a0.Z)
    
    # replace Y with Y_tilde
    dt$Y <- Y_tilde
    
    Y.family <- if(state_space == "continuous"){gaussian()}else{binomial()}
    
    modelY_null <- glm(formula = as.formula("Y~."), data = dt[, c(Y, cond_set), drop=F], family = Y.family)
    modelY_alt <- glm(formula = as.formula("Y~."), data=dt[, c(Y, cond_set, Z), drop=F], family = Y.family)
    
    chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
    
  }else{
    
    Y.family <- if(state_space == "continuous"){gaussian()}else{binomial()}
    
    modelY_null <- glm(formula = as.formula("Y~."), data = dt[, c(Y, cond_set), drop=F], family = Y.family, weights = weights)
    modelY_alt <- glm(formula = as.formula("Y~."), data=dt[, c(Y, cond_set, Z), drop=F], family = Y.family, weights = weights)
    
    chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
    
  }
  
  

  
  return(1-pchisq(chi2_stat, df=1))
  
}

# 
# num_samples = 5000
# # try the dgps that satisfy front-door
# data = fd_admg1(num_samples); print('the dgp is fd_admg1')
# data = nonfd_admg1(num_samples); print('the dgp is nonfd_admg1')
# data = nonfd_admg2(num_samples); print('the dgp is nonfd_admg2')
# 
# primal_weights = parametric_primal_weights(data, "A", "Y", "Z", c("Z", "A", "M"), a=1)
# primal_p_val = weighted_lr_test(data, "Y", "Z", c("M"), primal_weights, a=1, pseudo.outcome = F); print(paste0('Original primal p-value: ',primal_p_val))
# primal_p_val = weighted_lr_test(data, "Y", "Z", c("M"), primal_weights, a=1, pseudo.outcome = T); print(paste0('EIF based pseudo outcome primal p-value: ',primal_p_val))
# 
# 
# 
# dual_verma_weights = parametric_dual_weights(data, "M", c("Z", "A"), "A", a=1)
# dual_p_val = weighted_lr_test(data, "Y", "Z", c("M"), dual_verma_weights, a=1, pseudo.outcome = F); print(paste0('Original dual p-value: ',dual_p_val))
# dual_p_val = weighted_lr_test(data, "Y", "Z", c("M"), dual_verma_weights, a=1, pseudo.outcome = T); print(paste0('EIF based pseudo outcome dual p-value: ',dual_p_val))
# 
# print(paste0("FD DGP 1. dual: ", round(dual_p_val,2), " primal:",round(primal_p_val,2), " dual, primal p-vals should be > 0.05"))


num_samples = 5000
# try the dgps that satisfy front-door

example <- function(data=NULL,dgp.f.name,a=1){
  
  if(is.null(data)){
    
    data <- get(dgp.f.name)(num_samples)
  }
  
  dual_verma_weights = parametric_dual_weights(data, "M", c("Z", "A"), "A", a)
  dual_p_val.org = weighted_lr_test(data, "Y", "Z", c("M"), dual_verma_weights,a=a, pseudo.outcome = F)
  dual_p_val.pseudo = weighted_lr_test(data, "Y", "Z", c("M"), dual_verma_weights,a=a, pseudo.outcome = T)
  
  primal_weights = parametric_primal_weights(data, "A", "Y", "Z", c("Z", "A", "M"))
  primal_p_val.org = weighted_lr_test(data, "Y", "Z", c("M"), primal_weights, a=a, pseudo.outcome = F)
  primal_p_val.pseudo = weighted_lr_test(data, "Y", "Z", c("M"), primal_weights, a=a, pseudo.outcome = T)
  
  # primal_p_val
  
  if (dgp.f.name == "fd_admg1"){
    
    print(paste0("FD DGP 1. dual: (original) ", round(dual_p_val.org,2),'||| (pseudo) ',round(dual_p_val.pseudo,2), " primal: (original) ",round(primal_p_val.org,2),'||| (pseudo) ' ,round(primal_p_val.pseudo,2), " dual, primal p-vals should be > 0.05"))
    
  }else if(dgp.f.name == 'nonfd_admg1'){
    
    print(paste0("nonFD DGP 1. dual: (original) ", round(dual_p_val.org,2),'||| (pseudo) ',round(dual_p_val.pseudo,2), " primal: (original) ",round(primal_p_val.org,2),'||| (pseudo) ' ,round(primal_p_val.pseudo,2), " dual, primal p-vals should be < 0.05"))
    
  }else if(dgp.f.name == 'nonfd_admg2'){
    
    print(paste0("nonFD DGP 2. dual: (original) ", round(dual_p_val.org,2),'||| (pseudo) ',round(dual_p_val.pseudo,2), " primal: (original) ",round(primal_p_val.org,2),'||| (pseudo) ' ,round(primal_p_val.pseudo,2), " dual, primal p-vals should be < 0.05"))
    
  }
  
}

set.seed(7)
example(dgp.f.name = "fd_admg1")
example(dgp.f.name = "nonfd_admg1")
example(dgp.f.name = "nonfd_admg2")

example(dgp.f.name = "fd_admg1",a=0)
example(dgp.f.name = "nonfd_admg1",a=0)
example(dgp.f.name = "nonfd_admg2",a=0)

multiple.test <- function(nsim, num_samples, a, dgp.f.name){
  
  test.result <- rep(NA,200)
  
  for (t in 1:200){
    set.seed(t)
    
    data <- fd_admg1(num_samples)
    
    dual_p_val.pseudo = weighted_lr_test(data, "Y", "Z", c("M"), weights=0, a=1, pseudo.outcome = T)
    
    test.result[t] <- ifelse(dgp.f.name=='fd_admg1', dual_p_val.pseudo>0.05, dual_p_val.pseudo<0.05)
  }
  
  return(mean(test.result))
  
}

multiple.test(200, 5000, 1, 'fd_admg1')
multiple.test(200, 5000, 1, 'nonfd_admg1')
multiple.test(200, 5000, 1, 'nonfd_admg2')
