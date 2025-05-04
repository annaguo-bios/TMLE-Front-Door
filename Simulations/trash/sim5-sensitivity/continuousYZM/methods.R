parametric_dual_weights <- function(dt, M, mpM, A, a=1){
  
  modelM <- glm(formula = as.formula("M~."), data = dt[,c(M,mpM), drop=F], family = gaussian())
  
  # p(M|A,X)
  E.M.AX <- predict(modelM)
  sd.M.AX <- sqrt(var(dt[[M]]-E.M.AX))
  
  p.M.AX <- dnorm(dt[[M]], E.M.AX, sd.M.AX)
  
  #p(M|a,X)
  dt.a <- dt %>% mutate(!!A := a)
  E.M.aX <- predict(modelM, newdata = dt.a)
  sd.M.aX <- sqrt(var(dt[[M]]-E.M.aX))
  
  p.M.aX <- dnorm(dt[[M]], E.M.aX, sd.M.aX)
  
  return(p.M.aX/p.M.AX)
}



parametric_primal_weights <- function(dt, A, Y, mpA, mpY, Y.type='continuous'){
  
  Y.family <- if(Y.type == 'continuous'){gaussian()}else{binomial()} # gaussian() if Y is continuous, binomial() if Y is binary
  
  model_A <- glm(formula = as.formula('A~.'), data = dt[, c(A, mpA), drop=F], family = binomial())
  model_Y <- glm(formula = as.formula('Y~.'), data = dt[, c(Y, mpY), drop=F], family = Y.family)
  
  p.A1 <- predict(model_A, type = "response")
  p.A <- dt[[A]]*p.A1 + (1-dt[[A]])*(1-p.A1)
  
  dtA1 <- dt[, c(Y, mpY), drop=F] %>% mutate(!!A := 1)
  dtA0 <- dt[, c(Y, mpY), drop=F] %>% mutate(!!A := 0)
  
  EY.A <- predict(model_Y)
  EY.A1 <- predict(model_Y, newdata = dtA1)
  EY.A0 <- predict(model_Y, newdata = dtA0)
  
  stdY.A <- sqrt(var(dt[[Y]]-EY.A))
  stdY.A1 <- sqrt(var(dt[[Y]]-EY.A1))
  stdY.A0 <- sqrt(var(dt[[Y]]-EY.A0))
  
  p.Y.A <- dnorm(dt[[Y]], EY.A, stdY.A)
  p.Y.A1 <- dnorm(dt[[Y]], EY.A1, stdY.A1)
  p.Y.A0 <- dnorm(dt[[Y]], EY.A0, stdY.A0)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- (1-p.A1)*p.Y.A0 + p.A1*p.Y.A1
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y.A
  
  return(numerator/denominator)
  
}



weighted_lr_test <- function(dt, Y, Z, cond_set, weights, M.type = 'binary', Y.type = 'continuous', a=1, pseudo.outcome=T){
  
  if (pseudo.outcome){
    
    ## creating pseudo-outcome
    
    # nuisance estimation
    M.value <- dt$M
    Y.value <- dt$Y
    Z.value <- dt$Z
    A.value <- dt$A
    
    ## p(m|A,z)
    M.family <- if(M.type == 'binary'){binomial()}else{gaussian()} # binomal() if M is binary, gaussian() if M is continuous
    m.fit <- glm(formula = M ~ A + Z, data = dt, family = M.family)
    
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
    Y.family <- if(Y.type == 'continuous'){gaussian()}else{binomial()} # gaussian() if Y is continuous, binomial() if Y is binary
    y.fit <- glm(formula = Y ~ M + A + Z, data = dt, family = Y.family)
    
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
      (E.Y.Ma1Z*p.a1.Z + E.Y.Ma0Z*p.a0.Z)
    
    # replace Y with Y_tilde
    dt$Y <- Y_tilde
    
    modelY_null <- glm(formula = as.formula("Y~."), data = dt[, c(Y, cond_set), drop=F], family = Y.family)
    modelY_alt <- glm(formula = as.formula("Y~."), data=dt[, c(Y, cond_set, Z), drop=F], family = Y.family)
    
    chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
    
  }else{
    
    Y.family <- if(Y.type == 'continuous'){gaussian()}else{binomial()} # gaussian() if Y is continuous, binomial() if Y is binary
    
    modelY_null <- glm(formula = as.formula("Y~."), data = dt[, c(Y, cond_set), drop=F], family = Y.family, weights = weights)
    modelY_alt <- glm(formula = as.formula("Y~."), data=dt[, c(Y, cond_set, Z), drop=F], family = Y.family, weights = weights)
    
    chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
    
  }
  
  
  
  
  return(1-pchisq(chi2_stat, df=1))
  
}