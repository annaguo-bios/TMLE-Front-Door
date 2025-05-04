parametric_dual_weights <- function(dt, a=1){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  
  M.family <- binomial() # binomal() if M is binary, gaussian() if M is continuous
  modelM <- glm(formula = M ~ A+Z, family = M.family)
  
  # p(M|A,Z)
  p.M1.AZ <- predict(modelM, type = "response")
  p.M.AZ <- M*p.M1.AZ + (1-M)*(1-p.M1.AZ)
  
  #p(M|a,Z)
  p.M1.aZ <- predict(modelM, newdata = data.frame(A=a,Z=Z), type = "response")
  p.M.aZ <- M*p.M1.aZ + (1-M)*(1-p.M1.aZ)
  
  return(p.M.aZ/p.M.AZ)
}

parametric_primal_weights <- function(dt){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  Y <- dt$Y
  
  # p(A|Z)
  model_A <- glm(formula = A~Z, family = binomial())
  p.A1 <- predict(model_A, type = "response")
  p.A0 <- 1-p.A1
  p.A <- A*p.A1 + (1-A)*(1-p.A1)
  
  
  # p(Y|M,A,Z)
  Y.family <- gaussian()
  model_Y <- glm(formula = Y~M+A+Z, family = Y.family)
  
  EY.A <- predict(model_Y)
  EY.A1 <- predict(model_Y, newdata = data.frame(A=1, M=M, Z=Z))
  EY.A0 <- predict(model_Y, newdata = data.frame(A=0, M=M, Z=Z))
  
  stdY.A <- sqrt(var(Y-EY.A))
  stdY.A1 <- sqrt(var(Y-EY.A1))
  stdY.A0 <- sqrt(var(Y-EY.A0))
  
  p.Y.A <- dnorm(Y, EY.A, stdY.A)
  p.Y.A1 <- dnorm(Y, EY.A1, stdY.A1)
  p.Y.A0 <- dnorm(Y, EY.A0, stdY.A0)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.A0*p.Y.A0 + p.A1*p.Y.A1
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y.A
  
  return(numerator/denominator)
  
}


weighted_lr_test <- function(dt, weights){
  
  Y.family <- gaussian()
  modelY_null <- glm(formula = Y~M, data = dt, family = Y.family, weights = weights)
  modelY_alt <- glm(formula = Y~M+Z, data=dt, family = Y.family, weights = weights)
  
  chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null)) # rejecting H0 for large values of chi2_stat
  
  
  return(1-pchisq(chi2_stat, df=1))
  
}

wald_test <- function(dt,i, A.level=1){
  
  # resampling
  dt <- dt[i,]
  
  # derive primal and dual weights
  dual_weights <- parametric_dual_weights(dt, a=A.level)
  primal_weights <- parametric_primal_weights(dt)
  
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
  
  return(c(mean((p.pred1-p.pred0)^2), mean((d.pred1-d.pred0)^2)))
  
  
  
}
