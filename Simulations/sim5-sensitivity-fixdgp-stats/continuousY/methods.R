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
  
  p.Y.A <- dnorm(Y, EY.MAZ, sqrt(var(Y-EY.MAZ)) )
  p.Y.A1 <- dnorm(Y, EY.Ma1Z, sqrt(var(Y-EY.Ma1Z)) )
  p.Y.A0 <- dnorm(Y, EY.Ma0Z, sqrt(var(Y-EY.Ma0Z)) )
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a0.Z*p.Y.A0 + p.a1.Z*p.Y.A1
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y.A
  
  return(numerator/denominator)
  
}


weighted_lr_test <- function(dt, weights){
  
  Y.family <- gaussian()
  modelY_null <- glm(formula = Y~M, data = dt, family = Y.family, weights = weights)
  modelY_alt <- glm(formula = Y~M+Z+I(M*Z), data=dt, family = Y.family, weights = weights)
  
  chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null)) # rejecting H0 for large values of chi2_stat
  
  
  return(1-pchisq(chi2_stat, df=2))
  
}


wald_test <- function(dt,i, A.level=1){
  
  # resampling
  dt <- dt[i,]
  
  # derive primal and dual weights
  dual_weights <- parametric_dual_weights(dt, a=A.level)
  primal_weights <- parametric_primal_weights(dt)
  
  # fit weighted regression of Y~M+A+Z
  Y.family <- gaussian()
  
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