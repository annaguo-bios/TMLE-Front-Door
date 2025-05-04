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
  
  
  return(1-pchisq(chi2_stat, df=1))
  
}