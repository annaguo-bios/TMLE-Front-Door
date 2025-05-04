parametric_dual_weights <- function(dt, a=1){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  X <- dt$X
  
  # p(M|A,Z,X)
  p.m1.a1z1x1 <- mean(M[A==1 & Z==1 & X==1])
  p.m1.a1z1x0 <- mean(M[A==1 & Z==1 & X==0])
  p.m1.a1z0x1 <- mean(M[A==1 & Z==0 & X==1])
  p.m1.a0z1x1 <- mean(M[A==0 & Z==1 & X==1])
  p.m1.a1z0x0 <- mean(M[A==1 & Z==0 & X==0])
  p.m1.a0z1x0 <- mean(M[A==0 & Z==1 & X==0])
  p.m1.a0z0x1 <- mean(M[A==0 & Z==0 & X==1])
  p.m1.a0z0x0 <- mean(M[A==0 & Z==0 & X==0])
  
  # p(M|A,Z,X)
  p.M1.AZ <- p.m1.a1z1x1*(A==1 & Z==1 & X==1) + p.m1.a1z1x0*(A==1 & Z==1 & X==0) + p.m1.a1z0x1*(A==1 & Z==0 & X==1) + p.m1.a0z1x1*(A==0 & Z==1 & X==1) +
    p.m1.a1z0x0*(A==1 & Z==0 & X==0) + p.m1.a0z1x0*(A==0 & Z==1 & X==0) + p.m1.a0z0x1*(A==0 & Z==0 & X==1) + p.m1.a0z0x0*(A==0 & Z==0 & X==0)
  
  p.M.AZ <- M*p.M1.AZ + (1-M)*(1-p.M1.AZ)
  
  #p(M|a,Z,X)
  p.M1.aZ <-  p.m1.az1x1*(A==a & Z==1 & X==1) + p.m1.az1x0*(A==a & Z==1 & X==0) + p.m1.az0x1*(A==a & Z==0 & X==1) + p.m1.az0x0*(A==a & Z==0 & X==0)
  
  p.M.aZ <- M*p.M1.aZ + (1-M)*(1-p.M1.aZ)
  
  return(p.M.aZ/p.M.AZ)
}



parametric_primal_weights <- function(dt){
  
  M <- dt$M
  A <- dt$A
  Z <- dt$Z
  X <- dt$X
  Y <- dt$Y

  # p(A|Z,X)
  p.a1.z1x1 <- mean(A[Z==1 & X==1])
  p.a1.z1x0 <- mean(A[Z==1 & X==0])
  p.a1.z0x1 <- mean(A[Z==0 & X==1])
  p.a1.z0x0 <- mean(A[Z==0 & X==0])
  
  p.a1.ZX <- p.a1.z1x1*(Z==1 & X==1) + p.a1.z1x0*(Z==1 & X==0) + p.a1.z0x1*(Z==0 & X==1) + p.a1.z0x0*(Z==0 & X==0)
  p.a0.ZX <- 1-p.a1.ZX
  
  p.A.ZX <- A*p.a1.ZX + (1-A)*p.a0.ZX
  
  # p(Y|M,A,Z,X)
  EY.m1a1z1x1 <- mean(Y[M==1 & A==1 & Z==1 & X==1])
  EY.m1a1z1x0 <- mean(Y[M==1 & A==1 & Z==1 & X==0])
  EY.m1a1z0x1 <- mean(Y[M==1 & A==1 & Z==0 & X==1])
  EY.m1a0z1x1 <- mean(Y[M==1 & A==0 & Z==1 & X==1])
  EY.m0a1z1x1 <- mean(Y[M==0 & A==1 & Z==1 & X==1])
  EY.m1a1z0x0 <- mean(Y[M==1 & A==1 & Z==0 & X==0])
  EY.m1a0z1x0 <- mean(Y[M==1 & A==0 & Z==1 & X==0])
  EY.m0a1z1x0 <- mean(Y[M==0 & A==1 & Z==1 & X==0])
  EY.m1a0z0x1 <- mean(Y[M==1 & A==0 & Z==0 & X==1])
  EY.m0a1z0x1 <- mean(Y[M==0 & A==1 & Z==0 & X==1])
  EY.m0a0z1x1 <- mean(Y[M==0 & A==0 & Z==1 & X==1])
  EY.m1a0z0x0 <- mean(Y[M==1 & A==0 & Z==0 & X==0])
  EY.m0a1z0x0 <- mean(Y[M==0 & A==1 & Z==0 & X==0])
  EY.m0a0z1x0 <- mean(Y[M==0 & A==0 & Z==1 & X==0])
  EY.m0a0z0x1 <- mean(Y[M==0 & A==0 & Z==0 & X==1])
  EY.m0a0z0x0 <- mean(Y[M==0 & A==0 & Z==0 & X==0])
  
  EY.MAZX <- EY.m1a1z1x1*(M==1 & A==1 & Z==1 & X==1) + 
    EY.m1a1z1x0*(M==1 & A==1 & Z==1 & X==0) + EY.m1a1z0x1*(M==1 & A==1 & Z==0 & X==1) + EY.m1a0z1x1*(M==1 & A==0 & Z==1 & X==1) + EY.m0a1z1x1*(M==0 & A==1 & Z==1 & X==1) + 
    EY.m1a1z0x0*(M==1 & A==1 & Z==0 & X==0) + EY.m1a0z1x0*(M==1 & A==0 & Z==1 & X==0) + EY.m0a1z1x0*(M==0 & A==1 & Z==1 & X==0) +
    EY.m1a0z0x1*(M==1 & A==0 & Z==0 & X==1) + EY.m0a1z0x1*(M==0 & A==1 & Z==0 & X==1) + EY.m0a0z1x1*(M==0 & A==0 & Z==1 & X==1) + 
    EY.m1a0z0x0*(M==1 & A==0 & Z==0 & X==0) + EY.m0a1z0x0*(M==0 & A==1 & Z==0 & X==0) + EY.m0a0z1x0*(M==0 & A==0 & Z==1 & X==0) + EY.m0a0z0x1*(M==0 & A==0 & Z==0 & X==1) + 
    EY.m0a0z0x0*(M ==  0  &  A ==  0  &  Z ==  0  &  X ==  0)
  
  
  EY.Ma1ZX <- EY.m1a1z1x1*(M==1 & A==1 & Z==1 & X==1) + 
    EY.m1a1z1x0*(M==1 & A==1 & Z==1 & X==0) + EY.m1a1z0x1*(M==1 & A==1 & Z==0 & X==1) + EY.m0a1z1x1*(M==0 & A==1 & Z==1 & X==1) + 
    EY.m1a1z0x0*(M==1 & A==1 & Z==0 & X==0) + EY.m0a1z1x0*(M==0 & A==1 & Z==1 & X==0) +
    EY.m0a1z0x1*(M==0 & A==1 & Z==0 & X==1) + EY.m0a1z0x0*(M==0 & A==1 & Z==0 & X==0)
  
  EY.Ma0ZX <- EY.m1a0z1x1*(M==1 & A==0 & Z==1 & X==1) + EY.m1a0z1x0*(M==1 & A==0 & Z==1 & X==0) + 
    EY.m1a0z0x1*(M==1 & A==0 & Z==0 & X==1) + EY.m0a0z1x1*(M==0 & A==0 & Z==1 & X==1) + 
    EY.m1a0z0x0*(M==1 & A==0 & Z==0 & X==0) + EY.m0a0z1x0*(M==0 & A==0 & Z==1 & X==0) + EY.m0a0z0x1*(M==0 & A==0 & Z==0 & X==1) + 
    EY.m0a0z0x0*(M ==  0  &  A ==  0  &  Z ==  0  &  X ==  0)
  
  p.Y <- dbinom(Y, size=1, prob = EY.MAZX)
  p.Y.a1 <- dbinom(Y, size=1, prob = EY.Ma1ZX)
  p.Y.a0 <- dbinom(Y, size=1, prob = EY.Ma0ZX)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- p.a1.ZX*p.Y.a1 + p.a0.ZX*p.Y.a0
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A.ZX*p.Y
  
  return(numerator/denominator)
  
}



weighted_lr_test <- function(dt, weights){
  
  modelY_null <- glm(formula = as.formula("Y~M*X"), data = dt, family = binomial(), weights = weights)
  modelY_alt <- glm(formula = as.formula("Y~M*Z*X"), data=dt, family = binomial(), weights = weights)
  
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
  
  p.modelY_null <- glm(as.formula("Y~M*X"), data=dt, family=Y.family, weights = primal_weights)
  p.modelY_alt <- glm(as.formula("Y~M*X*Z"), data=dt, family=Y.family, weights = primal_weights)
  
  d.modelY_null <- glm(as.formula("Y~M*X"), data=dt, family=Y.family, weights = dual_weights)
  d.modelY_alt <- glm(as.formula("Y~M*X*Z"), data=dt, family=Y.family, weights = dual_weights)
  
  p.predY_null <- predict(p.modelY_null, newdata=dt, type="response")
  p.predY_alt <- predict(p.modelY_alt, newdata=dt, type="response")
  d.predY_null <- predict(d.modelY_null, newdata=dt, type="response")
  d.predY_alt <- predict(d.modelY_alt, newdata=dt, type="response")

  return(c(mean((p.predY_null-p.predY_alt)^2), mean((d.predY_null-d.predY_alt)^2)))
  
  
  
}