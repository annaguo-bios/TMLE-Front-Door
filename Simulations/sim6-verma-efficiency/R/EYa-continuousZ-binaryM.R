EYa.continuousZ.binaryM <- function(a,data,treatment, mediator, outcome, anchor, tilde_pz,
                                onestep=TRUE, superlearner=TRUE,crossfit=FALSE,K=5,
                                lib = c("SL.glm","SL.earth","SL.ranger","SL.mean"), n.iter=500, cvg.criteria=0.01,
                                formulaY="Y ~ .", formulaA="A ~ .", formulaM="M~.", linkY_binary="logit", linkA="logit", linkM_binary="logit",
                                truncate_lower=0.01, truncate_upper=0.99,
                                verbose=T){
  
  # attach(data, warn.conflicts=FALSE)
  
  n <- nrow(data)
  # Variables
  A <- data[,treatment]
  M <- data[,mediator]
  Z <- data[,anchor]
  Y <- data[,outcome]
  
  # p(Z)
  density.z <-function(z){
    
    den.zi <- density(Z)
    
    return(approx(den.zi$x, den.zi$y, xout = z)$y)
  }
  
  # p(A|Z)
  density.a.z <- function(a,z){
    
    ps_fit <- glm(as.formula(formulaA), data=data.frame(A=A,Z=Z),  family = binomial(linkA))
    
    newdata <- data.frame(A=a,Z=z)
    
    p.a1.z <- predict(ps_fit, newdata=newdata, type="response")
    
    p.a.z <- a*p.a1.z + (1-a)*(1-p.a1.z)
    
    return(p.a.z)
  }
  
  # p(M|A,Z)
  density.m.az <- function(m,a,z){
    
    m_fit <- glm(as.formula(formulaM), data=data.frame(M=M,A=A,Z=Z),  family = binomial(linkM_binary))
    
    newdata <- data.frame(M=m,A=a,Z=z)
    
    p.m1.az <- predict(m_fit, newdata=newdata, type="response")
    
    p.m.az <- m*p.m1.az + (1-m)*(1-p.m1.az)
    
    return(p.m.az)
    
  }
  
  # E(Y|M,A,Z)
  E.y.maz <- function(m,a,z){
    
    or_fit <- glm(as.formula(formulaY), data=data.frame(Y=Y,M=M,A=A,Z=Z),  family = gaussian())
    
    newdata <- data.frame(M=m,A=a,Z=z)
    
    mu.maz <- predict(or_fit, newdata=newdata, type="response")
    
    return(mu.maz)
    
  }
  
  ##################################################################
  #################### One-step estimator ##########################
  ##################################################################
  
  # outcome regression E[Y|M,A,z]
  mu <- E.y.maz(M,A,Z) # A=A, M=M, Z=Z
  mu.a1m1 <- E.y.maz(1,1,Z) # A=1, M=1, Z=Z
  mu.a1m0 <- E.y.maz(0,1,Z) # A=1, M=0, Z=Z
  mu.a0m1 <- E.y.maz(1,0,Z) # A=0, M=1, Z=Z
  mu.a0m0 <- E.y.maz(0,0,Z) # A=0, M=0, Z=Z
  
  # mediator density p(M|A=a,Z)
  p.m1.az1 <- density.m.az(m=1,a,z=1) #p(M=1|a,Z=1)
  p.m1.az0 <- density.m.az(m=1,a,z=0) #p(M=1|a,Z=0)
  p.m0.az1 <- density.m.az(m=0,a,z=1) #p(M=0|a,Z=1)
  p.m0.az0 <- density.m.az(m=0,a,z=0) #p(M=0|a,Z=0
  
  p.M.aZ <- density.m.az(M,a,Z) # p(M|A=a,Z)
  p.M.AZ <- density.m.az(M,A,Z) # p(M|A,Z)
  p.m1.aZ <- density.m.az(m=1,a,Z) # p(M=1|A=a,Z)
  p.m0.aZ <- density.m.az(m=0,a,Z) # p(M=0|A=a,Z)
  
  p.M.az1 <- density.m.az(M,a,z=1) # p(M|A=a,Z=1)
  p.M.az0 <- density.m.az(M,a,z=0) # p(M|A=a,Z=0)
  
  # propensity score p(A|Z=z)
  p.a.Z <- density.a.z(a,Z) # p(A=a|Z)
  p.a1.Z <- density.a.z(a=1,Z) # p(A=1|Z)
  p.a0.Z <- density.a.z(a=0,Z) # p(A=1|Z)
  
  # Z density
  p.z1 <- density.z(1) # p(Z=1)
  p.z0 <- density.z(0) # p(Z=0)
  
  # \int p(M|A=a,Z)p(Z)dZ
  int.fM1.Z <- mean(p.m1.aZ) # \int p(M=1|A=a,Z)p(Z)dZ
  int.fM0.Z <- mean(p.m0.aZ) # \int p(M=0|A=a,Z)p(Z)dZ
  int.fM.Z <- M*int.fM1.Z + (1-M)*int.fM0.Z # \int p(M|A=a,Z)p(Z)dZ
  
  integrand.xi.m1 <- function(z){(E.y.maz(m=1,a=1,z)*density.a.z(a=1,z) + E.y.maz(m=1,0,z)*density.a.z(a=0,z))*tilde_pz(z)}
  integrand.xi.m0 <- function(z){(E.y.maz(m=0,a=1,z)*density.a.z(a=1,z) + E.y.maz(m=0,0,z)*density.a.z(a=0,z))*tilde_pz(z)}
  
  xi.m1.z_star <- integrate(integrand.xi.m1, lower=-Inf, upper=Inf)$value
  xi.m0.z_star <- integrate(integrand.xi.m0, lower=-Inf, upper=Inf)$value
  
  theta_z <- xi.m1.z_star*p.m1.aZ + xi.m0.z_star*p.m0.aZ
  
  psi <- mean(theta_z) # E[\theta(Z)]
  
  # EIF
  EIF.Y <- {tilde_pz(Z)/density.z(Z)}*{int.fM.Z/p.M.AZ}*(Y-mu)
  EIF.M <- I(A==a)/p.a.Z*(mu.a1m1*p.a1.Z+mu.a0m1*p.a0.Z-mu.a1m0*p.a1.Z-mu.a0m0*p.a0.Z)*(M-p.m1.aZ)
  EIF.A <- {tilde_pz(Z)/density.z(Z)}*mean((mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ-mu.a0m1*p.m1.aZ-mu.a0m0*p.m0.aZ))*(A-p.a1.Z)
  EIF.Z <- theta_z - mean(theta_z)
  
  EIF <- EIF.Y + EIF.M + EIF.A + EIF.Z
  
  psi <- mean(EIF.Y+EIF.M+EIF.A) +  mean(theta_z)
  
  # confidence interval
  lower.ci <- psi-1.96*sqrt(mean(EIF^2)/n)
  upper.ci <- psi+1.96*sqrt(mean(EIF^2)/n)
  
  out_allz <- list(EIF=EIF, estimated=psi, lower.ci=lower.ci, upper.ci=upper.ci)
  
  if(verbose){print("Z is continuous. Computing one-step estimators at given tilde{p}(Z)")}
  
  onestep.out <- list(out.all.z=out_allz)
  
  ##################################################################
  #################### TMLE estimator ##########################
  ##################################################################
  
  tmle.out <- onestep.out
  
  out <- list(Onestep=onestep.out, TMLE=tmle.out)
  
  return(out)
  
}
