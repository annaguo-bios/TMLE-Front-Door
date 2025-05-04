generate_data <- function(n,parA = c(0.3,0.2,0.2), parU=c(1,1,-0.2,0), parM = c(-1,1,1,0), parY = c(1, 1, 1, 0), sd.U=1, sd.Y=1){ # change the parM to c(-1,1,1,0)
  
  Z <- rbinom(n, 1, 0.2) # p(Z)
  
  U <- rbinom(n, 1, 0.5) # p(U|A,X)
  
  A <- rbinom(n, 1, parA[1] + parA[2]*Z+parA[3]*U) # p(A|X,U) => p(A=1|X)=0.3+0.2X+0.2*0.5
  
  M <- rbinom(n,1,plogis(parM[1] + parM[2]*A + parM[3]*Z + parM[4]*A*Z)) # p(M|A,X)
  
  Y <- parY[1]*U + parY[2]*M + parY[3]*Z + rnorm(n, 0, sd.Y) # p(Y|U,M) => E[Y|M,A,Z] = parY[1]*E[U|A,Z] + parY[2]*M + parY[3]*Z
                                                             #                        = parY[1]*{p(U=1)*p(A|Z,U=1)/[p(U=1)*p(A|Z,U=1)+p(U=0)*p(A|Z,U=0)]} + parY[2]*M + parY[3]*Z
                                                             #                        = parY[1]*{A*[(parA[1] + parA[2]*Z+parA[3]*1)/(parA[1] + parA[2]*Z+parA[3]*1+parA[1] + parA[2]*Z+parA[3]*0)] 
                                                             #                             + (1-A)*[(1-parA[1] - parA[2]*Z-parA[3]*1)/(2 - parA[1] - parA[2]*Z-parA[3]*1-parA[1] - parA[2]*Z-parA[3]*0)]} + parY[2]*M + parY[3]*Z
  
  data <- data.frame(Z=Z, U=U, A=A, M=M, Y=Y)
  
  
  return(list(data = data, 
              parA=parA, 
              parU=parU,
              parM=parM, 
              parY=parY,
              sd.U=sd.U,
              sd.Y=sd.Y))
}

data <- generate_data(1000)$data
attach(data,warn.conflicts = F)




###################################
# Nuisance Estimation
###################################

# p(Z)
density.z <-function(z){
  mean(Z==z)
}

density.z(1)
density.z(0)

# p(A|Z)
density.a.z <- function(a,z){
  
  p.a1.z1 <- mean(A[Z==1]==1)
  p.a1.z0 <- mean(A[Z==0]==1)
  
  out <- dbinom(a, 1, (z*p.a1.z1 + (1-z)*p.a1.z0))
  
  return(out)
}

density.a.z(1,1)
density.a.z(1,0)
density.a.z(0,1)
density.a.z(0,0)

# p(M|A,Z)
density.m.az <- function(m,a,z){
  
  p.m1.a1z1 <- mean(M[A==1 & Z==1]==1)
  p.m1.a1z0 <- mean(M[A==1 & Z==0]==1)
  p.m1.a0z1 <- mean(M[A==0 & Z==1]==1)
  p.m1.a0z0 <- mean(M[A==0 & Z==0]==1)
  
  out <- dbinom(m, 1, {a*z*p.m1.a1z1 + a*(1-z)*p.m1.a1z0 + (1-a)*z*p.m1.a0z1 + (1-z)*(1-a)*p.m1.a0z0})
  
  return(out)
  
}

density.m.az(1,1,1)
density.m.az(1,1,0)
density.m.az(1,0,1)
density.m.az(1,0,0)
density.m.az(0,1,1)
density.m.az(0,1,0)
density.m.az(0,0,1)
density.m.az(0,0,0)

# E(Y|M,A,Z)
E.y.maz <- function(m,a,z){
  
  Ey.m1a1z1 <- mean(Y[M==1 & A==1 & Z==1])
  Ey.m1a1z0 <- mean(Y[M==1 & A==1 & Z==0])
  Ey.m1a0z1 <- mean(Y[M==1 & A==0 & Z==1])
  Ey.m1a0z0 <- mean(Y[M==1 & A==0 & Z==0])
  
  Ey.m0a1z1 <- mean(Y[M==0 & A==1 & Z==1])
  Ey.m0a1z0 <- mean(Y[M==0 & A==1 & Z==0])
  Ey.m0a0z1 <- mean(Y[M==0 & A==0 & Z==1])
  Ey.m0a0z0 <- mean(Y[M==0 & A==0 & Z==0])
  
  out <- m*{a*z*Ey.m1a1z1 + a*(1-z)*Ey.m1a1z0 + (1-a)*z*Ey.m1a0z1 + (1-a)*(1-z)*Ey.m1a0z0} + (1-m)*{a*z*Ey.m0a1z1 + a*(1-z)*Ey.m0a1z0 + (1-a)*z*Ey.m0a0z1 + (1-a)*(1-z)*Ey.m0a0z0}
  
  return(out)
  
}

E.y.maz(1,1,1)
E.y.maz(1,1,0)
E.y.maz(1,0,1)
E.y.maz(1,0,0)
E.y.maz(0,1,1)
E.y.maz(0,1,0)
E.y.maz(0,0,1)
E.y.maz(0,0,0)

###################################
# Nuisance Truth
###################################

# true p(Z)
density.z <-function(z){
  dbinom(z, 1, 0.2)
}

density.z(1)
density.z(0)

# true p(A|Z)
density.a.z <- function(a,z){
  parA = c(0.3,0.2,0.2)
  dbinom(a, 1, (parA[1] + parA[2]*z+parA[3]*0.5))
}

density.a.z(1,1)
density.a.z(1,0)
density.a.z(0,1)
density.a.z(0,0)

# true p(M|A,Z)
density.m.az <- function(m,a,z){
  parM = c(-1,1,1,0)
  dbinom(m,1,plogis(parM[1] + parM[2]*a + parM[3]*z + parM[4]*a*z))
}

density.m.az(1,1,1)
density.m.az(1,1,0)
density.m.az(1,0,1)
density.m.az(1,0,0)
density.m.az(0,1,1)
density.m.az(0,1,0)
density.m.az(0,0,1)
density.m.az(0,0,0)

# E(Y|M,A,Z)
E.y.maz <- function(m,a,z){
  parY = c(1, 1, 1, 0)
  parU=c(1,1,-0.2,0)
  parA = c(0.3,0.2,0.2)
  
  p.a1.z.u0 <- parA[1] + parA[2]*z+parA[3]*0
  p.a1.z.u1 <- parA[1] + parA[2]*z+parA[3]*1
  
  E.U.AZ <- {a*p.a1.z.u1+(1-a)*(1-p.a1.z.u1)}/{a*(p.a1.z.u1+p.a1.z.u0)+(1-a)*(1-p.a1.z.u1+1-p.a1.z.u0)}
  
  parY[1]*E.U.AZ + parY[2]*m + parY[3]*z
}

E.y.maz(1,1,1)
E.y.maz(1,1,0)
E.y.maz(1,0,1)
E.y.maz(1,0,0)
E.y.maz(0,1,1)
E.y.maz(0,1,0)
E.y.maz(0,0,1)
E.y.maz(0,0,0)

EYa <- function(a,z=NULL){
  
  if(is.null(z)){
    
  res <- E.y.maz(1,1,1)*density.m.az(1,a,1)*density.a.z(1,1)*density.z(1) + # M=1,A=1,Z=1
    E.y.maz(1,1,0)*density.m.az(1,a,0)*density.a.z(1,0)*density.z(0) + # M=1,A=1,Z=0
    E.y.maz(1,0,1)*density.m.az(1,a,1)*density.a.z(0,1)*density.z(1) + # M=1,A=0,Z=1
    E.y.maz(1,0,0)*density.m.az(1,a,0)*density.a.z(0,0)*density.z(0) + # M=1,A=0,Z=0
    E.y.maz(0,1,1)*density.m.az(0,a,1)*density.a.z(1,1)*density.z(1) + # M=0,A=1,Z=1
    E.y.maz(0,1,0)*density.m.az(0,a,0)*density.a.z(1,0)*density.z(0) + # M=0,A=1,Z=0
    E.y.maz(0,0,1)*density.m.az(0,a,1)*density.a.z(0,1)*density.z(1) + # M=0,A=0,Z=1
    E.y.maz(0,0,0)*density.m.az(0,a,0)*density.a.z(0,0)*density.z(0) # M=0,A=0,Z=0
  
  }else{
    
  res <- E.y.maz(1,1,z)*density.m.az(1,a,1)*density.a.z(1,z)*density.z(1) + # M=1,A=1,Z=1
    E.y.maz(1,1,z)*density.m.az(1,a,0)*density.a.z(1,z)*density.z(0) + # M=1,A=1,Z=0
    E.y.maz(1,0,z)*density.m.az(1,a,1)*density.a.z(0,z)*density.z(1) + # M=1,A=0,Z=1
    E.y.maz(1,0,z)*density.m.az(1,a,0)*density.a.z(0,z)*density.z(0) + # M=1,A=0,Z=0
    E.y.maz(0,1,z)*density.m.az(0,a,1)*density.a.z(1,z)*density.z(1) + # M=0,A=1,Z=1
    E.y.maz(0,1,z)*density.m.az(0,a,0)*density.a.z(1,z)*density.z(0) + # M=0,A=1,Z=0
    E.y.maz(0,0,z)*density.m.az(0,a,1)*density.a.z(0,z)*density.z(1) + # M=0,A=0,Z=1
    E.y.maz(0,0,z)*density.m.az(0,a,0)*density.a.z(0,z)*density.z(0) # M=0,A=0,Z=0
  
  }
  
  return(res)
  
}

EYa(1,1) - EYa(0,1) # 2.046212-1.815153=0.2310586
EYa(1,0) - EYa(0,0) # 1.046212-0.8151531=0.2310586
EYa(1) - EYa(0) # 1.246212-1.015153=0.2310586

##################################################################
#################### One-step estimator ##########################
##################################################################

f.onestep.binaryZM <- function(a,z=NULL){ # 1. if z=null, then return estimand from the regular ID functional where z is not fixed, 2. if z is not null, then return the estimand from the ID functional where z is fixed
  
  # outcome regression E[Y|M,A,z]
  if (!is.null(z)){
    mu.a1m1_z <- E.y.maz(1,1,z) # A=1, M=1, Z=z
    mu.a1m0_z <- E.y.maz(0,1,z) # A=1, M=0, Z=z
    mu.a0m1_z <- E.y.maz(1,0,z) # A=0, M=1, Z=z
    mu.a0m0_z <- E.y.maz(0,0,z) # A=0, M=0, Z=z
    mu_z <- E.y.maz(M,A,z) # A=A, M=M, Z=z
  }
  
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
  if (!is.null(z)){
    p.M.az <- density.m.az(M,a,z) # p(M|A=a,Z=z)
    p.M.Az <- density.m.az(M,A,z) # p(M|A,Z=z)
  } 
  
  # propensity score p(A|Z=z)
  if (!is.null(z)){
    p.a1.z <- density.a.z(a=1,z) # p(a=1|Z=z)
    p.a0.z <- density.a.z(a=0,z) # p(a=0|Z=z)
  }
  
  p.a.Z <- density.a.z(a,Z) # p(A=a|Z)
  p.a1.Z <- density.a.z(a=1,Z) # p(A=1|Z)
  p.a0.Z <- density.a.z(a=0,Z) # p(A=1|Z)
  
  # Z density
  p.z1 <- density.z(1) # p(Z=1)
  p.z0 <- density.z(0) # p(Z=0)
  if (!is.null(z)){p.z <- density.z(z)} # p(Z=z)
  
  # int E(Y|M,A,z)p(M|A=a,Z)p(A|z)p(Z)dMdAdZ
  if (is.null(z)){
    
    theta_z <- (mu.a1m1)*p.m1.aZ*p.a1.Z+ # M=1,A=1
      (mu.a0m1)*p.m1.aZ*p.a0.Z+ # M=1, A=0
      (mu.a1m0)*p.m0.aZ*p.a1.Z+ # M=0, A=1
      (mu.a0m0)*p.m0.aZ*p.a0.Z # M=0, A=0 
    
    psi <- mean(theta_z)
    
    # true EIF 
    EIF.Y <- {p.M.aZ/p.M.AZ}*(Y-mu)
    EIF.M <- I(A==a)/p.a.Z*(mu.a1m1*p.a1.Z+mu.a0m1*p.a0.Z-mu.a1m0*p.a1.Z-mu.a0m0*p.a0.Z)*(M-p.m1.aZ)
    EIF.A <- (mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ-mu.a0m1*p.m1.aZ-mu.a0m0*p.m0.aZ)*(A-p.a1.Z)
    EIF.Z <- theta_z - psi
    
    EIF <- EIF.Y+EIF.M+EIF.A+EIF.Z
    
    psi <- mean(EIF.Y+EIF.M+EIF.A) + mean(theta_z)
    
    
  }else{
    
    theta_z <- (mu.a1m1_z)*p.m1.aZ*p.a1.z+ # M=1,A=1
      (mu.a0m1_z)*p.m1.aZ*p.a0.z+ # M=1, A=0
      (mu.a1m0_z)*p.m0.aZ*p.a1.z+ # M=0, A=1
      (mu.a0m0_z)*p.m0.aZ*p.a0.z # M=0, A=0 
    
    psi <- mean(theta_z)
    
    # true EIF
    EIF.Y <- I(Z==z)*{p.M.az1*p.z1 + p.M.az0*p.z0}/{p.M.Az*p.z}*(Y-mu_z)
    EIF.M <- I(A==a)/p.a.Z*(mu.a1m1_z*p.a1.z+mu.a0m1_z*p.a0.z-mu.a1m0_z*p.a1.z-mu.a0m0_z*p.a0.z)*(M-p.m1.aZ)
    EIF.A <- I(Z==z)/p.z*{(mu.a1m1_z*p.m1.az1+mu.a1m0_z*p.m0.az1-mu.a0m1_z*p.m1.az1-mu.a0m0_z*p.m0.az1)*p.z1+(mu.a1m1_z*p.m1.az0+mu.a1m0_z*p.m0.az0-mu.a0m1_z*p.m1.az0-mu.a0m0_z*p.m0.az0)*p.z0}*(A-p.a1.z)
    EIF.Z <- theta_z - psi
    
    EIF <- EIF.Y + EIF.M + EIF.A + EIF.Z
    
    psi <- mean(EIF.Y + EIF.M + EIF.A) + mean(theta_z)
    
  }
  
  # confidence interval
  lower.ci <- psi-1.96*sqrt(mean(EIF^2)/n)
  upper.ci <- psi+1.96*sqrt(mean(EIF^2)/n)
  
  
  return(list(EIF=EIF, estimated=psi, lower.ci=lower.ci, upper.ci=upper.ci))
  
} # end of f.EIF.binary function


###################################
# Multiple simulation
###################################
set.seed(7)

R=500
n.vec <- c(4000,8000)

est_matrix_ate_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_ate_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_ate_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

est_matrix_EY1_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_EY1_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_EY1_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

est_matrix_EY0_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_EY0_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_EY0_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

est_matrix_var_ate_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_ate_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_ate_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

est_matrix_var_EY1_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_EY1_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_EY1_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

est_matrix_var_EY0_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_EY0_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
est_matrix_var_EY0_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

mean_EIF_EY1_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
mean_EIF_EY1_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
mean_EIF_EY1_allz <- matrix(NA, nrow=R, ncol=length(n.vec))

mean_EIF_EY0_z1 <- matrix(NA, nrow=R, ncol=length(n.vec))
mean_EIF_EY0_z0 <- matrix(NA, nrow=R, ncol=length(n.vec))
mean_EIF_EY0_allz <- matrix(NA, nrow=R, ncol=length(n.vec))


for (i in seq_along(n.vec)){
  
  print(paste0("n=", n.vec[i]))
  
  for (j in 1:R){
    
    n <- n.vec[i]
    
    data <- generate_data(n)$data
    attach(data, warn.conflicts=FALSE)
    
    ### Calculate E[Y^1]
    ## calculate the truth under different levels of z
    out_z1.a1 <- f.onestep.binaryZM(a=1,1)
    out_z0.a1 <- f.onestep.binaryZM(a=1,0)
    out_allz.a1 <- f.onestep.binaryZM(a=1)
    
    estimated_psi_z1.a1 <- out_z1.a1$estimated
    estimated_psi_z0.a1 <- out_z0.a1$estimated
    estimated_psi_allz.a1 <- out_allz.a1$estimated
    
    # EIF under different Z
    EIF_z1.a1 <- out_z1.a1$EIF
    EIF_z0.a1 <- out_z0.a1$EIF
    EIF_allZ.a1 <- out_allz.a1$EIF
    
    ### Calculate E[Y^0]
    ## calculate the truth under different levels of z
    out_z1.a0 <- f.onestep.binaryZM(a=0,1)
    out_z0.a0 <- f.onestep.binaryZM(a=0,0)
    out_allz.a0 <- f.onestep.binaryZM(a=0)
    
    estimated_psi_z1.a0 <- out_z1.a0$estimated
    estimated_psi_z0.a0 <- out_z0.a0$estimated
    estimated_psi_allz.a0 <- out_allz.a0$estimated
    
    # EIF under different Z
    EIF_z1.a0 <- out_z1.a0$EIF
    EIF_z0.a0 <- out_z0.a0$EIF
    EIF_allZ.a0 <- out_allz.a0$EIF
    
    # output
    est_matrix_ate_z1[j,i] <- estimated_psi_z1.a1 - estimated_psi_z1.a0
    est_matrix_ate_z0[j,i] <- estimated_psi_z0.a1 - estimated_psi_z0.a0
    est_matrix_ate_allz[j,i] <- estimated_psi_allz.a1 - estimated_psi_allz.a0
    
    est_matrix_EY1_z1[j,i] <- estimated_psi_z1.a1
    est_matrix_EY1_z0[j,i] <- estimated_psi_z0.a1
    est_matrix_EY1_allz[j,i] <- estimated_psi_allz.a1
    
    est_matrix_EY0_z1[j,i] <- estimated_psi_z1.a0
    est_matrix_EY0_z0[j,i] <- estimated_psi_z0.a0
    est_matrix_EY0_allz[j,i] <- estimated_psi_allz.a0
    
    est_matrix_var_ate_z1[j,i] <- mean((EIF_z1.a1 - EIF_z1.a0)^2)
    est_matrix_var_ate_z0[j,i] <- mean((EIF_z0.a1 - EIF_z0.a0)^2)
    est_matrix_var_ate_allz[j,i] <- mean((EIF_allZ.a1 - EIF_allZ.a0)^2)
    
    est_matrix_var_EY1_z1[j,i] <- mean((EIF_z1.a1)^2)
    est_matrix_var_EY1_z0[j,i] <- mean((EIF_z0.a1)^2)
    est_matrix_var_EY1_allz[j,i] <- mean((EIF_allZ.a1)^2)
    
    est_matrix_var_EY0_z1[j,i] <- mean((EIF_z1.a0)^2)
    est_matrix_var_EY0_z0[j,i] <- mean((EIF_z0.a0)^2)
    est_matrix_var_EY0_allz[j,i] <- mean((EIF_allZ.a0)^2)
    
    mean_EIF_EY1_z1[j,i] <- mean(EIF_z1.a1)
    mean_EIF_EY1_z0[j,i] <- mean(EIF_z0.a1)
    mean_EIF_EY1_allz[j,i] <- mean(EIF_allZ.a1)
    
    mean_EIF_EY0_z1[j,i] <- mean(EIF_z1.a0)
    mean_EIF_EY0_z0[j,i] <- mean(EIF_z0.a0)
    mean_EIF_EY0_allz[j,i] <- mean(EIF_allZ.a0)
    
    
    
    
  }
  
}

###################################
## investigate the results
###################################
# look at ATE
mean_ate_z1 <- colMeans(est_matrix_ate_z1)
mean_ate_z0 <- colMeans(est_matrix_ate_z0)
mean_ate_allz <- colMeans(est_matrix_ate_allz)
mean_ate_z1; mean_ate_z0; mean_ate_allz

var_ate_z1 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_ate_z1[,i]))
var_ate_z0 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_ate_z0[,i]))
var_ate_allz <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_ate_allz[,i]))
var_ate_z1; var_ate_z0; var_ate_allz

expected_var_ate_z1 <- colMeans(est_matrix_var_ate_z1)
expected_var_ate_z0 <- colMeans(est_matrix_var_ate_z0)
expected_var_ate_allz <- colMeans(est_matrix_var_ate_allz)
expected_var_ate_z1; expected_var_ate_z0; expected_var_ate_allz

eif_ate_z1 <- colMeans(mean_EIF_EY1_z1 - mean_EIF_EY0_z1)
eif_ate_z0 <- colMeans(mean_EIF_EY1_z0 - mean_EIF_EY0_z0)
eif_ate_allz <- colMeans(mean_EIF_EY1_allz - mean_EIF_EY0_allz)
eif_ate_z1; eif_ate_z0; eif_ate_allz

# look at E[Y^1]
mean_EY1_z1 <- colMeans(est_matrix_EY1_z1)
mean_EY1_z0 <- colMeans(est_matrix_EY1_z0)
mean_EY1_allz <- colMeans(est_matrix_EY1_allz)
mean_EY1_z1; mean_EY1_z0; mean_EY1_allz

var_EY1_z1 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY1_z1[,i]))
var_EY1_z0 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY1_z0[,i]))
var_EY1_allz <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY1_allz[,i]))
var_EY1_z1; var_EY1_z0; var_EY1_allz

expected_var_EY1_z1 <- colMeans(est_matrix_var_EY1_z1)
expected_var_EY1_z0 <- colMeans(est_matrix_var_EY1_z0)
expected_var_EY1_allz <- colMeans(est_matrix_var_EY1_allz)
expected_var_EY1_z1; expected_var_EY1_z0; expected_var_EY1_allz

eif_EY1_z1 <- colMeans(mean_EIF_EY1_z1)
eif_EY1_z0 <- colMeans(mean_EIF_EY1_z0)
eif_EY1_allz <- colMeans(mean_EIF_EY1_allz)
eif_EY1_z1; eif_EY1_z0; eif_EY1_allz

# look at E[Y^0]
mean_EY0_z1 <- colMeans(est_matrix_EY0_z1)
mean_EY0_z0 <- colMeans(est_matrix_EY0_z0)
mean_EY0_allz <- colMeans(est_matrix_EY0_allz)
mean_EY0_z1; mean_EY0_z0; mean_EY0_allz

var_EY0_z1 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY0_z1[,i]))
var_EY0_z0 <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY0_z0[,i]))
var_EY0_allz <- sapply(1:length(n.vec), function(i) n.vec[i]*var(est_matrix_EY0_allz[,i]))
var_EY0_z1; var_EY0_z0; var_EY0_allz

expected_var_EY0_z1 <- colMeans(est_matrix_var_EY0_z1)
expected_var_EY0_z0 <- colMeans(est_matrix_var_EY0_z0)
expected_var_EY0_allz <- colMeans(est_matrix_var_EY0_allz)
expected_var_EY0_z1; expected_var_EY0_z0; expected_var_EY0_allz

eif_EY0_z1 <- colMeans(mean_EIF_EY0_z1)
eif_EY0_z0 <- colMeans(mean_EIF_EY0_z0)
eif_EY0_allz <- colMeans(mean_EIF_EY0_allz)
eif_EY0_z1; eif_EY0_z0; eif_EY0_allz

