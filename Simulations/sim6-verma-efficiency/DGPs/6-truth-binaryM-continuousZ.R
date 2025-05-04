# true p(Z)
density.z <-function(z){
  dnorm(z,1,1)
}

# true p(A|Z)
density.a.z <- function(parA,a,z){
  dbinom(a, 1, plogis(parA[1] + parA[2]*z))
}

# true p(M|A,Z)
density.m.az <- function(parM,m,a,z){
  dbinom(m,1,plogis(parM[1] + parM[2]*a + parM[3]*z + parM[4]*a*z))
}

# E(Y|M,A,Z)
E.y.maz <- function(parY,parU,parA,m,a,z){
  parY[1]*{parU[1] + parU[2]*a + parU[3]*plogis(parA[1] + parA[2]*z) + parU[4]*a*z} + parY[2]*m
}

compute_psi_and_var <- function(a, parA, parU, parM, parY,tilde_pz, sample_pz){
    
    # outcome regression E[Y|M,A,z]
    mu <- E.y.maz(parY,parU,parA,M,A,Z) # A=A, M=M, Z=Z
    mu.a1m1 <- E.y.maz(parY,parU,parA,1,1,Z) # A=1, M=1, Z=Z
    mu.a1m0 <- E.y.maz(parY,parU,parA,0,1,Z) # A=1, M=0, Z=Z
    mu.a0m1 <- E.y.maz(parY,parU,parA,1,0,Z) # A=0, M=1, Z=Z
    mu.a0m0 <- E.y.maz(parY,parU,parA,0,0,Z) # A=0, M=0, Z=Z
    
    mu.a1m1_sampleZ <- E.y.maz(parY,parU,parA,1,1,sample_pz) # A=1, M=1, Z=sampleZ
    mu.a1m0_sampleZ <- E.y.maz(parY,parU,parA,0,1,sample_pz) # A=1, M=0, Z=sampleZ
    mu.a0m1_sampleZ <- E.y.maz(parY,parU,parA,1,0,sample_pz) # A=0, M=1, Z=sampleZ
    mu.a0m0_sampleZ <- E.y.maz(parY,parU,parA,0,0,sample_pz) # A=0, M=0, Z=sampleZ
    
    # mediator density p(M|A=a,Z)
    p.m1.az1 <- density.m.az(parM,m=1,a,z=1) #p(M=1|a,Z=1)
    p.m1.az0 <- density.m.az(parM,m=1,a,z=0) #p(M=1|a,Z=0)
    p.m0.az1 <- density.m.az(parM,m=0,a,z=1) #p(M=0|a,Z=1)
    p.m0.az0 <- density.m.az(parM,m=0,a,z=0) #p(M=0|a,Z=0
    
    p.M.aZ <- density.m.az(parM,M,a,Z) # p(M|A=a,Z)
    p.M.AZ <- density.m.az(parM,M,A,Z) # p(M|A,Z)
    p.m1.aZ <- density.m.az(parM,m=1,a,Z) # p(M=1|A=a,Z)
    p.m0.aZ <- density.m.az(parM,m=0,a,Z) # p(M=0|A=a,Z)
    
    p.M.az1 <- density.m.az(parM,M,a,z=1) # p(M|A=a,Z=1)
    p.M.az0 <- density.m.az(parM,M,a,z=0) # p(M|A=a,Z=0)
    
    # propensity score p(A|Z=z)
    p.a.Z <- density.a.z(parA,a,Z) # p(A=a|Z)
    p.a1.Z <- density.a.z(parA,a=1,Z) # p(A=1|Z)
    p.a0.Z <- density.a.z(parA,a=0,Z) # p(A=1|Z)
    
    p.a1_sampleZ <- density.a.z(parA,a=1,sample_pz) # p(A=1|Z=sampleZ)
    p.a0_sampleZ <- density.a.z(parA,a=0,sample_pz) # p(A=1|Z=sampleZ)
    
    # Z density
    p.z1 <- density.z(1) # p(Z=1)
    p.z0 <- density.z(0) # p(Z=0)
    
    # \int p(M|A=a,Z)p(Z)dZ
    int.fM1.Z <- mean(p.m1.aZ) # \int p(M=1|A=a,Z)p(Z)dZ
    int.fM0.Z <- mean(p.m0.aZ) # \int p(M=0|A=a,Z)p(Z)dZ
    int.fM.Z <- M*int.fM1.Z + (1-M)*int.fM0.Z # \int p(M|A=a,Z)p(Z)dZ
    
    xi_m1 <- mean((mu.a1m1_sampleZ*p.a1_sampleZ + mu.a0m1_sampleZ*p.a0_sampleZ)) # xi(m=1,tilde_pz)
    xi_m0 <- mean((mu.a1m0_sampleZ*p.a1_sampleZ + mu.a0m0_sampleZ*p.a0_sampleZ)) # xi(m=0,tilde_pz)
    
    
    theta_z <- xi_m1*p.m1.aZ + xi_m0*p.m0.aZ # \theta(Z)
    
    psi <- mean(theta_z) # E[\theta(Z)]
    
    # EIF
    EIF.Y <- {tilde_pz(Z)/density.z(Z)}*{int.fM.Z/p.M.AZ}*(Y-mu)
    EIF.M <- I(A==a)/p.a.Z*(xi_m1-xi_m0)*(M-p.m1.aZ)
    EIF.A <- {tilde_pz(Z)/density.z(Z)}*mean((mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ-mu.a0m1*p.m1.aZ-mu.a0m0*p.m0.aZ))*(A-p.a1.Z)
    EIF.Z <- theta_z - mean(theta_z)
    
    EIF <- EIF.Y + EIF.M + EIF.A + EIF.Z
    
    
    return(list(EIF=EIF, true_psi=psi))
  
}


compute_truth <- function(n, tilde_pz, sample_pz){
  
  dat_output = generate_data(n)
  data = dat_output$data
  parA=dat_output$parA 
  parU=dat_output$parU
  parM=dat_output$parM
  parY=dat_output$parY
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU, parM, parY,tilde_pz, sample_pz)
  E.Y1 = E.Y1_out$true_psi
  
  EIF_Y1 = E.Y1_out$EIF
  
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU, parM, parY,tilde_pz, sample_pz)
  E.Y0 = E.Y0_out$true_psi
  
  EIF_Y0 = E.Y0_out$EIF
  
  VAR.Y0 = mean(EIF_Y0^2)
  
  # ATE
  EIF_ATE = EIF_Y1 - EIF_Y0
  
  ATE = E.Y1 - E.Y0

  VAR.ATE = mean(EIF_ATE^2)
  
  return(list(E.Y1=E.Y1,
              VAR.Y1=VAR.Y1,
              EIF_Y1=EIF_Y1,
              #
              E.Y0=E.Y0,
              VAR.Y0=VAR.Y0,
              EIF_Y0=EIF_Y0,
              #
              ATE=ATE,
              VAR.ATE=VAR.ATE,
              EIF_ATE=EIF_ATE)
  )
}

