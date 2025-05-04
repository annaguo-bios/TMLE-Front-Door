# true p(Z)
density.z <-function(z, parZ){
 dbinom(z, 1, parZ)
}

# true p(A|Z)
density.a.z <- function(parA,a,z){
  dbinom(a, 1, (parA[1] + parA[2]*z))
}

# true p(M|A,Z)
density.m.az <- function(parM,m,a,z){
  dbinom(m,1,plogis(parM[1] + parM[2]*a + parM[3]*z + parM[4]*a*z))
}

# E(Y|M,A,Z)
E.y.maz <- function(parY,parU,m,a,z){
  parY[1]*{parU[1] + parU[2]*a + parU[3]*z + parU[4]*a*z} + parY[2]*m
}

compute_psi_and_var <- function(a, parZ, parA, parU, parM, parY){
  
  f.EIF.binary <- function(a,z=NULL){ # 1. if z=null, then return estimand from the regular ID functional where z is not fixed, 2. if z is not null, then return the estimand from the ID functional where z is fixed
    
    # outcome regression E[Y|M,A,z]
    if(!is.null(z) && (z!='opt')){
    mu.a1m1_z <- E.y.maz(parY,parU,1,1,z) # A=1, M=1, Z=z
    mu.a1m0_z <- E.y.maz(parY,parU,0,1,z) # A=1, M=0, Z=z
    mu.a0m1_z <- E.y.maz(parY,parU,1,0,z) # A=0, M=1, Z=z
    mu.a0m0_z <- E.y.maz(parY,parU,0,0,z) # A=0, M=0, Z=z
    mu_z <- E.y.maz(parY,parU,M,A,z) # A=A, M=M, Z=z
    }
    
    mu <- E.y.maz(parY,parU,M,A,Z) # A=A, M=M, Z=Z
    mu.a1m1 <- E.y.maz(parY,parU,1,1,Z) # A=1, M=1, Z=Z
    mu.a1m0 <- E.y.maz(parY,parU,0,1,Z) # A=1, M=0, Z=Z
    mu.a0m1 <- E.y.maz(parY,parU,1,0,Z) # A=0, M=1, Z=Z
    mu.a0m0 <- E.y.maz(parY,parU,0,0,Z) # A=0, M=0, Z=Z
    
    # mediator density p(M|A=a,Z)
    p.m1.az1 <- density.m.az(parM,m=1,a,z=1) #p(M=1|a,Z=1)
    p.m1.az0 <- density.m.az(parM,m=1,a,z=0) #p(M=1|a,Z=0)
    p.m0.az1 <- density.m.az(parM,m=0,a,z=1) #p(M=0|a,Z=1)
    p.m0.az0 <- density.m.az(parM,m=0,a,z=0) #p(M=0|a,Z=0
    
    p.M.aZ <- density.m.az(parM,M,a,Z) # p(M|A=a,Z)
    p.M.AZ <- density.m.az(parM,M,A,Z) # p(M|A,Z)
    p.m1.aZ <- density.m.az(parM,m=1,a,Z) # p(M=1|A=a,Z)
    p.m0.aZ <- density.m.az(parM,m=0,a,Z) # p(M=0|A=a,Z)
    
    if(!is.null(z) && (z!='opt')){
      p.M.az <- density.m.az(parM,M,a,z) # p(M|A=a,Z=z)
      p.M.Az <- density.m.az(parM,M,A,z) # p(M|A,Z=z)
      } 
    p.M.az1 <- density.m.az(parM,M,a,z=1) # p(M|A=a,Z=1)
    p.M.az0 <- density.m.az(parM,M,a,z=0) # p(M|A=a,Z=0)
    
    # propensity score p(A|Z=z)
    if(!is.null(z) && (z!='opt')){
    p.a1.z <- density.a.z(parA,a=1,z) # p(a=1|Z=z)
    p.a0.z <- density.a.z(parA,a=0,z) # p(a=0|Z=z)
    }
    
    p.a.Z <- density.a.z(parA,a,Z) # p(A=a|Z)
    p.a1.Z <- density.a.z(parA,a=1,Z) # p(A=1|Z)
    p.a0.Z <- density.a.z(parA,a=0,Z) # p(A=1|Z)
    
    # Z density
    p.z1 <- density.z(1,parZ) # p(Z=1)
    p.z0 <- density.z(0,parZ) # p(Z=0)
    if(!is.null(z) && (z!='opt')){p.z <- density.z(z,parZ)} # p(Z=z)
    
    # int E(Y|M,A,z)p(M|A=a,Z)p(A|z)p(Z)dMdAdZ
    if (is.null(z)){
      
      psi <- E.y.maz(parY,parU,1,1,1)*p.m1.az1*density.a.z(parA,a=1,z=1)*p.z1+ # M=1,A=1, Z=1
        E.y.maz(parY,parU,1,0,1)*p.m1.az1*density.a.z(parA,a=0,z=1)*p.z1+ # M=1, A=0, Z=1
        E.y.maz(parY,parU,1,1,0)*p.m1.az0*density.a.z(parA,a=1,z=0)*p.z0+ # M=1, A=1, Z=0
        E.y.maz(parY,parU,1,0,0)*p.m1.az0*density.a.z(parA,a=0,z=0)*p.z0+ # M=1, A=0, Z=0
        E.y.maz(parY,parU,0,1,0)*p.m0.az0*density.a.z(parA,a=1,z=0)*p.z0+ # M=0, A=1, Z=0
        E.y.maz(parY,parU,0,0,1)*p.m0.az1*density.a.z(parA,a=0,z=1)*p.z1+ # M=0, A=0, Z=1
        E.y.maz(parY,parU,0,1,1)*p.m0.az1*density.a.z(parA,a=1,z=1)*p.z1+ # M=0, A=1, Z=1
        E.y.maz(parY,parU,0,0,0)*p.m0.az0*density.a.z(parA,a=0,z=0)*p.z0 # M=0, A=0, Z=0
      
      theta_z <- (mu.a1m1)*p.m1.aZ*p.a1.Z+ # M=1,A=1
        (mu.a0m1)*p.m1.aZ*p.a0.Z+ # M=1, A=0
        (mu.a1m0)*p.m0.aZ*p.a1.Z+ # M=0, A=1
        (mu.a0m0)*p.m0.aZ*p.a0.Z # M=0, A=0 
      
      # true EIF 
      EIF.Y <- {p.M.aZ/p.M.AZ}*(Y-mu)
      EIF.M <- I(A==a)/p.a.Z*(mu.a1m1*p.a1.Z+mu.a0m1*p.a0.Z-mu.a1m0*p.a1.Z-mu.a0m0*p.a0.Z)*(M-p.m1.aZ)
      EIF.A <- (mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ-mu.a0m1*p.m1.aZ-mu.a0m0*p.m0.aZ)*(A-p.a1.Z)
      EIF.Z <- theta_z - psi
      
      EIF <- EIF.Y+EIF.M+EIF.A+EIF.Z
      
      
    }else if (z=='opt'){
      
      psi <- E.y.maz(parY,parU,1,1,1)*p.m1.az1*density.a.z(parA,a=1,z=1)*p.z1+ # M=1,A=1, Z=1
        E.y.maz(parY,parU,1,0,1)*p.m1.az1*density.a.z(parA,a=0,z=1)*p.z1+ # M=1, A=0, Z=1
        E.y.maz(parY,parU,1,1,0)*p.m1.az0*density.a.z(parA,a=1,z=0)*p.z0+ # M=1, A=1, Z=0
        E.y.maz(parY,parU,1,0,0)*p.m1.az0*density.a.z(parA,a=0,z=0)*p.z0+ # M=1, A=0, Z=0
        E.y.maz(parY,parU,0,1,0)*p.m0.az0*density.a.z(parA,a=1,z=0)*p.z0+ # M=0, A=1, Z=0
        E.y.maz(parY,parU,0,0,1)*p.m0.az1*density.a.z(parA,a=0,z=1)*p.z1+ # M=0, A=0, Z=1
        E.y.maz(parY,parU,0,1,1)*p.m0.az1*density.a.z(parA,a=1,z=1)*p.z1+ # M=0, A=1, Z=1
        E.y.maz(parY,parU,0,0,0)*p.m0.az0*density.a.z(parA,a=0,z=0)*p.z0 # M=0, A=0, Z=0
      
      theta_z <- (mu.a1m1)*p.m1.aZ*p.a1.Z+ # M=1,A=1
        (mu.a0m1)*p.m1.aZ*p.a0.Z+ # M=1, A=0
        (mu.a1m0)*p.m0.aZ*p.a1.Z+ # M=0, A=1
        (mu.a0m0)*p.m0.aZ*p.a0.Z # M=0, A=0 
      
      # true EIF 
      EIF.Y <- {p.M.aZ/p.M.AZ}*(Y-mu)
      EIF.M <- I(A==a)/p.a.Z*(mu.a1m1*p.a1.Z+mu.a0m1*p.a0.Z-mu.a1m0*p.a1.Z-mu.a0m0*p.a0.Z)*(M-p.m1.aZ)
      # EIF.A <- (mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ-mu.a0m1*p.m1.aZ-mu.a0m0*p.m0.aZ)*(A-p.a1.Z)
      EIF.Z <- theta_z - psi
      
      # for EIF.A
      f.AZX <- (1-A)/p.a0.Z - A/p.a1.Z
      E.f2.ZX <- (1/p.a1.Z)^2*p.a1.Z + (1/p.a0.Z)^2*p.a0.Z
      eta.1Z <- mu.a1m1*p.m1.aZ+mu.a1m0*p.m0.aZ
      eta.0Z <- mu.a0m1*p.m1.aZ+mu.a0m0*p.m0.aZ
      
      EIF.A <- f.AZX/E.f2.ZX*{(eta.1Z-eta.0Z)*(-p.a0.Z-p.a1.Z)}
      
      EIF <- EIF.Y+EIF.M+EIF.A+EIF.Z
      
    }else{
      
    psi <- mu.a1m1_z*p.m1.az1*p.a1.z*p.z1+ # M=1,A=1, Z=1
      (mu.a0m1_z)*p.m1.az1*p.a0.z*p.z1+ # M=1, A=0, Z=1
      (mu.a1m1_z)*p.m1.az0*p.a1.z*p.z0+ # M=1, A=1, Z=0
      (mu.a0m1_z)*p.m1.az0*p.a0.z*p.z0+ # M=1, A=0, Z=0
      (mu.a1m0_z)*p.m0.az0*p.a1.z*p.z0+ # M=0, A=1, Z=0
      (mu.a0m0_z)*p.m0.az1*p.a0.z*p.z1+ # M=0, A=0, Z=1
      (mu.a1m0_z)*p.m0.az1*p.a1.z*p.z1+ # M=0, A=1, Z=1
      (mu.a0m0_z)*p.m0.az0*p.a0.z*p.z0 # M=0, A=0, Z=0
    
    theta_z <- (mu.a1m1_z)*p.m1.aZ*p.a1.z+ # M=1,A=1
      (mu.a0m1_z)*p.m1.aZ*p.a0.z+ # M=1, A=0
      (mu.a1m0_z)*p.m0.aZ*p.a1.z+ # M=0, A=1
      (mu.a0m0_z)*p.m0.aZ*p.a0.z # M=0, A=0 
    
    # true EIF
    EIF.Y <- (Z==z)*{p.M.az1*p.z1 + p.M.az0*p.z0}/{p.M.Az*p.z}*(Y-mu_z)
    EIF.M <- I(A==a)/p.a.Z*(mu.a1m1_z*p.a1.z+mu.a0m1_z*p.a0.z-mu.a1m0_z*p.a1.z-mu.a0m0_z*p.a0.z)*(M-p.m1.aZ)
    EIF.A <- (Z==z)/p.z*{(mu.a1m1_z*p.m1.az1+mu.a1m0_z*p.m0.az1-mu.a0m1_z*p.m1.az1-mu.a0m0_z*p.m0.az1)*p.z1+(mu.a1m1_z*p.m1.az0+mu.a1m0_z*p.m0.az0-mu.a0m1_z*p.m1.az0-mu.a0m0_z*p.m0.az0)*p.z0}*(A-p.a1.z)
    EIF.Z <- theta_z - psi
    
    EIF <- EIF.Y + EIF.M + EIF.A + EIF.Z
    
    }
    
    
    return(list(EIF=EIF, psi=psi))
    
  } # end of f.EIF.binary function
  
  ## calculate the truth under different levels of z
  out_z1 <- f.EIF.binary(a,1)
  out_z0 <- f.EIF.binary(a,0)
  out_allz <- f.EIF.binary(a)
  out_opt <- f.EIF.binary(a, "opt")
  
  
  estimated_psi_z1 <- out_z1$psi
  estimated_psi_z0 <- out_z0$psi
  estimated_psi_allz <- out_allz$psi
  estimated_psi_opt <- out_opt$psi
  
  
  # EIF under different Z
  EIF_z1 <- out_z1$EIF
  EIF_z0 <- out_z0$EIF
  EIF_allZ <- out_allz$EIF
  EIF_opt <- out_opt$EIF
  
  output <- list(true_psi = c(estimated_psi_z1, estimated_psi_z0, estimated_psi_allz, estimated_psi_opt),
                 EIF = cbind(EIF_z1, EIF_z0, EIF_allZ, EIF_opt))
  
  

}


compute_truth <- function(n){
  
  dat_output = generate_data(n)
  data = dat_output$data
  parA=dat_output$parA 
  parU=dat_output$parU
  parM=dat_output$parM
  parY=dat_output$parY
  parZ=dat_output$parZ
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parZ, parA, parU, parM, parY)
  E.Y1_z1 = E.Y1_out$true_psi[1]
  E.Y1_z0 = E.Y1_out$true_psi[2]
  E.Y1_allz = E.Y1_out$true_psi[3]
  E.Y1_opt = E.Y1_out$true_psi[4]

  EIF_Y1_z1 = E.Y1_out$EIF[,1]
  EIF_Y1_z0 = E.Y1_out$EIF[,2]
  EIF_Y1_allz = E.Y1_out$EIF[,3]
  EIF_Y1_opt = E.Y1_out$EIF[,4]
  
  VAR.Y1_z1 = mean(EIF_Y1_z1^2)
  VAR.Y1_z0 = mean(EIF_Y1_z0^2)
  VAR.Y1_allz = mean(EIF_Y1_allz^2)
  VAR.Y1_opt = mean(EIF_Y1_opt^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parZ, parA, parU, parM, parY)
  E.Y0_z1 = E.Y0_out$true_psi[1]
  E.Y0_z0 = E.Y0_out$true_psi[2]
  E.Y0_allz = E.Y0_out$true_psi[3]
  E.Y0_opt = E.Y0_out$true_psi[4]
  
  EIF_Y0_z1 = E.Y0_out$EIF[,1]
  EIF_Y0_z0 = E.Y0_out$EIF[,2]
  EIF_Y0_allz = E.Y0_out$EIF[,3]
  EIF_Y0_opt = E.Y0_out$EIF[,4]
  
  VAR.Y0_z1 = mean(EIF_Y0_z1^2)
  VAR.Y0_z0 = mean(EIF_Y0_z0^2)
  VAR.Y0_allz = mean(EIF_Y0_allz^2)
  VAR.Y0_opt = mean(EIF_Y0_opt^2)

  # ATE
  EIF_ATE_z1 = EIF_Y1_z1 - EIF_Y0_z1
  EIF_ATE_z0 = EIF_Y1_z0 - EIF_Y0_z0
  EIF_ATE_allz = EIF_Y1_allz - EIF_Y0_allz
  EIF_ATE_opt = EIF_Y1_opt - EIF_Y0_opt
  
  ATE_z1 = E.Y1_z1 - E.Y0_z1
  ATE_z0 = E.Y1_z0 - E.Y0_z0
  ATE_allz = E.Y1_allz - E.Y0_allz
  ATE_opt = E.Y1_opt - E.Y0_opt

  
  VAR.ATE_z1 = mean(EIF_ATE_z1^2)
  VAR.ATE_z0 = mean(EIF_ATE_z0^2)
  VAR.ATE_allz = mean(EIF_ATE_allz^2)
  VAR.ATE_opt = mean(EIF_ATE_opt^2)

  ####################################
  ### re weight psi(z=1) & psi(z=0) ###
  ####################################
  
  # optimize variance over z for ATE
  f.var.z <- function(z){
    
    EIF_ATE_z <- z*EIF_ATE_z1 + (1-z)*EIF_ATE_z0
    
    return(mean(EIF_ATE_z^2))
    
  }
  
  z.opt <- optimize(f.var.z, interval = c(0, 1))
  
  EIF_ATE_opt.linear <- z.opt$minimum*EIF_ATE_z1 + (1-z.opt$minimum)*EIF_ATE_z0
  
  ATE_opt.linear = z.opt$minimum*ATE_z1 + (1-z.opt$minimum)*ATE_z0
  VAR.ATE_opt.linear = mean(EIF_ATE_opt.linear^2)
  
  # optimize variance over z for E[Y(1)]
  f.var.z.Y1 <- function(z){
    
    EIF_Y1_z <- z*EIF_Y1_z1 + (1-z)*EIF_Y1_z0
    
    return(mean(EIF_Y1_z^2))
    
  }
  
  z.opt.Y1 <- optimize(f.var.z.Y1, interval = c(0, 1))
  
  EIF_Y1_opt.linear <- z.opt.Y1$minimum*EIF_Y1_z1 + (1-z.opt.Y1$minimum)*EIF_Y1_z0
  
  E.Y1_opt.linear = z.opt.Y1$minimum*E.Y1_z1 + (1-z.opt.Y1$minimum)*E.Y1_z0
  VAR.Y1_opt.linear = mean(EIF_Y1_opt.linear^2)
  
  # optimize variance over z for E[Y(0)]
  f.var.z.Y0 <- function(z){
    
    EIF_Y0_z <- z*EIF_Y0_z1 + (1-z)*EIF_Y0_z0
    
    return(mean(EIF_Y0_z^2))
    
  }
  
  z.opt.Y0 <- optimize(f.var.z.Y0, interval = c(0, 1))
  
  EIF_Y0_opt.linear <- z.opt.Y0$minimum*EIF_Y0_z1 + (1-z.opt.Y0$minimum)*EIF_Y0_z0
  
  E.Y0_opt.linear = z.opt.Y0$minimum*E.Y0_z1 + (1-z.opt.Y0$minimum)*E.Y0_z0
  VAR.Y0_opt.linear = mean(EIF_Y0_opt.linear^2)
  
  ##############################################
  ### re weight psi(z=1) & psi(z=0) & psi(Z) ###
  ##############################################
  
  # optimize variance over z for ATE
  f.var.z3 <- function(par){
    
    a <- par[1]
    b <- par[2]
    
    EIF_ATE_z <- a*EIF_ATE_allz + b*EIF_ATE_z1 + (1-a-b)*EIF_ATE_z0
    
    return(mean(EIF_ATE_z^2))
    
  }
  
  z.opt3 <- optim(par=c(0.5,0.5), fn=f.var.z3, method="L-BFGS-B", lower=c(0,0), upper=c(1,1))
  
  EIF_ATE_opt.linear3 <- z.opt3$par[1]*EIF_ATE_allz + z.opt3$par[2]*EIF_ATE_z1 + (1-z.opt3$par[1]-z.opt3$par[2])*EIF_ATE_z0
  
  ATE_opt.linear3 = z.opt3$par[1]*ATE_allz + z.opt3$par[2]*ATE_z1 + (1-z.opt3$par[1]-z.opt3$par[2])*ATE_z0
  VAR.ATE_opt.linear3 = mean(EIF_ATE_opt.linear3^2)
  
  # optimize variance over z for E[Y(1)]
  f.var.z3.Y1 <- function(par){
    
    a <- par[1]
    b <- par[2]
    
    EIF_Y1_z <- a*EIF_Y1_allz + b*EIF_Y1_z1 + (1-a-b)*EIF_Y1_z0
    
    return(mean(EIF_Y1_z^2))
    
  }
  
  z.opt3.Y1 <- optim(par=c(0.5,0.5), fn=f.var.z3.Y1, method="L-BFGS-B", lower=c(0,0), upper=c(1,1))
  
  EIF_Y1_opt.linear3 <- z.opt3.Y1$par[1]*EIF_Y1_allz + z.opt3.Y1$par[2]*EIF_Y1_z1 + (1-z.opt3.Y1$par[1]-z.opt3.Y1$par[2])*EIF_Y1_z0
  
  E.Y1_opt.linear3 = z.opt3.Y1$par[1]*E.Y1_allz + z.opt3.Y1$par[2]*E.Y1_z1 + (1-z.opt3.Y1$par[1]-z.opt3.Y1$par[2])*E.Y1_z0
  VAR.Y1_opt.linear3 = mean(EIF_Y1_opt.linear3^2)
  
  # optimize variance over z for E[Y(0)]
  f.var.z3.Y0 <- function(par){
    
    a <- par[1]
    b <- par[2]
    
    EIF_Y0_z <- a*EIF_Y0_allz + b*EIF_Y0_z1 + (1-a-b)*EIF_Y0_z0
    
    return(mean(EIF_Y0_z^2))
    
  }
  
  z.opt3.Y0 <- optim(par=c(0.5,0.5), fn=f.var.z3.Y0, method="L-BFGS-B", lower=c(0,0), upper=c(1,1))
  
  EIF_Y0_opt.linear3 <- z.opt3.Y0$par[1]*EIF_Y0_allz + z.opt3.Y0$par[2]*EIF_Y0_z1 + (1-z.opt3.Y0$par[1]-z.opt3.Y0$par[2])*EIF_Y0_z0
  
  E.Y0_opt.linear3 = z.opt3.Y0$par[1]*E.Y0_allz + z.opt3.Y0$par[2]*E.Y0_z1 + (1-z.opt3.Y0$par[1]-z.opt3.Y0$par[2])*E.Y0_z0
  VAR.Y0_opt.linear3 = mean(EIF_Y0_opt.linear3^2)
  
  
  return(list(E.Y1=c(E.Y1_z1, E.Y1_z0, E.Y1_allz, E.Y1_opt, E.Y1_opt.linear, E.Y1_opt.linear3),
              VAR.Y1=c(VAR.Y1_z1, VAR.Y1_z0, VAR.Y1_allz, VAR.Y1_opt, VAR.Y1_opt.linear, VAR.Y1_opt.linear3),
              EIF_Y1=cbind(EIF_Y1_z1, EIF_Y1_z0, EIF_Y1_allz, EIF_Y1_opt, EIF_Y1_opt.linear, EIF_Y1_opt.linear3),
              #
              E.Y0=c(E.Y0_z1, E.Y0_z0, E.Y0_allz, E.Y0_opt, E.Y0_opt.linear, E.Y0_opt.linear3),
              VAR.Y0=c(VAR.Y0_z1, VAR.Y0_z0, VAR.Y0_allz, VAR.Y0_opt, VAR.Y0_opt.linear, VAR.Y0_opt.linear3),
              EIF_Y0=cbind(EIF_Y0_z1, EIF_Y0_z0, EIF_Y0_allz, EIF_Y0_opt, EIF_Y0_opt.linear, EIF_Y0_opt.linear3),
              #
              ATE=c(ATE_z1, ATE_z0, ATE_allz, ATE_opt, ATE_opt.linear, ATE_opt.linear3),
              VAR.ATE=c(VAR.ATE_z1, VAR.ATE_z0, VAR.ATE_allz, VAR.ATE_opt, VAR.ATE_opt.linear, VAR.ATE_opt.linear3),
              EIF_ATE=cbind(EIF_ATE_z1, EIF_ATE_z0, EIF_ATE_allz, EIF_ATE_opt, EIF_ATE_opt.linear, EIF_ATE_opt.linear3),
              #
              opt.linear=z.opt,
              opt.Y1.linear=z.opt.Y1,
              opt.Y0.linear=z.opt.Y0,
              opt.linear3=z.opt3,
              opt.Y1.linear3=z.opt3.Y1,
              opt.Y0.linear3=z.opt3.Y0
              )
  )
}

