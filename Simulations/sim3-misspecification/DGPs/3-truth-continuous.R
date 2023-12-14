# true p(A|X) = sum_{u} p(A|u,X)p(u)
density.a.x <- function(parA,a,x){
  dbinom(a, 1, plogis(parA[1] + parA[2]*x))
}

# true p(M|A,X)
density.m.ax <- function(parM,m,a,x,sd.M){
  dnorm(m,parM[1] + parM[2]*a + parM[3]*x + parM[4]*a*x,sd.M)
}

# E(Y|M,A,X)=parY[1]*E(U|A,X) + parY[2]*M + parY[3]*X + parY[4]*M*X
# =parY[1]*{parU[1] + parU[2]*A + parU[3]*X + parU[4]*A*X} + parY[2]*M + parY[3]*X + parY[4]*M*X
E.y.max <- function(parY,parU,m,a,x){
  parY[1]*{parU[1] + parU[2]*a + parU[3]*x + parU[4]*a*x} + parY[2]*m + parY[3]*x + parY[4]*m*x
}

compute_psi_and_var <- function(a, parA, parU, parM, parY, sd.M){
  
  # propensity A|X
  p.a.X <- density.a.x(parA,a,X) # p(a|X)
  p.a1.X <- density.a.x(parA,a=1,X) # p(A=1|X)
  
  # M|A,X
  p.M.aX <- density.m.ax(parM,M,a,X,sd.M) #p(M|a,X)
  p.M.AX <- density.m.ax(parM,M,A,X,sd.M)  #p(M|A,X)
  
  # outcome regression E[Y|M,A,X] 
  mu <- E.y.max(parY,parU,M,A,X)
  mu.a1 <- E.y.max(parY,parU,M,1,X)
  mu.a0 <- E.y.max(parY,parU,M,0,X)
  
  ## int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA
  # E(Y|M,A,X)p(M|A=a,X)
  mu.M <- function(m,x,A){E.y.max(parY,parU,m,A,x)*density.m.ax(parM,m,a,x,sd.M)}
  
  # int E(Y|M,A=1,X)p(M|A=a,X)
  int.mu.M1 <- vector(mode="numeric",length=length(A))
  for (i in seq_along(A)){
    integrand <- function(m){mu.M(m,X[i],1)}
    int.mu.M1[i] <- integrate(integrand, lower = -Inf, upper = Inf)$value
  }
  
  # int E(Y|M,A=0,X)p(M|A=a,X)
  int.mu.M0 <- vector(mode="numeric",length=length(A))
  for (i in seq_along(A)){
    integrand <- function(m){mu.M(m,X[i],0)}
    int.mu.M0[i] <- integrate(integrand, lower = -Inf, upper = Inf)$value
  }
  
  # int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA=int E(Y|M,A=1,X)p(M|A=a,X)dM*p(A=1|X)+int E(Y|M,A=0,X)p(M|A=a,X)dM*p(A=0|X)
  theta_x <- int.mu.M1*p.a1.X+int.mu.M0*(1-p.a1.X)
  
  # true_psi 
  true_psi = mean(theta_x)
  
  # true EIF 
  EIF <- {p.M.aX/p.M.AX}*(Y-mu)+ # line 1 of EIF
    I(A==a)/p.a.X*( # line 2 of EIF
      mu.a1*p.a1.X+
        mu.a0*(1-p.a1.X)-theta_x)+
    (A*int.mu.M1+(1-A)*int.mu.M0)-theta_x+ # line 3 of EIF
    theta_x - true_psi # line 4 of EIF
  
  return(list(true_psi=true_psi, 
              EIF=EIF))
}


compute_truth <- function(n){
  
  dat_output = generate_data(n)
  data = dat_output$data
  parA=dat_output$parA 
  parU=dat_output$parU
  parM=dat_output$parM
  parY=dat_output$parY
  sd.M=dat_output$sd.M
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU, parM, parY, sd.M)
  E.Y1 = E.Y1_out$true_psi
  EIF_Y1 = E.Y1_out$EIF
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU, parM, parY, sd.M)
  E.Y0 = E.Y0_out$true_psi
  EIF_Y0 = E.Y0_out$EIF
  VAR.Y0 = mean(EIF_Y0^2)
  
  # "ATE"
  ATE = E.Y1 - E.Y0
  EIF_ATE = EIF_Y1 - EIF_Y0 
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
              EIF_ATE=EIF_ATE))
}

