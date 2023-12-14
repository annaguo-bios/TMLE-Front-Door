# true p(A|X) = sum_{u} p(A|u,X)p(u)
density.a.x <- function(parA,a,x){
  dbinom(a, 1, plogis(parA[1] + parA[2]*x))
}

# true p(M|A,X)
density.m.ax <- function(parM,m,a,x){
  dbinom(m,1,plogis(parM[1] + parM[2]*a + parM[3]*x + parM[4]*a*x))
}

# E(Y|M,A,X)=parY[1]*E(U|A,X) + parY[2]*M + parY[3]*X + parY[4]*M*X
# =parY[1]*{parU[1] + parU[2]*A + parU[3]*X + parU[4]*A*X} + parY[2]*M + parY[3]*X + parY[4]*M*X
E.y.max <- function(parY,parU,m,a,x){
  parY[1]*{parU[1] + parU[2]*a + parU[3]*x + parU[4]*a*x} + parY[2]*m + parY[3]*x + parY[4]*m*x
}

compute_psi_and_var <- function(a, parA, parU, parM, parY){
  
  # propensity A|X
  p.a.X <- density.a.x(parA,a,X) # p(a|X)
  p.a1.X <- density.a.x(parA,a=1,X) # p(A=1|X)
  
  # M|A,X
  p.m1.aX <- density.m.ax(parM,m=1,a,X) #p(M=1|a,X)
  p.M.aX <- density.m.ax(parM,M,a,X) #p(M|a,X)
  p.M.AX <- density.m.ax(parM,M,A,X)  #p(M|A,X)
  
  # outcome regression E[Y|M,A,X]
  mu <- E.y.max(parY,parU,M,A,X)
  mu.a1m1 <- E.y.max(parY,parU,1,1,X) # A=1, M=1
  mu.a1m0 <- E.y.max(parY,parU,0,1,X) # A=1, M=0
  mu.a0m1 <- E.y.max(parY,parU,1,0,X) # A=0, M=1
  mu.a0m0 <- E.y.max(parY,parU,0,0,X) # A=0, M=0
  
  # int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA
  theta_x <- (mu.a1m1)*p.m1.aX*p.a1.X+ # M=1,A=1
      (mu.a0m1)*p.m1.aX*(1-p.a1.X)+ # M=1, A=0
      (mu.a1m0)*(1-p.m1.aX)*p.a1.X+ # M=0, A=1
      (mu.a0m0)*(1-p.m1.aX)*(1-p.a1.X) # M=0, A=0 
  
  # true_psi 
  true_psi = mean(theta_x)
  
  # true EIF 
  EIF <- {p.M.aX/p.M.AX}*(Y-mu)+
    I(A==a)/p.a.X*(
      mu.a1m1*p.a1.X+
        mu.a0m1*(1-p.a1.X)-
        mu.a1m0*p.a1.X-
        mu.a0m0*(1-p.a1.X))*(M-p.m1.aX)+
    (mu.a1m1*p.m1.aX+
       mu.a1m0*(1-p.m1.aX)-
       mu.a0m1*p.m1.aX-
       mu.a0m0*(1-p.m1.aX))*(A-p.a1.X)+
    theta_x - true_psi
  
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
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU, parM, parY)
  E.Y1 = E.Y1_out$true_psi
  EIF_Y1 = E.Y1_out$EIF
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU, parM, parY)
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

