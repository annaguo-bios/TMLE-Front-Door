# true p(A|X) = sum_{u} p(A|u,X)p(u)
density.a.x <- function(parA,a,x){
  dbinom(a, 1, (parA[1] + parA[2]*x))
}

# true p(M|A,X)
density.m.ax <- function(parM,m,a,x){
  dmvnorm(m, mean = c(parM[1,1] + parM[1,2]*a + parM[1,3]*x + parM[1,4]*a*x, 
                      parM[2,1] + parM[2,2]*a + parM[2,3]*x + parM[2,4]*a*x), sigma = matrix(c(2, 1, 1, 3), nrow = 2))
}

# E(Y|M,A,X)
E.y.max <- function(parY,parU,m,a,x){ # takes m as a dataframe or matrix
  parY[1]*{parU[1] + parU[2]*a + parU[3]*x + parU[4]*a*x} + parY[2]*m[,1]+ parY[3]*m[,2] + parY[4]*x+parY[5]*m[,1]*x
}

E.y.max.single <- function(parY,parU,m,a,x){ # takes m as a length 2 vector
  parY[1]*{parU[1] + parU[2]*a + parU[3]*x + parU[4]*a*x} + parY[2]*m[1]+ parY[3]*m[2] + parY[4]*x+parY[5]*m[1]*x
}

compute_psi_and_var <- function(a, parA, parU, parM, parY,n){
  
  # propensity A|X
  p.a.X <- density.a.x(parA,a,X) # p(a|X)
  p.a1.X <- density.a.x(parA,a=1,X) # p(A=1|X)

  # M|A,X
  p.M.aX <- sapply(1:n, function(i){density.m.ax(parM,c(M.1[i],M.2[i]),a,X[i])}) #p(M|a,X)
  p.M.AX <- sapply(1:n, function(i){density.m.ax(parM,c(M.1[i],M.2[i]),A[i],X[i])})  #p(M|A,X)

  # outcome regression E[Y|M,A,X] 
  mu <- E.y.max(parY,parU,cbind(M.1,M.2),A,X)
  mu.a1 <- E.y.max(parY,parU,cbind(M.1,M.2),1,X)
  mu.a0 <- E.y.max(parY,parU,cbind(M.1,M.2),0,X)
  
  ## int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA
  
  # E(Y|M,A,X)p(M|A=a,X)
  mu.M <- function(m,x,A){E.y.max.single(parY,parU,m,A,x)*density.m.ax(parM,m,a,x)}
  
  # int E(Y|M,A=1,X)p(M|A=1,X)
  int.mu.M1 <- parY[1]*{parU[1] + parU[2]*1 + parU[3]*X + parU[4]*1*X} +
    parY[2]*(parM[1,1] + parM[1,2]*a + parM[1,3]*X + parM[1,4]*a*X)+
    parY[3]*(parM[2,1] + parM[2,2]*a + parM[2,3]*X + parM[2,4]*a*X) +
    parY[4]*X +
    parY[5]*(parM[1,1] + parM[1,2]*a + parM[1,3]*X + parM[1,4]*a*X)*X
  
  # int E(Y|M,A=0,X)p(M|A=0,X)
  int.mu.M0 <- parY[1]*{parU[1] + parU[2]*0 + parU[3]*X + parU[4]*0*X} +
    parY[2]*(parM[1,1] + parM[1,2]*a + parM[1,3]*X + parM[1,4]*a*X)+
    parY[3]*(parM[2,1] + parM[2,2]*a + parM[2,3]*X + parM[2,4]*a*X) +
    parY[4]*X +
    parY[5]*(parM[1,1] + parM[1,2]*a + parM[1,3]*X + parM[1,4]*a*X)*X
  
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
  parM=dat_output$parM
  parY=dat_output$parY
  parU=dat_output$parU
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)]
  E.Y1_out = compute_psi_and_var(a=1, parA, parU, parM, parY,n)
  E.Y1 = E.Y1_out$true_psi
  EIF_Y1 = E.Y1_out$EIF
  VAR.Y1 = mean(EIF_Y1^2)
  
  # E[Y(0)]
  E.Y0_out = compute_psi_and_var(a=0, parA, parU, parM, parY,n)
  E.Y0 = E.Y0_out$true_psi
  EIF_Y0 = E.Y0_out$EIF
  VAR.Y0 = mean(EIF_Y0^2)
  
  # ATE
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
              VAR.ATE=VAR.ATE))
}


