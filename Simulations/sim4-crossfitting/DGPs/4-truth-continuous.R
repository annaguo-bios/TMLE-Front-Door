# true p(A|x) = sum_{u} p(A|u,x)p(u)
density.a.x <- function(parA,a,x){
  dbinom(a, 1, plogis(parA[1] + rowSums(sweep(x, 2, parA[2:11], "*")) +  rowSums(sweep(x^2,2,parA[12:21],"*")) ))
}

# true p(M|A,X)
density.m.ax <- function(parM,m,a,x){
  dnorm(m, mean = {parM[1] + parM[2]*a + rowSums(sweep(x, 2, parM[3:12], "*")) + rowSums(diag(a)%*%sweep(x[,1:5],2,parM[13:17],"*")) + rowSums(sweep(x[,6:10]^2,2,parM[18:22],"*"))},sd.M)
}

# E(Y|M,A,X)
E.y.max <- function(parY,parU,m,a,x){
  parY[1]*{parU[1] + parU[2]*a + rowSums(sweep(x, 2, parU[3:12], "*")) + rowSums(diag(a)%*%sweep(x[,1:5],2,parU[13:17],"*"))} + 
    parY[2]*m + rowSums(sweep(x, 2, parY[3:12], "*")) + rowSums(diag(m)%*%sweep(x[,1:5],2,parY[13:17],"*")) + parY[18]*m^2 + rowSums(sweep(x[,6:10]^2,2,parY[19:23],"*"))
}

compute_psi_and_var <- function(a, parA, parU, parM, parY,n){
  
  # propensity A|X
  p.a.X <- density.a.x(parA,a,X) # p(a|X)
  p.a1.X <- density.a.x(parA,a=1,X) # p(A=1|X)
  
  # M|A,X
  p.m1.aX <- density.m.ax(parM,m=1,rep(a,nrow(X)),X) #p(M=1|a,X)
  p.M.aX <- density.m.ax(parM,M,rep(a,nrow(X)),X) #p(M|a,X)
  p.M.AX <- density.m.ax(parM,M,A,X)  #p(M|A,X)

  # outcome regression E[Y|M,A,X] 
  mu <- E.y.max(parY,parU,M,A,X)
  mu.a1 <- E.y.max(parY,parU,M,rep(1,nrow(X)),X)
  mu.a0 <- E.y.max(parY,parU,M,rep(0,nrow(X)),X)
  
  ## int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA
  
  # int E(Y|M,A=1,X)p(M|A=a,X)
  int.mu.M1 <- parY[1]*{parU[1] + parU[2]*1 + rowSums(sweep(X, 2, parU[3:12], "*")) + rowSums(diag(rep(1,n))%*%sweep(X[,1:5],2,parU[13:17],"*"))}+
    parY[2]*{parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*"))}+
    rowSums(sweep(X, 2, parY[3:12], "*"))+
    rowSums(diag(parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*")))%*%sweep(X[,1:5],2,parY[13:17],"*")) + 
    parY[18]*{sd.M^2+{parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*"))}^2} + 
    rowSums(sweep(X[,6:10]^2,2,parY[19:23],"*"))
    
  
  # int E(Y|M,A=0,X)p(M|A=a,X)
  int.mu.M0 <- parY[1]*{parU[1] + parU[2]*0 + rowSums(sweep(X, 2, parU[3:12], "*")) + rowSums(diag(rep(0,n))%*%sweep(X[,1:5],2,parU[13:17],"*"))}+
    parY[2]*{parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*"))}+
    rowSums(sweep(X, 2, parY[3:12], "*"))+
    rowSums(diag(parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*")))%*%sweep(X[,1:5],2,parY[13:17],"*")) + 
    parY[18]*{sd.M^2+{parM[1] + parM[2]*a + rowSums(sweep(X, 2, parM[3:12], "*")) + rowSums(diag(rep(a,n))%*%sweep(X[,1:5],2,parM[13:17],"*")) + rowSums(sweep(X[,6:10]^2,2,parM[18:22],"*"))}^2} + 
    rowSums(sweep(X[,6:10]^2,2,parY[19:23],"*"))
  
  
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
  list2env(dat_output,globalenv())
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


