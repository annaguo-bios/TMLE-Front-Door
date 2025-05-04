# true p(A|x) = sum_{u} p(A|u,x)p(u)
density.a.x <- function(parA,a,x){
  dbinom(a, 1, plogis(parA[1] + rowSums(sweep(x, 2, parA[2:11], "*")) +  rowSums(sweep(x^2,2,parA[12:21],"*")) ))
}

# true p(M|A,X)
density.m.ax <- function(parM,m,a,x){
  # print(dim(diag(a)))
  # print(dim(sweep(x[,1:5],2,parM[13:17],"*")))
  dbinom(m,1,plogis(parM[1] + parM[2]*a + rowSums(sweep(x, 2, parM[3:12], "*")) + rowSums(a*sweep(x[,1:5],2,parM[13:17],"*")) + rowSums(sweep(x[,6:10]^2,2,parM[18:22],"*")) ))
}

# E(Y|M,A,X)
E.y.max <- function(parY,parU,m,a,x){
  parY[1]*{parU[1] + parU[2]*a + rowSums(sweep(x, 2, parU[3:12], "*")) + rowSums(a*sweep(x[,1:5],2,parU[13:17],"*"))} + 
    parY[2]*m + rowSums(sweep(x, 2, parY[3:12], "*")) + rowSums(m*sweep(x[,1:5],2,parY[13:17],"*")) + parY[18]*m^2 + rowSums(sweep(x[,6:10]^2,2,parY[19:23],"*"))
}

compute_psi_and_var <- function(a, parA, parU, parM, parY){
  
  alt <- 1-a
  # propensity A|X
  p.a.X <- density.a.x(parA,a,X) # p(a|X)
  p.a1.X <- density.a.x(parA,a=1,X) # p(A=1|X)
  p.alt.X <- density.a.x(parA,alt,X) # p(A=a'|X)
  
  # M|A,X
  p.m1.aX <- density.m.ax(parM,m=1,a,X) #p(M=1|a,X)
  p.M.aX <- density.m.ax(parM,M,a,X) #p(M|a,X)
  p.M.altX <- density.m.ax(parM,M,1-a,X)  #p(M|A,X)
  
  # outcome regression E[Y|M,A,X]
  mu.alt <- E.y.max(parY,parU,M,alt,X)
  mu.altm1 <- E.y.max(parY,parU,1,alt,X) # A=a', M=1
  mu.altm0 <- E.y.max(parY,parU,0,alt,X) # A=a', M=0
  
  # int E(Y|M,A,X)p(M|A=a,X)p(A|X)dMdA
  kappa <- (mu.altm1)*p.m1.aX+ # M=1
    (mu.altm0)*(1-p.m1.aX) # M=0
  
  # true_psi
  true_psi = mean((A==alt)/mean(A==alt)*kappa)
  
  # true EIF
  EIF <- (A==alt)/mean(A==alt)*{p.M.aX/p.M.altX}*(Y-mu.alt)+
    (A==a)/mean(A==alt)*{p.alt.X/p.a.X}*(mu.alt - kappa)+
    (A==alt)/mean(A==alt)*(kappa - true_psi)
  
  return(list(true_psi=true_psi,
              EIF=EIF))
}


compute_truth <- function(n){
  
  dat_output = generate_data(n)
  list2env(dat_output,globalenv())
  attach(data, warn.conflicts=FALSE)
  
  # E[Y(1)|1]
  E.Y1 = mean(Y[A==1])
  EIF_Y1 = (A==1)/mean(A==1)*(Y - E.Y1)
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

