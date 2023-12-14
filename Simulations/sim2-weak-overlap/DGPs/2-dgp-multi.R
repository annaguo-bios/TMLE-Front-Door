########################################################
# M is now multivariate with parM be
#  1  1.0    1    0
# -1 -0.5    2    0
# and the mean of M be
# c(parM[1,1] + parM[1,2]*A + parM[1,3]*X + parM[1,4]*A*X, parM[2,1] + parM[2,2]*A + parM[2,3]*X + parM[2,4]*A*X)
# and the variance covariance matrix be
# 2 1
# 1 3
########################################################

generate_data <- function(n,parA = c(0.001,0.998), parU=c(1,1,1,0), parM = matrix(c(1, 1, 1, 0,-1,-0.5,2,0), nrow = 2,byrow = T), parY = c(1, 1, -0.5,1, 0), sd.U=1, sd.Y=1){
  
  X <- runif(n, 0, 1) # p(X)
  
  A <- rbinom(n, 1, (parA[1] + parA[2]*X)) # p(A|X)
  
  U <- parU[1] + parU[2]*A + parU[3]*X + parU[4]*A*X + rnorm(n,0,sd.U) # p(U|A,X)
  
  M <- cbind(parM[1,1] + parM[1,2]*A + parM[1,3]*X + parM[1,4]*A*X,
             parM[2,1] + parM[2,2]*A + parM[2,3]*X + parM[2,4]*A*X)+ mvrnorm(n , mu =c(0,0) , Sigma = matrix(c(2, 1, 1, 3), nrow = 2))
  
  Y <- parY[1]*U + parY[2]*M[,1]+ parY[3]*M[,2] + parY[4]*X + parY[5]*M[,1]*X + rnorm(n, 0, sd.Y) # p(Y|U,M,X)
  
  data <- data.frame(X=X, U=U, A=A, M=M, Y=Y, AX=A*X, MX=M*X)
  
  # propensity score
  ps <- A*(parA[1] + parA[2]*X)+(1-A)*(1-(parA[1] + parA[2]*X))
  
  # mediator density ratio: p(M|a,X)/p(M|A,X)
  f.m.ratio.a1 <- function(i){
    dmvnorm(x=M[i,], mean=cbind(parM[1,1] + parM[1,2]*1 + parM[1,3]*X[i] + parM[1,4]*1*X[i],
                                parM[2,1] + parM[2,2]*1 + parM[2,3]*X[i] + parM[2,4]*1*X[i]),sigma=matrix(c(2, 1, 1, 3), nrow = 2))/dmvnorm(x=c(0,0), mean=cbind(parM[1,1] + parM[1,2]*A[i] + parM[1,3]*X[i] + parM[1,4]*A[i]*X[i],
                                                                                                                                                                 parM[2,1] + parM[2,2]*A[i] + parM[2,3]*X[i] + parM[2,4]*A[i]*X[i]),sigma=matrix(c(2, 1, 1, 3), nrow = 2))
  }
  f.m.ratio.a0 <- function(i){
    dmvnorm(x=M[i,], mean=cbind(parM[1,1] + parM[1,2]*0 + parM[1,3]*X[i] + parM[1,4]*0*X[i],
                                parM[2,1] + parM[2,2]*0 + parM[2,3]*X[i] + parM[2,4]*0*X[i]),sigma=matrix(c(2, 1, 1, 3), nrow = 2))/dmvnorm(x=c(0,0), mean=cbind(parM[1,1] + parM[1,2]*A[i] + parM[1,3]*X[i] + parM[1,4]*A[i]*X[i],
                                                                                                                                                                 parM[2,1] + parM[2,2]*A[i] + parM[2,3]*X[i] + parM[2,4]*A[i]*X[i]),sigma=matrix(c(2, 1, 1, 3), nrow = 2))
  }
  
  m.ratio.a1 <- sapply(1:n, f.m.ratio.a1)
  m.ratio.a0 <- sapply(1:n, f.m.ratio.a0)
  
  return(list(data = data, 
              parA=parA, 
              parU=parU,
              parM=parM, 
              parY=parY,
              sd.U=sd.U,
              sd.Y=sd.Y,
              sigma.M=matrix(c(2, 1, 1, 3), nrow = 2),
              ps=ps,
              m.ratio.a1=m.ratio.a1,
              m.ratio.a0=m.ratio.a0))
}


