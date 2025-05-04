generate_data <- function(n,parA = c(0.3,0.2), parU=c(1,1,-0.2,0), parM = c(1,1,1,0), parY = c(1, 1, 1, 0), sd.M=1, sd.U=1, sd.Y=1){
  
  Z <- rbinom(n, 1, 0.2) # p(Z)
  
  A <- rbinom(n, 1, (parA[1] + parA[2]*Z)) # p(A|X)
  
  U <- parU[1] + parU[2]*A + parU[3]*Z + parU[4]*A*Z + rnorm(n,0,sd.U) # p(U|A,Z)
  
  M <- parM[1] + parM[2]*A + parM[3]*Z + parM[4]*A*Z + rnorm(n,0,sd.M) # p(M|A,Z)
  
  Y <- parY[1]*U + parY[2]*M + rnorm(n, 0, sd.Y) # p(Y|U,M)
  
  data <- data.frame(Z=Z, U=U, A=A, M=M, Y=Y)
  
  return(list(data = data, 
              parA=parA, 
              parU=parU,
              parM=parM, 
              parY=parY,
              sd.U=sd.U,
              sd.Y=sd.Y,
              sd.M=sd.M))
}

dt <- generate_data(1000)$data
