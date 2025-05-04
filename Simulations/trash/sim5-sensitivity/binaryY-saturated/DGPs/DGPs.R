fd_admg1 <- function(num_samples, lb=1, ub=2) {

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y
  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.7)
  
  # Generate A with random coefficients for A|Z, U1, U2
  betaZA <- runif(1, lb, ub)
  alphaU1A <- runif(1, lb, ub)
  alphaU1ZA <- runif(1, lb, ub)
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU1ZA*Z*U1))
  
  # Generate M with random coefficients for M|A, Z
  betaZM <- runif(1, lb, ub)
  betaAM <- runif(1, lb, ub)
  betaZAM <- runif(1, lb, ub)
  M <- rbinom(num_samples, 1, plogis(-1 + betaZM * Z + betaAM * A - betaZAM * Z * A))
  
  # Generate Y with random coefficients for Y|M, U1, U2
  alphaU1Y <- runif(1, lb, ub)
  alphaU1MY <- runif(1, lb, ub)
  betaMY <- runif(1, lb, ub)
  Y <- rbinom(num_samples, 1, plogis(-0.5 + alphaU1Y * U1 + alphaU1MY * U1*M - betaMY * M))
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}


fd_admg2 <- function(num_samples, lb=1, ub=2) {
  

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z<->M; A<->Y

  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate Z with random coeffs for Z|U3,U4
  alphaU2Z <- runif(1, lb, ub)
  Z <- rbinom(num_samples, 1, plogis(0.5 + alphaU2Z * U2))
  
  # Generate A with random coeffs for A|Z,U1,U2
  betaZA <- runif(1, lb, ub)
  alphaU1A <- runif(1, lb, ub)
  alphaU1ZA <- runif(1, lb, ub)
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z + alphaU1A * U1 - alphaU1ZA * U1*Z))
  
  # Generate M with random coeffs for M|A,U3,U4
  alphaU2M <- runif(1, lb, ub)
  alphaU2AM <- runif(1, lb, ub)
  betaAM <- runif(1, lb, ub)
  M <- rbinom(num_samples, 1, plogis(-0.5 - alphaU2M * U2 + alphaU2AM * U2*A + betaAM * A))
  
  # Generate Y with random coeffs for Y|M,U1,U2
  alphaU1Y <- runif(1, lb, ub)
  alphaU1MY <- runif(1, lb, ub)
  betaMY <- runif(1, lb, ub)
  Y <- rbinom(num_samples,1, plogis(-1 + alphaU1Y * U1 + alphaU1MY * U1*M - betaMY * M))
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


nonfd_admg1 <- function(num_samples, lb=1, ub=2) {
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of confounding between A-M; M-Y
  # 
  # Z->A->M->Y; Z->M; A<->Y; A<->M, M<->Y

  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1, U2
  betaZA <- runif(1, lb, ub)
  alphaU1A <- runif(1, lb, ub)
  alphaU1ZA <- runif(1, lb, ub)
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z + alphaU1A * U1 - alphaU1ZA * U1*Z))
  
  # Generate M with random coefficients for M|A, Z, U1, U2
  betaZM <- runif(1, lb, ub)
  betaAM <- runif(1, lb, ub)
  betaZAM <- runif(1, lb, ub)
  alphaU1M <- runif(1, lb, ub)
  alphaU1ZM <- runif(1, lb, ub)
  alphaU1AM <- runif(1, lb, ub)
  alphaU1ZAM <- runif(1, lb, ub)
  M <- rbinom(num_samples, 1, plogis(0.5 - betaZM * Z + betaAM * A - betaZAM*Z*A - alphaU1M * U1 + alphaU1ZM * U1*Z - alphaU1AM*U1*A + alphaU1ZAM*U1*Z*A))
  
  # Generate Y with random coefficients for Y|M, U1, U2
  alphaU1Y <- runif(1, lb, ub)
  alphaU2Y <- runif(1, lb, ub)
  betaMY <- runif(1, lb, ub)
  alphaU1U2Y <- runif(1, lb, ub)
  alphaU1MY <- runif(1, lb, ub)
  alphaU2MY <- runif(1, lb, ub)
  alphaU1U2MY <- runif(1, lb, ub)
  Y <- rbinom(num_samples, 1, plogis(alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + alphaU1U2Y*U1*U2 - alphaU1MY*U1*M - alphaU2MY*U2*M + alphaU1U2MY*U1*U2*M))
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


nonfd_admg2 <- function(num_samples, lb=1, ub=2) {
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of A->Y direct effect
  # 
  # Z->A->M->Y; Z->M; A<->Y; A->Y

  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1
  betaZA <- runif(1, lb, ub)
  alphaU1A <- runif(1, lb, ub)
  alphaU1ZA <- runif(1, lb, ub)
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z + alphaU1A * U1 - alphaU1ZA * U1*Z))
  
  # Generate M with random coefficients for M|A, Z
  betaZM <- runif(1, lb, ub)
  betaAM <- runif(1, lb, ub)
  betaZAM <- runif(1, lb, ub)
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A + betaZAM*Z*A))
  
  # Generate Y with random coefficients for Y|A, M, U1
  alphaU1Y <- runif(1, lb, ub)
  alphaU1MY <- runif(1, lb, ub)
  alphaU1AY <- runif(1, lb, ub)
  alphaU1AMY <- runif(1, lb, ub)
  betaMY <- runif(1, lb, ub)
  betaAY <- runif(1, lb, ub)
  betaAMY <- runif(1, lb, ub)
  
  Y <- rbinom(num_samples, 1, plogis(-0.5 + alphaU1Y * U1 - betaAY * A - betaMY * M + betaAMY*A*M - alphaU1AY*U1*A - alphaU1MY*U1*M + alphaU1AMY*U1*A*M))
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}

