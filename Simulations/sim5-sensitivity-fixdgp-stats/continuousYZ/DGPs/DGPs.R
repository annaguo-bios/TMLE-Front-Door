fd_admg1 <- function(num_samples) {

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y
  
  # Generate Z
  Z <- rnorm(num_samples, 0, 1)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1, U2
  betaZA <- 1
  alphaU1A <- 1.2
  alphaU2A <- 2
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A, Z
  betaZM <- 1
  betaAM <- 1.5
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A))
  
  # Generate Y with random coefficients for Y|M, U1, U2
  alphaU1Y <- 1.5
  alphaU2Y <- 2
  betaMY <- 1.2
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + rnorm(num_samples, 0, 1)
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


fd_admg2 <- function(num_samples, lb=1, ub=2) {
  

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z<->M; A<->Y

  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  U3 <- runif(num_samples, -1, 1)
  U4 <- rbinom(num_samples, 1, 0.5)
  
  # Generate Z with random coeffs for Z|U3,U4
  alphaU3Z <- 1
  alphaU4Z <- 1.5
  Z <- 0.5 + alphaU3Z * U3 - alphaU4Z * U4 + rnorm(num_samples, 0, 1)
  
  # Generate A with random coeffs for A|Z,U1,U2
  betaZA <- 1
  alphaU1A <- 1.2
  alphaU2A <- 2
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coeffs for M|A,U3,U4
  alphaU3M <- 1
  alphaU4M <- 1.5
  betaAM <- 1.2
  M <- rbinom(num_samples, 1, plogis(-0.5 - alphaU3M * U3 + alphaU4M * U4 + betaAM * A))
  
  # Generate Y with random coeffs for Y|M,U1,U2
  alphaU1Y <- 1.5
  alphaU2Y <- 2
  betaMY <- 1.2
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + rnorm(num_samples, 0, 1)
  
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
  Z <- rnorm(num_samples, 0, 1)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1, U2
  betaZA <- 1
  alphaU1A <- 1.2
  alphaU2A <- 1.1
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A, Z, U1, U2
  betaZM <- 1
  betaAM <- 1.5
  alphaU1M <- 1.2
  alphaU2M <- 1
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A + alphaU1M * U1 - alphaU2M * U2))
  
  # Generate Y with random coefficients for Y|M, U1, U2
  alphaU1Y <- 1.5
  alphaU2Y <- 1.1
  betaMY <- 1.2
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + rnorm(num_samples, 0, 1)
  
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
  Z <- rnorm(num_samples, 0, 1)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1, U2
  betaZA <- 1.1
  alphaU1A <- 1.5
  alphaU2A <- 1
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A, Z
  betaZM <- 1
  betaAM <- 1.2
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A))
  
  # Generate Y with random coefficients for Y|A, M, U1, U2
  alphaU1Y <- 1
  alphaU2Y <- 1.1
  betaMY <- 1.2
  betaAY <- 1.5
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaAY * A - betaMY * M + rnorm(num_samples, 0, 1)
  
  # Return the generated data as a data frame
  data <- data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}

