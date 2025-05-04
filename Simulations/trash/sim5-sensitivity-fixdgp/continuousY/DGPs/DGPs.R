## The DGPs ====
fd_admg1 <- function(num_samples) {
  
  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y
  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z,U1,U2
  betaZA <- 0.5
  alphaU1A <- 1
  alphaU2A <- -1
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A,Z
  betaZM <- 0.8
  betaAM <- 1
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A))
  
  # Generate Y with random coefficients for Y|M,U1,U2
  alphaU1Y <- -1
  alphaU2Y <- 0.7
  betaMY <- 2
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + rnorm(num_samples, 0, 1)
  
  # Return as data frame
  data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
}

# Violation of FD due to A<->M, M<->Y
nonfd_admg1 <- function(num_samples) {
  
  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of confounding between A-M; M-Y
  # 
  # Z->A->M->Y; Z->M; A<->Y; A<->M, M<->Y
  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate random coefficients for A | Z, U1, U2
  betaZA <- 0.5
  alphaU1A <- -0.7
  alphaU2A <- 1
  
  # Generate A
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate random coefficients for M | A, Z, U1, U2
  betaZM <- 0.6
  betaAM <- -1
  alphaU1M <- 1
  alphaU2M <- 0.5
  
  # Generate M
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A + alphaU1M * U1 - alphaU2M * U2))
  
  # Generate random coefficients for Y | M, U1, U2
  alphaU1Y <- 0.3
  alphaU2Y <- 1
  betaMY <- -1
  
  # Generate Y
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + rnorm(num_samples, 0, 1)
  
  # Return data as a data frame
  data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
}


# violation of FD due to A->Y
nonfd_admg2 <- function(num_samples) {
  
  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y; A->Y
  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z,U1,U2
  betaZA <- 1
  alphaU1A <- 0.5
  alphaU2A <- 2
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A,Z
  betaZM <- 3
  betaAM <- -1
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A))
  
  # Generate Y with random coefficients for Y|M,U1,U2
  alphaU1Y <- 3
  alphaU2Y <- -2
  betaMY <- 0.5
  betaAY <- 1
  betaMAY <- 0
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + betaAY*A + betaMAY*A*M +rnorm(num_samples, 0, 1)
  
  # Return as data frame
  data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
}

# violation of FD due to A->Y but with m distribution more balanced
nonfd_admg3 <- function(num_samples) {
  
  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y; A->Y
  
  # Generate Z
  Z <- rbinom(num_samples, 1, 0.5)
  
  # Generate Us
  U1 <- runif(num_samples, -1, 1)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z,U1,U2
  betaZA <- 1
  alphaU1A <- 0.5
  alphaU2A <- 2
  A <- rbinom(num_samples, 1, plogis(-0.5 + betaZA * Z - alphaU1A * U1 + alphaU2A * U2))
  
  # Generate M with random coefficients for M|A,Z
  betaZM <- 1
  betaAM <- 1
  M <- rbinom(num_samples, 1, plogis(-0.5 - betaZM * Z + betaAM * A))
  
  # Generate Y with random coefficients for Y|M,U1,U2
  alphaU1Y <- 3
  alphaU2Y <- -2
  betaMY <- 0.5
  betaAY <- 1
  betaMAY <- 0
  Y <- alphaU1Y * U1 + alphaU2Y * U2 - betaMY * M + betaAY*A + betaMAY*A*M +rnorm(num_samples, 0, 1)
  
  # Return as data frame
  data.frame(Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
}

