fd_admg1 <- function(num_samples, sd.Y=0.5) {

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y
  
  X <- rbinom(num_samples, 1, 0.3)
  
  # Generate Z
  Z <- rbinom(num_samples, 1, plogis(-0.5 + 0.5 * X))
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.7)
  
  # Generate A with random coefficients for A|Z, U1,X
  Abeta_intercept <- -0.5
  Abeta_Z <- -1.1
  Abeta_U1 <- 1.3
  Abeta_X <- 0.5
  Abeta_U1Z <- -1.75
  Abeta_U1X <- 1.2
  Abeta_ZX <- 1.5
  Abeta_U1ZX <- -1.8

  
  # A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_U1 * U1 + Abeta_X * X - Abeta_U1Z * U1*Z - Abeta_U1X * U1*X - Abeta_ZX * Z*X + Abeta_U1ZX * U1*Z*X))
  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_U1 * U1 + Abeta_X * X ))
  
  # Generate M with random coefficients for M|A, Z, X
  Mbeta_intercept <- -0.5
  Mbeta_A <- -1
  Mbeta_Z <- 1.1
  Mbeta_X <- -0.5
  Mbeta_AZ <- 1.25
  Mbeta_AX <- -1.5
  Mbeta_ZX <- 1.5
  Mbeta_AZX <- -1.7
  
  
  
  # M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X - Mbeta_AZ * A*Z - Mbeta_AX * A*X - Mbeta_ZX * Z*X + Mbeta_AZX * A*Z*X))
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X ))
  
  # Generate Y with random coefficients for Y|M, U1, X
  Ybeta_intercept <- -0.5
  Ybeta_M <- -0.5
  Ybeta_U1 <- 1
  Ybeta_X <- 0.5
  Ybeta_MU1 <- 1.2
  Ybeta_MX <- -1.5
  Ybeta_U1X <- 1.5
  Ybeta_MUX <- -1.7
  
  
  # Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_U1 * U1 + Ybeta_X * X - Ybeta_MU1 * M*U1 - Ybeta_MX * M*X - Ybeta_U1X * U1*X + Ybeta_MUX * M*U1*X + rnorm(num_samples, 0, sd.Y)
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_U1 * U1 + Ybeta_X * X - Ybeta_MU1 * M*U1 + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}


fd_admg2 <- function(num_samples, sd.Y=1) {
  

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z<->M; A<->Y

  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate X
  X <- rbinom(num_samples, 1, 0.3)
  
  # Generate Z with random coeffs for Z|X, U2
  Zbeta_intercept <- -0.5
  Zbeta_X <- 1
  Zbeta_U2 <- 1.5
  Zbeta_XU2 <- -1.5

  
  
  Z <- rbinom(num_samples, 1, plogis(Zbeta_intercept + Zbeta_X * X + Zbeta_U2 * U2 - Zbeta_XU2 * X*U2))
  
  # Generate A with random coeffs for A|Z,X,U1
  Abeta_intercept <- -1
  Abeta_Z <- 1
  Abeta_X <- 1.5
  Abeta_U1 <- 1
  Abeta_ZX <- -1.5
  Abeta_U1Z <- 1.5
  Abeta_U1X <- -1.5
  Abeta_U1ZX <- -1.7

  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_X * X + Abeta_U1 * U1 - Abeta_ZX * Z*X - Abeta_U1Z * U1*Z - Abeta_U1X * U1*X + Abeta_U1ZX * U1*Z*X))
  
  # Generate M with random coeffs for M|A,X,U2
  Mbeta_intercept <- -1
  Mbeta_A <- 1
  Mbeta_X <- 1.5
  Mbeta_U2 <- 1
  Mbeta_AX <- -1.5
  Mbeta_AU2 <- 1.5
  Mbeta_U2X <- -1.5
  Mbeta_AU2X <- -1.7
  
  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_X * X + Mbeta_U2 * U2 - Mbeta_AX * A*X - Mbeta_AU2 * A*U2 - Mbeta_U2X * U2*X + Mbeta_AU2X * A*U2*X))
  
  # Generate Y with random coeffs for Y|M,X,U1
  Ybeta_intercept <- -1
  Ybeta_M <- 0.2
  Ybeta_X <- 1.2
  Ybeta_U1 <- 1
  Ybeta_XU1 <- -1.5
  Ybeta_MX <- 1.5
  Ybeta_MU1 <- -1.5
  Ybeta_MU1X <- -1.7

  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_X * X + Ybeta_U1 * U1 - Ybeta_XU1 * X*U1 - Ybeta_MX * M*X - Ybeta_MU1 * M*U1 + Ybeta_MU1X * M*U1*X + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


# Due to A<->M, M<->Y
nonfd_admg1 <- function(num_samples, sd.Y=1) { 
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of confounding between A-M; M-Y
  # 
  # Z->A->M->Y; Z->M; A<->Y; A<->M, M<->Y

  
  # Generate X
  X <- rbinom(num_samples, 1, 0.5)
  
  # Generate Z | X
  Zbeta_intercept <- -0.5
  Zbeta_X <- 0.5
  
  Z <- rbinom(num_samples, 1, plogis(Zbeta_intercept + Zbeta_X * X))
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, X, U1, U2
  Abeta_intercept <- -0.5
  Abeta_Z <- 1
  Abeta_X <- 1.5
  Abeta_U1 <- 1
  Abeta_U2 <- 1
  Abeta_ZX <- -1.5
  Abeta_ZU1 <- 1.5
  Abeta_ZU2 <- -1.5
  Abeta_XU1 <- -1.5
  Abeta_XU2 <- 1.5
  Abeta_U1U2 <- -1.5
  Abeta_ZXU1 <- -1.7
  Abeta_ZXU2 <- 1.2
  Abeta_ZU1U2 <- -1.7
  Abeta_XU1U2 <- -1.7
  Abeta_ZXU1U2 <- 1.4

  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_X * X + Abeta_U1 * U1 + Abeta_U2 * U2 + 
                                       Abeta_ZX * Z*X + Abeta_ZU1 * Z*U1 + Abeta_ZU2 * Z*U2 + Abeta_XU1 * X*U1 + Abeta_XU2 * X*U2 + Abeta_U1U2 * U1*U2 + 
                                       Abeta_ZXU1 * Z*X*U1 + Abeta_ZXU2 * Z*X*U2 + Abeta_ZU1U2 * Z*U1*U2 + Abeta_XU1U2 * X*U1*U2 + 
                                       Abeta_ZXU1U2 * Z*X*U1*U2))
  
  # Generate M with random coefficients for M|A, Z, X, U1
  Mbeta_intercept <- -1
  Mbeta_A <- 1
  Mbeta_Z <- 1.5
  Mbeta_X <- 1
  Mbeta_U1 <- 1
  Mbeta_AZ <- -1.5
  Mbeta_AX <- 1.5
  Mbeta_AU1 <- -1.5
  Mbeta_ZX <- -1.5
  Mbeta_ZU1 <- 1.5
  Mbeta_XU1 <- -1.5
  Mbeta_AZX <- -1.7
  Mbeta_AZU1 <- 1.2
  Mbeta_AXU1 <- -1.7
  Mbeta_ZXU1 <- -1.7
  Mbeta_AZXU1 <- 1.4

  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X + Mbeta_U1 * U1 + 
                                       Mbeta_AZ * A*Z + Mbeta_AX * A*X + Mbeta_AU1 * A*U1 + Mbeta_ZX * Z*X + Mbeta_ZU1 * Z*U1 + Mbeta_XU1 * X*U1 + 
                                       Mbeta_AZX * A*Z*X + Mbeta_AZU1 * A*Z*U1 + Mbeta_AXU1 * A*X*U1 + Mbeta_ZXU1 * Z*X*U1 +
                                       Mbeta_AZXU1 * A*Z*X*U1))
  
  # Generate Y with random coefficients for Y|M,X, U1, U2
  Ybeta_intercept <- -0.5
  Ybeta_M <- 0.5
  Ybeta_X <- 0.2
  Ybeta_U1 <- 1.2
  Ybeta_U2 <- -1.5
  Ybeta_MX <- -1
  Ybeta_MU1 <- -1.5
  Ybeta_MU2 <- 1
  Ybeta_XU1 <- 1.2
  Ybeta_XU2 <- 0.5
  Ybeta_U1U2 <- 1
  Ybeta_MXU1 <- 1.1
  Ybeta_MXU2 <- -0.75
  Ybeta_MU1U2 <- -1
  Ybeta_XU1U2 <- -0.2
  Ybeta_MXU1U2 <- 0.5

  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_X * X + Ybeta_U1 * U1 + Ybeta_U2 * U2 +
                                       Ybeta_MX * M*X + Ybeta_MU1 * M*U1 + Ybeta_MU2 * M*U2 + Ybeta_XU1 * X*U1 +Ybeta_XU2 * X*U2 + Ybeta_U1U2 * U1*U2 + 
                                       Ybeta_MXU1 * M*X*U1 + Ybeta_MXU2 * M*X*U2 + Ybeta_MU1U2 * M*U1*U2 + Ybeta_XU1U2 * X*U1*U2 +
                                       Ybeta_MXU1U2 * M*X*U1*U2 + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X,Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


# Due to A->Y
nonfd_admg2 <- function(num_samples, sd.Y=1) {
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of A->Y direct effect
  # 
  # Z->A->M->Y; Z->M; A<->Y; A->Y

  # Generate X
  X <- rbinom(num_samples, 1, 0.5)
  
  # Generate Z|X
  Zbeta_intercept <- -0.5
  Zbeta_X <- 0.5
  
  Z <- rbinom(num_samples, 1, plogis(Zbeta_intercept + Zbeta_X * X))
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  
  # Generate A with random coefficients for A|Z, U1
  Abeta_intercept <- -0.5
  Abeta_Z <- -1.1
  Abeta_U1 <- 1.3
  Abeta_X <- 0.5
  Abeta_U1Z <- -1.75
  Abeta_U1X <- 1.2
  Abeta_ZX <- 1.5
  Abeta_U1ZX <- -1.8

  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_U1 * U1 + Abeta_X * X - Abeta_U1Z * U1*Z - Abeta_U1X * U1*X - Abeta_ZX * Z*X + Abeta_U1ZX * U1*Z*X))
  
  # Generate M with random coefficients for M|A, Z, X
  Mbeta_intercept <- -0.5
  Mbeta_A <- -1
  Mbeta_Z <- 1.1
  Mbeta_X <- -0.5
  Mbeta_AZ <- 1.25
  Mbeta_AX <- -1.5
  Mbeta_ZX <- 1.5
  Mbeta_AZX <- -1.7

  
  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X - Mbeta_AZ * A*Z - Mbeta_AX * A*X - Mbeta_ZX * Z*X + Mbeta_AZX * A*Z*X))
  
  # Generate Y with random coefficients for Y|M,A,X, U1
  Ybeta_intercept <- -1
  Ybeta_M <- -0.2
  Ybeta_A <- 1.5
  Ybeta_X <- 0.5
  Ybeta_U1 <- 0.2
  Ybeta_MA <- -1.2
  Ybeta_MX <- 0.5
  Ybeta_MU1 <- 0.3
  Ybeta_AX <- -1
  Ybeta_AU1 <- 0.5
  Ybeta_XU1 <- -0.5
  Ybeta_MAX <- 0.5
  Ybeta_MAU1 <- -0.5
  Ybeta_MXU1 <- 0.2
  Ybeta_AXU1 <- -0.5
  Ybeta_MAXU1 <- 1

  
  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_A * A + Ybeta_X * X + Ybeta_U1 * U1 +
                                       Ybeta_MA * M*A + Ybeta_MX * M*X + Ybeta_MU1 * M*U1 + Ybeta_AX * A*X + Ybeta_AU1 * A*U1 + Ybeta_XU1 * X*U1 + 
                                       Ybeta_MAX * M*A*X + Ybeta_MAU1 * M*A*U1 + Ybeta_MXU1 * M*X*U1 + Ybeta_AXU1 * A*X*U1 + 
                                       Ybeta_MAXU1 * M*A*X*U1 + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}

