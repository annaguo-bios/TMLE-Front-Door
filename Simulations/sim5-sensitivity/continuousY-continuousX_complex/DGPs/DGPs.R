fd_admg1 <- function(num_samples, sd.X=1,sd.Y=1) {

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z->M; A<->Y
  
  X <- rnorm(num_samples, 1, sd.X)
  
  # Generate Z
  Z <- rbinom(num_samples, 1, plogis(-0.5 + 0.5 * X))
  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.7)
  
  # Generate A with random coefficients for A|Z, U1,X
  Abeta_intercept <- -0.5
  Abeta_Z <- -1.1
  Abeta_U1 <- 1.3
  Abeta_X <- 0.5
  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_U1 * U1 + Abeta_X * X ))
  
  # Generate M with random coefficients for M|A, Z, X
  Mbeta_intercept <- -0.5
  Mbeta_A <- -1
  Mbeta_Z <- 1.1
  Mbeta_X <- -0.5
  Mbeta_X2 <- 0
  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X +  Mbeta_X2 * X^2))
  
  # Generate Y with random coefficients for Y|M, U1, X
  Ybeta_intercept <- -0.5
  Ybeta_M <- -0.5
  Ybeta_U1 <- 1
  Ybeta_X <- 0.5
  Ybeta_MU1 <- 1.2
  Ybeta_MX <- -1.5
  Ybeta_U1X <- 1.5
  Ybeta_MUX <- -1.7
  Ybeta_X2 <- 1.2
  
  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_U1 * U1 + Ybeta_X * X + Ybeta_X2*X^2 + Ybeta_MX*M*X + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}

# check_verma_fd_admg1 <- function(num_samples, sd.X=1,sd.Y=1) {
#   
#   # Check the Verma front door criterion for the generated data
#   data <- fd_admg1(num_samples, sd.X, sd.Y)
#   
#   est.z1 <- mean(EY.ma1z1X*p.a1.mz1X + EY.ma0z1X*p.a0.mz1X)
#   
#   return(list(z_a_m_indep = z_a_m_indep, m_y_a_indep = m_y_a_a_indep))
# }


fd_admg2 <- function(num_samples, sd.X=0.5,sd.Y=0.5) {
  

  # Generate data from an ADMG that satisfies the front door
  # criterion.
  # 
  # Z->A->M->Y; Z<->M; A<->Y

  
  # Generate Us
  U1 <- rbinom(num_samples, 1, 0.5)
  U2 <- rbinom(num_samples, 1, 0.5)
  
  # Generate X
  X <- rnorm(num_samples, 1, sd.X)
  
  # Generate Z with random coeffs for Z|X, U2
  Zbeta_intercept <- -0.5
  Zbeta_X <- 1
  Zbeta_U2 <- 1.5

  
  
  Z <- rbinom(num_samples, 1, plogis(Zbeta_intercept + Zbeta_X * X + Zbeta_U2 * U2 ))
  
  # Generate A with random coeffs for A|Z,X,U1
  Abeta_intercept <- -1
  Abeta_Z <- 1
  Abeta_X <- 1.5
  Abeta_U1 <- 1

  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_X * X + Abeta_U1 * U1))
  
  # Generate M with random coeffs for M|A,X,U2
  Mbeta_intercept <- -1
  Mbeta_A <- 1
  Mbeta_X <- 1.5
  Mbeta_U2 <- 1
  Mbeta_X2 <- 0

  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_X * X + Mbeta_X2*X^2+ Mbeta_U2 * U2 ))
  
  # Generate Y with random coeffs for Y|M,X,U1
  Ybeta_intercept <- -1
  Ybeta_M <- 0.2
  Ybeta_X <- 1.2
  Ybeta_U1 <- 1
  Ybeta_XU1 <- -1.5
  Ybeta_MX <- 1.5
  Ybeta_MU1 <- -1.5
  Ybeta_MU1X <- -1.7
  Ybeta_X2 <- 1.2

  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_X * X +Ybeta_X2*X^2 + Ybeta_MX*M*X + Ybeta_U1 * U1 + rnorm(num_samples, 0, sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


# Due to A<->M, M<->Y
nonfd_admg1 <- function(num_samples, sd.X=1,sd.Y=1) { 
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of confounding between A-M; M-Y
  # 
  # Z->A->M->Y; Z->M; A<->Y; A<->M, M<->Y

  
  # Generate X
  X <- rnorm(num_samples, 1, sd.X)
  
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
  Abeta_ZX <- 0
  Abeta_U1 <- 1
  Abeta_U2 <- 1
  
  
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_ZX*Z*X+Abeta_X * X + Abeta_U1 * U1 ))
  
  # Generate M with random coefficients for M|A, Z, X, U1
  Mbeta_intercept <- -1
  Mbeta_A <- 1
  Mbeta_Z <- 1.5
  Mbeta_X <- 1
  Mbeta_U1 <- 1 # <-
  Mbeta_U2 <- -1
  Mbeta_X2 <- 0.5
  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X+Mbeta_X2 * X^2 + Mbeta_U1 * U1 + Mbeta_U2*U2 ))
  
  # Generate Y with random coefficients for Y|M,X, U1, U2
  Ybeta_intercept <- -0.5
  Ybeta_M <- 0.5
  Ybeta_X <- 0.2
  Ybeta_U1 <- 1.2
  Ybeta_U2 <- -1.5
  Ybeta_MX <- 0
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
  Ybeta_X2 <- 1.2
  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_X * X +Ybeta_X2*X^2 + Ybeta_MX*M*X + Ybeta_U1 * U1 + Ybeta_U2 * U2 + rnorm(num_samples,0,sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X,Z = Z, A = A, M = M, Y = Y, U1 = U1, U2 = U2)
  return(data)
}


# Due to A->Y
nonfd_admg2 <- function(num_samples, sd.X=1,sd.Y=1) {
  

  # Generate data from an ADMG that does not satisfy the front door
  # criterion because of A->Y direct effect
  # 
  # Z->A->M->Y; Z->M; A<->Y; A->Y

  # Generate X
  X <- rnorm(num_samples, 1, sd.X)
  
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
 
  A <- rbinom(num_samples, 1, plogis(Abeta_intercept + Abeta_Z * Z + Abeta_U1 * U1 + Abeta_X * X))
  
  # Generate M with random coefficients for M|A, Z, X
  Mbeta_intercept <- -0.5
  Mbeta_A <- -1
  Mbeta_Z <- 1.1
  Mbeta_X <- -0.5
  Mbeta_AZ <- 1.25
  Mbeta_AX <- -1.5
  Mbeta_ZX <- 1.5
  Mbeta_AZX <- -1.7
  Mbeta_X2 <- 0.5
 
  
  M <- rbinom(num_samples, 1, plogis(Mbeta_intercept + Mbeta_A * A + Mbeta_Z * Z + Mbeta_X * X +Mbeta_X2 * X^2))
  
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
  Ybeta_X2 <- 1.2
  
  
  Y <- Ybeta_intercept + Ybeta_M * M + Ybeta_A * A + Ybeta_X * X+Ybeta_X2*X^2 + Ybeta_MX*M*X + Ybeta_U1 * U1 + rnorm(num_samples,0,sd.Y)
  
  # Return the generated data as a data frame
  data <- data.frame(X=X, Z = Z, A = A, M = M, Y = Y, U1 = U1)
  return(data)
}

