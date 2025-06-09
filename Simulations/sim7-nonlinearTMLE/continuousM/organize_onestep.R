args = commandArgs(trailingOnly=T)
out.path=args[1] # require specifying output location, e.g. out.path <- "/output/"
out.name=args[2] # require specifying output file name, e.g. out.name <- "continuous_dat.Rdata"
n.vec.ind=args[3] # sample size indicator, 1: the long sample size vector, 0: the short sample size vector
nsim=as.integer(args[4]) # require specifying number of simulations, e.g. nsim <- 1000
truth=args[5] # path+name for the truth.Rdata


# out.path='Onestep-est1/output/'
# out.name='Onestep-est1/result.Rdata'
# n.vec.ind='3'
# nsim=1000
# truth='../DGPs/1-truth-binary.Rdata'

# Set optional argument with default value
ATT.arg = ifelse(length(args) >= 6, as.logical(args[6]), FALSE)

# Example: Print values
cat("ATT Argument (default FALSE):", ATT.arg, "\n")

# prefix depending on ATT
if (ATT.arg){prefix = "ATT_"} else {prefix = ""}


library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)

if (n.vec.ind=="1"){
  n.vec <- c(250,500,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
} else if (n.vec.ind=="0"){
  n.vec <- c(250,500,1000,2000,4000)
} else if (n.vec.ind=="2"){
  n.vec <- c(500,1000,2000,4000)
} else if (n.vec.ind=="3"){
  n.vec <- c(500,1000,2000)
} else if (n.vec.ind=="4"){
  n.vec <- c(250,500,1000,2000,4000,8000)
}else {
  n.vec <- c(as.integer(n.vec.ind))
}


#################################################
# Load truth
#################################################
load(truth)


############################
# Organize results
#
############################

# record results
bias_matrix_Y1 <- matrix(nrow = nsim, ncol = length(n.vec))
bias_matrix_Y0 <- matrix(nrow = nsim, ncol = length(n.vec))
bias_matrix_ate <- matrix(nrow = nsim, ncol = length(n.vec))

est_matrix_Y1 <- matrix(nrow = nsim, ncol = length(n.vec))
est_matrix_Y0 <- matrix(nrow = nsim, ncol = length(n.vec))
est_matrix_ate <- matrix(nrow = nsim, ncol = length(n.vec))

ci_matrix_Y1_lower <- matrix(nrow = nsim, ncol = length(n.vec))
ci_matrix_Y0_lower <- matrix(nrow = nsim, ncol = length(n.vec))
ci_matrix_ate_lower <- matrix(nrow = nsim, ncol = length(n.vec))

ci_matrix_Y1_upper <- matrix(nrow = nsim, ncol = length(n.vec))
ci_matrix_Y0_upper <- matrix(nrow = nsim, ncol = length(n.vec))
ci_matrix_ate_upper <- matrix(nrow = nsim, ncol = length(n.vec))

EIF_matrix_Y1 <- matrix(nrow = nsim, ncol = length(n.vec))
EIF_matrix_Y0 <- matrix(nrow = nsim, ncol = length(n.vec))


# 95% CI coverage

ci_coverage_Y1 <- data.frame(n=n.vec, # sample size
                             coverage=vector(mode="integer",length=length(n.vec)), # 95% CI coverage
                             coverage.lower=vector(mode="integer",length=length(n.vec)),coverage.upper=vector(mode="integer",length=length(n.vec))) # 95% CI for the above coverage

ci_coverage_Y0 <- data.frame(n=n.vec, # sample size
                             coverage=vector(mode="integer",length=length(n.vec)), # 95% CI coverage
                             coverage.lower=vector(mode="integer",length=length(n.vec)),coverage.upper=vector(mode="integer",length=length(n.vec))) # 95% CI for the above coverage

ci_coverage_ATE <- data.frame(n=n.vec, # sample size
                              coverage=vector(mode="integer",length=length(n.vec)), # 95% CI coverage
                              coverage.lower=vector(mode="integer",length=length(n.vec)),coverage.upper=vector(mode="integer",length=length(n.vec))) # 95% CI for the above coverage
# average point estimate

avg.Y1 <- data.frame(n=n.vec, # sample size
                     est=vector(mode="integer",length=length(n.vec)), # E(phi_hat)
                     sd=vector(mode="integer",length=length(n.vec)), # sd
                     upper=vector(mode="integer",length=length(n.vec)), # E(phi_hat)-1.96*sd
                     lower=vector(mode="integer",length=length(n.vec))) # E(phi_hat)+1.96*sd
avg.Y0 <- data.frame(n=n.vec, # sample size
                     est=vector(mode="integer",length=length(n.vec)), # E(phi_hat)
                     sd=vector(mode="integer",length=length(n.vec)), # sd
                     upper=vector(mode="integer",length=length(n.vec)), # E(phi_hat)-1.96*sd
                     lower=vector(mode="integer",length=length(n.vec))) # E(phi_hat)+1.96*sd
avg.ate <- data.frame(n=n.vec, # sample size
                      est=vector(mode="integer",length=length(n.vec)), # E(phi_hat)
                      sd=vector(mode="integer",length=length(n.vec)), # sd
                      upper=vector(mode="integer",length=length(n.vec)), # E(phi_hat)-1.96*sd
                      lower=vector(mode="integer",length=length(n.vec))) # E(phi_hat)+1.96*sd

avg.bias_Y1 <- data.frame(n=n.vec, # sample size
                          bias=vector(mode="integer",length=length(n.vec)), # sqrt(n)*E{E(phi_hat-phi_0)}
                          upper=vector(mode="integer",length=length(n.vec)), # bias-1.96*sd
                          lower=vector(mode="integer",length=length(n.vec))) # bias+1.96*sd
avg.bias_Y0 <- data.frame(n=n.vec, # sample size
                          bias=vector(mode="integer",length=length(n.vec)), # sqrt(n)*E{E(phi_hat-phi_0)}
                          upper=vector(mode="integer",length=length(n.vec)), # bias-1.96*sd
                          lower=vector(mode="integer",length=length(n.vec))) # bias+1.96*sd
avg.bias_ate <- data.frame(n=n.vec, # sample size
                           bias=vector(mode="integer",length=length(n.vec)), # sqrt(n)*E{E(phi_hat-phi_0)}
                           upper=vector(mode="integer",length=length(n.vec)), # bias-1.96*sd
                           lower=vector(mode="integer",length=length(n.vec))) # bias+1.96*sd

avg.variance_Y1 <- data.frame(n=n.vec, # sample size
                              variance=vector(mode="integer",length=length(n.vec)), # n*E{E(phi_hat-E(phi_hat))^2}
                              upper=vector(mode="integer",length=length(n.vec)), # var-1.96*sd
                              lower=vector(mode="integer",length=length(n.vec))) # var+1.96*sd
avg.variance_Y0 <- data.frame(n=n.vec, # sample size
                              variance=vector(mode="integer",length=length(n.vec)), # n*E{E(phi_hat-E(phi_hat))^2}
                              upper=vector(mode="integer",length=length(n.vec)), # var-1.96*sd
                              lower=vector(mode="integer",length=length(n.vec))) # var+1.96*sd
avg.variance_ate <- data.frame(n=n.vec, # sample size
                               variance=vector(mode="integer",length=length(n.vec)), # n*E{E(phi_hat-E(phi_hat))^2}
                               upper=vector(mode="integer",length=length(n.vec)), # var-1.96*sd
                               lower=vector(mode="integer",length=length(n.vec))) # var+1.96*sd


avg.MSE_Y1 <- data.frame(n=n.vec,  # sample size
                         mse=vector(mode="integer",length=length(n.vec)), # n*E(phi_hat-phi_0)^2
                         upper=vector(mode="integer",length=length(n.vec)), # mse-1.96*sd
                         lower=vector(mode="integer",length=length(n.vec))) # mse+1.96*sd
avg.MSE_Y0 <- data.frame(n=n.vec,  # sample size
                         mse=vector(mode="integer",length=length(n.vec)), # n*E(phi_hat-phi_0)^2
                         upper=vector(mode="integer",length=length(n.vec)), # mse-1.96*sd
                         lower=vector(mode="integer",length=length(n.vec))) # mse+1.96*sd
avg.MSE_ate <- data.frame(n=n.vec,  # sample size
                          mse=vector(mode="integer",length=length(n.vec)), # n*E(phi_hat-phi_0)^2
                          upper=vector(mode="integer",length=length(n.vec)), # mse-1.96*sd
                          lower=vector(mode="integer",length=length(n.vec))) # mse+1.96*sd


for (i in seq_along(n.vec)){

  # sample size
  n <- n.vec[i]

  for (t in 1:nsim){

    load(paste0(out.path,prefix,"output_",n,"_",t,".Rdata"))
    
    if(exists('tmle_output_Y1.T')){tmle_output_Y1 <- tmle_output_Y1.T}
    if(exists('tmle_output_Y1.F')){tmle_output_Y1 <- tmle_output_Y1.F}
    if(exists('tmle_output_Y0.T')){tmle_output_Y0 <- tmle_output_Y0.T}
    if(exists('tmle_output_Y0.F')){tmle_output_Y0 <- tmle_output_Y0.F}

    # record bias
    bias_matrix_Y1[t,i] <- hat_E.Y1 - E.Y1
    bias_matrix_Y0[t,i] <- hat_E.Y0 - E.Y0
    bias_matrix_ate[t,i] <- hat_ATE - ATE

    # record point estimate
    est_matrix_Y1[t,i] <- hat_E.Y1
    est_matrix_Y0[t,i] <- hat_E.Y0
    est_matrix_ate[t,i] <- hat_ATE

    # record lower CI
    ci_matrix_Y1_lower[t,i] <- lower.ci_Y1
    ci_matrix_Y0_lower[t,i] <- lower.ci_Y0
    ci_matrix_ate_lower[t,i] <- lower.ci_ATE
    # ci_matrix_Y1_lower[t,i] <- hat_E.Y1-1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
    # ci_matrix_Y0_lower[t,i] <- hat_E.Y0-1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
    # ci_matrix_ate_lower[t,i] <- hat_ATE - 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

    # record upper CI
    ci_matrix_Y1_upper[t,i] <- upper.ci_Y1
    ci_matrix_Y0_upper[t,i] <- upper.ci_Y0
    ci_matrix_ate_upper[t,i] <- upper.ci_ATE
    # ci_matrix_Y1_upper[t,i] <- hat_E.Y1+1.96*sqrt(mean(tmle_output_Y1$EIF^2-tmle_output_Y1$EIF)/n)
    # ci_matrix_Y0_upper[t,i] <- hat_E.Y0+1.96*sqrt(mean(tmle_output_Y0$EIF^2-tmle_output_Y0$EIF)/n)
    # ci_matrix_ate_upper[t,i] <- hat_ATE + 1.96*sqrt(mean((tmle_output_Y1$EIF-tmle_output_Y0$EIF)^2-(tmle_output_Y1$EIF-tmle_output_Y0$EIF))/n)

    # record mean of EIF for E(Y^a)
    EIF_matrix_Y1[t,i] <- mean(tmle_output_Y1$TMLE$EIF)
    EIF_matrix_Y0[t,i] <- mean(tmle_output_Y0$TMLE$EIF)

  }

  # record CI coverage
  ci_coverage_Y1[i,"coverage"] <- mean((ci_matrix_Y1_lower[,i] < E.Y1) & (ci_matrix_Y1_upper[,i] > E.Y1))
  ci_coverage_Y0[i,"coverage"] <- mean((ci_matrix_Y0_lower[,i] < E.Y0) & (ci_matrix_Y0_upper[,i] > E.Y0))
  ci_coverage_ATE[i,"coverage"] <- mean((ci_matrix_ate_lower[,i] < ATE) & (ci_matrix_ate_upper[,i] > ATE))

  ci_coverage_Y1[i,"coverage.lower"] <- ci_coverage_Y1[i,"coverage"]-1.96*sqrt(ci_coverage_Y1[i,"coverage"]*(1-ci_coverage_Y1[i,"coverage"])/n)
  ci_coverage_Y0[i,"coverage.lower"] <- ci_coverage_Y0[i,"coverage"]-1.96*sqrt(ci_coverage_Y0[i,"coverage"]*(1-ci_coverage_Y0[i,"coverage"])/n)
  ci_coverage_ATE[i,"coverage.lower"] <- ci_coverage_ATE[i,"coverage"]-1.96*sqrt(ci_coverage_ATE[i,"coverage"]*(1-ci_coverage_ATE[i,"coverage"])/n)

  ci_coverage_Y1[i,"coverage.upper"] <- ci_coverage_Y1[i,"coverage"]+1.96*sqrt(ci_coverage_Y1[i,"coverage"]*(1-ci_coverage_Y1[i,"coverage"])/n)
  ci_coverage_Y0[i,"coverage.upper"] <- ci_coverage_Y0[i,"coverage"]+1.96*sqrt(ci_coverage_Y0[i,"coverage"]*(1-ci_coverage_Y0[i,"coverage"])/n)
  ci_coverage_ATE[i,"coverage.upper"] <- ci_coverage_ATE[i,"coverage"]+1.96*sqrt(ci_coverage_ATE[i,"coverage"]*(1-ci_coverage_ATE[i,"coverage"])/n)

  # record average point estimate
  avg.Y1[i,"est"] <- mean(est_matrix_Y1[,i])
  avg.Y0[i,"est"] <- mean(est_matrix_Y0[,i])
  avg.ate[i,"est"] <- mean(est_matrix_ate[,i])

  avg.Y1[i,"sd"] <- sqrt(var(est_matrix_Y1[,i])/nsim)
  avg.Y0[i,"sd"] <- sqrt(var(est_matrix_Y0[,i])/nsim)
  avg.ate[i,"sd"] <- sqrt(var(est_matrix_ate[,i])/nsim)

  avg.Y1[i,"upper"] <- avg.Y1[i,"est"]+1.96*sqrt(var(est_matrix_Y1[,i])/nsim)
  avg.Y0[i,"upper"] <- avg.Y0[i,"est"]+1.96*sqrt(var(est_matrix_Y0[,i])/nsim)
  avg.ate[i,"upper"] <- avg.ate[i,"est"]+1.96*sqrt(var(est_matrix_ate[,i])/nsim)

  avg.Y1[i,"lower"] <- avg.Y1[i,"est"]-1.96*sqrt(var(est_matrix_Y1[,i])/nsim)
  avg.Y0[i,"lower"] <- avg.Y0[i,"est"]-1.96*sqrt(var(est_matrix_Y0[,i])/nsim)
  avg.ate[i,"lower"] <- avg.ate[i,"est"]-1.96*sqrt(var(est_matrix_ate[,i])/nsim)

  # record average bias
  avg.bias_Y1[i,"bias"] <- sqrt(n)*mean(bias_matrix_Y1[,i])
  avg.bias_Y0[i,"bias"] <- sqrt(n)*mean(bias_matrix_Y0[,i])
  avg.bias_ate[i,"bias"] <- sqrt(n)*mean(bias_matrix_ate[,i])

  avg.bias_Y1[i,"upper"] <- avg.bias_Y1[i,"bias"]+1.96*sqrt(var(sqrt(n)*bias_matrix_Y1[,i])/nsim)
  avg.bias_Y0[i,"upper"] <- avg.bias_Y0[i,"bias"]+1.96*sqrt(var(sqrt(n)*bias_matrix_Y0[,i])/nsim)
  avg.bias_ate[i,"upper"] <- avg.bias_ate[i,"bias"]+1.96*sqrt(var(sqrt(n)*bias_matrix_ate[,i])/nsim)

  avg.bias_Y1[i,"lower"] <- avg.bias_Y1[i,"bias"]-1.96*sqrt(var(sqrt(n)*bias_matrix_Y1[,i])/nsim)
  avg.bias_Y0[i,"lower"] <- avg.bias_Y0[i,"bias"]-1.96*sqrt(var(sqrt(n)*bias_matrix_Y0[,i])/nsim)
  avg.bias_ate[i,"lower"] <- avg.bias_ate[i,"bias"]-1.96*sqrt(var(sqrt(n)*bias_matrix_ate[,i])/nsim)

  # record var
  avg.variance_Y1[i,"variance"] <- n*mean((bias_matrix_Y1[,i]-mean(bias_matrix_Y1[,i]))^2)
  avg.variance_Y0[i,"variance"] <- n*mean((bias_matrix_Y0[,i]-mean(bias_matrix_Y0[,i]))^2)
  avg.variance_ate[i,"variance"] <- n*mean((bias_matrix_Y1[,i]-mean(bias_matrix_Y1[,i])-
                                              bias_matrix_Y0[,i]+mean(bias_matrix_Y0[,i]))^2)

  avg.variance_Y1[i,"upper"] <- avg.variance_Y1[i,"variance"]+1.96*sqrt(var(n*(bias_matrix_Y1[,i]-mean(bias_matrix_Y1[,i]))^2)/nsim)
  avg.variance_Y0[i,"upper"] <- avg.variance_Y0[i,"variance"]+1.96*sqrt(var(n*(bias_matrix_Y0[,i]-mean(bias_matrix_Y0[,i]))^2)/nsim)
  avg.variance_ate[i,"upper"] <- avg.variance_ate[i,"variance"]+1.96*sqrt(var(n*(bias_matrix_ate[,i]-mean(bias_matrix_ate[,i]))^2)/nsim)

  avg.variance_Y1[i,"lower"] <- avg.variance_Y1[i,"variance"]-1.96*sqrt(var(n*(bias_matrix_Y1[,i]-mean(bias_matrix_Y1[,i]))^2)/nsim)
  avg.variance_Y0[i,"lower"] <- avg.variance_Y0[i,"variance"]-1.96*sqrt(var(n*(bias_matrix_Y0[,i]-mean(bias_matrix_Y0[,i]))^2)/nsim)
  avg.variance_ate[i,"lower"] <- avg.variance_ate[i,"variance"]-1.96*sqrt(var(n*(bias_matrix_ate[,i]-mean(bias_matrix_ate[,i]))^2)/nsim)

  # record MSE
  avg.MSE_Y1[i,"mse"] <- n*mean(bias_matrix_Y1[,i]^2)
  avg.MSE_Y0[i,"mse"] <- n*mean(bias_matrix_Y0[,i]^2)
  avg.MSE_ate[i,"mse"] <- n*mean((bias_matrix_Y1[,i] - bias_matrix_Y0[,i])^2)

  avg.MSE_Y1[i,"upper"] <- avg.MSE_Y1[i,"mse"]+1.96*sqrt(var(n*bias_matrix_Y1[,i]^2)/nsim)
  avg.MSE_Y0[i,"upper"] <- avg.MSE_Y0[i,"mse"]+1.96*sqrt(var(n*bias_matrix_Y0[,i]^2)/nsim)
  avg.MSE_ate[i,"upper"] <- avg.MSE_ate[i,"mse"]+1.96*sqrt(var(n*bias_matrix_ate[,i]^2)/nsim)

  avg.MSE_Y1[i,"lower"] <- avg.MSE_Y1[i,"mse"]-1.96*sqrt(var(n*bias_matrix_Y1[,i]^2)/nsim)
  avg.MSE_Y0[i,"lower"] <- avg.MSE_Y0[i,"mse"]-1.96*sqrt(var(n*bias_matrix_Y0[,i]^2)/nsim)
  avg.MSE_ate[i,"lower"] <- avg.MSE_ate[i,"mse"]-1.96*sqrt(var(n*bias_matrix_ate[,i]^2)/nsim)

}

# save data
save(list = c("bias_matrix_Y1","bias_matrix_Y0","bias_matrix_ate",
              "est_matrix_Y1","est_matrix_Y0","est_matrix_ate",
              "EIF_matrix_Y1","EIF_matrix_Y0",
              "ci_matrix_Y1_lower","ci_matrix_Y0_lower","ci_matrix_ate_lower",
              "ci_matrix_Y1_upper","ci_matrix_Y0_upper","ci_matrix_ate_upper",
              "ci_coverage_Y1","ci_coverage_Y0","ci_coverage_ATE",
              "avg.Y1","avg.Y0","avg.ate",
              "avg.bias_Y1","avg.bias_Y0","avg.bias_ate",
              "avg.variance_Y1","avg.variance_Y0","avg.variance_ate",
              "avg.MSE_Y1","avg.MSE_Y0","avg.MSE_ate"),file = out.name)

