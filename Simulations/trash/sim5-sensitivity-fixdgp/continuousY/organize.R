args = commandArgs(trailingOnly=T)
dgp.f.name=args[1] # name of the DGP function
estimator=args[2] # estimator for estimation
nsim=as.integer(args[3]) # require specifying number of simulations, e.g. nsim <- 1000
m=as.integer(args[4]) # require specifying level of m

# an example
# dgp.f.name='fd_admg1'
# estimator='AIPW'
# nsim=200
# m=0


############################
# Organize results
#
############################

n.vec <- c(200,seq(2500,20000,2500))

fd.dgp.f.name <- c('fd_admg1')

# record results

est_matrix <- matrix(nrow = nsim, ncol = length(n.vec))

sd_matrix <- matrix(nrow = nsim, ncol = length(n.vec))

lower_ci_matrix <- matrix(nrow = nsim, ncol = length(n.vec))

upper_ci_matrix <- matrix(nrow = nsim, ncol = length(n.vec))

rate_matrix <- data.frame(n=n.vec, # sample size
                         TPR=vector(mode="integer",length=length(n.vec)), # true positive rate
                         FPR=vector(mode="integer",length=length(n.vec))) # false positive rate



for (i in seq_along(n.vec)){

  # sample size
  n <- n.vec[i]

  for (seed in 1:nsim){

    load(paste0(dgp.f.name,"/output/output_n",n,"_m",m,"_",estimator,"_seed",seed,".Rdata"))
    
    # record test statistic
    est_matrix[seed,i] <- test.statistic

    # record standard deviation of the test statistic
    sd_matrix[seed,i] <- test.statistic.sd

    # record lower CI
    lower_ci_matrix[seed,i] <- test.statistic.lower.ci

    # record upper CI
    upper_ci_matrix[seed,i] <- test.statistic.upper.ci

  }
  
  # record TPR and FPR
  rate_matrix[i,"TPR"] <- ifelse(dgp.f.name %in% fd.dgp.f.name,
                                 mean((lower_ci_matrix[,i] < 0) & (upper_ci_matrix[,i] > 0)), # if it's front-door model, the 95%CI should contain 0
                                 mean((lower_ci_matrix[,i] > 0) | (upper_ci_matrix[,i] < 0))) # if it's not front-door model, the 95%CI should NOT contain 0


}

# save data
save(list = c("est_matrix",
              "sd_matrix",
              "lower_ci_matrix",
              "upper_ci_matrix",
              "rate_matrix"),file = paste0(dgp.f.name,'/result_m',m,'_',estimator,'.Rdata'))

