args = commandArgs(trailingOnly=T)
dgp.f.name=args[1] # name of the DGP function
estimator=args[2] # estimator for estimation
nsim=as.integer(args[3]) # require specifying number of simulations, e.g. nsim <- 1000


############################
# Organize results
#
############################

alpha <- 0.05 # significant level

n.vec <- c(500,1000,2000,4000,10000)

#################################################
# record results - CATE based
#################################################
cate.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))

cate.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                               type1error_power=vector(mode="integer",length=length(n.vec))) # true positive rate


#################################################
# record results - weight based
#################################################

primal.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
primal.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                      type1error_power=vector(mode="integer",length=length(n.vec))) # true positive rate

dual.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
dual.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                    type1error_power=vector(mode="integer",length=length(n.vec))) # true positive rate


for (i in seq_along(n.vec)){

  # sample size
  n <- n.vec[i]

  for (seed in 1:nsim){

    load(paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_seed",seed,".Rdata"))
    
    # cate.wald.matrix[seed,i] <- (1-pchisq(wald.test, df=4))<=alpha # this is the type I error when data from front-door, and is the power when data not from front-door
    cate.wald.matrix[seed,i] <- wald.test
    
    ## weight based
    # permutation test
    primal.wald.matrix[seed,i] <- p.test$p.value<=alpha
    dual.wald.matrix[seed,i] <- d.test$p.value<=alpha

  }
  
  # record rate
  cate.wald.rate_matrix[i,"type1error_power"] <- mean(cate.wald.matrix[,i])
  
  primal.wald.rate_matrix[i,"type1error_power"] <- mean(primal.wald.matrix[,i])
  dual.wald.rate_matrix[i,"type1error_power"] <- mean(dual.wald.matrix[,i])


}

# save data
save(list = c("cate.wald.matrix","cate.wald.rate_matrix",
              "primal.wald.matrix","primal.wald.rate_matrix",
              "dual.wald.matrix","dual.wald.rate_matrix"),file = paste0(dgp.f.name,'/result_',estimator,'.Rdata'))

