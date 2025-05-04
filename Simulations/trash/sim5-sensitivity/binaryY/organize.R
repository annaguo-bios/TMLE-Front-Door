args = commandArgs(trailingOnly=T)
estimator=args[1] # estimator for estimation
nsim=as.integer(args[2]) # require specifying number of simulations, e.g. nsim <- 1000
m=as.integer(args[3]) # require specifying level of m

# an example
# dgp.f.name='fd_admg1'
# estimator='AIPW'
# nsim=200
# m=0


############################
# Organize results
#
############################

n.vec <- c(1000, 1500, 2000, 5000, 20000)

# record results

tprs <- c()
fprs <- c()



for (i in seq_along(n.vec)){
  
  # sample size
  n <- n.vec[i]
  
  combine.test.res <- c(0,0,0,0) #TP, FP, FN, TN
  
  for (seed in 1:nsim){
    
    load(paste0("./output/output_n",n,"_m",m,"_",estimator,"_seed",seed,".Rdata"))
    
    combine.test.res <- combine.test.res + test.res
    
  }
  
  # record TPR and FPR
  tprs <- c(tprs, round(combine.test.res[1] / (combine.test.res[1] + combine.test.res[3]), 3))
  fprs <- c(fprs, round(combine.test.res[2] / (combine.test.res[1] + combine.test.res[4]), 3))
  
  
}

result <- data.frame(n=n.vec, TPR=tprs, FPR=fprs)


# save data
save(list = c("result"), file = paste0('result_m',m,'_',estimator,'.Rdata'))
