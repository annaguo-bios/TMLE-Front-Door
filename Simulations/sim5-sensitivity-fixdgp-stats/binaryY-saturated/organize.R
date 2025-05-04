args = commandArgs(trailingOnly=T)
dgp.f.name=args[1] # name of the DGP function
estimator=args[2] # estimator for estimation
nsim=as.integer(args[3]) # require specifying number of simulations, e.g. nsim <- 1000


############################
# Organize results
#
############################

alpha <- 0.05 # significant level

n.vec <- c(1000, 1500, 2000, 5000, 20000)

#################################################
# record results - CATE based
#################################################
bf.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
cate.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))

bf.rate_matrix <- data.frame(n=n.vec, # sample size
                             type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

cate.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                               type12error=vector(mode="integer",length=length(n.vec))) # true positive rate


#################################################
# record results - weight based
#################################################
# Wald test
wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
wald.rate_matrix <- data.frame(n=n.vec, # sample size
                               type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

primal.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
primal.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                      type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

dual.wald.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
dual.wald.rate_matrix <- data.frame(n=n.vec, # sample size
                                    type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

# LRT
primal.lrt.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
primal.lrt.rate_matrix <- data.frame(n=n.vec, # sample size
                                     type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

dual.lrt.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
dual.lrt.rate_matrix <- data.frame(n=n.vec, # sample size
                                   type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

# Wilcoxon test
primal.wilcoxon.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
primal.wilcoxon.rate_matrix <- data.frame(n=n.vec, # sample size
                                          type12error=vector(mode="integer",length=length(n.vec))) # true positive rate

dual.wilcoxon.matrix <- matrix(nrow = nsim, ncol = length(n.vec))
dual.wilcoxon.rate_matrix <- data.frame(n=n.vec, # sample size
                                        type12error=vector(mode="integer",length=length(n.vec))) # true positive rate



for (i in seq_along(n.vec)){

  # sample size
  n <- n.vec[i]

  for (seed in 1:nsim){

    load(paste0(dgp.f.name,"/output/output_n",n,"_",estimator,"_seed",seed,".Rdata"))
    
    ## CATE based
    
    bf.matrix[seed,i] <- if(satisfies_fd){(lower.ci.m1>0 | upper.ci.m1 <0) | (lower.ci.m0>0 | upper.ci.m0 <0)}else{(lower.ci.m1<0 & upper.ci.m1 >0) & (lower.ci.m0<0 & upper.ci.m0 >0)}
    
    cate.wald.matrix[seed,i] <- if(satisfies_fd){(1-pchisq(wald.test, df=2))>alpha}else{(1-pchisq(wald.test, df=2))<=alpha}
    
    ## weight based
    # wald test
    wald.matrix[seed,i] <- if(satisfies_fd){(1-pchisq(d2.wald.test, df=2))>alpha}else{(1-pchisq(d2.wald.test, df=2))<=alpha}
    primal.wald.matrix[seed,i] <- if(satisfies_fd){(1-pchisq(d1.primal.wald.test, df=1))>alpha}else{(1-pchisq(d1.primal.wald.test, df=1))<=alpha}
    dual.wald.matrix[seed,i] <- if(satisfies_fd){(1-pchisq(d1.dual.wald.test, df=1))>alpha}else{(1-pchisq(d1.dual.wald.test, df=1))<=alpha}
    
    # LRT
    dual.lrt.matrix[seed,i] <- if(satisfies_fd){dual_pval>alpha}else{dual_pval<=alpha}
    primal.lrt.matrix[seed,i] <- if(satisfies_fd){primal_pval>alpha}else{primal_pval<=alpha}
    
    # Wilcoxon test
    primal.wilcoxon.matrix[seed,i] <- if(satisfies_fd){ dual_wilcoxon_pval > alpha}else{dual_wilcoxon_pval <= alpha}
    dual.wilcoxon.matrix[seed,i] <- if(satisfies_fd){primal_wilcoxon_pval > alpha}else{primal_wilcoxon_pval <= alpha}

  }
  
  # record rate
  bf.rate_matrix[i,"type12error"] <- mean(bf.matrix[,i])
  cate.wald.rate_matrix[i,"type12error"] <- 1 - mean(cate.wald.matrix[,i])
  
  wald.rate_matrix[i,"type12error"] <- 1 - mean(wald.matrix[,i])
  primal.wald.rate_matrix[i,"type12error"] <- 1 - mean(primal.wald.matrix[,i])
  dual.wald.rate_matrix[i,"type12error"] <- 1 - mean(dual.wald.matrix[,i])
  
  dual.lrt.rate_matrix[i,"type12error"] <- 1 - mean(dual.lrt.matrix[,i])
  primal.lrt.rate_matrix[i,"type12error"] <- 1 - mean(primal.lrt.matrix[,i])
  
  primal.wilcoxon.rate_matrix[i,"type12error"] <- 1 - mean(primal.wilcoxon.matrix[,i])
  dual.wilcoxon.rate_matrix[i,"type12error"] <- 1 - mean(dual.wilcoxon.matrix[,i])


}

# save data
save(list = c("bf.matrix","cate.wald.matrix","bf.rate_matrix","cate.wald.rate_matrix","wald.matrix","wald.rate_matrix","primal.wald.matrix","primal.wald.rate_matrix","dual.wald.matrix","dual.wald.rate_matrix",
              "dual.lrt.matrix","dual.lrt.rate_matrix",'primal.lrt.matrix','primal.lrt.rate_matrix',
              'primal.wilcoxon.matrix','primal.wilcoxon.rate_matrix','dual.wilcoxon.matrix','dual.wilcoxon.rate_matrix'),file = paste0(dgp.f.name,'/result_',estimator,'.Rdata'))

