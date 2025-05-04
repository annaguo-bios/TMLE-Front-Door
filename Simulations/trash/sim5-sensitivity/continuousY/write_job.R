# sample size
n.vec <- c(200, 500, 1000, 1500, 2000, 5000, 20000)

# number of simulations
nsim <- 200

estimators=c("AIPW","IPW","Gcomp","g-null") # IPW or AIPW
R=500 # number of bootstrap samples

for (estimator in estimators){
  
  for (i in seq_along(n.vec)){ # sample size
    
    joblist <- c()
    
    for (m in c(0,1)){ # level of m
      
      for (t in 1:nsim){ # number of simulations
        
        job <- paste0("Rscript main.R ",n.vec[i]," ",t," ",m," ",estimator," ", R)
        
        joblist <- c(joblist,job)
      }
      
      
    }
    
    write.table(joblist, file = paste0(estimator,"_joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
    
  }
  
}
