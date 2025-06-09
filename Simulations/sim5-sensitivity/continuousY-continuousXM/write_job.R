# sample size
n.vec <- c(500,1000,2000,3000)

# number of simulations
nsim <- 200

estimators=c("AIPW") # IPW or AIPW

## fd_admg1 ====
R=500 # number of bootstrap samples

for (estimator in estimators){
  
  for (m in c("fd_admg1","fd_admg2","nonfd_admg1","nonfd_admg2")){ # level of m
    
    for (i in seq_along(n.vec)){ # sample size
      
      joblist <- c()
      
      for (t in 1:nsim){ # number of simulations
        
        N <- n.vec[i]
        
        if (m %in% c('nonfd_admg1','nonfd_admg2')){
          
          t <- t+nsim
          
        }
        
        job <- paste0("Rscript main.R ",N," ",t," ",estimator," ",m," ", R," ","T")
        
        joblist <- c(joblist,job)
        
        job <- paste0("Rscript main.R ",N," ",t," ",estimator," ",m," ", R," ","F")
        
        joblist <- c(joblist,job)
      }
      
      write.table(joblist, file = paste0(m,"_",estimator,"_joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
      
      
    }
    
    
    
  }
  
}

