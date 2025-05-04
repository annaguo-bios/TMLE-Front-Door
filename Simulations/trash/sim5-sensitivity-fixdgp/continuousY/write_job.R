# sample size
n.vec <- c(200,seq(2500,20000,2500))

# number of simulations
nsim <- 200

## fd_admg1 ====
estimator='AIPW' # IPW or AIPW
dgp.f.name='fd_admg1' # dgp function name
R=500 # number of bootstrap samples

for (i in seq_along(n.vec)){ # sample size
  joblist <- c()
  
  for (m in c(0,1)){ # level of m
    
    for (t in 1:nsim){ # number of simulations
      
      job <- paste0("Rscript main.R ",n.vec[i]," ",t," ",m," ",estimator," ", dgp.f.name," ",R)
      
      joblist <- c(joblist,job)
    }
    
    
  }
  
  write.table(joblist, file = paste0("fd_admg1_n",i,".txt"),quote = F, col.names = F, row.names = F)
  
}


## nonfd_admg1 ====
estimator='AIPW' # IPW or AIPW
dgp.f.name='nonfd_admg1' # dgp function name
R=500 # number of bootstrap samples

for (i in seq_along(n.vec)){ # sample size
  joblist <- c()
  
  for (m in c(0,1)){ # level of m
    
    for (t in 1:nsim){ # number of simulations
      
      job <- paste0("Rscript main.R ",n.vec[i]," ",t," ",m," ",estimator," ", dgp.f.name," ",R)
      
      joblist <- c(joblist,job)
    }
    
    
  }
  
  write.table(joblist, file = paste0("nonfd_admg1_n",i,".txt"),quote = F, col.names = F, row.names = F)
  
}


## nonfd_admg2 ====
estimator='AIPW' # IPW or AIPW
dgp.f.name='nonfd_admg2' # dgp function name
R=500 # number of bootstrap samples

for (i in seq_along(n.vec)){ # sample size
  joblist <- c()
  
  for (m in c(0,1)){ # level of m
    
    for (t in 1:nsim){ # number of simulations
      
      job <- paste0("Rscript main.R ",n.vec[i]," ",t," ",m," ",estimator," ", dgp.f.name," ",R)
      
      joblist <- c(joblist,job)
    }
    
    
  }
  
  write.table(joblist, file = paste0("nonfd_admg2_n",i,".txt"),quote = F, col.names = F, row.names = F)
  
}



## nonfd_admg3 ====
estimator='AIPW' # IPW or AIPW
dgp.f.name='nonfd_admg3' # dgp function name
R=500 # number of bootstrap samples

for (i in seq_along(n.vec)){ # sample size
  joblist <- c()
  
  for (m in c(0,1)){ # level of m
    
    for (t in 1:nsim){ # number of simulations
      
      job <- paste0("Rscript main.R ",n.vec[i]," ",t," ",m," ",estimator," ", dgp.f.name," ",R)
      
      joblist <- c(joblist,job)
    }
    
    
  }
  
  write.table(joblist, file = paste0("nonfd_admg3_n",i,".txt"),quote = F, col.names = F, row.names = F)
  
}