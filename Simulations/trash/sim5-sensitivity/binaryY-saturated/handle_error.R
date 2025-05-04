# sample size
n.vec <- c(1000, 1500, 2000, 5000, 20000)

# number of simulations
nsim <- 200

R=500 # number of bootstrap samples

estimators <- c("AIPW","IPW","Gcomp","np")

time <- 2

for (estimator in estimators){
  
  
  for (i in seq_along(n.vec)){ # sample size
    
    n <- n.vec[i]
    
    joblist <- c()
    
    for (m in c(0,1)){ # level of m
      
      for (t in 1:nsim){ # number of simulations
        
        if(!file.exists(paste0("output/output_n",n,"_m",m,"_",estimator,"_seed",t,".Rdata"))){ # if the file doesn't exist
          
          job <- paste0("Rscript main.R ",n.vec[i]," ",200*time+t," ",m," ",estimator," ",R)
          
          joblist <- c(joblist,job)
          
        }
        
        
        
        
      }
      
      
    }
    
    write.table(joblist, file = paste0("error_",estimator,"_joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
    
  }
  
  
}
