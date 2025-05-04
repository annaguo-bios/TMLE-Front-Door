# sample size
n.vec <- c(200, 500, 1000, 1500, 2000, 5000, 20000)

# number of simulations
nsim <- 200

R=500 # number of bootstrap samples

for (i in seq_along(n.vec)){ # sample size
  
  n <- n.vec[i]
  
  joblist <- c()
  
  for (m in c(0,1)){ # level of m
    
    for (t in 1:nsim){ # number of simulations
      
      if(!file.exists(paste0("output/output_n",n,"_m",m,"_seed",t,".Rdata"))){ # if the file doesn't exist
        
        job <- paste0("Rscript main.R ",n.vec[i]," ",200+t," ",m," ",R)
        
        joblist <- c(joblist,job)
        
      }
      
      
      
      
    }
    
    
  }
  
  write.table(joblist, file = paste0("error_joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
  
}
