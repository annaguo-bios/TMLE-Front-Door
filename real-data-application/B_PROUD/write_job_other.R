for (set in 1:7){
  
  joblist <- c()
  
  for (m in 1:10){
    
    job <- paste0("Rscript main_other.R ",m," ",set)
    joblist <- c(joblist,job)
    
  }
  
  write.table(joblist, file = paste0("setjoblist_n",set,".txt"),quote = F, col.names = F, row.names = F)
  
}

