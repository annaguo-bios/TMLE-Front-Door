joblist <- c()
for (m in 1:10){
  
  job <- paste0("Rscript main_ordinal.R ",m)
  joblist <- c(joblist,job)
  
  
}
write.table(joblist, file = paste0("joblist_n",1,".txt"),quote = F, col.names = F, row.names = F)