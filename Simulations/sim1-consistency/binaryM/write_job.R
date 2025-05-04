# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulations
nsim <- 1000

dgp.f.name="../DGPs/1-dgp-binary.R" # name for the DGP function
truth= "../DGPs/1-truth-binary.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est1/output/" # path for the output folder
out.path.onestep= "Onestep-est1/output/" # path for the output folder
mediator.method="bayes"
superlearner="F"
crossfit="F"
K="5"
treatment="A"
mediators="'c(\"M\")'"
outcome="Y"
covariates="'c(\"X\")'"
lib="\"c('SL.ranger')\""
linkA="identity"
ATT.arg="T"

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}

truth= "../DGPs/1-truth-binary-ATT.Rdata" # path+name for the truth.Rdata
for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA," ",ATT.arg)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("ATT_joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}
