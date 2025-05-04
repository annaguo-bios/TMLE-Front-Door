# sample size
n.vec <- c(500,1000,2000)

# number of simulations
nsim <- 1000


## EST2a ====
dgp.f.name="../DGPs/2-dgp-multi.R" # name for the DGP function
truth= "../DGPs/2-truth-multi.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est2a/output/" # path for the output folder
out.path.onestep= "Onestep-est2a/output/" # path for the output folder
mediator.method="densratio"
superlearner="F"
crossfit="F"
K="5"
treatment="A"
mediators="'c(\"M.1\",\"M.2\")'"
outcome="Y"
covariates="'c(\"X\")'"
lib="\"c('SL.ranger')\""
linkA="identity"
np.dnorm="F"
truncate_lower="0.001"
truncate_upper="0.999"
ATT.arg="T"

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA," ",np.dnorm, " ", truncate_lower, " ", truncate_upper)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("est2a-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}

truth= "../DGPs/2-truth-multi-ATT.Rdata" # path+name for the truth.Rdata

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA," ",np.dnorm, " ", truncate_lower, " ", truncate_upper," ",ATT.arg)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("ATT-est2a-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}

## EST2b ====
dgp.f.name="../DGPs/2-dgp-multi.R" # name for the DGP function
truth= "../DGPs/2-truth-multi.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est2b/output/" # path for the output folder
out.path.onestep= "Onestep-est2b/output/" # path for the output folder
mediator.method="bayes"
superlearner="F"
crossfit="F"
K="5"
treatment="A"
mediators="'c(\"M.1\",\"M.2\")'"
outcome="Y"
covariates="'c(\"X\")'"
lib="\"c('SL.range')\""
linkA="identity"
np.dnorm="F"
truncate_lower="0.001"
truncate_upper="0.999"

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA," ",np.dnorm," ", truncate_lower," ", truncate_upper)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("est2b-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}

truth= "../DGPs/2-truth-multi-ATT.Rdata" # path+name for the truth.Rdata

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib," ",linkA," ",np.dnorm," ", truncate_lower," ", truncate_upper," ",ATT.arg)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("ATT-est2b-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}
