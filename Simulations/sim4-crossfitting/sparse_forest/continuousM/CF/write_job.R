# sample size
n.vec <- c(500,1000,2000)

# number of simulations
nsim <- 1000

## EST2a ====
dgp.f.name="../../../DGPs/4-dgp-continuous.R" # name for the DGP function
truth= "../../../DGPs/4-truth-continuous.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est2a/output/" # path for the output folder
out.path.onestep= "Onestep-est2a/output/" # path for the output folder
mediator.method="densratio"
superlearner="T"
crossfit="T"
K="5"
treatment="A"
mediators="'c(\"M\")'"
outcome="Y"
covariates="'c(\"X.1\",\"X.2\",\"X.3\",\"X.4\",\"X.5\",\"X.6\",\"X.7\",\"X.8\",\"X.9\",\"X.10\")'"
lib="\"c('SL.ranger.sparse')\""

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("est2a-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}


## EST2b ====
dgp.f.name="../../../DGPs/4-dgp-continuous.R" # name for the DGP function
truth= "../../../DGPs/4-truth-continuous.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est2b/output/" # path for the output folder
out.path.onestep= "Onestep-est2b/output/" # path for the output folder
mediator.method="bayes"
superlearner="T"
crossfit="T"
K="5"
treatment="A"
mediators="'c(\"M\")'"
outcome="Y"
covariates="'c(\"X.1\",\"X.2\",\"X.3\",\"X.4\",\"X.5\",\"X.6\",\"X.7\",\"X.8\",\"X.9\",\"X.10\")'"
lib="\"c('SL.ranger.sparse')\""

for (i in seq_along(n.vec)){
  joblist <- c()
  for (t in 1:nsim){
    job <- paste0("Rscript main.R ",n.vec[i]," ",t," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib)
    joblist <- c(joblist,job)
  }
  write.table(joblist, file = paste0("est2b-joblist_n",i,".txt"),quote = F, col.names = F, row.names = F)
}
