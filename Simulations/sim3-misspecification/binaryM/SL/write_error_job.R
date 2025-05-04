# sample size
n.vec <- c(500,1000,2000)

# number of simulations
nsim <- 1000

dgp.f.name="../../DGPs/3-dgp-binary.R" # name for the DGP function
truth= "../../DGPs/3-truth-binary.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est1/output/" # path for the output folder
out.path.onestep= "Onestep-est1/output/" # path for the output folder
mediator.method="bayes"
superlearner="T"
crossfit="F"
K="5"
treatment="A"
mediators="'c(\"M\")'"
outcome="Y"
covariates="'c(\"X\")'"
lib="\"c('SL.glm','SL.bayesglm', 'SL.gam','SL.bartMachine','SL.earth','SL.ranger','SL.svm','SL.xgboost','SL.mean')\""
ATT.arg='T'

truth= "../../DGPs/3-truth-binary-ATT.Rdata" # path+name for the truth.Rdata
prefix <- 'ATT_'
error_time <- 1

joblist <- c()

for (i in seq_along(n.vec)){
  
  n <- n.vec[i]
  
  for (t in 1:nsim){
    
    job <- paste0("Rscript main.R ",n.vec[i]," ",t+nsim*error_time," ", dgp.f.name," ",truth," ",out.path.tmle," ", out.path.onestep," ",mediator.method," ",superlearner," ",crossfit," ",K," ",treatment," ",mediators," ",
                  outcome," ",covariates," ",lib,' ',ATT.arg)
    
    if(!file.exists(paste0(out.path.tmle,prefix,"output_",n,"_",t,".Rdata")) | !file.exists(paste0(out.path.onestep,prefix,"output_",n,"_",t,".Rdata"))){
      
      joblist <- c(joblist,job)
      
    }
    
    
    
    
  }
  
}

write.table(joblist, file = paste0("error-ATT-joblist_n1.txt"),quote = F, col.names = F, row.names = F)