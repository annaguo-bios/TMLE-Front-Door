dgp.f.name="../DGPs/2-dgp-multi.R" # name for the DGP function
truth= "../DGPs/2-truth-multi.Rdata" # path+name for the truth.Rdata
out.path.tmle= "TMLE-est2b/output/" # path for the output folder
out.path.onestep= "Onestep-est2b/output/" # path for the output folder
mediator.method="bayes"
superlearner=F
crossfit=F
K=5
treatment="A"
mediators='c(\"M.1\",\"M.2\")'
outcome="Y"
covariates='c(\"X\")'
lib="c('SL.range')"
linkA="identity"
np.dnorm=F
truncate_lower=0.001
truncate_upper=0.999





mediator.method="bayes"
superlearner=F
crossfit=F
K=5
treatment="A"
mediators=c('M.1','M.2')
outcome="Y"
covariates=c('X')
lib=c('SL.range')
linkA="identity"
np.dnorm=F
truncate_lower=0.001
truncate_upper=0.999
seed=1
n=500
