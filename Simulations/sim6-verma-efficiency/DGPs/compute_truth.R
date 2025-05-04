args = commandArgs(trailingOnly=T)
truth.f.name=args[1] # require specifying truth function, e.g. truth.f.name <- "0-truth-binary.R"
dgp.f.name=args[2] # the dgp.R function, e.g. dgp.f.name <- "0-dgp-binary.R"
out.name=args[3] # require specifying the name for the output file, e.g. out.name <- "0-truth-binary.Rdata"
N=as.integer(args[4]) # require specifying the sample size for computing the truth numerically, e.g. N <- 100000


library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(cubature)
library(MASS)
library(mvtnorm)
library(Matrix)

set.seed(7)

#################################################
# Functions
#################################################
source(truth.f.name) # compute_truth(n)
source(dgp.f.name) # generate_data(n)


#################################################
# True parameters
#################################################

## true psi and variance
truth_output <- compute_truth(n = N)

z.opt <- truth_output$opt.linear$minimum
opt.Y1.linear <- truth_output$opt.Y1.linear$minimum
opt.Y0.linear <- truth_output$opt.Y0.linear$minimum

z.opt3 <- truth_output$opt.linear3$par
opt.Y1.linear3 <- truth_output$opt.Y1.linear3$par
opt.Y0.linear3 <- truth_output$opt.Y0.linear3$par

# to check that the mean of EIF=0
mean.EIF.Y1<- colMeans(truth_output$EIF_Y1); mean.EIF.Y1
mean.EIF.Y0<- colMeans(truth_output$EIF_Y0); mean.EIF.Y0

E.Y1 = truth_output$E.Y1; names(E.Y1) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")
VAR.Y1 = truth_output$VAR.Y1; names(VAR.Y1) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")
#
E.Y0 = truth_output$E.Y0; names(E.Y0) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")
VAR.Y0 = truth_output$VAR.Y0; names(VAR.Y0) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")
#
ATE = truth_output$ATE; names(ATE) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")
VAR.ATE = truth_output$VAR.ATE; names(VAR.ATE) = c("z1", "z0", "all.z","opt","opt.linear","opt.linear3")

# print out the results
at.Y1 <- data.frame(E.Y1=E.Y1, VAR.Y1=VAR.Y1)
rownames(at.Y1) <- c("z1", "z0", "all z","opt","opt.linear","opt.linear3")

at.Y0 <- data.frame(E.Y0=E.Y0, VAR.Y0=VAR.Y0)
rownames(at.Y0) <- c("z1", "z0", "all z","opt","opt.linear","opt.linear3")

at.ATE <- data.frame(ATE=ATE, VAR.ATE=VAR.ATE)
rownames(at.ATE) <- c("z1", "z0", "all z","opt","opt.linear","opt.linear3")

print("E[Y1] and it's variance")
print(at.Y1)
print(paste0("re-weight of two estimators is optimized at p(z=1)=",round(opt.Y1.linear,3)))
print(paste0("re-weight of three estimators a*psi+b*psi(z=1)+(1-a-b)*psi(z=0) is optimized at a=",round(opt.Y1.linear3[1],3)," b=",round(opt.Y1.linear3[2],3)))

print("E[Y0] and it's variance")
print(at.Y0)
print(paste0("re-weight of two estimators is optimized at p(z=1)=",round(opt.Y0.linear,3)))
print(paste0("re-weight of three estimators a*psi+b*psi(z=1)+(1-a-b)*psi(z=0) is optimized at a=",round(opt.Y0.linear3[1],3)," b=",round(opt.Y0.linear3[2],3)))

print("ATE and it's variance")
print(at.ATE)
print(paste0("re-weight of two estimators is optimized at p(z=1)=",round(z.opt,3)))
print(paste0("re-weight of three estimators a*psi+b*psi(z=1)+(1-a-b)*psi(z=0) is optimized at a=",round(z.opt3[1],3)," b=",round(z.opt3[2],3)))

save(list = c("E.Y1","E.Y0","ATE","VAR.Y1","VAR.Y0","VAR.ATE","mean.EIF.Y1","mean.EIF.Y0","z.opt","opt.Y1.linear",'opt.Y0.linear',"z.opt3","opt.Y1.linear3","opt.Y0.linear3"),file = out.name)
