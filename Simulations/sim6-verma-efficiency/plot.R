# packages
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(gridExtra)
library(cowplot)
library(here)

setwd(here::here("sim6-verma-efficiency"))

# plot function
source("plot-sub.R")

# sample size
n.vec <- c(500,1000,2000,4000,8000)

# number of simulation
nsim <- 1000

### p(Z) ====
# the truth
load("DGPs/6-truth-binaryM-continuousZ-pz.Rdata")

load("binaryM-continuousZ/Onestep/result.Rdata")
p.pz <- plot.tmle(r'($\psi_1^{+}(\hat{Q};\ \tilde{p}(Z));\ \tilde{p}(Z)=p(Z)$)',z="all.z",samplesize.label=F)

### N(0,1) ====
# the truth
load("DGPs/6-truth-binaryM-continuousZ-normal01-pz.Rdata")

load("binaryM-continuousZ-nomal01/Onestep/result.Rdata")
p.normal01 <- plot.tmle(r'($\psi_1^{+}(\hat{Q};\ \tilde{p}(Z)); \ \tilde{p}(Z):\ N(0,1)$)',z="all.z",samplesize.label=F)

### N(10,1) ====
# the truth
load("DGPs/6-truth-binaryM-continuousZ-normal10.1-pz.Rdata")

load("binaryM-continuousZ-nomal10/Onestep/result.Rdata")
p.normal10 <- plot.tmle(r'($\psi_1^{+}(\hat{Q};\ \tilde{p}(Z)); \ \tilde{p}(Z):\ N(10,1)$)',z="all.z")

# 
# p.tmle <- plot_grid(
#   p.z0.tmle,p.z1.tmle,p.all.z.tmle,p.opt.tmle,
#   align = "v", ncol = 1
# )
# 
# p.one <- plot_grid(
#   p.z0.one,p.z1.one,p.all.z.one,p.opt.one,
#   align = "v", ncol = 1
# )
# 
# 
p.final <- plot_grid(
  p.pz,p.normal01,p.normal10,
  align = "h",
  ncol = 1
)


ggsave("plot.pdf", plot = p.final, width = 8, height = 18, units = "in")

