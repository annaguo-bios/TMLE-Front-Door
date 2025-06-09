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

setwd(here::here("sim6-verma-efficiency/binaryM-binaryZ"))

# plot function
source("plot-sub.R")

# sample size
n.vec <- c(500,1000,2000,4000,8000)

# number of simulation
nsim <- 1000

### binary case ====
# the truth
load("../DGPs/6-truth-binaryM-binaryZ.Rdata")

## tmle====
# load("TMLE/result.Rdata")
# p.z1.tmle <- plot.tmle(r'($\psi_1(\hat{Q}^*;\\ z=1)$)',z="z1")
# p.z0.tmle <- plot.tmle(r'($\psi_1(\hat{Q}^*;z=0)$)',z="z0")
# p.all.z.tmle <- plot.tmle(r'($\psi_1(\hat{Q}^*;Z)$)',z="all.z")
# p.opt.tmle <- plot.tmle(r'($\psi_1(\hat{Q}^*;z=1)$)',z="opt.linear")

## onestep====
load("Onestep/result.Rdata")
p.z1.one <- plot.tmle(r'($\psi_{z^*=1}^{+}(\hat{Q})$)',z="z1",samplesize.label=F)
p.z0.one <- plot.tmle(r'($\psi_{z^*=0}^{+}(\hat{Q})$)',z="z0",samplesize.label=F)
p.all.z.one <- plot.tmle(r'($\psi_1^{+}(\hat{Q};\ Z)$)',z="all.z")
p.opt.one <- plot.tmle(r'($\psi_{\alpha_{opt}}^{+}(\hat{Q})$)',z="opt.linear")

# 
# p.tmle <- plot_grid(
#   p.z0.tmle,p.z1.tmle,p.all.z.tmle,p.opt.tmle,
#   align = "v", ncol = 1
# )

p.one <- plot_grid(
  p.z0.one,p.z1.one,p.opt.one,
  align = "v", ncol = 1
)


p.final <- plot_grid(
p.one,
  align = "h",
  ncol = 1
)


ggsave("plot.pdf", plot = p.final, width = 8, height = 16, units = "in")
ggsave("sim6-binary.pdf", plot = p.final, width = 8, height = 16, units = "in")
