# packages
library(ggplot2)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(stats)
library(xtable)
library(gridExtra)
library(cowplot)

# plot function
source("plot-sub.R")

# sample size
n.vec <- c(250,500,1000,2000,4000,8000)

# number of simulation
nsim <- 1000

### binary case ====
# the truth
load("../DGPs/1-truth-binary.Rdata")

## binary-est1====
load("TMLE-est1/result.Rdata")
p.binary.est1 <- plot.tmle(r'($\psi_1(\hat{Q}^*)$)')

## binary-onestep====
load("Onestep-est1/result.Rdata")
p.binary.one <- plot.tmle(r'($\psi_1^{+}(\hat{Q})$)')

p.binary <- plot_grid(
  p.binary.est1
  ,p.binary.one
  , align = "h"
  , ncol = 2
)


ggsave("plot.pdf", plot = p.binary, width = 16, height = 8, units = "in")
