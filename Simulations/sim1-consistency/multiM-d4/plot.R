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


### 4D case ====
# the truth
load("../DGPs/1-truth-multi-d4.Rdata")

## 4D est2 ====
load("TMLE-est2a/result.Rdata")
p.4d.est2 <- plot.tmle(r'($\psi_{2a}(\hat{Q}^*)$)',ycoord.bias = c(-0.6,0.6),ycoord.var = c(32,46))

## 4D est2-dnorm ====
load("TMLE-est2-dnorm/result.Rdata")
p.4d.est2.dnorm <- plot.tmle(r'($\psi_2(\hat{Q}^*)$ - dnorm)',ycoord.bias = c(-0.6,0.9),c(38,50))

## 4D est3 ====
load("TMLE-est2b/result.Rdata")
p.4d.est3 <- plot.tmle(r'($\psi_{2b}(\hat{Q}^*)$)',ycoord.var = c(35,55))

p.4d <- plot_grid(
  p.4d.est2
  ,p.4d.est2.dnorm
  ,p.4d.est3
  , align = "hv"
  , ncol = 1
)

## 4D onestep-densratio ====
load("Onestep-est2a/result.Rdata")
p.4d.1densratio <- plot.tmle(r'($\psi_{2a}^{+}(\hat{Q})$)',ycoord.bias = c(-0.6,0.6),ycoord.var = c(32,46))

## 4D onestep-dnorm ====
load("Onestep-est2-dnorm/result.Rdata")
p.4d.1dnorm.sr <- plot.tmle(r'($\psi_2^{+}(\hat{Q})$ - dnorm)',ycoord.bias = c(-0.6,0.9),c(38,50))

## 4D onestep-bayes ====
load("Onestep-est2b/result.Rdata")
p.4d.1bayes <- plot.tmle(r'($\psi_{2b}^{+}(\hat{Q})$)',ycoord.var = c(35,55))

p.con1 <- plot_grid(
  p.4d.est2
  ,p.4d.est2.dnorm
  ,p.4d.est3
  , align = "hv"
  , ncol = 1
)

p.con2 <- plot_grid(
  p.4d.1densratio
  ,p.4d.1dnorm.sr
  ,p.4d.1bayes
  , align = "hv"
  , ncol = 1
)


p <- plot_grid(
  p.con1
  ,p.con2,
  align="hv",ncol=2)

ggsave("plot.pdf", plot = p, width = 16, height = 20, units = "in")

