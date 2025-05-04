library(here)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity/binaryY")

for(estimator in c("np","AIPW","IPW","Gcomp")){
  
  load(paste0('result_m0_',estimator,'.RData'))
  m0.rate_matrix <- result %>% mutate(m=0)
  
  load(paste0('result_m1_',estimator,'.RData'))
  m1.rate_matrix <- result %>% mutate(m=1)
  
  pdata <- rbind(m0.rate_matrix, m1.rate_matrix) %>% mutate(m = factor(m)) %>% pivot_longer(cols = c(TPR, FPR),  names_to = "statistics", values_to = "value") 
  
  p.cate <- ggplot(pdata, aes(x = n, y = value, linetype = statistics, group = interaction(m, statistics), color = factor(m))) + 
    geom_line() + 
    theme_bw() + 
    ylab("TPR or FPR") + 
    xlab("Sample Size")+
    labs(color = "M=m")
  
  ggsave(paste0('cate_',estimator,'.pdf'), plot=p.cate, width = 8, height = 6, units = "in")
  
}

