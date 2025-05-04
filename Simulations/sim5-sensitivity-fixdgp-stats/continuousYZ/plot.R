library(here)
library(ggplot2)
library(dplyr)

setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity-fixdgp-stats/continuousYZ")

# CATE ====
for (dgp in c("fd_admg1","fd_admg2")){
  
  for (estimator in c("AIPW","IPW","Gcomp","verma")){
    
    load(paste0(dgp,"/result_",estimator,".RData"))
    
    bf.rate_matrix$method <- "BF"
    cate.wald.rate_matrix$method <- "Wald Test"
    
    p.data <- rbind(bf.rate_matrix, cate.wald.rate_matrix) %>% mutate(Method = factor(method))
    
    p.res <- p.data %>%
      ggplot( aes(x=n, y=type12error, group=Method, color=Method)) +
      geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed") + # Add horizontal line
      geom_text(aes(x = min(n), y = 0.05, label = "0.05"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
      geom_line() +
      theme_bw() +
      ylab("Type I Error")+
      xlab("Sample Size") 
    
    ggsave(paste0(dgp,'/cate_',estimator,'.pdf'), plot=p.res, width = 8, height = 6, units = "in")
    
  }
  
}


for (dgp in c("nonfd_admg1","nonfd_admg2")){
  
  for (estimator in c("AIPW","IPW","Gcomp","verma")){
    
    load(paste0(dgp,"/result_",estimator,".RData"))
    
    bf.rate_matrix$method <- "BF"
    cate.wald.rate_matrix$method <- "Wald Test"
    
    p.data <- rbind(bf.rate_matrix, cate.wald.rate_matrix) %>% mutate(Method = factor(method))
    
    p.res <- p.data %>%
      ggplot( aes(x=n, y=type12error, group=Method, color=Method)) +
      # geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed") + # Add horizontal line
      # geom_text(aes(x = min(n), y = 0.05, label = "0.05"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
      geom_line() +
      theme_bw() +
      ylab("Type II Error")+
      xlab("Sample Size") 
    
    ggsave(paste0(dgp,'/cate_',estimator,'.pdf'), plot=p.res, width = 8, height = 6, units = "in")
    
  }
  
}


# weighted method ====
for (dgp in c("fd_admg1","fd_admg2")){
  
  load(paste0(dgp,"/result_AIPW.RData"))
  
  wald.rate_matrix$method <- "D2 Wald Test"
  wald.rate_matrix$method_large <- "Wald Test"
  wald.rate_matrix$weight <- "Combined"
  
  primal.wald.rate_matrix$method <- "Primal D1 Wald Test"
  primal.wald.rate_matrix$weight <- "Primal"
  primal.wald.rate_matrix$method_large <- "Wald Test"
  
  dual.wald.rate_matrix$method <- "Dual D1 Wald Test"
  dual.wald.rate_matrix$method_large <- "Wald Test"
  dual.wald.rate_matrix$weight <- "Dual"
  
  primal.lrt.rate_matrix$method <- "Primal D1 LRT"
  primal.lrt.rate_matrix$method_large <- "LRT"
  primal.lrt.rate_matrix$weight <- "Primal"
  
  dual.lrt.rate_matrix$method <- "Dual D1 LRT"
  dual.lrt.rate_matrix$method_large <- "LRT"
  dual.lrt.rate_matrix$weight <- "Dual"
  
  primal.wilcoxon.rate_matrix$method <- "Primal D1 Wilcoxon"
  primal.wilcoxon.rate_matrix$method_large <- "Wilcoxon"
  primal.wilcoxon.rate_matrix$weight <- "Primal"
  
  dual.wilcoxon.rate_matrix$method <- "Dual D1 Wilcoxon"
  dual.wilcoxon.rate_matrix$method_large <- "Wilcoxon"
  dual.wilcoxon.rate_matrix$weight <- "Dual"
  
  p.data <- rbind(wald.rate_matrix, primal.wald.rate_matrix, dual.wald.rate_matrix, primal.lrt.rate_matrix, dual.lrt.rate_matrix, primal.wilcoxon.rate_matrix, dual.wilcoxon.rate_matrix) %>% mutate(weight= factor(weight))
  
  for (m in c("Wald Test","LRT","Wilcoxon")){
    
    p.res <- p.data %>% filter(method_large == m) %>%
      ggplot( aes(x=n, y=type12error, group=weight, color=weight)) +
      geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed") + # Add horizontal line
      geom_text(aes(x = min(n), y = 0.05, label = "0.05"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
      geom_line() +
      theme_bw() +
      ylab("Type I Error")+
      xlab("Sample Size") 
    
    ggsave(paste0(dgp,'/weighted_',m,'.pdf'), plot=p.res, width = 8, height = 6, units = "in")
    
  }
  
  
}


for (dgp in c("nonfd_admg1","nonfd_admg2")){
  
  
  load(paste0(dgp,"/result_AIPW.RData"))
  
  wald.rate_matrix$method <- "D2 Wald Test"
  wald.rate_matrix$method_large <- "Wald Test"
  wald.rate_matrix$weight <- "Combined"
  
  primal.wald.rate_matrix$method <- "Primal D1 Wald Test"
  primal.wald.rate_matrix$weight <- "Primal"
  primal.wald.rate_matrix$method_large <- "Wald Test"
  
  dual.wald.rate_matrix$method <- "Dual D1 Wald Test"
  dual.wald.rate_matrix$method_large <- "Wald Test"
  dual.wald.rate_matrix$weight <- "Dual"
  
  primal.lrt.rate_matrix$method <- "Primal D1 LRT"
  primal.lrt.rate_matrix$method_large <- "LRT"
  primal.lrt.rate_matrix$weight <- "Primal"
  
  dual.lrt.rate_matrix$method <- "Dual D1 LRT"
  dual.lrt.rate_matrix$method_large <- "LRT"
  dual.lrt.rate_matrix$weight <- "Dual"
  
  primal.wilcoxon.rate_matrix$method <- "Primal D1 Wilcoxon"
  primal.wilcoxon.rate_matrix$method_large <- "Wilcoxon"
  primal.wilcoxon.rate_matrix$weight <- "Primal"
  
  dual.wilcoxon.rate_matrix$method <- "Dual D1 Wilcoxon"
  dual.wilcoxon.rate_matrix$method_large <- "Wilcoxon"
  dual.wilcoxon.rate_matrix$weight <- "Dual"
  
  p.data <- rbind(wald.rate_matrix, primal.wald.rate_matrix, dual.wald.rate_matrix, primal.lrt.rate_matrix, dual.lrt.rate_matrix, primal.wilcoxon.rate_matrix, dual.wilcoxon.rate_matrix) %>% mutate(Method = factor(method), Method_large = factor(method_large), weight= factor(weight))
  
  for (m in c("Wald Test","LRT","Wilcoxon")){
    
    p.res <- p.data %>% filter(method_large == m) %>%
      ggplot( aes(x=n, y=type12error, group=weight, color=weight)) +
      geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed") + # Add horizontal line
      geom_text(aes(x = min(n), y = 0.05, label = "0.05"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
      geom_line() +
      theme_bw() +
      ylab("Type II Error")+
      xlab("Sample Size") 
    
    ggsave(paste0(dgp,'/weighted_',m,'.pdf'), plot=p.res, width = 8, height = 6, units = "in")
    
  }
  
  
  
}

