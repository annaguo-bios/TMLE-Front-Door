library(here)
library(ggplot2)

setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity/continuousY")

## fd_admg1
load('fd_admg1/result_m0_AIPW.RData')
fd.admg1.m0.rate_matrix <- rate_matrix %>% mutate(m=0)

load('fd_admg1/result_m1_AIPW.RData')
fd.admg1.m1.rate_matrix <- rate_matrix %>% mutate(m=1)

fd_admg1.pdata <- rbind(fd.admg1.m0.rate_matrix, fd.admg1.m1.rate_matrix) %>% mutate(m = factor(m))

p.fd_admg1 <- fd_admg1.pdata %>%
  ggplot( aes(x=n, y=TPR, group=m, color=m)) +
  geom_hline(yintercept = 0.95, color = "grey", linetype = "dashed") + # Add horizontal line
  geom_text(aes(x = min(n), y = 0.95, label = "0.95"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
  geom_line() +
  theme_bw() +
  ylab("True Positive Rate")+
  xlab("Sample Size") 

ggsave("fd_admg1/fd_admg1.pdf", plot=p.fd_admg1, width = 8, height = 6, units = "in")

## nonfd_admg1
load('nonfd_admg1/result_m0_AIPW.RData')
nonfd.admg1.m0.rate_matrix <- rate_matrix %>% mutate(m=0)

load('nonfd_admg1/result_m1_AIPW.RData')
nonfd.admg1.m1.rate_matrix <- rate_matrix %>% mutate(m=1)

nonfd.admg1.pdata <- rbind(nonfd.admg1.m0.rate_matrix, nonfd.admg1.m1.rate_matrix) %>% mutate(m = factor(m))

p.nonfd_admg1 <- nonfd.admg1.pdata %>%
  ggplot( aes(x=n, y=TPR, group=m, color=m)) +
  # geom_hline(yintercept = 0.95, color = "grey", linetype = "dashed") + # Add horizontal line
  # geom_text(aes(x = min(n), y = 0.95, label = "0.95"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
  geom_line() +
  theme_bw() +
  ylab("True Positive Rate")+
  xlab("Sample Size") 

ggsave("nonfd_admg1/nonfd_admg1.pdf", plot=p.nonfd_admg1, width = 8, height = 6, units = "in")

## nonfd_admg2
load('nonfd_admg2/result_m0_AIPW.RData')
nonfd.admg2.m0.rate_matrix <- rate_matrix %>% mutate(m=0)

load('nonfd_admg2/result_m1_AIPW.RData')
nonfd.admg2.m1.rate_matrix <- rate_matrix %>% mutate(m=1)

nonfd.admg2.pdata <- rbind(nonfd.admg2.m0.rate_matrix, nonfd.admg2.m1.rate_matrix) %>% mutate(m = factor(m))

p.nonfd_admg2 <- nonfd.admg2.pdata %>%
  ggplot( aes(x=n, y=TPR, group=m, color=m)) +
  geom_hline(yintercept = 0.95, color = "grey", linetype = "dashed") + # Add horizontal line
  geom_text(aes(x = min(n), y = 0.95, label = "0.95"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
  geom_line() +
  theme_bw() +
  ylab("True Positive Rate")+
  xlab("Sample Size") 

ggsave("nonfd_admg2/nonfd_admg2.pdf", plot=p.nonfd_admg2, width = 8, height = 6, units = "in")


## nonfd_admg3
load('nonfd_admg3/result_m0_AIPW.RData')
nonfd.admg3.m0.rate_matrix <- rate_matrix %>% mutate(m=0)

load('nonfd_admg3/result_m1_AIPW.RData')
nonfd.admg3.m1.rate_matrix <- rate_matrix %>% mutate(m=1)

nonfd.admg3.pdata <- rbind(nonfd.admg3.m0.rate_matrix, nonfd.admg3.m1.rate_matrix) %>% mutate(m = factor(m))

p.nonfd_admg3 <- nonfd.admg3.pdata %>%
  ggplot( aes(x=n, y=TPR, group=m, color=m)) +
  geom_hline(yintercept = 0.95, color = "grey", linetype = "dashed") + # Add horizontal line
  geom_text(aes(x = min(n), y = 0.95, label = "0.95"), vjust = -0.5, hjust = 0.5, color = "grey") +  # Add label
  geom_line() +
  theme_bw() +
  ylab("True Positive Rate")+
  xlab("Sample Size")

ggsave("nonfd_admg3/nonfd_admg3.pdf", plot=p.nonfd_admg3, width = 8, height = 6, units = "in")

