setwd("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/TMLE-Front-Door/Simulations/sim5-sensitivity/binaryY-saturated")

library(tidyr)
library(ggplot2)

load("results.RData")

orig.result.long <- orig.result %>%
  pivot_longer(cols = -n,    # All columns except sample size n
               names_to = c("method", "statistics"), 
               names_sep = "_", 
               values_to = "value") 


p.orig <- ggplot(orig.result.long, aes(x = n, y = value, linetype = statistics, color = method)) + 
  geom_line() +
  labs(x = "Sample Size", y = "Value") +
  theme_minimal()

ggsave("orig.pdf", plot=p.orig, width = 8, height = 6, units = "in")




