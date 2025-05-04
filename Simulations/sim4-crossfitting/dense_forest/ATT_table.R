library(here)
library(outliers) #containing function outlier
library(dplyr)

setwd(here('./sim4-crossfitting/dense_forest'))

# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3

######################################################################
############## Simulation 3: cross fitting - ranger ##############
######################################################################
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper")

# Binary M, Onestep, SL -----------------
load("binaryM/RF/Onestep-est1/ATT_result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.onestep.SL_", obj)
  assign(new_name, get(obj))
}

# Binary M, Onestep, CF -----------------
load("binaryM/CF/Onestep-est1/ATT_result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.onestep.CF_", obj)
  assign(new_name, get(obj))
}


# Binary M, Estimator1, SL-----------------
load("binaryM/RF/TMLE-est1/ATT_result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.est1.SL_", obj)
  assign(new_name, get(obj))
}

# Binary M, Estimator1, CF-----------------
load("binaryM/CF/TMLE-est1/ATT_result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.est1.CF_", obj)
  assign(new_name, get(obj))
}


# Continuous M, Onestep, SL-den-----------------
load("continuousM/RF/Onestep-est2a/ATT_result.Rdata")
for (obj in objs) {
  new_name <- paste0("continuous.onestep.SL.den_", obj)
  assign(new_name, get(obj))
}


# Continuous M, Onestep, SL-bayes-----------------
load("continuousM/RF/Onestep-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.SL.bayes_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Onestep, CF-den-----------------
load("continuousM/CF/Onestep-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.CF.den_", obj)
  assign(new_name, get(obj))
}


# Continuous M, Onestep, CF-bayes-----------------
load("continuousM/CF/Onestep-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.CF.bayes_", obj)
  assign(new_name, get(obj))
}


# Continuous M, Estimator2, SL-----------------
load("continuousM/RF/TMLE-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est2.SL_", obj)
  assign(new_name, get(obj))
}


# Continuous M, Estimator3, SL-----------------
load("continuousM/RF/TMLE-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est3.SL_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator2, CF-----------------
load("continuousM/CF/TMLE-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est2.CF_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator3, CF-----------------
load("continuousM/CF/TMLE-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est3.CF_", obj)
  assign(new_name, get(obj))
}



# binary est1, SL
binary.est1.SL.ate <- cbind(colMeans(binary.est1.SL_bias_matrix_ate[,n.ind]),apply(binary.est1.SL_bias_matrix_ate[,n.ind],2,sd),binary.est1.SL_avg.MSE_ate[n.ind,2]/n,binary.est1.SL_ci_coverage_ATE[n.ind,2],colMeans((binary.est1.SL_ci_matrix_ate_upper-binary.est1.SL_ci_matrix_ate_lower)[,n.ind]))

# binary est1, CF
binary.est1.CF.ate <- cbind(colMeans(binary.est1.CF_bias_matrix_ate[,n.ind]),apply(binary.est1.CF_bias_matrix_ate[,n.ind],2,sd),binary.est1.CF_avg.MSE_ate[n.ind,2]/n,binary.est1.CF_ci_coverage_ATE[n.ind,2],colMeans((binary.est1.CF_ci_matrix_ate_upper-binary.est1.CF_ci_matrix_ate_lower)[,n.ind]))


# continuous est2, SL
continuous.est2.SL.ate <- cbind(colMeans(continuous.est2.SL_bias_matrix_ate[,n.ind]),apply(continuous.est2.SL_bias_matrix_ate[,n.ind],2,sd),continuous.est2.SL_avg.MSE_ate[n.ind,2]/n,continuous.est2.SL_ci_coverage_ATE[n.ind,2],colMeans((continuous.est2.SL_ci_matrix_ate_upper-continuous.est2.SL_ci_matrix_ate_lower)[,n.ind]))


# continuous est3, SL
continuous.est3.SL.ate <- cbind(colMeans(continuous.est3.SL_bias_matrix_ate[,n.ind]),apply(continuous.est3.SL_bias_matrix_ate[,n.ind],2,sd),continuous.est3.SL_avg.MSE_ate[n.ind,2]/n,continuous.est3.SL_ci_coverage_ATE[n.ind,2],colMeans((continuous.est3.SL_ci_matrix_ate_upper-continuous.est3.SL_ci_matrix_ate_lower)[,n.ind]))

# continuous est2, CF
continuous.est2.CF.ate <- cbind(colMeans(continuous.est2.CF_bias_matrix_ate[,n.ind]),apply(continuous.est2.CF_bias_matrix_ate[,n.ind],2,sd),continuous.est2.CF_avg.MSE_ate[n.ind,2]/n,continuous.est2.CF_ci_coverage_ATE[n.ind,2],colMeans((continuous.est2.CF_ci_matrix_ate_upper-continuous.est2.CF_ci_matrix_ate_lower)[,n.ind]))

# continuous est3, CF
continuous.est3.CF.ate <- cbind(colMeans(continuous.est3.CF_bias_matrix_ate[,n.ind]),apply(continuous.est3.CF_bias_matrix_ate[,n.ind],2,sd),continuous.est3.CF_avg.MSE_ate[n.ind,2]/n,continuous.est3.CF_ci_coverage_ATE[n.ind,2],colMeans((continuous.est3.CF_ci_matrix_ate_upper-continuous.est3.CF_ci_matrix_ate_lower)[,n.ind]))


# binary onestep, SL
binary.onestep.SL.ate <- cbind(colMeans(binary.onestep.SL_bias_matrix_ate[,n.ind]),apply(binary.onestep.SL_bias_matrix_ate[,n.ind],2,sd),binary.onestep.SL_avg.MSE_ate[n.ind,2]/n,binary.onestep.SL_ci_coverage_ATE[n.ind,2],colMeans((binary.onestep.SL_ci_matrix_ate_upper-binary.onestep.SL_ci_matrix_ate_lower)[,n.ind]))

# binary onestep, CF
binary.onestep.CF.ate <- cbind(colMeans(binary.onestep.CF_bias_matrix_ate[,n.ind]),apply(binary.onestep.CF_bias_matrix_ate[,n.ind],2,sd),binary.onestep.CF_avg.MSE_ate[n.ind,2]/n,binary.onestep.CF_ci_coverage_ATE[n.ind,2],colMeans((binary.onestep.CF_ci_matrix_ate_upper-binary.onestep.CF_ci_matrix_ate_lower)[,n.ind]))


# continuous onestep, SL-densratio
continuous.onestep.SL.den.ate <- cbind(colMeans(continuous.onestep.SL.den_bias_matrix_ate[,n.ind]),apply(continuous.onestep.SL.den_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.SL.den_avg.MSE_ate[n.ind,2]/n,continuous.onestep.SL.den_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.SL.den_ci_matrix_ate_upper-continuous.onestep.SL.den_ci_matrix_ate_lower)[,n.ind]))


# continuous onestep, SL-bayes
continuous.onestep.SL.bayes.ate <- cbind(colMeans(continuous.onestep.SL.bayes_bias_matrix_ate[,n.ind]),apply(continuous.onestep.SL.bayes_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.SL.bayes_avg.MSE_ate[n.ind,2]/n,continuous.onestep.SL.bayes_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.SL.bayes_ci_matrix_ate_upper-continuous.onestep.SL.bayes_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep, CF-densratio
continuous.onestep.CF.den.ate <- cbind(colMeans(continuous.onestep.CF.den_bias_matrix_ate[,n.ind]),apply(continuous.onestep.CF.den_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.CF.den_avg.MSE_ate[n.ind,2]/n,continuous.onestep.CF.den_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.CF.den_ci_matrix_ate_upper-continuous.onestep.CF.den_ci_matrix_ate_lower)[,n.ind]))


# continuous onestep, CF-bayes
continuous.onestep.CF.bayes.ate <- cbind(colMeans(continuous.onestep.CF.bayes_bias_matrix_ate[,n.ind]),apply(continuous.onestep.CF.bayes_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.CF.bayes_avg.MSE_ate[n.ind,2]/n,continuous.onestep.CF.bayes_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.CF.bayes_ci_matrix_ate_upper-continuous.onestep.CF.bayes_ci_matrix_ate_lower)[,n.ind]))

# 
# dat <- list(binary.est1.nonSL.ate,binary.est1.SL.ate,binary.est1.CF.ate,continuous.est2.nonSL.ate,continuous.est2.SL.ate,continuous.est2.CF.ate,continuous.est3.nonSL.ate,continuous.est3.SL.ate,continuous.est3.CF.ate,binary.onestep.nonSL.ate,binary.onestep.SL.ate,binary.onestep.CF.ate,continuous.onestep.nonSL.den.ate,continuous.onestep.SL.den.ate,continuous.onestep.CF.den.ate,continuous.onestep.nonSL.bayes.ate,continuous.onestep.SL.bayes.ate,continuous.onestep.CF.bayes.ate)

# remove the Linear Reg
dat <- list(binary.est1.SL.ate,binary.est1.CF.ate,continuous.est2.SL.ate,continuous.est2.CF.ate,continuous.est3.SL.ate,continuous.est3.CF.ate,binary.onestep.SL.ate,binary.onestep.CF.ate,
            continuous.onestep.SL.den.ate,continuous.onestep.CF.den.ate,continuous.onestep.SL.bayes.ate,continuous.onestep.CF.bayes.ate)


for (i in seq_along(n)) {
  # Extract the column from each data frame and cbind
  columns <- lapply(dat,function(x) x[i,])  # Extract the first column from each data frame
  assign(paste0("dt",i),round(do.call(cbind, columns),decimal))   # Combine the columns using cbind
  
  df <- get(paste0("dt",i))  # Get the data frame by name
  df.char <- trimws(format(df,nsmall=decimal))
  
  df[4, ] <- paste0(df[4, ] * 100, "%")  # Modify the value in the data frame
  
  df.char[4,] <- df[4,]
  assign(paste0("dt",i), df.char)  # Update the data frame with the modified value
  
}

dat <- rbind(dt1,dt2,dt3)

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(dat) %>% 
  insert_row("TMLEs","","","","","","One-step estimators","","","","","", after = 0) %>% 
  merge_cells(1, 1:6) %>% 
  merge_cells(1, 7:12) %>% 
  insert_row("Univariate Binary","","Univariate Continuous","","","","Univariate Binary","","Univariate Continuous","","","", after = 1) %>% 
  merge_cells(2, 1:2) %>%
  merge_cells(2, 3:6) %>%
  merge_cells(2, 7:8) %>%
  merge_cells(2, 9:12) %>%
  insert_row("\\(\\psi_1(\\hat{Q}^\\star)\\)","","\\(\\psi_{2a}(\\hat{Q}^\\star)\\)","","\\(\\psi_{2b}(\\hat{Q}^\\star)\\)","","\\(\\psi_1^{+}(\\hat{Q})\\)","","\\(\\psi_{2a}^{+}(\\hat{Q})\\)","","\\(\\psi_{2b}^{+}(\\hat{Q})\\)","", after = 2) %>% 
  insert_row("RF","CF","RF","CF","RF","CF","RF","CF","RF","CF","RF","CF", after = 3) %>% 
  merge_cells(3, 1:2) %>%
  merge_cells(3, 3:4) %>%
  merge_cells(3, 5:6) %>%
  merge_cells(3, 7:8) %>%
  merge_cells(3, 9:10) %>%
  merge_cells(3, 11:12) %>%
  insert_row("","","","","","","","","","","","", after = 4) %>% 
  insert_row("","","","","","","","","","","","", after = 10) %>% 
  insert_row("","","","","","","","","","","","", after = 16) %>% 
  insert_column(c("","","","","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(3, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(0)  %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(5,11,17), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 4, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(5:nrow(.), 3, brdr(0.6,"double")) %>%
  set_right_border(5:nrow(.), 5, brdr(0.4,"solid")) %>%
  set_right_border(5:nrow(.), 7, brdr(0.6,"double")) %>%
  set_right_border(5:nrow(.), 9, brdr(0.6,"double")) %>%
  set_right_border(5:nrow(.), 11, brdr(0.4,"solid")) %>%
  # insert_row(c("Note: Number of simulations=1000. \\(\\psi^{1,2}(\\hat{Q}^*)\\) refer to TMLE estimators and \\(\\psi^{1,2}_{+}(\\hat{Q})\\) refer to the corresponding Onestep estimators. Lin. Reg refers to linear regression, RF refers to random forest with 50 trees and a minimum node size of 1, and CR denotes random forest with cross fitting using 5 folds.","","","","","","","","","","","","","","","","","",""), after = nrow(.)) %>%
  # merge_cells(nrow(.),1:ncol(.)) %>% 
  # set_row_height(everywhere, rep(0.02/nrow(.),nrow(.))) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(6.5) %>% set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% 
  set_caption("Comparative analysis for the impact of cross-fitting on TMLEs and one-step estimators in conjunction with the use of random forests.
RF refers to random forest with 500 trees and a minimum node size of 5 for a continuous
variable and 1 for binary, and CF denotes random forest with cross fitting using 5 folds.") %>% set_col_width(1/14*c(2,rep(1,12)))

table1
quick_latex(table1, file='ATT_table.tex')

