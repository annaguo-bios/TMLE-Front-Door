library(here)
library(outliers) #containing function outlier
library(dplyr)

setwd(here('./sim7-nonlinearTMLE'))

# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3
nsim <- 1000
#########################################
############## Simulation 7##############
#########################################

## Binary M ==
# Binary M, Onestep-----------------

load("binaryM/Onestep-est1/ATT_result.Rdata")
objs <- c("ci_coverage_ATE","avg.MSE_ate","bias_matrix_ate","ci_matrix_ate_lower","ci_matrix_ate_upper","bias_matrix_Y1","bias_matrix_Y0")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.onestep_", obj)
  assign(new_name, get(obj))
}

# Binary M, Estimator1-----------------
load("binaryM/TMLE-est1/ATT_result.Rdata")

# Add the prefix
for (obj in objs) {
  new_name <- paste0("binary.est1_", obj)
  assign(new_name, get(obj))
}

## Continuous M ==

# Continuous M, Onestep-np-----------------
load("continuousM/Onestep-est2-dnorm/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator1-----------------
load("continuousM/TMLE-est2-dnorm/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est2.dnorm_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Onestep-densratio-----------------
load("continuousM/Onestep-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.den_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator2-----------------
load("continuousM/TMLE-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est2_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Onestep-bayes-----------------
load("continuousM/Onestep-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.bayes_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator2b-----------------
load("continuousM/TMLE-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est3_", obj)
  assign(new_name, get(obj))
}

## Multi ==

# Multivariate M, Onestep-densratio
load("multiM-d2/Onestep-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.onestep.den_", obj)
  assign(new_name, get(obj))
}


# Multivariate M, Estimator2
load("multiM-d2/TMLE-est2a/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.est2_", obj)
  assign(new_name, get(obj))
}

# Multivariate M, Onestep-bayes
load("multiM-d2/Onestep-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.onestep.bayes_", obj)
  assign(new_name, get(obj))
}

# Multivariate M, Estimator3
load("multiM-d2/TMLE-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.est3_", obj)
  assign(new_name, get(obj))
}

# Multivariate M, Onestep-bayes
load("multiM-d2/Onestep-est2-dnorm/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.onestep.dnorm_", obj)
  assign(new_name, get(obj))
}

# Multivariate M, Estimator3
load("multiM-d2/TMLE-est2-dnorm/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("multi.est2.dnorm_", obj)
  assign(new_name, get(obj))
}

# binary est1
binary.est1.ate <- cbind(colMeans(binary.est1_bias_matrix_ate[,n.ind]),apply(binary.est1_bias_matrix_ate[,n.ind],2,sd),binary.est1_avg.MSE_ate[n.ind,2]/n,binary.est1_ci_coverage_ATE[n.ind,2],colMeans((binary.est1_ci_matrix_ate_upper-binary.est1_ci_matrix_ate_lower)[,n.ind]))


continuous.est.dnorm.ate <- cbind(colMeans(continuous.est2.dnorm_bias_matrix_ate[,n.ind]),apply(continuous.est2.dnorm_bias_matrix_ate[,n.ind],2,sd),continuous.est2.dnorm_avg.MSE_ate[n.ind,2]/n,continuous.est2.dnorm_ci_coverage_ATE[n.ind,2],colMeans((continuous.est2.dnorm_ci_matrix_ate_upper-continuous.est2.dnorm_ci_matrix_ate_lower)[,n.ind]))


# continuous est2
continuous.est2.ate <- cbind(colMeans(continuous.est2_bias_matrix_ate[,n.ind]),apply(continuous.est2_bias_matrix_ate[,n.ind],2,sd),continuous.est2_avg.MSE_ate[n.ind,2]/n,continuous.est2_ci_coverage_ATE[n.ind,2],colMeans((continuous.est2_ci_matrix_ate_upper-continuous.est2_ci_matrix_ate_lower)[,n.ind]))


# continuous est3
continuous.est3.ate <- cbind(colMeans(continuous.est3_bias_matrix_ate[,n.ind]),apply(continuous.est3_bias_matrix_ate[,n.ind],2,sd),continuous.est3_avg.MSE_ate[n.ind,2]/n,continuous.est3_ci_coverage_ATE[n.ind,2],colMeans((continuous.est3_ci_matrix_ate_upper-continuous.est3_ci_matrix_ate_lower)[,n.ind]))

# multi est2
multi.est2.ate <- cbind(colMeans(multi.est2_bias_matrix_ate[,n.ind]),apply(multi.est2_bias_matrix_ate[,n.ind],2,sd),multi.est2_avg.MSE_ate[n.ind,2]/n,multi.est2_ci_coverage_ATE[n.ind,2],colMeans((multi.est2_ci_matrix_ate_upper-multi.est2_ci_matrix_ate_lower)[,n.ind]))

multi.est3.ate <- cbind(colMeans(multi.est3_bias_matrix_ate[,n.ind]),apply(multi.est3_bias_matrix_ate[,n.ind],2,sd),multi.est3_avg.MSE_ate[n.ind,2]/n,multi.est3_ci_coverage_ATE[n.ind,2],colMeans((multi.est3_ci_matrix_ate_upper-multi.est3_ci_matrix_ate_lower)[,n.ind]))

multi.est2.dnorm.ate <- cbind(colMeans(multi.est2.dnorm_bias_matrix_ate[,n.ind]),apply(multi.est2.dnorm_bias_matrix_ate[,n.ind],2,sd),multi.est2.dnorm_avg.MSE_ate[n.ind,2]/n,multi.est2.dnorm_ci_coverage_ATE[n.ind,2],colMeans((multi.est2.dnorm_ci_matrix_ate_upper-multi.est2.dnorm_ci_matrix_ate_lower)[,n.ind]))

# binary onestep
binary.onestep.ate <- cbind(colMeans(binary.onestep_bias_matrix_ate[,n.ind]),apply(binary.onestep_bias_matrix_ate[,n.ind],2,sd),binary.onestep_avg.MSE_ate[n.ind,2]/n,binary.onestep_ci_coverage_ATE[n.ind,2],colMeans((binary.onestep_ci_matrix_ate_upper-binary.onestep_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-dnorm
continuous.onestep.dnorm.ate <- cbind(colMeans(continuous.onestep.dnorm_bias_matrix_ate[,n.ind]),apply(continuous.onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.dnorm_avg.MSE_ate[n.ind,2]/n,continuous.onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.dnorm_ci_matrix_ate_upper-continuous.onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-den
continuous.onestep.den.ate <- cbind(colMeans(continuous.onestep.den_bias_matrix_ate[,n.ind]),apply(continuous.onestep.den_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.den_avg.MSE_ate[n.ind,2]/n,continuous.onestep.den_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.den_ci_matrix_ate_upper-continuous.onestep.den_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-bayes
continuous.onestep.bayes.ate <- cbind(colMeans(continuous.onestep.bayes_bias_matrix_ate[,n.ind]),apply(continuous.onestep.bayes_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.bayes_avg.MSE_ate[n.ind,2]/n,continuous.onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.bayes_ci_matrix_ate_upper-continuous.onestep.bayes_ci_matrix_ate_lower)[,n.ind]))


# multi onestep-den
multi.onestep.den.ate <- cbind(colMeans(multi.onestep.den_bias_matrix_ate[,n.ind]),apply(multi.onestep.den_bias_matrix_ate[,n.ind],2,sd),multi.onestep.den_avg.MSE_ate[n.ind,2]/n,multi.onestep.den_ci_coverage_ATE[n.ind,2],colMeans((multi.onestep.den_ci_matrix_ate_upper-multi.onestep.den_ci_matrix_ate_lower)[,n.ind]))


# multi onestep-bayes
multi.onestep.bayes.ate <- cbind(colMeans(multi.onestep.bayes_bias_matrix_ate[,n.ind]),apply(multi.onestep.bayes_bias_matrix_ate[,n.ind],2,sd),multi.onestep.bayes_avg.MSE_ate[n.ind,2]/n,multi.onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((multi.onestep.bayes_ci_matrix_ate_upper-multi.onestep.bayes_ci_matrix_ate_lower)[,n.ind]))

# multi onestep-dnorm
multi.onestep.dnorm.ate <- cbind(colMeans(multi.onestep.dnorm_bias_matrix_ate[,n.ind]),apply(multi.onestep.dnorm_bias_matrix_ate[,n.ind],2,sd),multi.onestep.dnorm_avg.MSE_ate[n.ind,2]/n,multi.onestep.dnorm_ci_coverage_ATE[n.ind,2],colMeans((multi.onestep.dnorm_ci_matrix_ate_upper-multi.onestep.dnorm_ci_matrix_ate_lower)[,n.ind]))


positivity.dat <- list(binary.est1.ate,binary.onestep.ate,
                       continuous.est.dnorm.ate,continuous.onestep.dnorm.ate,continuous.est2.ate,continuous.onestep.den.ate,continuous.est3.ate,continuous.onestep.bayes.ate,
                       multi.est2.dnorm.ate,multi.onestep.dnorm.ate,multi.est2.ate,multi.onestep.den.ate,multi.est3.ate,multi.onestep.bayes.ate)

for (i in seq_along(n)) {
  # Extract the column from each data frame and cbind
  columns <- lapply(positivity.dat,function(x) x[i,])  # Extract the first column from each data frame
  assign(paste0("dt",i),round(do.call(cbind, columns),decimal))   # Combine the columns using cbind
  
  # turn df into character table to keep decimal place 
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
  insert_row("Univariate Binary","","Univariate Continuous","","","","","","Bivariate Continuous","","","","","", after = 0) %>% 
  merge_cells(1, 1:2) %>% 
  merge_cells(1, 3:8) %>% 
  merge_cells(1, 9:14) %>% 
  insert_row("\\(\\psi_1(\\hat{Q}^\\star)\\)","","\\(\\psi_2(\\hat{Q}^\\star)-\\text{dnorm}\\)","","\\(\\psi_{2a}(\\hat{Q}^\\star)\\)","","\\(\\psi_{2b}(\\hat{Q}^\\star)\\)","",
             "\\(\\psi_{2}(\\hat{Q}^\\star)-\\text{dnorm}\\)","","\\(\\psi_{2a}(\\hat{Q}^\\star)\\)","","\\(\\psi_{2b}(\\hat{Q}^\\star)\\)","", after = 1) %>% 
  merge_cells(2, 1:2) %>% 
  merge_cells(2, 3:4) %>% 
  merge_cells(2, 5:6) %>% 
  merge_cells(2, 7:8) %>% 
  merge_cells(2, 9:10) %>% 
  merge_cells(2, 11:12) %>% 
  merge_cells(2, 13:14) %>% 
  insert_row("Linear","Logit","Linear","Logit","Linear","Logit","Linear","Logit","Linear","Logit","Linear","Logit","Linear","Logit",after=2) %>%
  insert_column(c("","","Submodels","Bias","SD","MSE","Coverage","CI width","Bias","SD","MSE","Coverage","CI width","Bias","SD","MSE","Coverage","CI width"), after = 0) %>%
  insert_column(c("","","","n=500","","","","","n=1000","","","","","n=2000","","","",""), after = 0) %>%
  merge_cells(4:8, 1) %>% 
  merge_cells(9:13, 1) %>% 
  merge_cells(14:18, 1) %>%
  set_valign(c(4,9,14), col=1,"middle") %>% 
  set_rotation(c(4,9,14), 1, 90) %>% 
  set_number_format(everywhere,everywhere, "%s") %>% 
  set_escape_contents(2, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_tb_padding(1, everywhere, 0) %>% 
  set_bold(everywhere, 1) %>%
  set_bold(1, everywhere) %>%
  set_italic(2,everywhere) %>%
  set_italic(3,2) %>%
  set_bottom_border(row = 1, col =3:ncol(.)) %>% 
  set_bottom_border(row = 2, col =3:ncol(.)) %>% 
  set_bottom_border(row = 3, col =3:ncol(.)) %>% 
  set_right_border(4:nrow(.), 4, brdr(0.4,"double")) %>%
  set_right_border(4:nrow(.), 6, brdr(0.4,"dotted")) %>%
  set_right_border(4:nrow(.), 8, brdr(0.4,"dotted")) %>%
  set_right_border(4:nrow(.), 10, brdr(0.4,"double")) %>%
  set_right_border(4:nrow(.), 12, brdr(0.4,"dotted")) %>%
  set_right_border(4:nrow(.), 14, brdr(0.4,"dotted")) %>%
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% set_caption("Comparison between TMLEs for ATT using linear and logit submodels across mediator types.") %>%
  set_all_padding(1) %>% set_font_size(6) 

table1
quick_latex(table1,file = "ATT_table.tex")


