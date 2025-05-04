library(here)
library(outliers) #containing function outlier
library(dplyr)

setwd(here('./sim2-weak-overlap'))

# sample size
n= c(500,1000,2000)
n.vec <- c(500,1000,2000)
n.ind <- which(n.vec %in% n)

# round the numbers
decimal <- 3
nsim <- 1000
######################################################################
############## Simulation 1: positivity violation ##############
######################################################################

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

# Continuous M, Onestep-np-----------------
load("continuousM/Onestep-est1/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.onestep.np_", obj)
  assign(new_name, get(obj))
}

# Continuous M, Estimator1-----------------
load("continuousM/TMLE-est1/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est1_", obj)
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

# Continuous M, Estimator3-----------------
load("continuousM/TMLE-est2b/ATT_result.Rdata")

for (obj in objs) {
  new_name <- paste0("continuous.est3_", obj)
  assign(new_name, get(obj))
}

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

# binary est1
binary.est1.ate <- cbind(colMeans(binary.est1_bias_matrix_ate[,n.ind]),apply(binary.est1_bias_matrix_ate[,n.ind],2,sd),binary.est1_avg.MSE_ate[n.ind,2]/n,binary.est1_ci_coverage_ATE[n.ind,2],colMeans((binary.est1_ci_matrix_ate_upper-binary.est1_ci_matrix_ate_lower)[,n.ind]))

# continuous est1 <----------------------- remove one outlier one MSE
# load("/Users/apple/Library/CloudStorage/Dropbox/Front-door_Anna/code/functions/1-truth-continuous-v1.Rdata")
# bias.ate <- continuous.est1_bias_matrix_Y1[,n.ind]-continuous.est1_bias_matrix_Y0[,n.ind]
# bias.ate <- bias.ate[,1]
# continuous.est1.MSE.n1 <- mean(bias.ate[!(bias.ate %in% head(sort(bias.ate),2))]^2)
# continuous.est1.bias.n1 <- mean(bias.ate[!(bias.ate %in% head(sort(bias.ate),2))])
# continuous.est1.sd.n1 <- sd(bias.ate[!(bias.ate %in% head(sort(bias.ate),2))])
# 
# index <- which(bias.ate %in% head(sort(bias.ate),2))
# continuous.est1.ci_coveraage.n1 <- mean((continuous.est1_ci_matrix_ate_lower[(1:nsim)[-index],1] < ATE) & (continuous.est1_ci_matrix_ate_upper[(1:nsim)[-index],1] > ATE))
# continuous.est1.ci_width.n1 <- mean(continuous.est1_ci_matrix_ate_upper[(1:nsim)[-index],1]-continuous.est1_ci_matrix_ate_lower[(1:nsim)[-index],1])
# 
# continuous.est1.ate <- cbind(c(continuous.est1.bias.n1,colMeans(continuous.est1_bias_matrix_ate[,n.ind])[-1]),c(continuous.est1.sd.n1,apply(continuous.est1_bias_matrix_ate[,n.ind],2,sd)[-1]),
#                              c(continuous.est1.MSE.n1,(continuous.est1_avg.MSE_ate[n.ind,2]/n)[-1]),
#                              c(continuous.est1.ci_coveraage.n1,continuous.est1_ci_coverage_ATE[n.ind,2][-1]),
#                              c(continuous.est1.ci_width.n1,colMeans((continuous.est1_ci_matrix_ate_upper-continuous.est1_ci_matrix_ate_lower)[,n.ind])[-1]))

continuous.est1.ate <- cbind(colMeans(continuous.est1_bias_matrix_ate[,n.ind]),apply(continuous.est1_bias_matrix_ate[,n.ind],2,sd),continuous.est1_avg.MSE_ate[n.ind,2]/n,continuous.est1_ci_coverage_ATE[n.ind,2],colMeans((continuous.est1_ci_matrix_ate_upper-continuous.est1_ci_matrix_ate_lower)[,n.ind]))


# continuous est2
continuous.est2.ate <- cbind(colMeans(continuous.est2_bias_matrix_ate[,n.ind]),apply(continuous.est2_bias_matrix_ate[,n.ind],2,sd),continuous.est2_avg.MSE_ate[n.ind,2]/n,continuous.est2_ci_coverage_ATE[n.ind,2],colMeans((continuous.est2_ci_matrix_ate_upper-continuous.est2_ci_matrix_ate_lower)[,n.ind]))


# continuous est3
continuous.est3.ate <- cbind(colMeans(continuous.est3_bias_matrix_ate[,n.ind]),apply(continuous.est3_bias_matrix_ate[,n.ind],2,sd),continuous.est3_avg.MSE_ate[n.ind,2]/n,continuous.est3_ci_coverage_ATE[n.ind,2],colMeans((continuous.est3_ci_matrix_ate_upper-continuous.est3_ci_matrix_ate_lower)[,n.ind]))

# multi est2
multi.est2.ate <- cbind(colMeans(multi.est2_bias_matrix_ate[,n.ind]),apply(multi.est2_bias_matrix_ate[,n.ind],2,sd),multi.est2_avg.MSE_ate[n.ind,2]/n,multi.est2_ci_coverage_ATE[n.ind,2],colMeans((multi.est2_ci_matrix_ate_upper-multi.est2_ci_matrix_ate_lower)[,n.ind]))


# multi est3
# ci.ate <- (multi.est3_ci_matrix_ate_upper-multi.est3_ci_matrix_ate_lower)[,n.ind]
# ci.ate <- ci.ate[,1]
# multi.est3.ci.n1 <- mean(ci.ate[!(ci.ate %in% c(outlier(ci.ate)))]^2)

multi.est3.ate <- cbind(colMeans(multi.est3_bias_matrix_ate[,n.ind]),apply(multi.est3_bias_matrix_ate[,n.ind],2,sd),multi.est3_avg.MSE_ate[n.ind,2]/n,multi.est3_ci_coverage_ATE[n.ind,2],colMeans((multi.est3_ci_matrix_ate_upper-multi.est3_ci_matrix_ate_lower)[,n.ind]))


# binary onestep
binary.onestep.ate <- cbind(colMeans(binary.onestep_bias_matrix_ate[,n.ind]),apply(binary.onestep_bias_matrix_ate[,n.ind],2,sd),binary.onestep_avg.MSE_ate[n.ind,2]/n,binary.onestep_ci_coverage_ATE[n.ind,2],colMeans((binary.onestep_ci_matrix_ate_upper-binary.onestep_ci_matrix_ate_lower)[,n.ind]))


# continuous onestep-np
continuous.onestep.np.ate <- cbind(colMeans(continuous.onestep.np_bias_matrix_ate[,n.ind]),apply(continuous.onestep.np_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.np_avg.MSE_ate[n.ind,2]/n,continuous.onestep.np_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.np_ci_matrix_ate_upper-continuous.onestep.np_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-den
continuous.onestep.den.ate <- cbind(colMeans(continuous.onestep.den_bias_matrix_ate[,n.ind]),apply(continuous.onestep.den_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.den_avg.MSE_ate[n.ind,2]/n,continuous.onestep.den_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.den_ci_matrix_ate_upper-continuous.onestep.den_ci_matrix_ate_lower)[,n.ind]))

# continuous onestep-bayes
continuous.onestep.bayes.ate <- cbind(colMeans(continuous.onestep.bayes_bias_matrix_ate[,n.ind]),apply(continuous.onestep.bayes_bias_matrix_ate[,n.ind],2,sd),continuous.onestep.bayes_avg.MSE_ate[n.ind,2]/n,continuous.onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((continuous.onestep.bayes_ci_matrix_ate_upper-continuous.onestep.bayes_ci_matrix_ate_lower)[,n.ind]))


# multi onestep-den
multi.onestep.den.ate <- cbind(colMeans(multi.onestep.den_bias_matrix_ate[,n.ind]),apply(multi.onestep.den_bias_matrix_ate[,n.ind],2,sd),multi.onestep.den_avg.MSE_ate[n.ind,2]/n,multi.onestep.den_ci_coverage_ATE[n.ind,2],colMeans((multi.onestep.den_ci_matrix_ate_upper-multi.onestep.den_ci_matrix_ate_lower)[,n.ind]))


# multi onestep-bayes
multi.onestep.bayes.ate <- cbind(colMeans(multi.onestep.bayes_bias_matrix_ate[,n.ind]),apply(multi.onestep.bayes_bias_matrix_ate[,n.ind],2,sd),multi.onestep.bayes_avg.MSE_ate[n.ind,2]/n,multi.onestep.bayes_ci_coverage_ATE[n.ind,2],colMeans((multi.onestep.bayes_ci_matrix_ate_upper-multi.onestep.bayes_ci_matrix_ate_lower)[,n.ind]))


positivity.dat <- list(binary.est1.ate,binary.onestep.ate,continuous.est1.ate,continuous.onestep.np.ate,continuous.est2.ate,continuous.onestep.den.ate,continuous.est3.ate,
                       continuous.onestep.bayes.ate,multi.est2.ate,multi.onestep.den.ate,multi.est3.ate,multi.onestep.bayes.ate)

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
# dat[is.numeric(dat)] <- sprintf("%s.3f",dat[is.numeric(dat)])

# make table
library(huxtable)
library(dplyr)
options(scipen = 999)

table1 <- as_hux(dat) %>% 
  insert_row("Univariate Binary","","Univariate Continuous","","","","","","Bivariate Continuous","","","", after = 0) %>% 
  merge_cells(1, 1:2) %>% 
  merge_cells(1, 3:8) %>% 
  merge_cells(1, 9:12) %>% 
  insert_row("\\(\\psi_1(\\hat{Q}^\\star)\\)","\\(\\psi_1^{+}(\\hat{Q})\\)","\\(\\psi_1(\\hat{Q}^\\star)\\)","\\(\\psi_1^{+}(\\hat{Q})\\)","\\(\\psi_{2a}(\\hat{Q}^\\star)\\)","\\(\\psi_{2a}^{+}(\\hat{Q})\\)","\\(\\psi_{2b}(\\hat{Q}^\\star)\\)","\\(\\psi_{2b}^{+}(\\hat{Q})\\)","\\(\\psi_{2a}(\\hat{Q}^\\star)\\)","\\(\\psi_{2a}^{+}(\\hat{Q})\\)","\\(\\psi_{2b}(\\hat{Q}^\\star)\\)","\\(\\psi_{2b}^{+}(\\hat{Q})\\)", after = 1) %>% 
  insert_row("","","","","","","","","","","","",after=2) %>%
  insert_row("","","","","","","","","","","","",after=8) %>%
  insert_row("","","","","","","","","","","","",after=14) %>%
  insert_column(c("","","n=500","Bias","SD","MSE","CI coverage","CI width","n=1000","Bias","SD","MSE","CI coverage","CI width","n=2000","Bias","SD","MSE","CI coverage","CI width"), after = 0) %>%
  set_escape_contents(2, 1:ncol(.), FALSE) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_tb_padding(1, everywhere, 0) %>% 
  set_bold(1, everywhere) %>%
  set_bold(c(3,9,15), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_right_border(3:nrow(.), 3, brdr(0.4,"double")) %>%
  set_right_border(3:nrow(.), 5, brdr(0.4,"dotted")) %>%
  set_right_border(3:nrow(.), 7, brdr(0.4,"dotted")) %>%
  set_right_border(3:nrow(.), 9, brdr(0.4,"double")) %>%
  set_right_border(3:nrow(.), 11, brdr(0.4,"dotted")) %>%
  # insert_row(paste0("Note: Number of simulations=1000. \\(\\psi^{1,2,3}(\\hat{Q}^*)\\) refer to TMLE estimators and \\(\\psi^{1,2,3}_{+}(\\hat{Q})\\) refer to the corresponding onestep estimators."),"","","","","","","","","","","","", after = nrow(.)) %>%
  # merge_cells(nrow(.),1:ncol(.)) %>% 
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% set_caption("Comparison between TMLE estimators and one-step EIF estimator under positivity assumption violation with binary, continuous, and multivariate mediators.") %>%
  set_all_padding(1) %>% set_font_size(6) 

table1
quick_latex(table1, file='ATT_table.tex')


