
set1.TMLE.dim.est <- rep(NA,10); set1.TMLE.dim.var <- rep(NA,10)
set1.TMLE.piie.a1.est <- rep(NA,10); set1.TMLE.piie.a1.var <- rep(NA,10)
set1.TMLE.piie.a0.est <- rep(NA,10); set1.TMLE.piie.a0.var <- rep(NA,10)
set1.TMLE.lor.est <- rep(NA,10); set1.TMLE.lor.var <- rep(NA,10)
set1.onestep.dim.est <- rep(NA,10); set1.onestep.dim.var <- rep(NA,10)
set1.onestep.piie.a1.est <- rep(NA,10); set1.onestep.piie.a1.var <- rep(NA,10)
set1.onestep.piie.a0.est <- rep(NA,10); set1.onestep.piie.a0.var <- rep(NA,10)
set1.onestep.lor.est <- rep(NA,10); set1.onestep.lor.var <- rep(NA,10)

TMLE.PMF.Ya1 <- data.frame(matrix(ncol = 8, nrow = 10)); colnames(TMLE.PMF.Ya1) <- c("m", "E.Y0", "E.Y1", "E.Y2", "E.Y3", "E.Y4", "E.Y5", "E.Y6")
TMLE.PMF.Ya0 <- data.frame(matrix(ncol = 8, nrow = 10)); colnames(TMLE.PMF.Ya0) <- c("m", "E.Y0", "E.Y1", "E.Y2", "E.Y3", "E.Y4", "E.Y5", "E.Y6")
onestep.PMF.Ya1 <- data.frame(matrix(ncol = 8, nrow = 10)); colnames(onestep.PMF.Ya1) <- c("m", "E.Y0", "E.Y1", "E.Y2", "E.Y3", "E.Y4", "E.Y5", "E.Y6")
onestep.PMF.Ya0 <- data.frame(matrix(ncol = 8, nrow = 10)); colnames(onestep.PMF.Ya0) <- c("m", "E.Y0", "E.Y1", "E.Y2", "E.Y3", "E.Y4", "E.Y5", "E.Y6")

set.seed(7)
for (m in 1:10){
  
  load(paste0('output/output_', m, '.RData'))
  
  set1.TMLE.dim.est[m] <- set1.TMLE.dim.est_m
  set1.TMLE.piie.a1.est[m] <- set1.TMLE.piie.a1.est_m
  set1.TMLE.piie.a0.est[m] <- set1.TMLE.piie.a0.est_m
  set1.TMLE.lor.est[m] <- set1.TMLE.lor.est_m
  
  set1.onestep.dim.est[m] <- set1.onestep.dim.est_m
  set1.onestep.piie.a1.est[m] <- set1.onestep.piie.a1.est_m
  set1.onestep.piie.a0.est[m] <- set1.onestep.piie.a0.est_m
  set1.onestep.lor.est[m] <- set1.onestep.lor.est_m
  
  set1.TMLE.dim.var[m] <- set1.TMLE.dim.var_m
  set1.TMLE.piie.a1.var[m] <- set1.TMLE.piie.a1.var_m
  set1.TMLE.piie.a0.var[m] <- set1.TMLE.piie.a0.var_m
  
  set1.onestep.dim.var[m] <- set1.onestep.dim.var_m
  set1.onestep.piie.a1.var[m] <- set1.onestep.piie.a1.var_m
  set1.onestep.piie.a0.var[m] <- set1.onestep.piie.a0.var_m
  
  TMLE.PMF.Ya1[m,] <- TMLE.PMF.Ya1_m
  TMLE.PMF.Ya0[m,] <- TMLE.PMF.Ya0_m
  onestep.PMF.Ya1[m,] <- onestep.PMF.Ya1_m
  onestep.PMF.Ya0[m,] <- onestep.PMF.Ya0_m
  
}

set1.TMLE.dim.mean <- mean(set1.TMLE.dim.est)
set1.onestep.dim.mean <- mean(set1.onestep.dim.est)

set1.TMLE.piie.a1.mean <- mean(set1.TMLE.piie.a1.est)
set1.TMLE.piie.a0.mean <- mean(set1.TMLE.piie.a0.est)
set1.onestep.piie.a1.mean <- mean(set1.onestep.piie.a1.est)
set1.onestep.piie.a0.mean <- mean(set1.onestep.piie.a0.est)

set1.TMLE.lor.mean <- mean(set1.TMLE.lor.est)
set1.onestep.lor.mean <- mean(set1.onestep.lor.est)

set1.TMLE.dim.imp.var <- mean(set1.TMLE.dim.var) + sum((set1.TMLE.dim.est - set1.TMLE.dim.mean)^2)/9
set1.onestep.dim.imp.var <- mean(set1.onestep.dim.var) + sum((set1.onestep.dim.est - set1.onestep.dim.mean)^2)/9
set1.TMLE.piie.a1.imp.var <- mean(set1.TMLE.piie.a1.var) + sum((set1.TMLE.piie.a1.est - set1.TMLE.piie.a1.mean)^2)/9
set1.TMLE.piie.a0.imp.var <- mean(set1.TMLE.piie.a0.var) + sum((set1.TMLE.piie.a0.est - set1.TMLE.piie.a0.mean)^2)/9
set1.onestep.piie.a1.imp.var <- mean(set1.onestep.piie.a1.var) + sum((set1.onestep.piie.a1.est - set1.onestep.piie.a1.mean)^2)/9
set1.onestep.piie.a0.imp.var <- mean(set1.onestep.piie.a0.var) + sum((set1.onestep.piie.a0.est - set1.onestep.piie.a0.mean)^2)/9


round(set1.TMLE.dim.mean - 1.96*sqrt(set1.TMLE.dim.imp.var),3); round(set1.TMLE.dim.mean + 1.96*sqrt(set1.TMLE.dim.imp.var),3)
round(set1.onestep.dim.mean - 1.96*sqrt(set1.onestep.dim.imp.var),3); round(set1.onestep.dim.mean + 1.96*sqrt(set1.onestep.dim.imp.var),3)


set_contiM_ordinalY.result <- list(TMLE = data.frame(dim = set1.TMLE.dim.mean, dim.var= set1.TMLE.dim.imp.var, lor = set1.TMLE.lor.mean, 
                                                     piie.a1= set1.TMLE.piie.a1.mean, piie.a0 = set1.TMLE.piie.a0.mean,
                                                     piie.a1.var = set1.TMLE.piie.a1.imp.var, piie.a0.var = set1.TMLE.piie.a0.imp.var),
                                   onestep = data.frame(dim = set1.onestep.dim.mean, dim.var= set1.onestep.dim.imp.var, lor = set1.onestep.lor.mean,
                                                        piie.a1= set1.onestep.piie.a1.mean, piie.a0 = set1.onestep.piie.a0.mean,
                                                        piie.a1.var = set1.onestep.piie.a1.imp.var, piie.a0.var = set1.onestep.piie.a0.imp.var))

round(colMeans(TMLE.PMF.Ya1),2)*100


# ATE #
# TMLE
cat(round(set1.TMLE.dim.mean,3),' (95\\% CI: (', round(set1.TMLE.dim.mean-1.96*sqrt(set1.TMLE.dim.imp.var),3),',', round(set1.TMLE.dim.mean+1.96*sqrt(set1.TMLE.dim.imp.var),3),'))',sep = '')

# Onestep
cat(round(set1.onestep.dim.mean,3),' (95\\% CI: (', round(set1.onestep.dim.mean-1.96*sqrt(set1.onestep.dim.imp.var),3),',', round(set1.onestep.dim.mean+1.96*sqrt(set1.onestep.dim.imp.var),3),'))',sep = '')


# piie a1 #
# TMLE
cat(round(set1.TMLE.piie.a1.mean,3),' (95\\% CI: (', round(set1.TMLE.piie.a1.mean-1.96*sqrt(set1.TMLE.piie.a1.imp.var),3),',', round(set1.TMLE.piie.a1.mean+1.96*sqrt(set1.TMLE.piie.a1.imp.var),3),')',sep = '')

# Onestep
cat(round(set1.onestep.piie.a1.mean,3),' (95\\% CI: (', round(set1.onestep.piie.a1.mean-1.96*sqrt(set1.onestep.piie.a1.imp.var),3),',', round(set1.onestep.piie.a1.mean+1.96*sqrt(set1.onestep.piie.a1.imp.var),3),')',sep = '')

# piie a0 #
# TMLE
cat(round(set1.TMLE.piie.a0.mean,3),' (95\\% CI: (', round(set1.TMLE.piie.a0.mean-1.96*sqrt(set1.TMLE.piie.a0.imp.var),3),',', round(set1.TMLE.piie.a0.mean+1.96*sqrt(set1.TMLE.piie.a0.imp.var),3),')',sep = '')

# Onestep
cat(round(set1.onestep.piie.a0.mean,3),' (95\\% CI: (', round(set1.onestep.piie.a0.mean-1.96*sqrt(set1.onestep.piie.a0.imp.var),3),',', round(set1.onestep.piie.a0.mean+1.96*sqrt(set1.onestep.piie.a0.imp.var),3),')',sep = '')


save(list = c('set_contiM_ordinalY.result',
              'set1.TMLE.dim.mean','set1.onestep.dim.mean',
              'set1.TMLE.piie.a1.mean','set1.TMLE.piie.a0.mean',
              'set1.TMLE.piie.a1.imp.var','set1.TMLE.piie.a0.imp.var',
              'set1.onestep.piie.a1.mean','set1.onestep.piie.a0.mean',
              'set1.onestep.piie.a1.imp.var','set1.onestep.piie.a0.imp.var',
              'set1.TMLE.lor.mean','set1.onestep.lor.mean',
              'set1.TMLE.dim.imp.var','set1.onestep.dim.imp.var',
              'TMLE.PMF.Ya1','TMLE.PMF.Ya0',
              'onestep.PMF.Ya1','onestep.PMF.Ya0'), file="output/estimation_contiM_ordinalY.Rdata")
