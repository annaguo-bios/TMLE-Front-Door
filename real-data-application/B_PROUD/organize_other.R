set1.TMLE.est <- rep(NA,10); set1.TMLE.var <- rep(NA,10)
set1.onestep.est <- rep(NA,10); set1.onestep.var <- rep(NA,10)

set1.piie.a1.TMLE.est <- rep(NA,10); set1.piie.a1.TMLE.var <- rep(NA,10)
set1.piie.a0.TMLE.est <- rep(NA,10); set1.piie.a0.TMLE.var <- rep(NA,10)
set1.piie.a1.onestep.est <- rep(NA,10); set1.piie.a1.onestep.var <- rep(NA,10)
set1.piie.a0.onestep.est <- rep(NA,10); set1.piie.a0.onestep.var <- rep(NA,10)

#----------
set2.TMLE.est <- rep(NA,10); set2.TMLE.var <- rep(NA,10)
set2.onestep.est <- rep(NA,10); set2.onestep.var <- rep(NA,10)
set2.piie.a1.TMLE.est <- rep(NA,10); set2.piie.a1.TMLE.var <- rep(NA,10)
set2.piie.a0.TMLE.est <- rep(NA,10); set2.piie.a0.TMLE.var <- rep(NA,10)
set2.piie.a1.onestep.est <- rep(NA,10); set2.piie.a1.onestep.var <- rep(NA,10)
set2.piie.a0.onestep.est <- rep(NA,10); set2.piie.a0.onestep.var <- rep(NA,10)

#----------
set3.TMLE.est <- rep(NA,10); set3.TMLE.var <- rep(NA,10)
set3.onestep.est <- rep(NA,10); set3.onestep.var <- rep(NA,10)
set3.piie.a1.TMLE.est <- rep(NA,10); set3.piie.a1.TMLE.var <- rep(NA,10)
set3.piie.a0.TMLE.est <- rep(NA,10); set3.piie.a0.TMLE.var <- rep(NA,10)
set3.piie.a1.onestep.est <- rep(NA,10); set3.piie.a1.onestep.var <- rep(NA,10)
set3.piie.a0.onestep.est <- rep(NA,10); set3.piie.a0.onestep.var <- rep(NA,10)

#----------
set4.TMLE.est <- rep(NA,10); set4.TMLE.var <- rep(NA,10)
set4.onestep.est <- rep(NA,10); set4.onestep.var <- rep(NA,10)
set4.piie.a1.TMLE.est <- rep(NA,10); set4.piie.a1.TMLE.var <- rep(NA,10)
set4.piie.a0.TMLE.est <- rep(NA,10); set4.piie.a0.TMLE.var <- rep(NA,10)
set4.piie.a1.onestep.est <- rep(NA,10); set4.piie.a1.onestep.var <- rep(NA,10)
set4.piie.a0.onestep.est <- rep(NA,10); set4.piie.a0.onestep.var <- rep(NA,10)

#----------
set5.TMLE.est <- rep(NA,10); set5.TMLE.var <- rep(NA,10)
set5.onestep.est <- rep(NA,10); set5.onestep.var <- rep(NA,10)
set5.piie.a1.TMLE.est <- rep(NA,10); set5.piie.a1.TMLE.var <- rep(NA,10)
set5.piie.a0.TMLE.est <- rep(NA,10); set5.piie.a0.TMLE.var <- rep(NA,10)
set5.piie.a1.onestep.est <- rep(NA,10); set5.piie.a1.onestep.var <- rep(NA,10)
set5.piie.a0.onestep.est <- rep(NA,10); set5.piie.a0.onestep.var <- rep(NA,10)

#----------
set6.TMLE.est <- rep(NA,10); set6.TMLE.var <- rep(NA,10)
set6.onestep.est <- rep(NA,10); set6.onestep.var <- rep(NA,10)
set6.piie.a1.TMLE.est <- rep(NA,10); set6.piie.a1.TMLE.var <- rep(NA,10)
set6.piie.a0.TMLE.est <- rep(NA,10); set6.piie.a0.TMLE.var <- rep(NA,10)
set6.piie.a1.onestep.est <- rep(NA,10); set6.piie.a1.onestep.var <- rep(NA,10)
set6.piie.a0.onestep.est <- rep(NA,10); set6.piie.a0.onestep.var <- rep(NA,10)

#----------
set7.TMLE.est <- rep(NA,10); set7.TMLE.var <- rep(NA,10)
set7.onestep.est <- rep(NA,10); set7.onestep.var <- rep(NA,10)
set7.piie.a1.TMLE.est <- rep(NA,10); set7.piie.a1.TMLE.var <- rep(NA,10)
set7.piie.a0.TMLE.est <- rep(NA,10); set7.piie.a0.TMLE.var <- rep(NA,10)
set7.piie.a1.onestep.est <- rep(NA,10); set7.piie.a1.onestep.var <- rep(NA,10)
set7.piie.a0.onestep.est <- rep(NA,10); set7.piie.a0.onestep.var <- rep(NA,10)

for (set in 1:7){
  
  set.TMLE.est <- rep(NA,10); set.TMLE.var <- rep(NA,10)
  set.onestep.est <- rep(NA,10); set.onestep.var <- rep(NA,10)
  
  set.piie.a1.TMLE.est <- rep(NA,10); set.piie.a1.TMLE.var <- rep(NA,10)
  set.piie.a0.TMLE.est <- rep(NA,10); set.piie.a0.TMLE.var <- rep(NA,10)
  set.piie.a1.onestep.est <- rep(NA,10); set.piie.a1.onestep.var <- rep(NA,10)
  set.piie.a0.onestep.est <- rep(NA,10); set.piie.a0.onestep.var <- rep(NA,10)
  
  
  for (m in 1:10){
    
    load(paste0("output/set_", set, "_m_", m, ".RData"))
    
    set.TMLE.est[m] <- set.TMLE.est_m
    set.TMLE.var[m] <- set.TMLE.var_m
    set.onestep.est[m] <- set.onestep.est_m
    set.onestep.var[m] <- set.onestep.var_m
    set.piie.a1.TMLE.est[m] <- set.piie.a1.TMLE.est_m
    set.piie.a1.TMLE.var[m] <- set.piie.a1.TMLE.var_m
    set.piie.a0.TMLE.est[m] <- set.piie.a0.TMLE.est_m
    set.piie.a0.TMLE.var[m] <- set.piie.a0.TMLE.var_m
    set.piie.a1.onestep.est[m] <- set.piie.a1.onestep.est_m
    set.piie.a1.onestep.var[m] <- set.piie.a1.onestep.var_m
    set.piie.a0.onestep.est[m] <- set.piie.a0.onestep.est_m
    set.piie.a0.onestep.var[m] <- set.piie.a0.onestep.var_m
    
    
  }
  
  set.TMLE.mean <- mean(set.TMLE.est)
  set.onestep.mean <- mean(set.onestep.est)
  set.piie.a1.TMLE.mean <- mean(set.piie.a1.TMLE.est)
  set.piie.a0.TMLE.mean <- mean(set.piie.a0.TMLE.est)
  set.piie.a1.onestep.mean <- mean(set.piie.a1.onestep.est)
  set.piie.a0.onestep.mean <- mean(set.piie.a0.onestep.est)
  
  set.TMLE.imp.var <- mean(set.TMLE.var) + sum((set.TMLE.est - set.TMLE.mean)^2)/9
  set.onestep.imp.var <- mean(set.onestep.var) + sum((set.onestep.est - set.onestep.mean)^2)/9
  set.piie.a1.TMLE.imp.var <- mean(set.piie.a1.TMLE.var) + sum((set.piie.a1.TMLE.est - set.piie.a1.TMLE.mean)^2)/9
  set.piie.a0.TMLE.imp.var <- mean(set.piie.a0.TMLE.var) + sum((set.piie.a0.TMLE.est - set.piie.a0.TMLE.mean)^2)/9
  set.piie.a1.onestep.imp.var <- mean(set.piie.a1.onestep.var) + sum((set.piie.a1.onestep.est - set.piie.a1.onestep.mean)^2)/9
  set.piie.a0.onestep.imp.var <- mean(set.piie.a0.onestep.var) + sum((set.piie.a0.onestep.est - set.piie.a0.onestep.mean)^2)/9
  
  round(set.TMLE.mean - 1.96*sqrt(set.TMLE.imp.var),3); round(set.TMLE.mean + 1.96*sqrt(set.TMLE.imp.var),3)
  round(set.onestep.mean - 1.96*sqrt(set.onestep.imp.var),3); round(set.onestep.mean + 1.96*sqrt(set.onestep.imp.var),3)
  round(set.piie.a1.TMLE.mean - 1.96*sqrt(set.piie.a1.TMLE.imp.var),3); round(set.piie.a1.TMLE.mean + 1.96*sqrt(set.piie.a1.TMLE.imp.var),3)
  round(set.piie.a0.TMLE.mean - 1.96*sqrt(set.piie.a0.TMLE.imp.var),3); round(set.piie.a0.TMLE.mean + 1.96*sqrt(set.piie.a0.TMLE.imp.var),3)
  round(set.piie.a1.onestep.mean - 1.96*sqrt(set.piie.a1.onestep.imp.var),3); round(set.piie.a1.onestep.mean + 1.96*sqrt(set.piie.a1.onestep.imp.var),3)
  round(set.piie.a0.onestep.mean - 1.96*sqrt(set.piie.a0.onestep.imp.var),3); round(set.piie.a0.onestep.mean + 1.96*sqrt(set.piie.a0.onestep.imp.var),3)
  
  set.result <- list(TMLE = data.frame(est = set.TMLE.mean, var= set.TMLE.imp.var), 
                     TMLE.piie.a1 = data.frame(est = set.piie.a1.TMLE.mean, var= set.piie.a1.TMLE.imp.var),
                     TMLE.piie.a0 = data.frame(est = set.piie.a0.TMLE.mean, var= set.piie.a0.TMLE.imp.var),
                     onestep = data.frame(est=set.onestep.mean, var=set.onestep.imp.var),
                     onestep.piie.a1 = data.frame(est = set.piie.a1.onestep.mean, var= set.piie.a1.onestep.imp.var),
                     onestep.piie.a0 = data.frame(est = set.piie.a0.onestep.mean, var= set.piie.a0.onestep.imp.var)
  )
  

  assign(paste0('set',set, '.result'), set.result)
  
}

save(list = paste0('set', 1:7, '.result'), file="output/estimation_v2.Rdata")


library(huxtable)
library(dplyr)
options(scipen = 999)

dat <- data.frame(Scenario= c("Continuous",rep('Binary',6)),
                  M.cutoff=c('-',rep('48 mins',3), rep('75 mins',3)),
                  Y.cutoff=c('-',rep(2:4,2)),
                  Onestep=rep(NA,7),
                  TMLE=rep(NA,7)
)

for (i in 1:7){
  onestep.est <- get(paste0('set',i,'.result'))$onestep$est
  onestep.var <- get(paste0('set',i,'.result'))$onestep$var
  TMLE.est <- get(paste0('set',i,'.result'))$TMLE$est
  TMLE.var <- get(paste0('set',i,'.result'))$TMLE$var
  
  dat[i,4] <- paste0(round(onestep.est,3),", 95%CI (", round(onestep.est - 1.96*sqrt(onestep.var),3),", ", round(onestep.est + 1.96*sqrt(onestep.var),3),")")
  dat[i,5] <- paste0(round(TMLE.est,3),", 95%CI (", round(TMLE.est - 1.96*sqrt(TMLE.var),3),", ", round(TMLE.est + 1.96*sqrt(TMLE.var),3),")")
}

table1 <- as_hux(dat) %>%
  slice(-1) %>%  # Remove the first row
  insert_row("$M$ and $Y$ type","$M$ cutoff","$Y$ cutoff","One-step estimator","TMLE", after = 0) %>% # Insert first row
  set_escape_contents(1, 1:3, FALSE) %>%
  set_align(col=c(1,2,3), everywhere, "left") %>%
  set_align(col=c(4,5),everywhere,"center") %>%
  set_valign(2:nrow(.), 1:5, "middle" ) %>%
  set_tb_padding(1, everywhere, 0) %>% 
  set_bold(1, everywhere) %>%
  # set_italic(c(2,4,13,18),everywhere) %>%
  set_bottom_border(row = 1, col =1:ncol(.)) %>% 
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% 
  set_caption("One-step and TMLE estimates of the average causal effect of additional mobile stroke unit (MSU) care on modified Rankin scale (mRS)") %>%
  set_all_padding(1) %>% set_font_size(7)  %>% set_width(value=1) %>% set_col_width(c(0.15,0.11,0.11,0.32,0.32)) %>% set_wrap(T) %>% 
  insert_row(c("* Adopt one-step estimator $\\psi_1(\\hat{Q})$ and TMLE $\\psi_1(\\hat{Q}^\\star)$ under binary $M$, and adopt one-step estimator $\\psi_{2b}(\\hat{Q})$ and TMLE $\\psi_{2b}(\\hat{Q}^\\star)$ under continuous $M$.","","","",""), after = nrow(.)) %>% 
  merge_cells(nrow(.), 1:ncol(.)) %>% 
  set_escape_contents(nrow(.), 1:ncol(.), FALSE) %>% 
  set_bottom_border(row = nrow(.)-1,col = everywhere,brdr(1, "solid"))

table1

quick_latex(table1,file = "B_PROUD/table_other.tex")
