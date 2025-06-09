library(here)
library(huxtable)
setwd(here('sim5-sensitivity'))

## CATE
cases <- c('continuousY-continuousX_complex')

dgps <- c('fd_admg1','fd_admg2','nonfd_admg1','nonfd_admg2')



for (i in 1:length(cases)) {
  
  dat <- rep(1,5)
  
  for (j in 1:length(dgps)) {
    load(paste0(cases[i],'/',dgps[j],'/result_AIPW.Rdata'))
    
    dat <- cbind(dat,dual.wald.rate_matrix[,2])
    
  }
  
  dat <- dat[,-1]
  
  dat.table2 <-dat
}

colnames(dat.table2) <- c(paste0('V',1:4))

for (i in 1:length(cases)) {
  
  dat <- rep(1,5)
  
  for (j in 1:length(dgps)) {
    load(paste0(cases[i],'/',dgps[j],'/result_AIPW.Rdata'))
    
    dat <- cbind(dat,primal.wald.rate_matrix[,2])
    
  }
  
  dat <- dat[,-1]
  
  dat.table3 <- dat
}

colnames(dat.table3) <- c(paste0('V',1:4))


dat.table4 <- c(500,1000,2000,4000,10000)
for (i in 1:length(cases)) {
  
  dat <- rep(1,5)
  
  for (j in 1:length(dgps)) {
    load(paste0(cases[i],'/',dgps[j],'/result_AIPW.Rdata'))
    
    dat <- cbind(dat,dr.cate.wald.rate_matrix[,2])
    
  }
  
  dat <- dat[,-1]
  
  dat.table4 <- cbind(dat.table4,dat)
}

colnames(dat.table4) <- c('N',paste0('V',1:4))

dat.table <- cbind(dat.table4,dat.table2,dat.table3)

# make table
library(huxtable)
library(dplyr)
library(stringr)
options(scipen = 999)

dat.table <- as.matrix(dat.table)

table1 <- as_hux(dat.table) %>% 
  insert_row("","DR-CCM test","","","","Dual test","","","","Primal test","","","", after = 0) %>% 
  insert_row("","Type I error","","Power","","Type I error","","Power","","Type I error","","Power","", after = 1) %>% 
  insert_row("n","DAG1","DAG2","DAG3","DAG4","DAG1","DAG2","DAG3","DAG4","DAG1","DAG2","DAG3","DAG4",after=2) %>%
  merge_cells(1,2:5) %>% 
  merge_cells(1,6:9) %>% 
  merge_cells(1,10:13) %>% 
  merge_cells(2, 2:3) %>% 
  merge_cells(2, 4:5) %>% 
  merge_cells(2, 6:7) %>% 
  merge_cells(2, 8:9) %>% 
  merge_cells(2, 10:11) %>% 
  merge_cells(2, 12:13) %>% 
  set_number_format(everywhere,everywhere, "%s") %>% 
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(-1)  %>% 
  set_bold(1, everywhere) %>%
  # set_bold(c(3,9,15), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_right_border(4:nrow(.), 5, brdr(0.6,"double")) %>%
  set_right_border(4:nrow(.), 9, brdr(0.6,"double")) %>%
  set_right_border(4:nrow(.), 3, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 7, brdr(0.4,"solid")) %>%
  set_right_border(4:nrow(.), 11, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(6) %>% set_caption("Comparative analysis of DR-CCM, dual, and primal tests under model misspecifications.")

table1

quick_latex(table1, file = "table_dr.tex")
