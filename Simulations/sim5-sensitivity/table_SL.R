library(here)
library(huxtable)
setwd(here('sim5-sensitivity'))

## CATE
cases <- c('continuousY-continuousXM')

dgps <- c('fd_admg1','fd_admg2','nonfd_admg1','nonfd_admg2')


## dual
dat.table1 <- c(500,1000,2000)

dat <- rep(1,3)

for (j in 1:length(dgps)) {
  
  load(paste0(cases,'/',dgps[j],'/result_AIPW.Rdata'))
  
  dat <- cbind(dat,dual.wald.rate_matrix[,2],dual.wald.rate_matrix.SL[,2])
  
}

dat <- dat[,-1]

dat.table1 <- cbind(dat.table1,dat)

## primal
dat.table2 <- c(500,1000,2000)

dat <- rep(1,3)

for (j in 1:length(dgps)) {
  
  load(paste0(cases,'/',dgps[j],'/result_AIPW.Rdata'))
  
  dat <- cbind(dat,primal.wald.rate_matrix[,2],primal.wald.rate_matrix.SL[,2])
  
}

dat <- dat[,-1]

dat.table2 <- cbind(dat.table2,dat)

dat.table <- rbind(dat.table1,dat.table2)

# make table
library(huxtable)
library(dplyr)
library(stringr)
options(scipen = 999)

dat.table <- as.matrix(dat.table)

table1 <- as_hux(dat.table[,-1]) %>% 
  insert_row("Y,M,Z,X continuous","","","","","","","", after = 0) %>% 
  insert_row("Type I error","","","","Power","","","", after = 1) %>% 
  insert_row("ADMG1","","ADMG2","","ADMG3","","ADMG4","",after=2) %>%
  insert_row("Linear","SL","Linear","SL","Linear","SL","Linear","SL",after=3) %>%
  insert_row("Dual test","","","","","","","",after=4) %>%
  insert_row("Primal test","","","","","","","",after=8) %>%
  merge_cells(1,everywhere) %>% 
  merge_cells(2, 1:4) %>% 
  merge_cells(2, 5:8) %>% 
  merge_cells(3,1:2) %>% 
  merge_cells(3,3:4) %>%
  merge_cells(3,5:6) %>%
  merge_cells(3,7:8) %>%
  insert_column(c("","N","","Nuisance models","Dual test","500","1000","2000","Primal test","500","1000","2000"), after = 0) %>%
  # set_number_format(6, 1, function(x) format(x, scientific = FALSE)) %>% 
  set_number_format(everywhere,everywhere, "%s") %>% 
  merge_cells(5, 1:3) %>%
  merge_cells(9, 1:3) %>%
  set_align(col=1, everywhere, "left") %>%
  set_align(col=2:ncol(.),everywhere,"center") %>%
  set_all_padding(-1)  %>% 
  set_bold(1, everywhere) %>%
  # set_bold(c(3,9,15), everywhere) %>%
  set_italic(2,everywhere) %>%
  set_italic(4,1) %>%
  set_bottom_border(row = 1, col =2:ncol(.)) %>% 
  # set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_bottom_border(row = 2, col =2:ncol(.)) %>% 
  set_bottom_border(row = 3, col =2:ncol(.)) %>% 
  set_bottom_border(row = 4, col =2:ncol(.)) %>% 
  # set_right_border(4:nrow(.), 4, brdr(0.4,"solid")) %>%
  set_right_border(5:nrow(.), 5, brdr(0.4,"double")) %>%
  # set_right_border(4:nrow(.), 9, brdr(0.4,"double")) %>%
  # set_right_border(4:nrow(.), 13, brdr(0.4,"double")) %>%
  # set_right_border(4:nrow(.), 10, brdr(0.4,"solid")) %>%
  # set_right_border(4:nrow(.), 13, brdr(0.4,"double")) %>%
  # set_right_border(4:nrow(.), 16, brdr(0.4,"solid")) %>%
  set_top_border(row=1,col=everywhere,brdr(1, "solid")) %>% set_bottom_border(row = nrow(.),col = everywhere,brdr(1, "solid")) %>%
  set_font_size(6) %>% set_caption("Comparative analysis of dual and primal tests using linear vs Super Learners under model misspecification.")

table1

quick_latex(table1, file = "table_SL.tex")

