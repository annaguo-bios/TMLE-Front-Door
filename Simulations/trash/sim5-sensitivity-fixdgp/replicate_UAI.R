library(dplyr)

parametric_dual_weights <- function(data, M, mpM, A, Avalue=1){
  
  modelM <- glm(formula = as.formula("M~."), data = data[,c(M,mpM), drop=F], family = binomial())
  
  # p(M|A,X)
  p.M1.AX <- predict(modelM, type = "response")
  p.M.AX <- data[[M]]*p.M1.AX + (1-data[[M]])*(1-p.M1.AX)
  
  #p(M|a,X)
  data.a <- data %>% mutate(!!A := Avalue)
  p.M1.aX <- predict(modelM, newdata = data.a, type = "response")
  p.M.aX <- data[[M]]*p.M1.aX + (1-data[[M]])*(1-p.M1.aX)
  
  return(p.M.aX/p.M.AX)
}



parametric_primal_weights <- function(data, A, Y, mpA, mpY){
  
  model_A <- glm(formula = as.formula('A~.'), data = data[, c(A, mpA), drop=F], family = binomial())
  model_Y <- glm(formula = as.formula('Y~.'), data = data[, c(Y, mpY), drop=F], family = gaussian())
  
  p.A1 <- predict(model_A, type = "response")
  p.A <- data[[A]]*p.A1 + (1-data[[A]])*(1-p.A1)
  
  dataA1 <- data[, c(Y, mpY), drop=F] %>% mutate(!!A := 1)
  dataA0 <- data[, c(Y, mpY), drop=F] %>% mutate(!!A := 0)
  
  EY.A <- predict(model_Y)
  EY.A1 <- predict(model_Y, newdata = dataA1)
  EY.A0 <- predict(model_Y, newdata = dataA0)
  
  stdY.A <- sqrt(var(data[[Y]]-EY.A))
  stdY.A1 <- sqrt(var(data[[Y]]-EY.A1))
  stdY.A0 <- sqrt(var(data[[Y]]-EY.A0))
  
  p.Y.A <- dnorm(data[[Y]], EY.A, stdY.A)
  p.Y.A1 <- dnorm(data[[Y]], EY.A1, stdY.A1)
  p.Y.A0 <- dnorm(data[[Y]], EY.A0, stdY.A0)
  
  # compute numerator \sum_A p(A|mpA)p(Y|mpY)
  numerator <- (1-p.A1)*p.Y.A0 + p.A1*p.Y.A1
  
  # compute denominator p(A|mpA)p(Y|mpY)
  denominator <- p.A*p.Y.A
  
  return(numerator/denominator)
  
}



weighted_lr_test <- function(data, Y, Z, cond_set, weights, state_space = 'continuous'){
  
  Y.family <- if(state_space == "continuous"){gaussian()}else{binomial()}
  
  modelY_null <- glm(formula = as.formula("Y~."), data = data[, c(Y, cond_set), drop=F], family = Y.family, weights = weights)
  modelY_alt <- glm(formula = as.formula("Y~."), data=data[, c(Y, cond_set, Z), drop=F], family = Y.family, weights = weights)
  
  chi2_stat <- 2*(logLik(modelY_alt) - logLik(modelY_null))
  
  return(1-pchisq(chi2_stat, df=1))
  
}


num_samples = 5000
# try the dgps that satisfy front-door

example <- function(data=NULL,dgp.f.name,a=1){
  
  if (is.null(data)){
    data <- get(dgp.f.name)(num_samples)
  }
  
  
  dual_verma_weights = parametric_dual_weights(data, "M", c("Z", "A"), "A", a)
  dual_p_val = weighted_lr_test(data, "Y", "Z", c("M"), dual_verma_weights)
  dual_p_val
  
  primal_weights = parametric_primal_weights(data, "A", "Y", "Z", c("Z", "A", "M"))
  primal_p_val = weighted_lr_test(data, "Y", "Z", c("M"), primal_weights)
  primal_p_val
  
  if (dgp.f.name == "fd_admg1"){
    print(paste0("FD DGP 1. dual: ", round(dual_p_val,2), " primal:",round(primal_p_val,2), " dual, primal p-vals should be > 0.05"))
    
  }else if(dgp.f.name == 'nonfd_admg1'){
    
    print(paste0("nonFD DGP 1. dual: ", round(dual_p_val,2), " primal:",round(primal_p_val,2), " dual, primal p-vals should be < 0.05"))
    
  }else if(dgp.f.name == 'nonfd_admg2'){
    
    print(paste0("nonFD DGP 2. dual: ", round(dual_p_val,2), " primal:",round(primal_p_val,2), " dual, primal p-vals should be < 0.05"))
    
  }
  
}

set.seed(7)
data <- nonfd_admg1(num_samples)
example(data,dgp.f.name = "fd_admg1")
example(data,dgp.f.name = "nonfd_admg1",a=0)
example(dgp.f.name = "nonfd_admg2")
