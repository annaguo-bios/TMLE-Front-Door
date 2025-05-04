library(here)

source("../R/EYa-continuousZ-binaryM.R")

frontdoor_effect_est <- function(a,data,treatment, mediator, outcome, anchor,tilde_pz,
                                 onestep=TRUE, superlearner=TRUE,crossfit=FALSE,K=5,
                                 lib = c("SL.glm","SL.earth","SL.ranger","SL.mean"), n.iter=500, cvg.criteria=0.01,
                                 formulaY="Y ~ .", formulaA="A ~ .", formulaM="M~.", linkY_binary="logit", linkA="logit", linkM_binary="logit",
                                 truncate_lower=0.01, truncate_upper=0.99,
                                 verbose=T){
  
  # sample size
  
  n <- nrow(data)
  
  
  ################################## ATE
  
  if (is.vector(a) & length(a)>2){ ## Invalid input ==
    
    stop("Invalid input. Enter a=c(1,0) for Average Causal Effect estimation. Enter x=1 or x=0 for average counterfactual outcome estimation at the specified treatment level.")
    
  }else if (is.vector(a) & length(a)==2){ ## ATE estimate ==
    
    ## TMLE estimator
    
    out.a1 <- EYa.continuousZ.binaryM(a = a[1], 
                                  data = data, 
                                  treatment = treatment, 
                                  mediator = mediator, 
                                  outcome = outcome, 
                                  anchor = anchor, 
                                  tilde_pz=tilde_pz,
                                  onestep = onestep, 
                                  superlearner = superlearner, 
                                  crossfit = crossfit, 
                                  K = K, 
                                  lib = lib, 
                                  n.iter = n.iter, 
                                  cvg.criteria = cvg.criteria, 
                                  formulaY = formulaY, 
                                  formulaA = formulaA, 
                                  formulaM = formulaM, 
                                  linkY_binary = linkY_binary, 
                                  linkA = linkA, 
                                  linkM_binary = linkM_binary, 
                                  truncate_lower = truncate_lower, 
                                  truncate_upper = truncate_upper, 
                                  verbose = verbose)
    
    out.a0 <- EYa.continuousZ.binaryM(a = a[2], 
                                  data = data, 
                                  treatment = treatment, 
                                  mediator = mediator, 
                                  outcome = outcome, 
                                  anchor = anchor, 
                                  tilde_pz=tilde_pz,
                                  onestep = onestep, 
                                  superlearner = superlearner, 
                                  crossfit = crossfit, 
                                  K = K, 
                                  lib = lib, 
                                  n.iter = n.iter, 
                                  cvg.criteria = cvg.criteria, 
                                  formulaY = formulaY, 
                                  formulaA = formulaA, 
                                  formulaM = formulaM, 
                                  linkY_binary = linkY_binary, 
                                  linkA = linkA, 
                                  linkM_binary = linkM_binary, 
                                  truncate_lower = truncate_lower, 
                                  truncate_upper = truncate_upper, 
                                  verbose = verbose)
    
    
    # run TMLE
    TMLE_output_Y1 <- out.a1$TMLE
    TMLE_output_Y0 <- out.a0$TMLE
    
    # run onestep
    Onestep_output_Y1 <- out.a1$Onestep
    Onestep_output_Y0 <- out.a0$Onestep
    
    # levels of z used for estimation
    level.z <- sub("^out", "", names(Onestep_output_Y1))
    
    level.z.number <- sub("^out\\.z", "", names(Onestep_output_Y1))
    level.z.number[level.z.number=="out.all.z"] <- "all z"
    
    output <- vector("list", 2*length(level.z)+2)
    estimators <- c('TMLE','Onestep')
    
    output[[1]] <- out.a1
    output[[2]] <- out.a0
    names(output)[1:2] <- c("est.Y1","est.Y0")
    
    # count of method
    count <- 3
    
    for (m in estimators){ # loop over TMLE and onestep
      
      for (est in level.z){ # loop over levels of z
        
        out.est1 <- get(paste0(m,'_output_Y1'))[[paste0('out',est)]] # get output for Y(1)
        out.est0 <- get(paste0(m,'_output_Y0'))[[paste0('out',est)]] # get output for Y(0)
        
        # assign(paste0(m,'_output_Y',i,est), out.est)
        
        # estimate E[Y(1)], E[Y(0)], and ATE
        assign(paste0('hat_E.Y1',est), out.est1$estimated)
        assign(paste0('hat_E.Y0',est), out.est0$estimated)
        assign(paste0('hat_ATE',est), get(paste0('hat_E.Y1',est)) - get(paste0('hat_E.Y0',est)))
        
        
        # estimated EIF
        assign(paste0('hat_EIF.Y1',est), out.est1$EIF)
        assign(paste0('hat_EIF.Y0',est), out.est0$EIF)
        assign(paste0('hat_EIF.ATE',est), get(paste0('hat_EIF.Y1',est)) - get(paste0('hat_EIF.Y0',est)))
        
        ## CI
        assign(paste0('lower.ci_ATE',est), get(paste0('hat_ATE',est)) - 1.96*sqrt(mean(get(paste0('hat_EIF.ATE',est))^2)/n)) # lower CI
        
        assign(paste0('upper.ci_ATE',est), get(paste0('hat_ATE',est)) + 1.96*sqrt(mean(get(paste0('hat_EIF.ATE',est))^2)/n)) # upper CI
        
        ## variance
        assign(paste0('var_ATE',est), mean(get(paste0('hat_EIF.ATE',est))^2)/n)
        
        
        # TMLE and onestep output
        # ATE
        assign(paste0(m,est),list(ATE=get(paste0('hat_ATE',est)), # estimated parameter
                                  lower.ci=get(paste0('lower.ci_ATE',est)), # lower bound of 95% CI
                                  upper.ci=get(paste0('upper.ci_ATE',est)), # upper bound of 95% CI
                                  EIF=get(paste0('hat_EIF.ATE',est)), # EIF
                                  var_ATE=get(paste0('var_ATE',est)))) # variance of ATE
        
        output[[count]] <- get(paste0(m,est))
        names(output)[count] <- paste0(m,est)
        count <- count + 1
        
        indicator <- which(level.z==est)
        
        # print estimates
        cat(paste0(m," estimated ACE for z=",level.z.number[indicator],": ",round(get(paste0(m,est))$ATE,2),"; 95% CI: (",round(get(paste0(m,est))$lower.ci,2),", ",round(get(paste0(m,est))$upper.ci,2),") \n"))
        
      }
      
      
      
    }
    
    
    
    return(output)
    
  } # end of ATE estimate
  
  
  
  
  ################################## E(Y(x))
  
  
  if (length(a)==1) { ## E(Y^1) estimate ==
    
    out.a <- EYa.binaryZ.binaryM(a = a, 
                                 data = data, 
                                 treatment = treatment, 
                                 mediator = mediator, 
                                 outcome = outcome, 
                                 anchor = anchor, 
                                 onestep = onestep, 
                                 superlearner = superlearner, 
                                 crossfit = crossfit, 
                                 K = K, 
                                 lib = lib, 
                                 n.iter = n.iter, 
                                 cvg.criteria = cvg.criteria, 
                                 formulaY = formulaY, 
                                 formulaA = formulaA, 
                                 formulaM = formulaM, 
                                 linkY_binary = linkY_binary, 
                                 linkA = linkA, 
                                 linkM_binary = linkM_binary, 
                                 truncate_lower = truncate_lower, 
                                 truncate_upper = truncate_upper, 
                                 verbose = verbose)
    
    
    
    # run TMLE
    TMLE_output <- out.a$TMLE
    
    
    # run onestep
    Onestep_output <- out.a$Onestep
    
    
    # levels of z used for estimation
    level.z <- sub("^out", "", names(Onestep_output))
    
    
    level.z.number <- sub("^out\\.z", "", names(Onestep_output))
    level.z.number[level.z.number=="out.all.z"] <- "all z"
    
    
    output <- vector("list", 2*length(level.z))
    estimators <- c('TMLE','Onestep')
    
    # count of method
    count <- 1
    
    for (m in estimators){ # loop over TMLE and onestep
      
      for (est in level.z){ # loop over levels of z
        
        output[[count]] <- get(paste0(m,'_output'))[[paste0('out',est)]] # get output for Y(x)
        names(output)[count] <- paste0(m,est)
        count <- count + 1
        
        indicator <- which(level.z==est)
        # print estimates
        cat(paste0(m," estimated E(Y(",a,")) for z=",level.z.number[indicator],": ",round(get(paste0(m,'_output'))[[paste0('out',est)]]$estimated,2),";95% CI: (",round(get(paste0(m,'_output'))[[paste0('out',est)]]$lower.ci,2),",",round(get(paste0(m,'_output'))[[paste0('out',est)]]$upper.ci,2),") \n"))
        
        
        
        
      }
      
    }
    
    return(output)
    
  } # end of E(Y(x)) estimate
  
  
  
  
  
} # end of the whole function


