test.z <- function(dt, m.level, z){
  
  dt.z <- dt %>% mutate(Z==z)
  dt.a1z <- dt %>% mutate(A=1, Z=z)
  dt.a0z <- dt %>% mutate(A=0, Z=z)
  
  ## Y fit
  # y.fit <- glm(formula = Y ~ M + A + Z + I(M*Z) + I(A*M) + I(A*Z) + I(M*A*Z), data = dt, family = binomial())
  # 
  # E.Y.Ma1z <- predict(y.fit, newdata = dt.a1z, type = "response")
  # E.Y.Ma0z <- predict(y.fit, newdata = dt.a0z, type = "response")
  
  p.y1.m1a1z <- mean(dt$Y[dt$M==1 & dt$A==1 & dt$Z==z])
  p.y1.m0a1z <- mean(dt$Y[dt$M==0 & dt$A==1 & dt$Z==z])
  p.y1.m1a0z <- mean(dt$Y[dt$M==1 & dt$A==0 & dt$Z==z])
  p.y1.m0a0z <- mean(dt$Y[dt$M==0 & dt$A==0 & dt$Z==z])
  
  
  ## A fit
  # a.fit <- glm(formula = A ~ Z, data = dt, family = binomial())
  # 
  # p.a1.z <- predict(a.fit, type = "response")
  # p.a0.z <- 1-p.a1.z
  
  p.a1.z <- mean(dt$A[dt$Z==z])
  p.a0.z <- 1-p.a1.z
  
  ## test statistic
  test.stat.m1 <- p.y1.m1a1z*p.a1.z + p.y1.m1a0z*p.a0.z
  test.stat.m0 <- p.y1.m0a1z*p.a1.z + p.y1.m0a0z*p.a0.z

  return(m.level*test.stat.m1 + (1-m.level)*test.stat.m0)
}


test <- function(dt,i,m.level){
  
  dt <- dt[i,]
  
  test.stat.z1 <- test.z(dt, m.level, z=1)
  test.stat.z0 <- test.z(dt, m.level, z=0)
  
  return(test.stat.z1 - test.stat.z0)
  
}

set.seed(7)
dt <- fd_admg1(1000)
test.fd1 <- boot(data=dt, statistic= test, R=500, m.level=1)
print(paste0("Estimate: ", round(mean(test.fd1$t),2)," CI: (",round(mean(test.fd1$t)-1.96*sd(test.fd1$t),2),", ", round(mean(test.fd1$t)+1.96*sd(test.fd1$t),2),")" ))

dt <- nonfd_admg1(1000)
test.nonfd1 <- boot(data=dt, statistic= test, R=500, m.level=1)
print(paste0("Estimate: ", round(mean(test.nonfd1$t),2)," CI: (",round(mean(test.nonfd1$t)-1.96*sd(test.nonfd1$t),2),", ", round(mean(test.nonfd1$t)+1.96*sd(test.nonfd1$t),2),")" ))

dt <- nonfd_admg3(1000)
test.nonfd3 <- boot(data=dt, statistic= test, R=500, m.level=1)
print(paste0("Estimate: ", round(mean(test.nonfd3$t),2)," CI: (",round(mean(test.nonfd3$t)-1.96*sd(test.nonfd3$t),2),", ", round(mean(test.nonfd3$t)+1.96*sd(test.nonfd3$t),2),")" ))
