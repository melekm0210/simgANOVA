####2 Faktörlü Rasgele Etkili Etkileşimsiz Model####
library("lme4")
sim2=function(n.f1,n.f2,A,B,r,mu){ 
  dat <- expand.grid(F1 = as.factor(1:n.f1), F2 = 1:n.f2)
  dat$Y <- 0 + rnorm(n.f1*n.f2)
  myformula <- "Y ~ (1|F1) + (1|F2)"  # model formülü
  mylF <- lFormula(eval(myformula), data = dat) 
  Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  
  b1 <- rnorm(n.f2, 0, sqrt(B))            
  b2 <- rnorm(n.f1, 0, sqrt(A))    
  b <- c(b1, b2)
  res <- mu + Z %*% b + rnorm(nrow(dat),0,sqrt(r))
  dat$Y <- res
  sonuc=list("y"=res,"Tum model"=dat,Kullanilan_model=1)
  return(sonuc)
}
