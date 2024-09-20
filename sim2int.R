####2 Faktörlü Rasgele Etkili Etkileşimli Model####
library("lme4")
sim2int=function(n.f1,n.f2,n.fr,po,A,B,r,mu){ 
  dat <- expand.grid(F1 = as.factor(1:n.f1), F2 = 1:n.f2,rep=1:n.fr)
  dat$Y <- 0 + rnorm(n.f1*n.f2*n.fr)
  myformula <- "Y ~ (1|F1) + (1|F2) + (1|F1:F2)"  # model formülü
  mylF <- lFormula(eval(myformula), data = dat) 
  Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  
  b1 <- rnorm(n.f1* n.f2, 0 , sqrt(AB)) 
  b2 <- rnorm(n.f2, 0, sqrt(B))           
  b3 <- rnorm(n.f1, 0, sqrt(A))    
  b <- c(b1, b2, b3)
  res <- 10 + Z %*% b + rnorm(nrow(dat),0,sqrt(r))
  dat$Y <- res
  sonuc=list("y"=res,"Tum model"=dat,Kullanilan_model=2)
  return(sonuc)
}
