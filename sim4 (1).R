####4 Faktörlü Rasgele Etkili Etkileşimsiz Model####
library("lme4")
sim4=function(n.f1,n.f2,n.f3,n.f4,A,B,C,D,r,mu){ 
  dat <- expand.grid(F1 = as.factor(1:n.f1), F2 = 1:n.f2,F3=1:n.f3,F4=1:n.f4)
  dat$Y <- 0 + rnorm(n.f1*n.f2*n.f3)
  myformula <- "Y ~ (1|F1) + (1|F2)+ (1|F3)+(1|F4)"  # model formula
  mylF <- lFormula(eval(myformula), data = dat) # Process the formula against the data
  Z <- mylF$reTrms$Zt %>% as.matrix() %>% t()  # Extract the Z matrix
  b1 <- rnorm(n.f4, 0, sqrt(D))
  b2 <- rnorm(n.f3, 0, sqrt(C))
  b3 <- rnorm(n.f2, 0, sqrt(B))             # random interecepts for B
  b4 <- rnorm(n.f1, 0, sqrt(A))     # random interecepts for A
  b <- c(b1, b2,b3,b4)
  res <- 10 + Z %*% b + rnorm(nrow(dat),0,sqrt(r))
  dat$Y <- res
  sonuc=list("y"=res,"Tum model"=dat,Kullanilan_model=4)
  return(sonuc)
}
