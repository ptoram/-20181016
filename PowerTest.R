Power.test<- function (score.type, alignement,absolute, n, tau, beta, std, mu, dist, r, method.test, alpha, par.location = 0, par.scale = 1, par.shape = 1){
  
  if (alpha < 0 | alpha > 1){
    print("Error: alpha value should be between 0 and 1")
    return()
  }
  
  #variables are initialized
  reject = rep(NA,r)
  p = rep(NA, r)
  
  #power is estimated
  for(i in 1:r){
    X = scenario.generator(n = n, tau = tau, beta = beta, std = std, mu = mu, dist = dist)
    results = Anompa(x = X$observations, n = n, Treatment = X$A, Block = X$B, alignement = alignement, 
               absolute = absolute, score.type = score.type, method.test = method.test)
    #save the pvalue 
    p = results$pvalue
    
    if(p<=alpha){
    reject[i] = 1
    }else{
    reject[i] = 0
    }
  }
  Power = mean(reject) #, na.rm = TRUE)
  return(Power)
}


