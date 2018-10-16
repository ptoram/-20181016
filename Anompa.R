Anompa <- function(x, n, Treatment, Block, alignement, absolute, score.type, method.test){
  
  #error verification
  feasibles.methods = c("chiSO","FstaO","chiSN", "FstaN")
  if(all(method.test != feasibles.methods)){
    print("Error: method test not recognized")
    return()
  }
  
  k = length(levels(Treatment))
  b = length(levels(Block))
  df = k-1
  dat = blockalignedranks(x= x, Treatment = Treatment, Block = Block, alignement = alignement, absolute = absolute, score.type = score.type)
  xframe = data.frame( dat, Block, Treatment)
  N = length(xframe$S)
  Xi = xframe$S
  Cj = tapply(Xi, xframe$Treatment, sum)
  Ri = tapply(Xi, xframe$Block, sum)
  Tij = sum(Ri)
  OX = Tij/(b*k*n)  #OverallMean
  CF = (Tij^2)/(b*k*n) #Correction Factor
  TSS = sum(Xi^2)-CF
  SST = (1/(b*n))*sum(Cj^2)-CF
  SSB = (1/(n*k))*sum(Ri^2)-CF
  Sij = tapply(Xi, list(xframe$Treatment, xframe$Block), sum)
  SSI = (1/n)*sum(Sij^2)-(1/(n*k))*sum(Ri^2)-(1/(b*n))*sum(Cj^2)+CF
  SSE = TSS-SST-SSB-SSI
  
  #obtained old values
  varx = TSS/(n*b*k-1) # variance 
  MST = SST/(k-1) 
  MSE = SSE/(b*k)*(n-1)
  chiSO = (MST/varx)*((k-1)/k)
  FstaO = MST/MSE
  
  #obtained New values
  chiSN = (b*(n*k-1)*SST)/TSS
  denF = SST/(k-1)
  nuF = (TSS-SST)/(n*k*b-k)
  FstaN = denF/nuF
  
  
  if(method.test == "chiSO"){
    pvalue = 1 - pchisq(q = chiSO, df = df, lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue,  chiSO = chiSO, df= df )
    return(results)
  }
  
  
  if(method.test == "FstaO"){
    pvalue = 1 - pf(FstaO, df1 = (k-1), df2 = (n-1)*(b*k), lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue, FstaO = FstaO, df1 = (k-1), df2 = (n-1)*(b*k))
    return(results)
  }
  
  
  if(method.test == "chiSN"){
    pvalue = 1 - pchisq(q = chiSN, df = df, lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue,  chiSN = chiSN, df= df )
    return(results)
  }
  
  
  if(method.test == "FstaN"){
    pvalue = 1 - pf(FstaN, df1 = (k-1), df2 = (n-1)*(b*k), lower.tail = TRUE, log.p = FALSE)
    results = list(pvalue = pvalue, FstaN = FstaN, df1 = (k-1), df2 = (n-1)*(b*k))
    return(results)
  }
}
