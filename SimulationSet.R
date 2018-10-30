setwd("C:/Users/User/OneDrive/MSM/Tesis/Codigos R")
source("GetDist.R")
source("Scenario Generator.R")
source("alignedranks.R")
source("scoring.R")
source("blockalignedranks.R")
source("PowerTest.R")
source("Anompa.R")


r = 1 #amount of replicates in simulation
n.set = c(2,5,10,20,50,100) #replicates in the design
mu = 0
tau.set = list(tau5 = c(0,0,0,0),
               tau6 = c(-1,-1,1,1),
               tau7 = c(-2,-2,2,2),
               tau8 = c(-3,-3,3,3))

beta = c(0,0,0,0)

std.set = list(std5 = c(1,1,1,1),
               std6 = c(1,1,2,2),
               std7 = c(1,1,3,3),
               std8 = c(1,1,4,4))


distr.set = c("Normal", "Normal2","DoubleExp","DoubleExp2","LogNormal","Gamma","Weibull")

par.location = 0

par.scale = 1

par.shape.set = c(0.5,1,3,10)

score.type.set = c("Mood", "FAB", "Klotz", "SR", "TG", "FK")

alignement.set = c("unadjusted", "overallmean", "overallmedian")

absolute.set = c(TRUE, FALSE)

method.test.set =  c("chiSO", "FstaO","chiSN", "FstaN")[1]

alpha = 0.05

#Data base initialization
DB.score.type = NA
DB.alignement = NA 
DB.absolute = NA
DB.n = NA
DB.tau = NA
DB.beta = NA
DB.std = NA
DB.mu = NA
DB.dist = NA
DB.par.shape = NA
DB.r = NA
DB.method.test = NA
DB.alpha = NA
DB.power = NA

##################################################################################################
## Normal 
i = 0
for(dist in distr.set){
  if(any(dist == c("Normal","Normal2", "DoubleExp","DoubleExp2","LogNormal"))){
    par.location = 0
    par.scale = 1
    par.shape.set = 0
  }
 
  if(any(dist == c("Gamma", "Weibull"))){
    par.location = 0
    par.scale = 1
    par.shape.set = c(0.5,1,3,10)
  } 

for(par.shape in par.shape.set){
for(n in n.set){
  for(tau in tau.set){
    for(std in std.set){
      for(score.type in score.type.set){
        if(any(score.type == c("Mood","FAB","NPL","Klotz"))){
          alignement.set = "unadjusted"
          absolute.set = F
        }
        if(any(score.type == c("SR","FK"))){
          alignement.set = c("overallmean", "overallmedian")
          absolute.set = T
        }
        
        if(any(score.type == "TG")){
          alignement.set = c("unadjusted", "overallmean", "overallmedian")
        }
        for(alignement in alignement.set){
          if((alignement == "unadjusted") & (score.type == "TG")){
            absolute.set = F
          }
          if(any(alignement == c("overallmean", "overallmedian") & score.type == "TG")){
            absolute.set = T
          }
          for(absolute in absolute.set){
            
            for(method.test in method.test.set){
              
              i = i+1
              print(i)
              DB.score.type[i] = score.type
              DB.alignement[i] = alignement 
              DB.absolute[i] = absolute
              DB.n[i] = n
              DB.tau[i] = paste(tau, collapse =" ")
              DB.beta[i] = "Default"
              DB.std[i] = paste(std, collapse =" ")
              DB.mu[i] = mu
              DB.dist[i] = dist
              DB.par.shape[i] = par.shape
              DB.r[i] = r
              DB.method.test[i] = method.test
              DB.alpha[i] = alpha
              DB.power[i] = Power.test (score.type = score.type, alignement = alignement, absolute= absolute, n = n, 
                                        tau = tau, beta = beta, std = std, mu = mu, dist = dist, r = r, method.test = method.test, alpha = alpha, par.shape = par.shape)
              
            }
          }
        }
      }
    }
  }
}
}

}
Results = data.frame(DB.score.type, DB.alignement, DB.absolute,DB.n, DB.tau, DB.beta, DB.std, DB.mu, DB.dist, DB.par.shape, DB.r, DB.method.test, DB.alpha, DB.power)



