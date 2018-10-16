#####################################################################################
#
#     ALIGNED RANKS
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: June 1, 2018
#     Versión: 2.0
#
#     INSTRUCTIONS
#
#     Function calculates the average ranks of aligned observations of k>=1 groups
#
#     x: is a numerical vector
#     groups: is a factor that indicates the group of each observation in x
#     alignment: is a vector of characters. Choose one type:
#         "unadjusted": nothing is sustracte from x
#         "overallmean": overall mean is sustracted from x
#         "overallmedian": overall median is sustracted from x
#         "samplemean": group mean from corresponding group is sustracted from x
#         "samplemedian": group median from corresponding group is sustracted from x
#     absolute: TRUE for the rank of absolute aligned values.


alignedranks <- function(x, groups, alignement, absolute){

  #Error verification
  feasibles.alignements = c("unadjusted", "overallmean", "overallmedian", "samplemean", "samplemedian")
  if(all(alignement != feasibles.alignements)){
    print("Error: alignement type not recognized")
    return()
  }
  
  #Preliminaries
  n = tapply(x,groups,length)#used in samplemean and samplemedian
  k = length(n)
  N = sum(n) #combined samples total
  a = rep(0,N) #vector a is initialized
  #Alignment
  if(alignement == "unadjusted"){
    a = x
  }
  if(alignement == "overallmean"){
    a = round(x-mean(x),7)
  }
  if(alignement == "overallmedian"){
    a = round(x-median(x),7)
  }
  if(alignement == "samplemean"){
    xmean = tapply(x,groups,mean)
    vmean = rep(NA, N)
    for(i in 1:k){
      vmean[groups==i]= xmean[i]
    }
    a = round(x - vmean,7)
  }
  if(alignement == "samplemedian"){
    xmedian = tapply(x,groups,median)
    vmedian = rep(NA, N)
    for(i in 1:k){
      vmedian[groups==i]= xmedian[i]
    }
    a = round(x - vmedian,7)
  }
  
  #Ranking
  if(absolute == T){
    a = abs(a)
    r = rank(a)
  }
  if(absolute == F){
    r = rank(a)
  }
  results = list(a=a, r=r)
  return(results)
}
