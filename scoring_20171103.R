#####################################################################################
#
#     SCORE RANKS
#
#     Author: Dr. Víctor G. Tercero Gómez
#     Affiliation: Tecnologico de Monterrey
#
#     Date: November 3, 2017
#     Versión: 1.0
#
#     DESCRIPTION
#
#     The function get different linear rank scores from a vector of ranks.
#
#     r: is a numerical vector of ranks (ranks transformation was applied to x observataions)
#     groups: is a factor that indicates the group each element of x belong to
#     score.type: is a vector of characters. Choose one type:
#         "Mood": Mood (1954) statistic is used (r-(N+1)/2)^2
#         "FAB": Freund and Ansari (1957) and Ansari and Bradley (1960) statistis is used (N+1)/2 - abs(r - (N+1)/2)
#         "Klotz": Klotz (1962) statistic is used (qnorm(r/(N+1),0,1))^2
#         "NPL": Nonparametric Levene is used abs(r-groupmean(r))
#         "SR": Squared Ranks statistic is used r^2
#         "TG": Nothing is actually done here. Ordinary ranks are obtained from ordinary ranks
#         "FK": Fligner and Killeen (1976) statistic is used qnorm(1/2 + r/(2*(N+1 )),0,1)



scoreX <- function(r, groups, score.type){

  #Error verification
  feasibles.scores = c("Mood", "FAB", "Klotz", "NPL", "SR", "TG", "FK")
  if(all(score.type != feasibles.scores)){
    print("Error: score type is not recognized")
    return()
  }
  
  #Preliminaries
  N = length(r) #Combined total
  
  #Scoring
  if(score.type == "Mood"){
    s = (r-(N+1)/2)^2
  }
  if(score.type == "FAB"){
    s = (N+1)/2 - abs(r - (N+1)/2)
  }
  if(score.type == "Klotz"){
    s = (qnorm(r/(N+1),0,1))^2
  }
  if(score.type == "NPL"){
    groupmean = tapply(r,groups,mean)
    n = tapply(r,groups,length)
    k = length(n)
    rmeans = rep(NA, N)
    for(i in 1:k){
      rmeans[groups==i]= groupmean[i]
    }
    
    s = abs(r-rmeans)
  }
  if(score.type == "SR"){
    s = r^2 #nothing to see here.
  }
  if(score.type == "TG"){
    s = r #nothing to see here.
  }
  if(score.type == "FK"){
    s = qnorm(1/2 + r/(2*(N+1 )),0,1)
  }
  #Now we are done. Let's get the result:
 return(s)
}