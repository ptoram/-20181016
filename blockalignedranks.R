# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BLOCKALIGNEDRANKS
#
# Author: Pamela Torres Nuñez
#         MSM
#
# Date: May 01, 2018
# Versión: 1.0
#
# Reference: "alignedranks_20180601.R" & "scoring_20171103.R" by Víctor Tercero
#
# DESCRIPTION: 
# 
# blockalignedranks <- function(x, Treatment, Block, alignement, absolute, score.type)
#
# x: is a numerical vector
# Treatment: is the Treatment defined by the user
# Block: is the Block defined by the user
# alignement: is a vector of characters. Choose one type:
#         "unadjusted": nothing is sustracte from x
#         "overallmean": overall mean is sustracted from x
#         "overallmedian": overall median is sustracted from x
#         "samplemean": group mean from corresponding group is sustracted from x
#         "samplemedian": group median from corresponding group is sustracted from x
# absolute: TRUE for the rank of absolute aligned values.
# score.type: is a vector of characters. Choose one type:
#         "Mood": Mood (1954) statistic is used (r-(N+1)/2)^2
#         "FAB": Freund and Ansari (1957) and Ansari and Bradley (1960) statistis is used (N+1)/2 - abs(r - (N+1)/2)
#         "Klotz": Klotz (1962) statistic is used (qnorm(r/(N+1),0,1))^2
#         "NPL": Nonparametric Levene is used abs(r-groupmean(r))
#         "SR": Squared Ranks statistic is used r^2
#         "TG": Nothing is actually done here. Ordinary ranks are obtained from ordinary ranks
#         "FK": Fligner and Killeen (1976) statistic is used qnorm(1/2 + r/(2*(N+1 )),0,1)

blockalignedranks <- function(x, Treatment, Block, alignement, absolute, score.type){
  a = length(levels(Treatment))
  b = length(levels(Block))
  r = rep(NA, length(x))
  scores = rep(NA, length(x))
  x.aligned = rep(NA,length(x))
  for(i in 1:b){
    xtemp = x[Block==i] 
    groups = Treatment[Block==i]
    aligned.temp = alignedranks(x = xtemp , groups = groups, alignement = alignement, absolute = absolute)
    r[Block==i]= aligned.temp$r
    x.aligned[Block==i] = aligned.temp$a
    scores[Block==i]= scoreX(r = r[Block==i], groups = groups, score.type = score.type)
  }
  results = list(R = r, S = scores, x.aligned = x.aligned)
  return(results)
}


