# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SCENARIO GENERATOR
#
# Author: Pamela Torres Nuñez
#         MSM
#
# Date: Oct 16, 2018
# Versión: 1.2
#
# DESCRIPTION: 
# 
# Different Scenarios are generate for obtained data
#
#
# scenario.generator <- function(n, tau, beta, std, mu, dist, par.location = 0, par.scale = 1, par.shape = 1)
#
# n: is the amount of replicates
# tau: is the mean of the treatment defined by the user
# beta: is the factor that represents the blocks
# std: is the standard deviation defined by the user
# mu: is the global mean defined by the user
# dist: is the vector of characters choose one type:
#         "Normal": Normal distribution.
#         "Normal2": Normal-squared distribution from a N(0,1). Basically a Chi^2 with df = 1.
#         "DoubleExp": Double exponential distribution (also known as Laplace distribution).
#         "DoubleExp2": Double exponential squared distribution from a DoubleExp(0,1).
#         "LogNormal": Lognormal distribution.
#         "Gamma": Gamma distribution
#         "Weibull": Weibull distribution
# par.location: location parameter defined by the user usually is 0
# par.scale: scale parameter defined by the user usually is 1
# par.shape: shape parameter defined by the user usually is 0

scenario.generator <- function(n, tau, beta, std, mu, dist, par.location = 0, par.scale = 1, par.shape = 1){
  a= length(tau)
  b= length(beta)
  N= a * b * n # total number of rows
  r= rep(b*a, n) #number of elements in each replicate
  R= rep(1:n, r) #label that defines replicates of each element
  an=  rep(b, a) #number of elements per treatment within a replicate
  A= rep(1:a, an) #treatment labels within a replicate
  A= rep(A, n)  #treatment labels for all replicates
  A= factor(A) #Factor A is created
  #bn = rep(a, b) #number of treatments per block within a replicate
  B= rep(1:b, a) #block labels per replicate
  B= rep(B, n) #block labels for all replicates
  B= factor(B) #Factor B is created
  M= rep (mu, N ) #Overall mean
  Tt= rep(tau, an) #tau values within a replicate
  Tt= rep(Tt, n) #tau values for all replicates
  Bet= rep(beta, a) #bet values within a replicate
  Bet= rep(Bet, n) #bet values for all replicates
  s = rep(std,an) #std values within a replicate 
  S = rep (s, n) #std values for all replicates
  E = getDist(n= N, dist= dist, mu= 0, stdev= S , par.location = par.location, par.scale= par.scale, par.shape= par.shape)
  observations= M + Tt + Bet + E
  scenario = data.frame(R, A, B, M, Tt, Bet, S, E, observations)
  return(scenario)
}
