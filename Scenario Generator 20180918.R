# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SCENARIO GENERATOR
#
# Author: Pamela Torres Nuñez
#         MSM
#
# Date: Sep 18, 2018
# Versión: 1.1
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
  r= rep(b*a, n)
  R= rep(1:n, r)
  an<- rep(b,a)
  A= rep(1:a, an) #create rep
  A= rep(A, n)  #realiza las reps n veces
  A= factor(A) #factor del modelo
  bn = rep(a, b)
  B= rep(1:b, n)
  B= rep(B, a)
  B= factor(B)
  M= rep (mu, N )
  Tt= rep(tau, an)
  Tt= rep(Tt, n)
  Bet= rep(beta, n)
  Bet= rep(Bet, a)
  s = rep(std,an)
  S = rep (s, n)
  E = getDist(n= N, dist= dist, mu= 0, stdev= S , par.location = par.location, par.scale= par.scale, par.shape= par.shape)#
  observations= M + Tt + Bet + E
  scenario = data.frame(R, A, B, M, Tt, Bet, S, E, observations)
  return(scenario)
}
