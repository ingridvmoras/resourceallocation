# This script tests different energy investment into parasite development
# Created by Paola Carrillo-Bustamante 
# 2019-06-13

rm(list=ls()) #remove old values to avoid troubles
setwd("C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\resourceallocation\\src")
library(deSolve)
library(dplyr)
source('functions.R')  # contains BM periodic waves
source('models.R')     # contains the ODE models we are testing

###sweep functions ----
computeSimulations<-function(s.vec, p.vec, O_init, n.bm, T.sigma, name)
{
  #set the values for the delayed repression of beta_P
  b<-seq(0.0,0.1, by = 0.01)
  
  #set the indection status
  my_s<-my_replace(s,c(O=O_init))
  
  all_runs<-c()
  for(i in b)
  {
    print(paste("simulating ODE system with beta_P ", i, sep = ""))
    my_p<-my_replace(p, c(beta_P = i, number_BM = n.bm, T_sigma = T.sigma*24))
    
    tmp<-run(condition = i, params = my_p, state = my_s)
    all_runs<-bind_rows(all_runs,tmp)
  }
  
  T<-rep(T.sigma, nrow(all_runs))
  NB<-rep(n.bm, nrow(all_runs))
  Infection<-rep(name, nrow(all_runs))
  all_runs<-cbind(all_runs,T,NB, Infection)
  
  #save the generated data
  write.csv2(all_runs, file = paste0("../results/",Sys.Date(),"_paramSweep_betaP_", T.sigma,"_",n.bm,"_",name,".csv"), row.names = FALSE)
}

runSweep<-function(infected=FALSE, n.bm = 1, s.vec,p.vec, T.sigma)
{
  O_init<-0
  name<-"uninfected"
  if(infected)
  {
    O_init<-0.1
    name<-"infected"
  }
  computeSimulations(s.vec, p.vec, O_init, n.bm, T.sigma, name)
}


#set initial states and parameters
s<-c(B = 0, R = 0.5, E = 0.1, O = 0.0, S = 0, beta_E = 0)
p<-c(K = 1, alpha = 1, delta_B = 0.001, beta_P = 0.08, delta_R = 0.005, delta_E = 0.5, gamma=0.1,
     T_sigma = 24*7, delta_sigma = 6, shift_sigma = 12, number_BM = 2,bm.qual=1, number_BM_quality = 1,
     delta_sigma_e20 = 24,a = 0.5, b = 0.5,c=0.3,
     traversalTime = 3*24, sporogonyTime = 11*24)

#set the intervalls we are interested in looking at
T.sigma<-c(5,7,9,11,13)
for(i in T.sigma)
{
  #Run simulations with:
  runSweep(s.vec = s, p.vec = p, infected=TRUE, n.bm = 2, T.sigma = i) # actual sweep
  runSweep(s.vec = s, p.vec = p, infected=FALSE, n.bm = 2, T.sigma = i) #control for eggs
  runSweep(s.vec = s, p.vec = p, infected=TRUE, n.bm = 1, T.sigma = i) #control for parasite
}

