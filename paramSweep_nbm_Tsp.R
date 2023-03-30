# This script sweeps across combinations of numbers of blood meals nbm and the development length of the parasite Tsp
# Created by Paola Carrillo-Bustamante 
# 2020-07-10

rm(list=ls()) #remove old values to avoid troubles
setwd('~/Nextcloud/Documents/Projects/Metabolism/resourceAllocation/code/')

library(deSolve)
library(dplyr)
source('functions.R')  # contains BM periodic waves
source('models.R')     # contains the ODE models we are testing

###sweep functions ----
computeSimulations<-function(s.vec, p.vec, O_init, T.sigma, name)
{
  #create the parameter vectors for Tsigma and Tsp
  n_bm<-c(1,2,3,4,5)
  T.sp<-c(8,9,10,11,12,13,14)
  
  #generate data frames with the results
  all_runs<-c()
  
  #set the indection status
  my_s<-my_replace(s, c(O = O_init))
  
  #go through each for loop
  for(i in n_bm)
  {
    for (j in T.sp)
    {
      print(paste0("simulating ODE system with n.bm=", i, " and T.sp = ", j))
      
      my_p<-my_replace(p,c(T_sigma = T.sigma*24, sporogonyTime= j*24, number_BM = i))
      
      
      tmp<-run(condition = i, params = my_p, state = my_s,tend = 25)
      names(tmp)[5]<-"NB"
      tmp<-add.type(tmp, j)
      names(tmp)[6]<-"T.sp"
      tmp<-add.type(tmp,name)
      names(tmp)[7]<-"Infection"
      all_runs<-bind_rows(all_runs,tmp)
    }
  }
  
  T_sigma<-rep(T.sigma, nrow(all_runs))
  all_runs<-cbind(all_runs,T_sigma)
  
  print(paste0("saving simulations to: ../results/",Sys.Date(),"_paramSweep_nbm_t.sp_",name,".csv"))
  
  #save the generated data
  write.csv2(all_runs, file = paste0("../results/",Sys.Date(),"_paramSweep_nbm_t.sp_",name,".csv"), row.names = FALSE)
}

runSweep<-function(infected, T_sigma = 5, s.vec, p.vec)
{
  if(infected)
  {
    O_init<-0.1
    name<-"infected"
  }else{
    O_init<-0
    name<-"uninfected"
  }
  print(name)
  computeSimulations(s.vec, p.vec, O_init, T_sigma, name)
}


### set initial states and parameters ---
s<-c(B = 0, R = 0.5, E = 0.1, O = 0.0, S = 0, beta_E = 0)
p<-c(K = 1, alpha = 1, delta_B = 0.001, beta_P = 0.08, delta_R = 0.005, delta_E = 0.5, gamma=0.1,
     T_sigma = 24*5, delta_sigma = 6, shift_sigma = 12, number_BM = 1,bm.qual=1, number_BM_quality = 1,
     delta_sigma_e20 = 24,a = 0.5, b = 0.5,c=0.3,
     traversalTime = 3*24, sporogonyTime = 11*24)

#Run simulations with:
runSweep(infected=TRUE, T_sigma = 5) # actual sweep
runSweep(infected=FALSE, T_sigma = 5) #control for eggs
#runSweep(infected=TRUE, T_sigma = 5) #control for parasite


