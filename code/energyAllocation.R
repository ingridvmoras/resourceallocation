# This script simulates the energy allocation wihtin the mosquito after a bloodmeal
# Created by Paola Carrillo-Bustamante 
# 2019-05-29

rm(list=ls()) #remove old values to avoid troubles
setwd("C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\resourceallocation\\code")

library(deSolve)
source('functions.R')  # contains BM periodic waves and the function running the simulations
source('models.R')     # contains the ODE model
source('../src/visualize.R')  # contains the plotting functions


### set the values of states and parameters for all conditions ----
# These are the starting parameters: simulation of one blood meal without parasite 
s<-c(B = 0, R = 0.8, E = 0.1, O = 0.0, S = 0, beta_E = 0)
p<-c(K = 1, alpha = 1, delta_B = 0.001, beta_P = 0.08, delta_R = 0.005, delta_E = 0.5, gamma=0.1,
     T_sigma = 24*5, delta_sigma = 6, shift_sigma = 12, number_BM = 1,bm.qual=1, number_BM_quality = 1,
     delta_sigma_e20 = 24,a = 0.5, b = 0.5,c=0.3,
     traversalTime = 3*24, sporogonyTime = 11*24)

## there are the parameters of 2 bm
p.2bm<-my_replace(p,c(number_BM=3))

##there are the states of infected bms
s.par<-my_replace(s, c(O=0.01))

### 1. run the control simulations ----
bm_1<-run(condition = "BM") 
bm_2<-run(condition = "3BM", params  = p.2bm)

## ibm
ibm_1<-run(condition = "iBM", state = s.par)
ibm_2<-run(condition = "iBM + 2BM", state = s.par, params = p.2bm)

#save this simulation as a 'control experiment'
write.csv2(ibm_1, file = "..\\results\\iBM_control.csv", row.names = FALSE)

### 2. model a 'weak' mosquito ----
w_bm_K<-run(condition = "K=0.5", state = s, params = my_replace(p.2bm,c(K=0.5)))
w_bm_R<-run(condition = "R=0.15", state = my_replace(s, c(R=0.15)), params = p.2bm)

w_ibm_K<-run(condition = "iBM K=0.5", state = s.par, params = my_replace(p.2bm,c(K=0.5)))
w_ibm_R<-run(condition = "iBM R=0.15", state = my_replace(s.par, c(R=0.15)), params = p.2bm)
w_ibm_1_R<-run(condition = "1 iBM R=0.15", state = my_replace(s.par, c(R=0.15)), params = p)
### 3. change the quality of a blood meal ----
poor_bm<-run(condition = "poor bm", state = s.par, params = my_replace(p.2bm, c(bm.qual  = 0.05)))
#rich_bm<-run(condition = "rich bm", state = s.par, params = my_replace(p.2bm, c(bm.qual = 2)))

## 4. run representative examples for every coming sweep----
# 4.1 beta_P 
ibm_beta_P<-run(condition = "ibm", state = s.par, params = my_replace(p,c(beta_P=0.06)))
ibm_2_beta_P<-run(condition = "ibm + BM", state = s.par, params = my_replace(p.2bm,c(beta_P=0.06)))

pdf(paste0("../results/figures/",Sys.Date(), "_betaP.pdf"), width = 9.5, height = 6.25)
visualize(rbind(bm_2, ibm_beta_P, ibm_2_beta_P), cum = TRUE)+theme_bw(base_size = 16*96/72)
dev.off()

## visualize the results ----
png(paste0("../results/figures/",Sys.Date(), "_all.png"), width = 12, height = 6.25, units = "in", res = 400)
visualize(rbind(ibm_1, bm_2, ibm_2))+theme_bw(base_size = 16*96/72)
dev.off()

pdf(paste0("../results/figures/",Sys.Date(), "_all.pdf"), width = 9.5, height = 6.25)
visualize(rbind(bm_1, ibm_1, bm_2, ibm_2))+theme_bw(base_size = 16*96/72)
dev.off()

png(paste0("../results/figures/",Sys.Date(), "_weak_all.png"), width = 18.5, height = 12.25, units = "in", res = 400)
visualize(rbind(bm_2, ibm_2,w_bm_K,w_ibm_K,w_bm_R,w_ibm_R, poor_bm))+theme_bw(base_size = 16*96/72)
dev.off()

png(paste0("../results/figures/",Sys.Date(), "_weak_R.png"), width = 9.5, height = 6.25, units = "in", res = 400)
visualize(rbind(bm_2, ibm_2,w_bm_R,w_ibm_R))+theme_bw(base_size = 16*96/72)
dev.off()

png(paste0("../results/figures/",Sys.Date(), "_weak_K.png"), width = 9.5, height = 6.25, units = "in", res = 400)
visualize(rbind(bm_2, ibm_2,w_bm_K,w_ibm_K))+theme_bw(base_size = 16*96/72)
dev.off()

png(paste0("../results/figures/",Sys.Date(), "_weak_bm.png"), width = 9.5, height = 6.25, units = "in", res = 400)
visualize(rbind(bm_2, ibm_2,poor_bm))+theme_bw(base_size = 16*96/72)
dev.off()