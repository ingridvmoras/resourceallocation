# This script visualizes the result from the paramSweeBeta_P (energy investment into parasite development)
# Created by Paola Carrillo-Bustamante 
# 2019-10-17


rm(list=ls()) #remove old values to avoid troubles
setwd("C:\\Users\\isabe\\OneDrive\\Documentos\\GitHub\\resourceallocation\\coderesults")

library(cowplot)
library(dplyr)
library(RColorBrewer)

source('..\\src\\visualize.R')  # contains the plotting functions

#files containing the data
nm<-list.files(pattern = "betaP_")

#read the data
all_runs<-do.call(bind_rows, lapply(nm, function(x) read.csv2(x))) 

#make the data ggplot friendly
levels(all_runs$variable)<-c("Blood","e20","Eggs", "Oocysts","Reserves","Sporozoites")

#plot
panelA<-ggplot(subset(all_runs, variable == "Eggs"), aes(x=time/24, y = value, colour = condition, group = condition)) +geom_line()+ scale_colour_gradient(name=expression(paste("Parasite\ndevelopment",(beta[P]))),low = "#190C3EFF", high = "#FCA007FF") +theme_bw(base_size = 12*96/72) +ylab("Energy (AU)") + xlab("Time") +facet_grid(rows = vars(T))


panelB<-ggplot(subset(all_runs, variable == "Sporozoites"), aes(x=time/24, y = value, colour = condition, group = condition)) +geom_line()+ scale_colour_gradient(name=expression(paste("Parasite\ndevelopment",(beta[P]))),low = "#190C3EFF", high = "#FCA007FF") +theme_bw(base_size = 10*96/72) +ylab("Energy (AU)") + xlab("Time")+facet_grid(rows = vars(T))

#save the plots
pdf(file = paste0("figures/", Sys.Date(), "_paramSweep_betaP_all.pdf"), width = 12*1.25, height = 6*1.25)
plot_grid(panelA, panelB, labels = c("A","B"), label_size = 12*96/72, nrow = 1)
dev.off()

png(file = paste0("figures/", Sys.Date(), "_paramSweep_betaP_all.png"), width = 12*1.25, height = 6*1.25, units = "in", res = 400)
plot_grid(panelA, panelB, labels = c("A","B"), label_size = 12*96/72, nrow = 1)
dev.off()