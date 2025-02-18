library(ggplot2)
library(cowplot)
library(viridis)

# scales::show_col(viridis::viridis_pal()(50))  ---> to see which colors may be nice from the viridis pal

visualize<-function(df, xlim =30, e20=FALSE, cum = FALSE)
{
  n <- length(unique(df$condition))*length(unique(df$type))
  maxEggs<-0.6
  if(cum){maxEggs<-1}
  df_scale_setter_max<-data.frame(time = rep(0.0,n),
                                  condition = rep(unique(df$condition), length(unique(df$type))),
                                  type = rep(c("Blood", "e20","Reserves", "Eggs","Parasite"),length(unique(df$condition))),
                                  value = rep(c(1,1,1.0,maxEggs,1.0),length(unique(df$condition))),  
                                  variable = (rep("R",length(unique(df$type))*length(unique(df$condition)))))
  df_scale_setter_min<-data.frame(time = rep(0.0,n),
                                  condition = rep(unique(df$condition), length(unique(df$type))),
                                  type = rep(c("Blood", "e20","Reserves", "Eggs","Parasite"),length(unique(df$condition))),
                                  value = rep(c(0,0,0.0,0,0),length(unique(df$condition))),  
                                  variable = (rep("R",length(unique(df$type))*length(unique(df$condition)))))
  
  if(isTRUE(e20))
  {
    tmp<-ggplot(df, aes(x = time/24, y = value, colour = variable)) +geom_line(size = 1) +facet_grid(type~condition, scales = "free_y") + geom_point(data = df_scale_setter_max, aes(x = time, y = value), alpha = 0)+ geom_point(data = df_scale_setter_min, aes(x = time, y = value), alpha = 0)
    
    pl.energy<-tmp + xlab("Days") +ylab("Energy (AU)") +theme_gray(base_size = 12) +coord_cartesian(xlim = c(0,xlim))+ scale_color_viridis_d(end = 0.8, labels = c("Blood", "Reproductive Inv","Reserves",  "Eggs","Oocysts", "Sporozoites"), name = "") 
  }else
  {
    if(cum)
    {
      #take the egg variables
      tmp<-subset(df, variable =="E")
      
      #calculate the cumsum in every condition
      out<-ddply(tmp, .(condition), function(X)
        {
        value<-cumsum(X$value -0.1)/3900
        data.frame(value)
      })
      #value<-subset(df, variable == "E")$value
      df[df$variable=="E",]$value<-out$value
    }
    
    tmp<-ggplot(subset(df,type !="e20"), aes(x = time/24, y = value, colour = variable)) +geom_line(size = 1) +facet_grid(type~condition, scales = "free_y",drop = TRUE) + geom_point(data = subset(df_scale_setter_max, type !="e20"), aes(x = time, y = value), alpha = 0)+ geom_point(data = subset(df_scale_setter_min, type !="e20"), aes(x = time, y = value), alpha = 0)
    
    pl.energy<-tmp + xlab("Days") +ylab("Energy (AU)") +theme_gray(base_size = 12) +coord_cartesian(xlim = c(0,xlim))+ scale_color_viridis_d(end = 0.8, labels = c("Blood", "Reserves",  "Eggs","Oocysts", "Sporozoites"), name = "") 
  }
  return(pl.energy)
}

visualize_diffR0<-function(df, xlim = 20)
{
  n <- length(unique(df$condition))*length(unique(df$type))
  maxEggs<-0.6
  df_scale_setter_max<-data.frame(time = rep(0.0,n),
                                  condition = rep(unique(df$condition), length(unique(df$type))),
                                  type = rep(c("Blood", "e20","Reserves", "Eggs","Parasite"),length(unique(df$condition))),
                                  value = rep(c(1,1,1.0,maxEggs,1.0),length(unique(df$condition))),  
                                  R0 = rep(unique(df$R0), length(unique(df$condition))),
                                  NB = rep(unique(df$NB), length(unique(df$condition))),
                                  variable = (rep("R",length(unique(df$type))*length(unique(df$condition)))))
  df_scale_setter_min<-data.frame(time = rep(0.0,n),
                                  condition = rep(unique(df$condition), length(unique(df$type))),
                                  type = rep(c("Blood", "e20","Reserves", "Eggs","Parasite"),length(unique(df$condition))),
                                  value = rep(c(0,0,0.0,0,0),length(unique(df$condition))),  
                                  R0 = rep(unique(df$R0), length(unique(df$condition))),
                                  NB = rep(unique(df$NB), length(unique(df$condition))),  
                                  variable = (rep("R",length(unique(df$type))*length(unique(df$condition)))))
  
  tmp<-ggplot(subset(df,type !="e20"), aes(x = time/24, y = value, colour = variable)) +
    geom_line(size = 1) +
    facet_grid(R0~condition, scales = "free_y",drop = TRUE) +
    geom_point(data = subset(df_scale_setter_max, type !="e20"), aes(x = time, y = value), alpha = 0)+ 
    geom_point(data = subset(df_scale_setter_min, type !="e20"), aes(x = time, y = value), alpha = 0)
  
  pl.energy<-tmp + xlab("Days") +ylab("Energy (AU)") +theme_gray(base_size = 12) +coord_cartesian(xlim = c(0,xlim))+ scale_color_viridis_d(end = 0.8, labels = c("Blood", "Reserves",  "Eggs","Oocysts", "Sporozoites"), name = "") 
}