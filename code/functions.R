library(ggplot2)
library(reshape2)

### blood meal intake: periodic function -----
my_sigma<-function(t, T, delta, shift) #T: period of new blood meals (hours); delta: duration of blood feeding (hours): shift:time to first BM (hours)   
{
  if(delta>T/2)
  {
    stop("duration of blood meal delta must be smaller than the period T/2")
  }
  if(( (t-shift) %% (T)) < delta)
  {
    x<-1
  }else
  {
    x<-0
  }
  return(x)
}

## function that runs the simulations, visualizes the results, and returns the data frame. -----
run <- function(state = s, params = p,tmin = 0, tend = 30, condition = "")
{
  #set time vector
  t<-seq(from = 0, to = tend*24, by = 0.01)
  
  out <- data.frame(ode(y = state, times = t, func = model, parms = params, method = "impAdams"))
  out.t <- transform.df(out, cond = condition)
  return(out.t)
}

#change parameters -----
my_replace<-function(p.old,p.new)
{
  replace(p.old,names(p.new), p.new)
}

#functions that transform the obtained data sets -----
add.type<-function(df, category)
{
  type<-rep(category, nrow(df))
  new.df<-cbind(df,type)
}

transform.df<-function(df, cond)
{
  #use melt to transform the data
  variables=c('B','R','E', "O","S", "beta_E")
  new.df<-melt(df, measure.vars = variables)
  new.df$variable <- factor(new.df$variable, levels = c('B','R','E',"beta_E", "O","S"))
  
  #separate the df into the three groups to add the type
  resources<-subset(new.df, variable == "R")
  resources<-add.type(resources, "Reserves")
  
  storage<-subset(new.df, variable == "B")
  storage<-add.type(storage, "Blood")
  
  parasite<-subset(new.df, variable == "O" | variable == "S")
  parasite<-add.type(parasite, "Parasite")
  
  eggs<-subset(new.df, variable == "E")
  eggs<-add.type(eggs, "Eggs")
  
  e20<-subset(new.df, variable == "beta_E")
  e20<-add.type(e20, "e20")
  
  df.complete<-rbind(storage,e20, resources,eggs,parasite)
  df.complete$type<-factor(df.complete$type, levels = c("Blood","e20","Reserves", "Eggs", "Parasite"))
  df.complete$variable<-factor(df.complete$variable, levels = c("B", "beta_E", "R", "E", "O", "S"))
  condition<-rep(cond,nrow(df.complete))
  df.complete<-cbind(df.complete,condition)
  return(df.complete)
}

## function to calculate fitness of egg and parasites ----
localMaxima<-function(params, data, tend)
{
  freq<-params["T_sigma"]
  N<-24*tend/freq
  seq<-0:N#
  #seq<-0:(params["number_BM"]-1)
  
  df<-subset(data, variable =="E")
  out<-c()
  for(i in seq)
  {
    tmp<-subset(df, (time>=freq*i & time <freq*(i+1)))
    out<-rbind(max(tmp$value),out)
  }
  localMaximum<-sum(out)
  return(localMaximum)
}

calculateEnergyPerBloodMeal<-function(params,data,tend)
{
  freq<-params["T_sigma"]
  N<-24*tend/freq
  #seq<-0:N
  seq<-0:(params["number_BM"]-1)
  df<-subset(data, variable == "E")
  out<-c()
  for(i in seq)
  {
    tmp<-subset(df, (time>=freq*i & time <freq*(i+1)))
    tmp2<-data.frame(i+1,max(tmp$value))
    out<-rbind(tmp2,out)
  }
  return(out)
}


calculateFitness<-function(parVector, df, var = "E", tend = 20)
{
  if(var =="E")
  {
    value<-localMaxima(params=parVector, data=df, tend)
  }
  if(var =="S")
  {
    value<-max(subset(df, variable =="S")$value)
  }
  fitness<-data.frame(value,var)
  
  return(fitness)
}

calculateFitnessDifference<-function(parVector, df, control.df, var ="E", setting = "", tend = 30)
{
  control.df.fit<-calculateFitness(parVector, control.df, var = var, tend = tend)
  df.fit<-calculateFitness(parVector, df, var = var, tend = tend)
  value<-df.fit$value - control.df.fit$value
  out<-data.frame(var, value, setting)
  return(out)
}

calculateFitnessFoldDifference<-function(parVector, df, control.df, var ="E", setting = "", tend = 30)
{
  control.df.fit<-calculateFitness(parVector, control.df, var = var, tend = tend)
  df.fit<-calculateFitness(parVector, df, var = var, tend = tend)
  value<-df.fit$value / control.df.fit$value
  out<-data.frame(var, value, setting)
  return(out)
}