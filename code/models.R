## model of energy/resource allocation and time-dependent ovary development ----
model<-function(t, state, parms){
  with(as.list(c(state,parms)),{
    
    #make the number of blood meals variable---> make sigma become zero after number_BM
    if(t< T_sigma*number_BM)
    {
      #to model bm quality/size: step function that needs to be substracted from sigma
      my_bm.qual <-(1-bm.qual)*my_sigma(t, T_sigma*(number_BM + 3),delta_sigma, shift_sigma + T_sigma*(number_BM_quality-1)) #quality of a bm: square wave function with a period much larger than T_Sigma (so that it only affects one bm), and is shifted depending on which BM should be affected
      sigma<-my_sigma(t, T_sigma, delta_sigma, shift_sigma) -my_bm.qual
      sigma_b<-my_sigma(t, T_sigma, delta_sigma_e20, shift_sigma)  #e20 activation --> describes when beta_E(t) will be 'on'
      sigma_eggs<-my_sigma(t, T_sigma, delta_sigma, shift_sigma+ 2.5*24)  #this is the time period during which oviposition happens
      sigma_refill<-my_sigma(t,T_sigma, 60, shift_sigma +24) #this is the time during which the reserves are filled...it happens with a delay...after 24 h approximately and lasts ~3 days
      if(sigma_b == 1)
      {
        b <-0
      }
    }
    else{
      sigma <-0
      sigma_b<-0
      sigma_eggs<-0
      sigma_refill <-0
    }
     
    #set the initial_time (in hours)
    init_time = shift_sigma+delta_sigma
    
    #set time-dependent variables for parasite development
    # beta_P(t)
    if(t<=traversalTime + init_time)
    {
      beta = 0
    }
    else
    {
      beta = beta_P
    }
    
    # gamma(t)
    if(t <= sporogonyTime +init_time)
    {
      gamma1 = 0
    }
    else
    {
      gamma1 = gamma
    }
    
    #alpha(t)
    alpha = alpha*sigma_refill
    
    #make sure there is a 'minimal' reserve left 
    ce=c
    if(R<=0.1)
    {
      delta_R = 0
      beta = 0
      ce = 1e-2
    }
    #make sure there is a 'minimal' energy left in the ovaries
    if(E<=0.1)
    {
      delta_E = 0
    }
    
    #beta_E(t): 
    f = 1-beta_E
    dbeta_E<-a*f*sigma_b - b*beta_E 
    
    #oviposition
    delta_E = delta_E*sigma_eggs
    
    dB<-sigma*(1-B) -alpha*B*R*(1-R/K) -delta_B*B
    dR<-alpha*B*R*(1-R/K) -ce*beta_E*R*E*(1-E) -beta*R*O*(1-O) -delta_R*R
    dE<- ce*beta_E*R*E*(1-E) -delta_E*E
    dO<- beta*R*O*(1-O) - gamma1*O
    dS<- gamma1*O
    
    return(list(c(dB,dR,dE,dO,dS,dbeta_E)))
    })
}