###########################
#   CUSTOM FUNCTIONS FOR SIMULATIONS (GENERATING DATA / POWER ANALYSIS): DATA MISSING IN BLOCKS  
###########################

# packages
library(truncnorm)
library(nlme)
library(missMethods)
library(MASS)
library(dplyr)


# Three function follow: 
# 1) AR_simulate_data_block = Data generation function. Creates a single dataset, introduces data missing in a block of consecutive observations)
# 2) AR_fit_model_block = Function that fits the models and extracts relevant information
# 3) AR_simulate_missing_block


# The functions are a modified version of code initially written by Ginette Lafit 

#####################################################
###################### AR_simulate_data_block
#####################################################

AR_simulate_data_block = function(N,T.obs,Ylag.center,
                              b00, b10, sigma, rho.v, sigma.v0, sigma.v1, compliance_mean = 0.85){
  
  # Create number of observations: T.obs + T.burning
  T.burning = 1000
  T.total = T.burning + T.obs
  
  # Simulate error within-person errors
  E = rnorm(T.total*N,0,sigma)
  
  # Simulate error level-2
  # Simulate between-subject random effect
  Sigma.nu = cbind(c(sigma.v0^2,rho.v*sigma.v0*sigma.v1),c(rho.v*sigma.v0*sigma.v1,sigma.v1^2))
  
  # Ensure the model is stationary
  V.j = matrix(0,N,ncol(Sigma.nu))
  colnames(V.j) = c('V.0','V.1')
  
  for (i in 1:N){ # THE PARTICIPANT LOOP
    lambda.max = 1
    iter = 1
    while(abs(lambda.max)>=1){
      nu.i = mvrnorm(1,rep(0,ncol(Sigma.nu)),Sigma.nu)
      names(nu.i) = c('V.0','V.1')
      Psi.i = c(b10+nu.i['V.1'])
      lambda.max = Psi.i
      iter = iter + 1
    }
    V.j[i,] = nu.i
  }
  
  V = NULL
  for (j in 1:N){
    V = rbind(V,matrix(unlist(lapply(1:ncol(Sigma.nu), function(p) rep(V.j[j,p],T.total))), ncol=ncol(Sigma.nu), byrow=F))
  }
  
  colnames(V) = c('V.0','V.1')
  
  # Function to get recursive equation
  data.Y = cbind(expand.grid(Obs=1:T.total,subjno=1:N))
  Y = rep(0, T.total*N)
  comp = rep(0, T.total*N)
  compliance = rep(0, N)
  true_mean = rep(0, N)
  missing = rep(0, N)
  n.ID = unique(data.Y$subjno)
  for (i in n.ID){
    T.obs.i = which(data.Y$subjno==i)
    
    compliance[i] = compliance_mean
    # Initial value
    Y[T.obs.i[1]] = b00 + V[T.obs.i[1],'V.0'] + E[T.obs.i[1]]
    
    for (t in T.obs.i[-1]){
      # Simulate Dependent Variable
      Y[t] = b00 + b10*Y[t-1] + V[t,'V.0'] + V[t,'V.1']*Y[t-1] + E[t] # the "full" time series without missing data
      compliance[t] = compliance[i]
      true_mean[t] =  (b00 + V[t,'V.0']) / (1 - (b10 + V[t,'V.1']))# save the individual participant's true process mean. mean = person's intercept / (1 - person's AR effect)
    }
  }
  
  data.Y = cbind(data.Y,Y,compliance, true_mean)
  
  
  T.total.i = NULL
  for (i in n.ID){
    T.total.i = c(T.total.i,which(data.Y$subjno==i)[-seq(1:T.burning)])
  }
  
  # Create a data frame for T.obs
  data.Y = data.Y[T.total.i,]
  
  # Introduce blocks of missing data in the time-series, separately for each participant
  data = data.frame()
  for(subjno in 1:N){
    data_participant = data.Y[data.Y$subjno == subjno,]
    block_length = T.obs * (1 - compliance_mean)
    data_participant$Y[5:(5+block_length)] = NA
    data = rbind(data, data_participant)
  }
  
  # Create the lag variable
  data = data %>%
    group_by({{subjno}}) %>% 
    mutate(Ylag = dplyr::lag(Y, n = 1, default = NA),
           observed_mean = mean(Y, na.rm = TRUE), # compute the observed mean in the time-series with missing data
           bias_mean = abs(true_mean - observed_mean)) # compute bias of the person-mean estimation = the absolute value of the difference between the true and observed person-mean
           
  
  return(list(data = data, RE = V.j)) 
}


#####################################################
###################### AR_fit_model_block 
#####################################################


AR_fit_model_block = function(data,N,T.obs,Ylag.center,
                                       b00, b10, sigma, rho, sigma.v0, sigma.v1, alpha, estimate_randomslopes){
  if (Ylag.center==TRUE){
    # If Ylag.center is TRUE Mean centered lag varying variable per-individual
    N.subject = unique(data$subjno)
    for (i in N.subject){
      data$Ylag[which(data$subjno==i)] = data$Ylag[which(data$subjno==i)] - mean(data$Y[which(data$subjno==i)],na.rm=TRUE)
    }}
  
  # Fit linear mixed-effects models 
  if ( estimate_randomslopes == TRUE) {
    fit.lme = try(lme(Y ~ Ylag, random = ~ 1 + Ylag|subjno,data=data,na.action=na.omit,control=lmeControl(opt='optim')), silent = FALSE)
  } else if (estimate_randomslopes == FALSE) {
    fit.lme = try(lme(Y ~ Ylag, random = ~ 1|subjno,data=data,na.action=na.omit,control=lmeControl(opt='optim')), silent = FALSE)}
  
  
  if (length(fit.lme)>1){
    
    # Obtain the random effects
    Random.Effects.lme = random.effects(fit.lme)
    # Obtain the number of observations used in the model 
    model_observations = fit.lme$dims[[1]]
    
    fit.lme = coef(summary(fit.lme))

    # Compute power and standard error from lme
    beta.hat.lme = fit.lme[,'Value']
    power.hat.lme = fit.lme[,'p-value'] < alpha
    StdError.beta.lme = fit.lme[,'Std.Error']
    bias_mean = mean(data$bias_mean)
    
    return(list(beta.hat.lme=beta.hat.lme,
                power.hat.lme=power.hat.lme,
                StdError.beta.lme=StdError.beta.lme,
                Random.Effects.lme=Random.Effects.lme,
                bias_mean = bias_mean,
                model_observations = model_observations))}
  
  if (length(fit.lme)==1){
    return(list(fit.lme))
  }
}


#####################################################
###################### AR_simulate_missing_block 
#####################################################

AR_simulate_missing_block = function(N,T.obs,Ylag.center,
                                     b00, b10,  sigma, rho, sigma.v0, sigma.v1,
                                     rho.v, alpha, R, compliance_mean, estimate_randomslopes){
  
  # Simulate data from the linear mixed-effects model
  data.list = lapply(1:R, function(r) AR_simulate_data_block(N,T.obs,Ylag.center,
                                                         b00, b10, sigma, rho.v, sigma.v0, sigma.v1, compliance_mean))
  
  fit.list.sim = lapply(1:R, function(r) AR_fit_model_block(data.list[[r]]$data,N,T.obs,Ylag.center,
                                                                     b00, b10, sigma, rho, sigma.v0, sigma.v1,alpha, estimate_randomslopes))
  
  # Get a vector with the iterations that converge
  errors = rep(0,R)
  for (r in 1:R){errors[r] = length(fit.list.sim[[r]])}
  
  R.converge = which(errors>1)
  
  # Number of replicates that converge
  n.R = length(R.converge)
  
  # average person-mean bias per simulation 
  bias_mean = Reduce('+',lapply(R.converge, function(r) fit.list.sim[[r]]$bias_mean))/n.R
  
  # average number of observations per simulation 
  model_observations = Reduce('+',lapply(R.converge, function(r) fit.list.sim[[r]]$model_observations))/n.R
  
  # Estimates of the fixed effects
  beta.hat.lme = Reduce('+',lapply(R.converge, function(r) fit.list.sim[[r]]$beta.hat.lme))/n.R
  # Get the fixed effects for each replicate to study variability across the R replicates
  beta.hat.lme.list = matrix(unlist(lapply(R.converge, function(r) fit.list.sim[[r]]$beta.hat.lme)),
                             byrow=TRUE, ncol=2)
  
  # Power
  power.hat.lme = Reduce('+',lapply(R.converge, function(r) fit.list.sim[[r]]$power.hat.lme))/n.R
  
  # Standard errors
  StdError.beta.hat.lme = Reduce('+',lapply(R.converge, function(r) fit.list.sim[[r]]$StdError.beta.lme))/n.R
  
  # Get the estimated random effects for each replicate
  RE.hat.lme = lapply(R.converge, function(r) fit.list.sim[[r]]$Random.Effects.lme)
  
  # Get the simulated random effects for each replicate
  RE.hat = lapply(R.converge, function(r) data.list[[r]]$RE)
  
  return(list(b10 = b10,
              sigma.v1 = sigma.v1,
              beta.hat.lme=beta.hat.lme,
              power.hat.lme=power.hat.lme,
              StdError.beta.hat.lme=StdError.beta.hat.lme,
              n.R=n.R,
              N = N,
              T.obs = T.obs,
              comp_mean = compliance_mean,
              beta.hat.lme.list=beta.hat.lme.list,
              RE.hat.lme=RE.hat.lme,
              RE.hat=RE.hat,
              bias_mean = bias_mean,
              model_observations = model_observations,
              missing_type = "block"))}


