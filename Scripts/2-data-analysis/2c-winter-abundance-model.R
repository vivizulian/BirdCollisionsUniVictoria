#######################
## N-Mixture model to analyze count data
## to estimate abundance of birds and vulnerability in the University of Victoria
## winter (late and early)
#######################



# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
data_run_date <- 'XXX'
dir <- 'XXX'



# load packages -----------------------------------------------------------

library(MCMCvis)
library(jagsUI)
library(mcmcOutput)



# read in data -----------------------------------------------------------

flockWinter <- read.csv(paste0(dir, 'Data/L2/flockWinter-', data_run_date, '.csv'))

#point counts + effort for each data set in each year/season

#2020 - Late winter (jan/feb)
PointDataLateWinter2020 <- readRDS(paste0(dir, 'Data/L2/PointDataLateWinter2020Array-', data_run_date, '.rds'))
effortPointLateWinter2020 <- read.csv(paste0(dir, 'Data/L2/effortPointLateWinter2020-', data_run_date, '.csv'))
CollisionsDataLateWinter2020 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataLateWinter2020-', data_run_date, '.csv'))


#2021 - Early winter (nov/dec)
PointDataEarlyWinter2021 <- readRDS(paste0(dir, 'Data/L2/PointDataEarlyWinter2021Array-', data_run_date, '.rds'))
effortPointEarlyWinter2021 <- read.csv(paste0(dir, 'Data/L2/effortPointEarlyWinter2021-', data_run_date, '.csv'))
CollisionsDataEarlyWinter2021 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataEarlyWinter2021-', data_run_date, '.csv'))


#2021 - Late winter (jan/feb)
PointDataLateWinter2021 <- readRDS(paste0(dir, 'Data/L2/PointDataLateWinter2021Array-', data_run_date, '.rds'))
effortPointLateWinter2021 <- read.csv(paste0(dir, 'Data/L2/effortPointLateWinter2021-', data_run_date, '.csv'))
CollisionsDataLateWinter2021 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataLateWinter2021-', data_run_date, '.csv'))


#2022 - Late winter (jan/feb)
PointDataLateWinter2022 <- readRDS(paste0(dir, 'Data/L2/PointDataLateWinter2022Array-', data_run_date, '.rds'))
effortPointLateWinter2022 <- read.csv(paste0(dir, 'Data/L2/effortPointLateWinter2022-', data_run_date, '.csv'))
CollisionsDataLateWinter2022 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataLateWinter2022-', data_run_date, '.csv'))



# specify model in BUGS language -----------------------------------------------------------

#same model for all year/seasons

cat(file = "modelWinter.txt","
model {
  
  #Biological model for true abundance
  for(k in 1:nsp){
    N[k] ~ dpois(lambda[k])
    
    for (i in 1:NsiteUV){
      N_UV[i,k] ~ dpois(lambda[k]*area[i])
      for(j in 1:NvisitUV){
        countsUV[i,j,k] ~ dbin(pUVic[k], N_UV[i,k])
      }
    }
    
    logit(pUVic[k]) <- alpha0[k] + alpha1[flock[k]]
    
    N_UV_tot[k] <- sum(N_UV[,k]) #sum of abundance on campus
    N_UV_totLog[k] <- log(N_UV_tot[k])/log(10)  ##This is equivalent to log10(NUVic_tot[k])
    
    #Vulnerability Analysis
    y_mort[k] ~ dnorm(mu[k], tau) #Regression of log10 mortality 
    mu[k] <- beta0 + beta1*N_UV_totLog[k]
  }
  
  #Priors
  beta0 ~ dnorm(0, 0.01)
  beta1 ~ dnorm(0, 0.01)
  tau <- 1/(sigma*sigma)
  sigma ~ dunif(0, 10)
  
  alpha1[1] <- 0
  alpha1[2] ~ dnorm(0, 0.01)
  
  for(k in 1:nsp){
    lambda[k] ~ dgamma(0.01, 0.01)
    alpha0[k] ~ dnorm(0, 0.01)
  }
  
}
")



# run model for Late winter 2020 -----------------------------------------------------------

#Data object
str(bdataLate2020 <- list(nsp = dim(PointDataLateWinter2020)[3], 
                        NvisitUV = dim(PointDataLateWinter2020)[2],
                        NsiteUV = dim(PointDataLateWinter2020)[1], 
                        area = as.numeric(effortPointLateWinter2020$Effort), 
                        flock = flockWinter$FlockingWinter+1, 
                        y_mort = log10(CollisionsDataLateWinter2020$Ncollisions+1), 
                        countsUV = PointDataLateWinter2020))

##Initial Values
Nst <- apply(PointDataLateWinter2020, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst)}

params <- c("lambda", "pUVIc", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "alpha0", "alpha1", "beta0", "beta1")

#MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10   #test
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100

#call JAGS
out_model_late_winter2020 <- jags(bdataLate2020, inits, params, "modelWinter.txt", 
                                  n.adapt = na, n.chains = nc, 
                                  n.thin = nt, n.iter = ni, n.burnin = nb, 
                                  parallel = TRUE)

#check convergence
mcmcOutput::diagPlot(out_model_late_winter2020, params = c("beta0", "beta1", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_late_winter2020,
                  round = 4,
                  file_name = paste0('LateWinter2020-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('LateWinter2020-output-', run_date),
                  probs = c(0.025, 0.5, 0.975),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('LateWinter2020-output-', run_date),
                  add_obj = list(c(bdataLate2020, Nst)),
                  add_obj_names = paste0('LateWinter2020-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2c-winter-abundance-model.R'),
                  cp_file_names = paste0('2c-winter-abundance-model', run_date, '.R'))




# run model for Early winter 2021 -----------------------------------------------------------

#Data object
str(bdataEarly2021 <- list(nsp = dim(PointDataEarlyWinter2021)[3], 
                        NvisitUV = dim(PointDataEarlyWinter2021)[2],
                        NsiteUV = dim(PointDataEarlyWinter2021)[1], 
                        area = as.numeric(effortPointEarlyWinter2021$Effort), 
                        flock = flockWinter$FlockingWinter+1, 
                        y_mort = log10(CollisionsDataEarlyWinter2021$Ncollisions+1), 
                        countsUV = PointDataEarlyWinter2021))

##Initial Values
Nst <- apply(PointDataEarlyWinter2021, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst)}

params <- c("lambda", "pUVIc", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "alpha0", "alpha1", "beta0", "beta1")

#MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10   #test
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100

#call JAGS
out_model_early_winter2021 <- jags(bdataEarly2021, inits, params, "modelWinter.txt", 
                                  n.adapt = na, n.chains = nc, 
                                  n.thin = nt, n.iter = ni, n.burnin = nb, 
                                  parallel = TRUE)

#check convergence
mcmcOutput::diagPlot(out_model_early_winter2021, params = c("beta0", "beta1", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_early_winter2021,
                  round = 4,
                  file_name = paste0('EarlyWinter2021-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('EarlyWinter2021-output-', run_date),
                  probs = c(0.025, 0.5, 0.975),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('EarlyWinter2021-output-', run_date),
                  add_obj = list(c(bdataEarly2021, Nst)),
                  add_obj_names = paste0('EarlyWinter2021-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2c-winter-abundance-model.R'),
                  cp_file_names = paste0('2c-winter-abundance-model', run_date, '.R'))




# run model for Late winter 2021 -----------------------------------------------------------

#Data object
str(bdataLate2021 <- list(nsp = dim(PointDataLateWinter2021)[3], 
                          NvisitUV = dim(PointDataLateWinter2021)[2],
                          NsiteUV = dim(PointDataLateWinter2021)[1], 
                          area = as.numeric(effortPointLateWinter2021$Effort), 
                          flock = flockWinter$FlockingWinter+1, 
                          y_mort = log10(CollisionsDataLateWinter2021$Ncollisions+1), 
                          countsUV = PointDataLateWinter2021))

##Initial Values
Nst <- apply(PointDataLateWinter2021, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst)}

params <- c("lambda", "pUVIc", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "alpha0", "alpha1", "beta0", "beta1")

#MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10   #test
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100

#call JAGS
out_model_late_winter2021 <- jags(bdataLate2021, inits, params, "modelWinter.txt", 
                                  n.adapt = na, n.chains = nc, 
                                  n.thin = nt, n.iter = ni, n.burnin = nb, 
                                  parallel = TRUE)

#check convergence
mcmcOutput::diagPlot(out_model_late_winter2021, params = c("beta0", "beta1", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_late_winter2021,
                  round = 4,
                  file_name = paste0('LateWinter2021-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('LateWinter2021-output-', run_date),
                  probs = c(0.025, 0.5, 0.975),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('LateWinter2021-output-', run_date),
                  add_obj = list(c(bdataLate2021, Nst)),
                  add_obj_names = paste0('LateWinter2021-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2c-winter-abundance-model.R'),
                  cp_file_names = paste0('2c-winter-abundance-model', run_date, '.R'))




# run model for Late winter 2022 -----------------------------------------------------------

#Data object
str(bdataLate2022 <- list(nsp = dim(PointDataLateWinter2022)[3], 
                          NvisitUV = dim(PointDataLateWinter2022)[2],
                          NsiteUV = dim(PointDataLateWinter2022)[1], 
                          area = as.numeric(effortPointLateWinter2022$Effort), 
                          flock = flockWinter$FlockingWinter+1, 
                          y_mort = log10(CollisionsDataLateWinter2022$Ncollisions+1), 
                          countsUV = PointDataLateWinter2022))

##Initial Values
Nst <- apply(PointDataLateWinter2022, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst)}

params <- c("lambda", "pUVIc", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "alpha0", "alpha1", "beta0", "beta1")

#MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10   #test
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100

#call JAGS
out_model_late_winter2022 <- jags(bdataLate2022, inits, params, "modelWinter.txt", 
                                  n.adapt = na, n.chains = nc, 
                                  n.thin = nt, n.iter = ni, n.burnin = nb, 
                                  parallel = TRUE)

#check convergence
mcmcOutput::diagPlot(out_model_late_winter2022, params = c("beta0", "beta1", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_late_winter2022,
                  round = 4,
                  file_name = paste0('LateWinter2022-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('LateWinter2022-output-', run_date),
                  probs = c(0.025, 0.5, 0.975),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('LateWinter2022-output-', run_date),
                  add_obj = list(c(bdataLate2022, Nst)),
                  add_obj_names = paste0('LateWinter2022-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2c-winter-abundance-model.R'),
                  cp_file_names = paste0('2c-winter-abundance-model', run_date, '.R'))


