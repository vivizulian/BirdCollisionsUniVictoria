#######################
## Integrated Model to analyze count and banding data
## to estimate abundance of birds and vulnerability in the University of Victoria

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

flockFall <- read.csv(paste0(dir, 'Data/L2/flockFall-', data_run_date, '.csv'))

#banding + point counts + effort for each data set in each year
#2019
BandingDataFall2019 <- readRDS(paste0(dir, 'Data/L2/BandingDataFall2019Array-', data_run_date, '.rds'))
PointDataFall2019 <- readRDS(paste0(dir, 'Data/L2/PointDataFall2019Array-', data_run_date, '.rds'))
effortBandingFall2019 <- read.csv(paste0(dir, 'Data/L2/effortBandingFall2019-', data_run_date, '.csv'))
effortPointFall2019 <- read.csv(paste0(dir, 'Data/L2/effortPointFall2019-', data_run_date, '.csv'))
CollisionsDataFall2019 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataFall2019-', data_run_date, '.csv'))


#2020
BandingDataFall2020 <- readRDS(paste0(dir, 'Data/L2/BandingDataFall2020Array-', data_run_date, '.rds'))
PointDataFall2020 <- readRDS(paste0(dir, 'Data/L2/PointDataFall2020Array-', data_run_date, '.rds'))
effortBandingFall2020 <- read.csv(paste0(dir, 'Data/L2/effortBandingFall2020-', data_run_date, '.csv'))
effortPointFall2020 <- read.csv(paste0(dir, 'Data/L2/effortPointFall2020-', data_run_date, '.csv'))
CollisionsDataFall2020 <- read.csv(paste0(dir, 'Data/L2/CollisionsDataFall2020-', data_run_date, '.csv'))




# specify model in BUGS language -----------------------------------------------------------

#same model for both years

cat(file = "modelFall.txt","
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
  
  for(k in 1:nsp){
    for(n in 1:NsiteBand){
      N_band[n,k] ~ dpois(lambda[k]*areaNet[n])
      
      for(t in 1:NvisitBand){
        countsBand[n,t,k] ~ dbin(pNet[k]*effort[t], N_band[n,k]) #Adam's suggestion
      }
    }
    N_band_tot[k] <- sum(N_band[,k]) #sum of abundance on nets
  }
  
  #Priors
  beta0 ~ dnorm(0, 0.01)
  beta1 ~ dnorm(0, 0.01)
  tau <- 1/(sigma*sigma)
  sigma ~ dunif(0, 10)
  
  alpha1[1] <- 0
  alpha1[2] ~ dnorm(0, 0.1)
  
  for(n in 1:NsiteBand){
    areaNet[n] ~ dgamma(0.01, 0.01)
  }
  
  for(k in 1:nsp){
    lambda[k] ~ dgamma(0.01, 0.01)
    pNet[k] ~ dbeta(1,1)
    alpha0[k] ~ dnorm(0, 0.1)
  }
  
}
")



# run model for Fall 2019 -----------------------------------------------------------

#Data object
str(bdata2019 <- list(nsp = dim(PointDataFall2019)[3], 
                      NvisitBand = dim(BandingDataFall2019)[2], 
                      NvisitUV = dim(PointDataFall2019)[2],
                      NsiteUV = dim(PointDataFall2019)[1], 
                      NsiteBand = dim(BandingDataFall2019)[1], 
                      area = as.numeric(effortPointFall2019$Effort), 
                      flock = flockFall$FlockingFall+1, 
                      y_mort = log10(CollisionsDataFall2019$Ncollisions+1), 
                      effort = as.numeric(effortBandingFall2019$effort_str),
                      countsBand = BandingDataFall2019, 
                      countsUV = PointDataFall2019))


##Initial Values
Nst <- apply(PointDataFall2019, c(1,3), max, na.rm=T) + 5
Nbandst <- apply(BandingDataFall2019, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst, N_band = Nbandst, 
                         pNet = runif(dim(BandingDataFall2019)[3], 0.1, 0.5))}


params <- c("lambda", "pUVIc", "pNet", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "N_band_tot", "alpha0", "alpha1", "areaNet", "beta0", "beta1")


#MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10   #test
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100


#call JAGS
out_model_fall2019 <- jags(bdata2019, inits, params, "modelFall.txt", n.adapt = na, n.chains = nc, 
                      n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)


#check convergence
mcmcOutput::diagPlot(out_model_fall2019, params = c("beta0", "beta1", "areaNet", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_fall2019,
                  round = 4,
                  file_name = paste0('Fall2019-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('Fall2019-output-', run_date),
                  probs = c(0.025, 0.5, 0.975),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('Fall2019-output-', run_date),
                  add_obj = list(c(bdata2019, Nst, Nbandst)),
                  add_obj_names = paste0('Fall2019-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2b-fall-abundance-model.R'),
                  cp_file_names = paste0('2b-fall-abundance-model', run_date, '.R'))




# run model for Fall 2020 -----------------------------------------------------------

#Data object
str(bdata2020 <- list(nsp = dim(PointDataFall2020)[3], 
                      NvisitBand = dim(BandingDataFall2020)[2], 
                      NvisitUV = dim(PointDataFall2020)[2],
                      NsiteUV = dim(PointDataFall2020)[1], 
                      NsiteBand = dim(BandingDataFall2020)[1], 
                      area = as.numeric(effortPointFall2020$Effort), 
                      flock = flockFall$FlockingFall+1, 
                      y_mort = log10(CollisionsDataFall2020$Ncollisions+1), 
                      effort = as.numeric(effortBandingFall2020$effort_str),
                      countsBand = BandingDataFall2020, 
                      countsUV = PointDataFall2020))


##Initial Values
Nst <- apply(PointDataFall2020, c(1,3), max, na.rm=T) + 5
Nbandst <- apply(BandingDataFall2020, c(1,3), max, na.rm=T) + 5
inits <- function(){list(N_UV = Nst, N_band = Nbandst, 
                         pNet = runif(dim(BandingDataFall2020)[3], 0.1, 0.5))}


params <- c("lambda", "pUVIc", "pNet", "N_UV_totLog", "N_UV_tot", "N", "mu",
            "N_band_tot", "alpha0", "alpha1", "areaNet", "beta0", "beta1")


# MCMC settings
#na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10
na <- 50000;   nc <- 4;   nb <- 100000;   ni <- 200000;   nt <- 100


##Call JAGS
out_model_fall2020 <- jags(bdata2020, inits, params, "modelFall.txt", n.adapt = na, n.chains = nc, 
                           n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)


#check convergence

mcmcOutput::diagPlot(out_model_fall2020, params = c("beta0", "beta1", "areaNet", "alpha0", "alpha1"))


# save summary ------

#save out summary, model fit, data
MCMCvis::MCMCdiag(out_model_fall2020,
                  round = 4,
                  file_name = paste0('Fall2020-output-', run_date),
                  dir = paste0(dir, '/Results'),
                  mkdir = paste0('Fall2020-output-', run_date),
                  probs = c(0.025, 0.5, 0.975), #95%CI or if needed 89%CI = c(0.055, 0.5, 0.945),
                  pg0 = TRUE,
                  save_obj = TRUE,
                  obj_name = paste0('Fall2020-output-', run_date),
                  add_obj = list(c(bdata2020, Nst, Nbandst)),
                  add_obj_names = paste0('Fall2020-data-', run_date),
                  cp_file = paste0(dir,'Scripts/2-data-analysis/2b-fall-abundance-model.R'),
                  cp_file_names = paste0('2b-fall-abundance-model', run_date, '.R'))


