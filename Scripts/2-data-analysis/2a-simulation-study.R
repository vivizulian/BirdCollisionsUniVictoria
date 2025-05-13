
### simulation study



### Code to evaluate the performance of the integrated model using simulations ###

##________________________________________________________________________________##
##

nsims = 500

out_SIMS <- list()

for(sim in 1:nsims){
  
  ## Set a seed for the simulation
  set.seed(42+sim)
  
  print(paste("Starting", sim, "of", nsims, "simulations"))
  
  Area = 6
  nsp = 30
  lambda <- rep(1, nsp)
  siteBand <- 8 #number of nets
  visitBand <- 5 #number of sampling days
  #effort <- rgamma(siteBand,3,1) #effort of nets
  siteUv <- 12 #sites (point counts fo UVic)
  visitCoun <- 4 #occasions (visits for UVic)
  area = rep(0.5, siteUv) #surveyed area of point count 
  #flock <- 
  #gamma0 <- 0.5
  #gamma1 <- 3
  p1 <- rep(0.5, nsp)
  p2 <- rep(0.3, nsp)
  
  areaNets <- 10 #gamma0 + effort
  
  #areaNets <- gamma0 + effort
  
  areaNet <- matrix(NA, siteBand, nsp)
  areaNet[,1:30] <- rep(areaNets, nsp)
  
  #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
  
  #Objects to hold results:
  N <- rep(NA, nsp)
  Nband <- matrix(NA, siteBand, nsp)
  countBand <- array(NA, dim=c(siteBand, visitBand, nsp))
  NUVic <- matrix(NA, siteUv, nsp)
  countsUV <- array(NA, dim = c(siteUv, visitCoun, nsp))
  
  #Total population
  for(k in 1:nsp){
    N[k] <- rpois(1, lambda[k]*Area)
  }
  
  #Sampling process at Banding station:
  for(k in 1:nsp){
    for(n in 1:siteBand){
      Nband[n,k] <- rpois(1, lambda[k]*areaNet[n,k])
      
      for(t in 1:visitBand){
        countBand[n,t,k] <- rbinom(1, Nband[n,k], p1[k])
      }
    }
  }
  
  #Sampling process at UVIc:
  for(k in 1:nsp){
    for(i in 1:siteUv){
      NUVic[i,k] <- rpois(1, lambda[k]*area[i])
      
      for(j in 1:visitCoun){
        countsUV[i,j,k] <- rbinom(1, NUVic[i,k], p2[k])
      }
    }
  }
  
  
  #Data object
  str(bdata <- list(nsp = dim(countsUV)[3], visitBan = dim(countBand)[2], visitCoun = dim(countsUV)[2],
                    countBand = countBand, countsUV = countsUV,
                    siteUv = dim(countsUV)[1], siteBand = dim(countBand)[1], 
                    area=area, Area = Area))#, #nflock = max(flock),
  #effort = effort))
  
  #Specify model in BUGS language
  cat(file = "modelIntSIMS.txt","
  model {
  
     #Biological model for true abundance
     for(k in 1:nsp){
        N[k] ~ dpois(lambda[k]*Area)
        
        for(n in 1:siteBand){
           Nband[n,k] ~ dpois(lambda[k]*areaNet[n])
           
           for(t in 1:visitBan){
              countBand[n,t,k] ~ dbin(p1[k], Nband[n,k])
           }
        }
        
        for (i in 1:siteUv){
           NUVic[i,k] ~ dpois(lambda[k]*area[i])
           for(j in 1:visitCoun){
              countsUV[i,j,k] ~ dbin(p2[k], NUVic[i,k])
           }
        }
        #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
     }
  
     #Priors
     #p1 ~ dbeta(1,1)
     #gamma0 ~ dgamma(0.01, 0.01)
     #gamma1 ~ dnorm(0, 0.01)
     #lambda ~ dgamma(0.01, 0.01)
     #p1[k] ~ dunif(0,1)
     #alpha1[1] <- 0
     #alpha1[2] ~ dnorm(0, 0.01)
     
     for(n in 1:siteBand){
        areaNet[n] ~ dgamma(0.01, 0.01)
     }
  
     for(k in 1:nsp){
        lambda[k] ~ dgamma(0.01, 0.01)
        p1[k] ~ dbeta(1,1)
        p2[k] ~ dbeta(1,1)
     }
     
  }
  ")
  
  ##Initial Values
  Nst <- apply(countsUV, c(1,3), max, na.rm=T) + 5
  Nbandst <- apply(countBand, c(1,3), max, na.rm=T) + 5
  inits <- function(){list(NUVic = Nst, Nband = Nbandst, p1=runif(nsp, 0.2,0.3), 
                           p2=runif(nsp, 0.15, 0.2))}
  
  params <- c("lambda", "p1", "p2", "NUVic", "N", "Nband", "areaNet")
  
  # MCMC settings
  #na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10
  na <- 1000;   nc <- 3;   nb <- 5000;   ni <- 20000;   nt <- 100
  
  ##Call JAGS
  library(jagsUI)
  out_modelSIMS <- jags(bdata, inits, params, "modelIntSIMS.txt", n.adapt = na, n.chains = nc, 
                        n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
  
  
  out_SIMS[[sim]] <- out_modelSIMS
  
  rm(out_modelSIMS)
  
}

nsims = 500 
nsp = 30
lambda <- matrix(NA, nsims, nsp)
p1 <- matrix(NA, nsims, nsp)
p2 <- matrix(NA, nsims, nsp)
for(i in 1:nsims){
  lambda[i,] <- out_SIMS[[i]]$mean$lambda
  p1[i,] <- out_SIMS[[i]]$mean$p1
  p2[i,] <- out_SIMS[[i]]$mean$p2
}

par(mfrow=c(2,2))
plot(density(lambda), xlab = 'Mean lambda', main="Estimated lambda")
abline(v=1, col="red", lwd=2)

plot(density(p1), xlab = 'Mean detection (Banding)', main="Estimated detection of banding")
abline(v=0.5, col="red", lwd=2)

plot(density(p2), xlab = 'Mean detection (point count)', main="Estimated detection of point count")
abline(v=0.3, col="red", lwd=2)

save(out_SIMS, file="outSims1.RData")




### Code to evaluate the performance of the integrated model using simulations ###

##________________________________________________________________________________##
##

# p1 <- rep(0.3, nsp)
# p2 <- rep(0.1, nsp)

## Low detection probability 

nsims = 500

out_SIMS2 <- list()

for(sim in 1:nsims){
  
  ## Set a seed for the simulation
  set.seed(42+sim)
  
  print(paste("Starting", sim, "of", nsims, "simulations"))
  
  Area = 6
  nsp = 30
  lambda <- rep(1, nsp)
  siteBand <- 8 #number of nets
  visitBand <- 5 #number of sampling days
  #effort <- rgamma(siteBand,3,1) #effort of nets
  siteUv <- 12 #sites (point counts fo UVic)
  visitCoun <- 4 #occasions (visits for UVic)
  area = rep(0.5, siteUv) #surveyed area of point count 
  #flock <- 
  #gamma0 <- 0.5
  #gamma1 <- 3
  p1 <- rep(0.3, nsp)
  p2 <- rep(0.1, nsp)
  
  areaNets <- 10 #gamma0 + effort
  
  #areaNets <- gamma0 + effort
  
  areaNet <- matrix(NA, siteBand, nsp)
  areaNet[,1:30] <- rep(areaNets, nsp)
  
  
  #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
  
  #Objects to hold results:
  N <- rep(NA, nsp)
  Nband <- matrix(NA, siteBand, nsp)
  countBand <- array(NA, dim=c(siteBand, visitBand, nsp))
  NUVic <- matrix(NA, siteUv, nsp)
  countsUV <- array(NA, dim = c(siteUv, visitCoun, nsp))
  
  #Total population
  for(k in 1:nsp){
    N[k] <- rpois(1, lambda[k]*Area)
  }
  
  #Sampling process at Banding station:
  for(k in 1:nsp){
    for(n in 1:siteBand){
      Nband[n,k] <- rpois(1, lambda[k]*areaNet[n,k])
      
      for(t in 1:visitBand){
        countBand[n,t,k] <- rbinom(1, Nband[n,k], p1[k])
      }
    }
  }
  
  #Sampling process at UVIc:
  for(k in 1:nsp){
    for(i in 1:siteUv){
      NUVic[i,k] <- rpois(1, lambda[k]*area[i])
      
      for(j in 1:visitCoun){
        countsUV[i,j,k] <- rbinom(1, NUVic[i,k], p2[k])
      }
    }
  }
  
  
  #Data object
  str(bdata <- list(nsp = dim(countsUV)[3], visitBan = dim(countBand)[2], visitCoun = dim(countsUV)[2],
                    countBand = countBand, countsUV = countsUV,
                    siteUv = dim(countsUV)[1], siteBand = dim(countBand)[1], 
                    area=area, Area = Area))#, #nflock = max(flock),
  #effort = effort))
  
  #Specify model in BUGS language
  cat(file = "modelIntSIMS2.txt","
  model {
  
     #Biological model for true abundance
     for(k in 1:nsp){
        N[k] ~ dpois(lambda[k]*Area)
        
        for(n in 1:siteBand){
           Nband[n,k] ~ dpois(lambda[k]*areaNet[n])
           
           for(t in 1:visitBan){
              countBand[n,t,k] ~ dbin(p1[k], Nband[n,k])
           }
        }
        
        for (i in 1:siteUv){
           NUVic[i,k] ~ dpois(lambda[k]*area[i])
           for(j in 1:visitCoun){
              countsUV[i,j,k] ~ dbin(p2[k], NUVic[i,k])
           }
        }
        #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
     }
  
     #Priors
     #p1 ~ dbeta(1,1)
     #gamma0 ~ dgamma(0.01, 0.01)
     #gamma1 ~ dnorm(0, 0.01)
     #lambda ~ dgamma(0.01, 0.01)
     #p1[k] ~ dunif(0,1)
     
     #alpha1[1] <- 0
     #alpha1[2] ~ dnorm(0, 0.01)
     for(n in 1:siteBand){
        areaNet[n] ~ dgamma(0.01, 0.01)
     }
  
     for(k in 1:nsp){
        lambda[k] ~ dgamma(0.01, 0.01)
        p1[k] ~ dbeta(1,1)
        p2[k] ~ dbeta(1,1)
        #alpha0[k] ~ dnorm(0, 0.01)
     }
     
  }
  ")
  
  ##Initial Values
  Nst <- apply(countsUV, c(1,3), max, na.rm=T) + 5
  Nbandst <- apply(countBand, c(1,3), max, na.rm=T) + 5
  inits <- function(){list(NUVic = Nst, Nband = Nbandst, p1=runif(nsp, 0.2,0.4), 
                           p2=runif(nsp, 0.05, 0.2))}
  
  params <- c("lambda", "p1", "p2", "NUVic", "N", "Nband", "areaNet")
  
  # MCMC settings
  #na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10
  na <- 1000;   nc <- 3;   nb <- 5000;   ni <- 20000;   nt <- 100
  
  ##Call JAGS
  library(jagsUI)
  out_modelSIMS2 <- jags(bdata, inits, params, "modelIntSIMS2.txt", n.adapt = na, n.chains = nc, 
                         n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
  
  out_SIMS2[[sim]] <- out_modelSIMS2
  
  rm(out_modelSIMS2)
  
}

save(out_SIMS2, file="outSims2.RData")

nsims = 500
nsp = 30
lambda <- matrix(NA, nsims, nsp)
p1 <- matrix(NA, nsims, nsp)
p2 <- matrix(NA, nsims, nsp)
areaNet <- matrix(NA, nsims, siteBand)
for(i in 1:nsims){
  lambda[i,] <- out_SIMS2[[i]]$mean$lambda
  p1[i,] <- out_SIMS2[[i]]$mean$p1
  p2[i,] <- out_SIMS2[[i]]$mean$p2
  areaNet[i,] <- out_SIMS2[[i]]$mean$areaNet
}

par(mfrow=c(2,2))
plot(density(lambda), xlab = 'Mean lambda', main="Estimated lambda")
abline(v=1, col="red", lwd=2)

plot(density(p1), xlab = 'Mean detection (Banding)', main="Estimated detection of banding")
abline(v=0.3, col="red", lwd=2)

plot(density(p2), xlab = 'Mean detection (point count)', main="Estimated detection of point count")
abline(v=0.1, col="red", lwd=2)





### Code to evaluate the performance of the integrated model using simulations ###

##________________________________________________________________________________##
##
# lambda <- rep(10, nsp)
# p1 <- rep(0.3, nsp)
# p2 <- rep(0.1, nsp)

## Low detection probability and low lambda

nsims = 500

out_SIMS3 <- list()

for(sim in 1:nsims){
  
  ## Set a seed for the simulation
  set.seed(42+sim)
  
  print(paste("Starting", sim, "of", nsims, "simulations"))
  
  Area = 120+60
  nsp = 20
  lambda <- rep(0.2, nsp)
  siteBand <- 8 #number of nets
  visitBand <- 5 #number of sampling days
  #effort <- rgamma(siteBand,3,1) #effort of nets
  siteUv <- 12 #sites (point counts fo UVic)
  visitCoun <- 4 #occasions (visits for UVic)
  area = rep(60, siteUv) #surveyed area of point count 
  #flock <- 
  #gamma0 <- 0.5
  #gamma1 <- 3
  p1 <- rep(0.3, nsp)
  p2 <- rep(0.1, nsp)
  
  areaNets <- 120 #gamma0 + effort
  areaNet <- matrix(NA, siteBand, nsp)
  areaNet[,1:20] <- rep(areaNets, nsp)
  
  #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
  
  #Objects to hold results:
  N <- rep(NA, nsp)
  Nband <- matrix(NA, siteBand, nsp)
  countBand <- array(NA, dim=c(siteBand, visitBand, nsp))
  NUVic <- matrix(NA, siteUv, nsp)
  countsUV <- array(NA, dim = c(siteUv, visitCoun, nsp))
  
  #Total population
  for(k in 1:nsp){
    N[k] <- rpois(1, lambda[k]*Area)
  }
  
  #Sampling process at Banding station:
  for(k in 1:nsp){
    for(n in 1:siteBand){
      Nband[n,k] <- rpois(1, lambda[k]*areaNet[n,k])
      
      for(t in 1:visitBand){
        countBand[n,t,k] <- rbinom(1, Nband[n,k], p1[k])
      }
    }
  }
  
  #Sampling process at UVIc:
  for(k in 1:nsp){
    for(i in 1:siteUv){
      NUVic[i,k] <- rpois(1, lambda[k]*area[i])
      
      for(j in 1:visitCoun){
        countsUV[i,j,k] <- rbinom(1, NUVic[i,k], p2[k])
      }
    }
  }
  
  
  #Data object
  str(bdata <- list(nsp = dim(countsUV)[3], visitBan = dim(countBand)[2], visitCoun = dim(countsUV)[2],
                    countBand = countBand, countsUV = countsUV,
                    siteUv = dim(countsUV)[1], siteBand = dim(countBand)[1], 
                    area=area, Area = Area))#, #nflock = max(flock),
  #effort = effort))
  
  #Specify model in BUGS language
  cat(file = "modelIntSIMS3.txt","
  model {
  
     #Biological model for true abundance
     for(k in 1:nsp){
        N[k] ~ dpois(lambda[k]*Area)
        
        for(n in 1:siteBand){
           Nband[n,k] ~ dpois(lambda[k]*areaNet[n])
           #areaNet[n,k] <- gamma0 + effort[n]
           
           for(t in 1:visitBan){
              countBand[n,t,k] ~ dbin(p1[k], Nband[n,k])
           }
        }
        
        for (i in 1:siteUv){
           NUVic[i,k] ~ dpois(lambda[k]*area[i])
           for(j in 1:visitCoun){
              countsUV[i,j,k] ~ dbin(p2[k], NUVic[i,k])
           }
        }
        #logit(p2[k]) <- alpha0[k] + alpha1[flock[k]]
     }
  
     #Priors
     #p1 ~ dbeta(1,1)
     #gamma0 ~ dgamma(0.01, 0.01)
     #gamma1 ~ dnorm(0, 0.01)
     #lambda ~ dgamma(0.01, 0.01)
     #p1[k] ~ dunif(0,1)
     
     #alpha1[1] <- 0
     #alpha1[2] ~ dnorm(0, 0.01)
     for(n in 1:siteBand){
        areaNet[n] ~ dgamma(0.01, 0.01)
     }
  
     for(k in 1:nsp){
        lambda[k] ~ dgamma(0.01, 0.01)
        p1[k] ~ dbeta(1,1)
        p2[k] ~ dbeta(1,1)
        #alpha0[k] ~ dnorm(0, 0.01)
     }
     
  }
  ")
  
  ##Initial Values
  Nst <- apply(countsUV, c(1,3), max, na.rm=T) + 5
  Nbandst <- apply(countBand, c(1,3), max, na.rm=T) + 5
  inits <- function(){list(NUVic = Nst, Nband = Nbandst, p1=runif(nsp, 0.25,0.3), 
                           p2=runif(nsp, 0.05, 0.2))}
  
  params <- c("lambda", "p1", "p2", "NUVic", "N", "Nband", "areaNet")
  
  # MCMC settings
  #na <- 100;   nc <- 3;   nb <- 100;   ni <- 250;   nt <- 10
  na <- 1000;   nc <- 3;   nb <- 5000;   ni <- 20000;   nt <- 100
  
  ##Call JAGS
  library(jagsUI)
  out_modelSIMS3 <- jags(bdata, inits, params, "modelIntSIMS3.txt", n.adapt = na, n.chains = nc, 
                         n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
  
  
  out_SIMS3[[sim]] <- out_modelSIMS3
  
  rm(out_modelSIMS3)
  
}


nsims = 60
nsp = 20
lambda <- matrix(NA, nsims, nsp)
p1 <- matrix(NA, nsims, nsp)
p2 <- matrix(NA, nsims, nsp)
for(i in 1:nsims){
  lambda[i,] <- out_SIMS3[[i]]$mean$lambda
  p1[i,] <- out_SIMS3[[i]]$mean$p1
  p2[i,] <- out_SIMS3[[i]]$mean$p2
}

par(mfrow=c(2,2))
plot(density(lambda), xlab = 'Mean lambda', main="Estimated lambda")
abline(v=10, col="red", lwd=2)

plot(density(p1), xlab = 'Mean detection (Banding)', main="Estimated detection of banding")
abline(v=0.3, col="red", lwd=2)

plot(density(p2), xlab = 'Mean detection (point count)', main="Estimated detection of point count")
abline(v=0.1, col="red", lwd=2)

