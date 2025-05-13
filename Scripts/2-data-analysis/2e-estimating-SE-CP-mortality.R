#######################
# Code to create objects to estimate Searcher Efficiency and Carcass Persistence
# at the University of Victoria campus          

#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
data_run_date <- 'XXX'
dir <- 'XXX'


# load packages -----------------------------------------------------------

library(GenEst)
library(ggplot2)
library(gridExtra)
library(dplyr)



# read data -----------------------------------------------------------

mortality_observations <- read.csv(file = paste0(dir, 'Data/L1/mortality_observations-', 
                                                 data_run_date, '.csv'))

collision_observations <- read.csv(file = paste0(dir, 'Data/L1/collision_observations-', 
                                                 data_run_date, '.csv'))

#search efficiency
search_efficiency <- read.csv(paste0(dir, 'Data/L0/searcher_efficiency_data/SearcherEfficiencyData.csv'))

#carcass persistence
carcass_persistence <- read.csv(paste0(dir, 'Data/L0/carcass_persistence_data/CarcassPersistenceData.csv'))

#search schedule
search_schedule <- read.csv(paste0(dir, 'Data/L0/searcher_efficiency_data/SearcherScheduleData.csv'))
search_schedule$Date <- as.Date(search_schedule$SearchDate, format = '%m/%d/%y')



# run models -----------------------------------------------------------

# compile data for search efficiency model

efficiency <- data.frame(carcID = search_efficiency$ID,
                         Season = search_efficiency$SeasonGen,
                         BuildingType = search_efficiency$Building_Type,
                         s1 = search_efficiency$s1,
                         s2 = search_efficiency$s2,
                         s3 = search_efficiency$s3,
                         s4 = search_efficiency$s4,
                         s5 = search_efficiency$s5)


# Fit the pkm model using the averaged searcher data

model_SeEf <- GenEst::pkm(formula_p = p ~ Season,
                          formula_k = k ~ Season,
                          data = efficiency, 
                          obsCol = c('s1', 's2', 's3', 's4', 's5'),
                          allCombos = TRUE)

summary(model_SeEf)

# AICC table
GenEst::aicc(model_SeEf)

# AIC weights for Searcher Efficiency Model
AIC.wts.SeEf=exp(-0.5*aicc(model_SeEf)[,'ΔAICc'])/sum(exp(-0.5*aicc(model_SeEf)[,'ΔAICc']))

round(AIC.wts.SeEf,6)
# 0.535453 0.346580 0.062372 0.055596

# Select model 4 - null model
best_model_SeEf <- model_SeEf[[4]]

#plot(model_SeEf[[4]])



# compile data for carcass persistence model

persistence <- data.frame(ID = carcass_persistence$ID, 
                          Season = carcass_persistence$SeasonGen,
                          BuildingType = carcass_persistence$BuildingTyp,
                          LastPresent = carcass_persistence$LastPresent, 
                          FirstAbsent = carcass_persistence$FirstAbsent)


# Fit the cpm model using the averaged searcher data

model_CaPe <- GenEst::cpm(formula_l = l ~ Season, 
                          formula_s = s ~ Season, 
                          data = carcass_persistence,
                          left = 'LastPresent', 
                          right = 'FirstAbsent', 
                          dist = c('exponential', 'weibull', 'lognormal', 'loglogistic'), 
                          allCombos = T)

# AICC table
GenEst::aicc(model_CaPe)

GenEst::cpmSetFail(model_CaPe) #model did not fail

# AIC weights for Carcass Persistence Model
AIC.wts.CaPe=exp(-0.5*aicc(model_CaPe)[,'ΔAICc'])/sum(exp(-0.5*aicc(model_CaPe)[,'ΔAICc']))
round(AIC.wts.CaPe,4)
# 0.2380 0.1333 0.1059 0.0939 0.0925 0.0788 0.0703 0.0682 0.0658 0.0332 0.0131 0.0064 0.0004 0.0002

# Select model 1
best_model_CaPe <- model_CaPe[[6]]

#plot(model_CaPe[[6]])



#write out model results

saveRDS(best_model_SeEf, file = paste0(dir, 'Results/search_efficiency_model-', run_date, '.rds'))

saveRDS(best_model_CaPe, file = paste0(dir, 'Results/carcass_persistence_model-', run_date, '.rds'))



# EXTRA:

# checking if Fall 2020 SE results are similar to other seasons
# Or at least the previous fall season

efficiencySeason <- data.frame(carcID = search_efficiency$ID,
                               Season = search_efficiency$Season,
                               SeasonGen = search_efficiency$SeasonGen,
                               Year = search_efficiency$SeasonYear,
                               BuildingType = search_efficiency$Building_Type,
                               s1 = search_efficiency$s1,
                               s2 = search_efficiency$s2,
                               s3 = search_efficiency$s3,
                               s4 = search_efficiency$s4,
                               s5 = search_efficiency$s5)

efficiencySeasonFall <- subset(efficiencySeason, Season == 'Fall')

model_SeEf_season <- GenEst::pkm(formula_p = p ~ Year,
                                 formula_k = k ~ Year,
                                 data = efficiencySeasonFall, 
                                 obsCol = c('s1', 's2', 's3', 's4', 's5'),
                                 allCombos = TRUE)

summary(model_SeEf_season)

# AICC table
GenEst::aicc(model_SeEf_season)

#   p Formula k Formula  AICc ΔAICc
# 2     p ~ 1  k ~ Year 49.24  0.00
# 4     p ~ 1     k ~ 1 49.26  0.02
# 1  p ~ Year  k ~ Year 50.93  1.69
# 3  p ~ Year     k ~ 1 51.13  1.89

# Year did not affect the SE in fall


# check for late winter

efficiencySeasonLateWinter <- subset(efficiencySeason, SeasonGen == 'Winter')

model_SeEf_season2 <- GenEst::pkm(formula_p = p ~ Year,
                                 formula_k = k ~ Year,
                                 data = efficiencySeasonLateWinter, 
                                 obsCol = c('s1', 's2', 's3', 's4', 's5'),
                                 allCombos = TRUE)

summary(model_SeEf_season2)

# AICC table
GenEst::aicc(model_SeEf_season2)

#    p Formula k Formula  AICc ΔAICc
# 4     p ~ 1     k ~ 1 56.08  0.00
# 3  p ~ Year     k ~ 1 57.58  1.50
# 2     p ~ 1  k ~ Year 58.47  2.39
# 1  p ~ Year  k ~ Year 61.95  5.87

# Year did not affect the SE in winter



# checking if Fall 2020 SE results are similar to other seasons (year and season combinations)

efficiencySeasonYear <- data.frame(carcID = search_efficiency$ID,
                                   Season = search_efficiency$SeasonGen,
                                   Year = substr(search_efficiency$SeasonYear, 
                                                 nchar(search_efficiency$SeasonYear) - 3, 
                                                 nchar(search_efficiency$SeasonYear)),
                                   BuildingType = search_efficiency$Building_Type,
                                   s1 = search_efficiency$s1,
                                   s2 = search_efficiency$s2,
                                   s3 = search_efficiency$s3,
                                   s4 = search_efficiency$s4,
                                   s5 = search_efficiency$s5)


# Fit the pkm model using the averaged searcher data

model_SeEf_SeasYear <- GenEst::pkm(formula_p = p ~ Season + Year,
                                   formula_k = k ~ Season + Year,
                                   data = efficiencySeasonYear, 
                                   obsCol = c('s1', 's2', 's3', 's4', 's5'),
                                   allCombos = TRUE)

summary(model_SeEf_SeasYear)

# AICC table
AICtable <- GenEst::aicc(model_SeEf_SeasYear)

AIC.wts.SeEf.SeasonYear=exp(-0.5*aicc(model_SeEf_SeasYear)[,'ΔAICc'])/sum(exp(-0.5*aicc(model_SeEf_SeasYear)[,'ΔAICc']))
round(AIC.wts.SeEf.SeasonYear,4)

write.csv(AICtable, file = paste0(dir, 'Results/Tables/SuppS1_SE_AICTable_SeasonYear-', run_date, '.csv'))





# Estimate mortality for 6 study buildings by season (21 day sampling period), by year 

buildings_mortality <- GenEst::estM(data_CO = mortality_observations, 
                                    data_SS = search_schedule,
                                    data_DWP = NULL, 
                                    model_SE = best_model_SeEf, 
                                    model_CP = best_model_CaPe,
                                    unitCol = "Building",
                                    COdate = "Date",
                                    nsim = 10000) 

summary(buildings_mortality)
#   median     5%    95% 
#   179.67 159.66 205.78 

splitSeasonMortality <- GenEst::calcSplits(M = buildings_mortality, 
                                           split_CO = c("SeasonYear"),
                                           data_CO = mortality_observations)

ResultSeasonMortality <- splitSeasonMortality$M

rowMeans(ResultSeasonMortality)
# EarlyWinter2021   Fall2019        Fall2020  LateWinter2020  LateWinter2021  LateWinter2022 
# 18.246565       74.237081       73.019706        5.812366        3.366628        6.068034


#plot results

#Mortality
ResultSeasonMortalityT <- data.frame(t(ResultSeasonMortality))

ResultSeasonMortalityT <- ResultSeasonMortalityT %>%
  dplyr::rename("E_EarlyWinter2021" = "EarlyWinter2021",  
                "A_Fall2019" = "Fall2019",        
                "C_Fall2020" = "Fall2020",        
                "B_LateWinter2020" = "LateWinter2020",  
                "D_LateWinter2021" = "LateWinter2021", 
                "F_LateWinter2022" = "LateWinter2022")


# Reshape the data from wide to long format
ResultSeasonYearMort <- as.data.frame(ResultSeasonMortalityT) %>%
  tidyr::pivot_longer(cols = everything(),  # Reshape all columns
                      names_to = "SeasonYear",  # Create a column for simulation numbers
                      values_to = "Value")  # Create a column for simulation values

birdMortality <- ggplot(ResultSeasonYearMort, aes(x = SeasonYear, y = Value)) +
  geom_boxplot(fill = '#238A8DFF', alpha = 0.8,
               outlier.alpha = 0.1, # Transparency for outliers
               outlier.shape = 16,
               outlier.size = 1,
               outlier.color = "black",  # Color of outliers
               lwd = 0.2) +
  stat_summary(fun = median,       # Change this to `mean` if you prefer the mean
               geom = "point", 
               shape = 21,       # Circle shape with border
               size = 2,         # Size of the point
               color = "black",  # Border color
               fill = "black") +
  labs(x = "Season", 
       y = "Bias-corrected estimate for sampled buildings",
       title = 'A) Bird collision mortality (carcasses)') +
  scale_x_discrete(labels = c("E_EarlyWinter2021" = "EarlyWinter 2021",  
                              "A_Fall2019" = "Fall 2019",        
                              "C_Fall2020" = "Fall 2020",        
                              "B_LateWinter2020" = "LateWinter 2020",  
                              "D_LateWinter2021" = "LateWinter 2021", 
                              "F_LateWinter2022" = "LateWinter 2022")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12, angle = 15, hjust = 1),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))      






# Estimate number of collisions (including feather smears + live birds) by season (21 day sampling period), by year 
buildings_collision <- GenEst::estM(data_CO = collision_observations, 
                                    data_SS = search_schedule,
                                    data_DWP = NULL, 
                                    model_SE = best_model_SeEf, 
                                    model_CP = best_model_CaPe,
                                    unitCol = "Building",
                                    COdate = "Date",
                                    nsim = 10000) 

summary(buildings_collision)
# median     5%    95% 
# 510.95 449.98 592.43 

splitSeasonCollision <- GenEst::calcSplits(M = buildings_collision, 
                                           split_CO = c("SeasonYear"),
                                           data_CO = collision_observations)

ResultSeasonCollision <- splitSeasonCollision$M

rowMeans(ResultSeasonCollision)
# EarlyWinter2021        Fall2019        Fall2020  LateWinter2020  LateWinter2021  LateWinter2022 
# 131.022791      124.202997      204.840724        7.310311       38.531146        8.609967



#collisions
ResultSeasonCollisionT <- data.frame(t(ResultSeasonCollision))

ResultSeasonCollisionT <- ResultSeasonCollisionT %>%
  dplyr::rename("E_EarlyWinter2021" = "EarlyWinter2021",  
                "A_Fall2019" = "Fall2019",        
                "C_Fall2020" = "Fall2020",        
                "B_LateWinter2020" = "LateWinter2020",  
                "D_LateWinter2021" = "LateWinter2021", 
                "F_LateWinter2022" = "LateWinter2022")

# Reshape the data from wide to long format
ResultSeasonYearColl <- as.data.frame(ResultSeasonCollisionT) %>%
  tidyr::pivot_longer(cols = everything(),  # Reshape all columns
                      names_to = "SeasonYear",  # Create a column for simulation numbers
                      values_to = "Value")  # Create a column for simulation values

birdCollision <- ggplot(ResultSeasonYearColl, aes(x = SeasonYear, y = Value)) +
  geom_boxplot(fill = '#482677FF', alpha = 0.7,
               outlier.alpha = 0.1, # Transparency for outliers
               outlier.shape = 16,
               outlier.size = 1,
               outlier.color = "black",  # Color of outliers
               lwd = 0.2) +
  stat_summary(fun = median,       # Change this to `mean` if you prefer the mean
               geom = "point", 
               shape = 21,       # Circle shape with border
               size = 2,         # Size of the point
               color = "black",  # Border color
               fill = "black") +
  labs(x = "Season", 
       y = "Bias-corrected estimate for sampled buildings",
       title = 'B) ALL Bird collisions (including live birds and feather smears)') +
  scale_x_discrete(labels = c("E_EarlyWinter2021" = "EarlyWinter2021",  
                              "A_Fall2019" = "Fall2019",        
                              "C_Fall2020" = "Fall2020",        
                              "B_LateWinter2020" = "LateWinter2020",  
                              "D_LateWinter2021" = "LateWinter2021", 
                              "F_LateWinter2022" = "LateWinter2022")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12, angle = 15, hjust = 1),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))


#arrange a grid with mortality and collision estimates into two plots and export pdf
pdf(paste0(dir, 'Results/estimates_sampled_buildings-', run_date, '.pdf'), 
    width = 12, height = 6)

gridExtra::grid.arrange(birdMortality, birdCollision, ncol=2)

dev.off()





# plot estimates for all years combined for each season

# mortality

fall_mort_results <- ResultSeasonYearMort %>%
  dplyr::filter(SeasonYear == 'A_SepOct19' | SeasonYear == 'C_SepOct20') 
fall_mort_results$season <- rep('A_fall', nrow(fall_mort_results))


earlywinter_mort_results <- ResultSeasonYearMort %>%
  dplyr::filter(SeasonYear == 'E_NovDec21') 
earlywinter_mort_results$season <- rep('B_earlywinter', nrow(earlywinter_mort_results))


latewinter_mort_results <- ResultSeasonYearMort %>%
  dplyr::filter(SeasonYear == 'B_JanFeb20' | SeasonYear == 'D_JanFeb21' | SeasonYear == 'F_JanFeb22') 
latewinter_mort_results$season <- rep('C_latewinter', nrow(latewinter_mort_results))


# join the data frames
results_mort_season <- rbind(fall_mort_results, latewinter_mort_results, earlywinter_mort_results)



# collisions

fall_coll_results <- ResultSeasonYearColl %>%
  dplyr::filter(SeasonYear == 'A_SepOct19' | SeasonYear == 'C_SepOct20') 
fall_coll_results$season <- rep('A_fall', nrow(fall_coll_results))


earlywinter_coll_results <- ResultSeasonYearColl %>%
  dplyr::filter(SeasonYear == 'E_NovDec21') 
earlywinter_coll_results$season <- rep('B_earlywinter', nrow(earlywinter_coll_results))


latewinter_coll_results <- ResultSeasonYearColl %>%
  dplyr::filter(SeasonYear == 'B_JanFeb20' | SeasonYear == 'D_JanFeb21' | SeasonYear == 'F_JanFeb22') 
latewinter_coll_results$season <- rep('C_latewinter', nrow(latewinter_coll_results))


# join the data frames
results_coll_season <- rbind(fall_coll_results, latewinter_coll_results, 
                             earlywinter_coll_results)



# plot mortality for the three seasons 

birdMortality_season <- ggplot(results_mort_season, aes(x = season, y = Value)) +
  geom_boxplot(fill = '#238A8DFF', alpha = 0.8,
               outlier.alpha = 0.1, # Transparency for outliers
               outlier.shape = 16,
               outlier.size = 1,
               outlier.color = "black",  # Color of outliers
               lwd = 0.2) +
  stat_summary(fun = median,       # Change this to `mean` if you prefer the mean
               geom = "point", 
               shape = 21,       # Circle shape with border
               size = 2,         # Size of the point
               color = "black",  # Border color
               fill = "black") +
  labs(x = "Season", 
       y = "Bias-corrected estimate for sampled buildings",
       title = 'Bird mortality') +
  scale_x_discrete(labels = c("A_fall" = "Fall",
                              "B_earlywinter" = "Early Winter", 
                              "C_latewinter" = "Late Winter")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))      


birdCollisions_season <- ggplot(results_coll_season, aes(x = season, y = Value)) +
  geom_boxplot(fill = '#482677FF', alpha = 0.8,
               outlier.alpha = 0.1, # Transparency for outliers
               outlier.shape = 16,
               outlier.size = 1,
               outlier.color = "black",  # Color of outliers
               lwd = 0.2) +
  stat_summary(fun = median,       # Change this to `mean` if you prefer the mean
               geom = "point", 
               shape = 21,       # Circle shape with border
               size = 2,         # Size of the point
               color = "black",  # Border color
               fill = "black") +
  labs(x = "Season", 
       y = "Bias-corrected estimate for sampled buildings",
       title = 'Bird collision') +
  scale_x_discrete(labels = c("A_fall" = "Fall",
                              "B_earlywinter" = "Early Winter", 
                              "C_latewinter" = "Late Winter")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))      


#arrange a grid with mortality and collision estimates into two plots and export pdf
pdf(paste0(dir, 'Results/estimates_sampled_buildings-season-', run_date, '.pdf'), 
    width = 5, height = 8)

gridExtra::grid.arrange(birdMortality_season, birdCollisions_season, nrow=2)

dev.off()






# Calculate summary statistics

#mortality
mort_N <- mortality_observations %>% 
  dplyr::group_by(SeasonYear) %>%
  dplyr::summarise(Ncarcasses = n())

mort_N$SeasonYear <- c("B_JanFeb20", "D_JanFeb21",
                       "F_JanFeb22", "E_NovDec21",
                       "A_SepOct19", "C_SepOct20")

mortality_buildings <- ResultSeasonYearMort %>%
  dplyr::group_by(SeasonYear) %>%
  dplyr::summarise(meanValue = round(mean(Value),0),
                   medianValue = round(median(Value),0),
                   q025 = round(quantile(Value, probs = 0.025),0),
                   q975 = round(quantile(Value, probs = 0.975),0))

mortality_buildings <- mortality_buildings %>%
  dplyr::left_join(mort_N, by = 'SeasonYear')

#export results
write.csv(mortality_buildings, paste0(dir, 'Results/MortalityEstimates-', run_date, '.csv'))


#collisions
coll_N <- collision_observations %>%
  dplyr::group_by(SeasonYear) %>%
  dplyr::summarise(Ncollisions = n())

coll_N$SeasonYear <- c("B_JanFeb20", "D_JanFeb21",
                       "F_JanFeb22", "E_NovDec21",
                       "A_SepOct19", "C_SepOct20")

collisions_buildings <- ResultSeasonYearColl %>%
  dplyr::group_by(SeasonYear) %>%
  dplyr::summarise(meanValue = round(mean(Value),0),
                   medianValue = round(median(Value),0),
                   q025 = round(quantile(Value, probs = 0.025),0),
                   q975 = round(quantile(Value, probs = 0.975),0))

collisions_buildings <- collisions_buildings %>%
  dplyr::left_join(coll_N, by = 'SeasonYear')

#export results
write.csv(collisions_buildings, paste0(dir, 'Results/CollisionEstimates-', run_date, '.csv'))






