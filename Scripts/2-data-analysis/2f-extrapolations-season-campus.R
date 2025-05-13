#######################
## Use fraction for one of the variables and calculate a rate to extrapolate for the other one.
## Example: Separate high from low buildings for each season - run separate models for each one using splits for buildings - 
## join estimates - calculate a mortality rate per day for the whole campus for the sampled period - 
## multiply by the total number of days. 

#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
model_run_date <- 'XXX' 
data_run_date <- 'XXX' 
dir <- 'XXX'


# load packages -----------------------------------------------------------

library(GenEst)
library(ggplot2)



# read in data -----------------------------------------------------------

#CP model results
carcass_persistence <- readRDS(paste0(dir, 'Results/carcass_persistence_model-', model_run_date, '.rds'))

#SE model results
search_efficiency <- readRDS(paste0(dir, 'Results/search_efficiency_model-', model_run_date, '.rds'))

#search schedule
search_schedule <- read.csv(paste0(dir, 'Data/L0/searcher_efficiency_data/SearcherScheduleData.csv'))
search_schedule$Date <- as.Date(search_schedule$SearchDate, format = '%m/%d/%y')

#collisions obs
collision_obs <- read.csv(paste0(dir, 'Data/L1/collision_observations-', data_run_date, '.csv'))

#mortality obs
mortality_obs <- read.csv(paste0(dir, 'Data/L1/mortality_observations-', data_run_date, '.csv'))


# filter data for each season -----------------------------------------------------------

#FALL 2019

#collisions
col_observations_Fall19 <- subset(collision_obs, SeasonYear == "Fall2019")

#mortality
mort_observations_Fall19 <- subset(mortality_obs, SeasonYear == "Fall2019")

#search schedule
search_schedule_Fall19 <- search_schedule[search_schedule$SeasonYear %in% c("SepOct2019"),]


#FALL 2020

#collisions
col_observations_Fall20 <- subset(collision_obs, SeasonYear == "Fall2020")

#mortality
mort_observations_Fall20 <- subset(mortality_obs, SeasonYear == "Fall2020")

#search schedule
search_schedule_Fall20 <- search_schedule[search_schedule$SeasonYear %in% c("SepOct2020"),]


#EARLY WINTER 2021

#collisions
col_observations_EarlyWinter <- subset(collision_obs, SeasonYear == "EarlyWinter2021")

#mortality
mort_observations_EarlyWinter <- subset(mortality_obs, SeasonYear == "EarlyWinter2021")

#search schedule
search_schedule_EarlyWinter <- search_schedule[search_schedule$SeasonYear %in% c("NovDec2021"),]


#LATE WINTER 2020

#collisions
col_observations_LateWinter20 <- subset(collision_obs, SeasonYear == "LateWinter2020")

#mortality
mort_observations_LateWinter20 <- subset(mortality_obs, SeasonYear == "LateWinter2020")

#search schedule
search_schedule_LateWinter20 <- search_schedule[search_schedule$SeasonYear %in% c("JanFeb2020"),]


# LATE WINTER 2021

#collisions
col_observations_LateWinter21 <- subset(collision_obs, SeasonYear == "LateWinter2021")

#mortality
mort_observations_LateWinter21 <- subset(mortality_obs, SeasonYear == "LateWinter2021")

#search schedule
search_schedule_LateWinter21 <- search_schedule[search_schedule$SeasonYear %in% c("JanFeb2021"),]


# LATE WINTER 2022

#collisions
col_observations_LateWinter22 <- subset(collision_obs, SeasonYear == "LateWinter2022")

#mortality
mort_observations_LateWinter22 <- subset(mortality_obs, SeasonYear == "LateWinter2022")

#search schedule
search_schedule_LateWinter22 <- search_schedule[search_schedule$SeasonYear %in% c("JanFeb2022"),]



# COLLISIONS - extrapolate for each season -----------------------------------------------------------

#FALL 2019
all_col_Fall19 <- GenEst::estM(data_CO = col_observations_Fall19, 
                               data_SS = search_schedule_Fall19,
                               data_DWP = NULL, 
                               model_SE = search_efficiency, 
                               model_CP = carcass_persistence,
                               unitCol = "Building",
                               frac=((6/51)*(21/45)), #6 buildings/total #buildings (51) * 21/total of 45 days in fall 2019.
                               COdate = "Date", 
                               nsim = 10000) 

summary(all_col_Fall19)
#   median      5%     95% 
#   2260.84 1907.21 2642.14 

plot(all_col_Fall19)



#FALL 2020
all_col_Fall20 <- estM(data_CO = col_observations_Fall20, 
                       data_SS = search_schedule_Fall20,
                       data_DWP = NULL, 
                       model_SE = search_efficiency, 
                       model_CP = carcass_persistence,
                       unitCol = "Building",
                       frac=((6/51)*(21/45)), #6 buildings/total #buildings (51) * 21/total of 45 days in fall 2019.
                       COdate = "Date", 
                       nsim = 10000) 

summary(all_col_Fall20)
#   median      5%     95% 
#   1808.20 1494.26 2151.82 

plot(all_col_Fall20)


#Extract the simulated values
sims_col_Fall19 <- colSums(all_col_Fall19$Mhat)
sims_col_Fall20 <- colSums(all_col_Fall20$Mhat)

allCollSimsFall <- c(sims_col_Fall19, sims_col_Fall20)
CollFallResult <- data.frame(meanValue = mean(allCollSimsFall), medianValue=median(allCollSimsFall),
                             q025 = quantile(allCollSimsFall, probs = 0.025),
                             q975 = quantile(allCollSimsFall, probs = 0.975), row.names = 'Fall')

#       meanValue medianValue     q025     q975
# Fall  2040.681    2027.097 1494.093 2642.182



#EARLY WINTER 2020
all_col_EarlyWinter <- estM(data_CO = col_observations_EarlyWinter, 
                            data_SS = search_schedule_EarlyWinter,
                            data_DWP = NULL, 
                            model_SE = search_efficiency, 
                            model_CP = carcass_persistence,
                            unitCol = "Building",
                            frac=((6/51)*(21/68)), #6 buildings/total #buildings (51) * 21/total # days in earlyWinter (68).
                            COdate = "Date", 
                            nsim = 10000) 
summary(all_col_EarlyWinter)
# median      5%     95% 
#   1022.30  748.15 1332.14 

plot(all_col_EarlyWinter)

sims_col_EarlyWinter <- colSums(all_col_EarlyWinter$Mhat)

CollEarlyWinterResult <- data.frame(meanValue = mean(sims_col_EarlyWinter), medianValue=median(sims_col_EarlyWinter),
                                    q025 = quantile(sims_col_EarlyWinter, probs = 0.025),
                                    q975 = quantile(sims_col_EarlyWinter, probs = 0.975), row.names = 'EarlyWinter')

#             meanValue medianValue     q025     q975
# EarlyWinter  1029.414    1022.297 703.0794 1396.152



#LATE WINTER 2020
all_col_LateWinter20 <- estM(data_CO = col_observations_LateWinter20, 
                             data_SS = search_schedule_LateWinter20,
                             data_DWP = NULL, 
                             model_SE = search_efficiency, 
                             model_CP = carcass_persistence,
                             unitCol = "Building",
                             frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                             COdate = "Date", 
                             nsim = 10000) 
summary(all_col_LateWinter20)
#   median     5%    95% 
#   220.40  76.63 381.79 

plot(all_col_LateWinter20)



#LATE WINTER 2021
all_col_LateWinter21 <- estM(data_CO = col_observations_LateWinter21, 
                             data_SS = search_schedule_LateWinter21,
                             data_DWP = NULL, 
                             model_SE = search_efficiency, 
                             model_CP = carcass_persistence,
                             unitCol = "Building",
                             frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                             COdate = "Date", 
                             nsim = 10000) 

summary(all_col_LateWinter21)
#    median      5%     95% 
#   722.73  464.13 1011.16

plot(all_col_LateWinter21)



#LATE WINTER 2022
all_col_LateWinter22 <- estM(data_CO = col_observations_LateWinter22, 
                             data_SS = search_schedule_LateWinter22,
                             data_DWP = NULL, 
                             model_SE = search_efficiency, 
                             model_CP = carcass_persistence,
                             unitCol = "Building",
                             frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                             COdate = "Date", 
                             nsim = 10000) 

summary(all_col_LateWinter22)
#   median     5%    95% 
#   184.16  71.81 332.20

plot(all_col_LateWinter22)


#Extract the simulated values
sims_col_Winter20 <- colSums(all_col_LateWinter20$Mhat)
sims_col_Winter21 <- colSums(all_col_LateWinter21$Mhat)
sims_col_Winter22 <- colSums(all_col_LateWinter22$Mhat)

allCollSimsWinter <- c(sims_col_Winter20, sims_col_Winter21, sims_col_Winter22)
CollWinterResult <- data.frame(meanValue = mean(allCollSimsWinter), medianValue=median(allCollSimsWinter),
                               q025 = quantile(allCollSimsWinter, probs = 0.025),
                               q975 = quantile(allCollSimsWinter, probs = 0.975), row.names = 'LateWinter')

#            meanValue medianValue     q025     q975
# LateWinter  380.1149    261.9611 72.03057 972.6964


CollisionsResults <- rbind(CollFallResult, CollEarlyWinterResult, CollWinterResult)

#             meanValue medianValue       q025      q975
# Fall        2040.6810   2027.0968 1494.09307 2642.1825
# EarlyWinter 1029.4138   1022.2967  703.07941 1396.1522
# LateWinter   380.1149    261.9611   72.03057  972.6964



#write out results
write.csv(CollisionsResults, paste0(dir, 'Results/Collisions_Campus_Season_Extrapolation-', run_date, '.csv'))


# join simulation results

allCollSimsWinterLong <- data.frame(value = allCollSimsWinter, 
                                      season = rep('C_LateWinter', length(allCollSimsWinter)))

sims_col_EarlyWinterLong <- data.frame(value = sims_col_EarlyWinter, 
                                       season = rep('B_EarlyWinter', length(sims_col_EarlyWinter)))

allCollSimsFallLong <- data.frame(value = allCollSimsFall, 
                                  season = rep('A_Fall', length(allCollSimsFall)))


ResultSeasonAllColl <- rbind(allCollSimsWinterLong, 
                             sims_col_EarlyWinterLong,
                             allCollSimsFallLong)


birdCollisionTotal <- ggplot(ResultSeasonAllColl, aes(x = season, y = value)) +
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
       y = "Campus- and season-wide bias-corrected estimate",
       title = 'B) ALL Bird collisions (including live birds and feather smears)') +
  scale_x_discrete(labels = c("A_Fall" = "Fall",
                              "B_EarlyWinter" = "Early Winter", 
                              "C_LateWinter" = "Late Winter")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))




# MORTALITY - extrapolate for each season -----------------------------------------------------------

#FALL 2019
all_mort_Fall19 <- estM(data_CO = mort_observations_Fall19, 
                        data_SS = search_schedule_Fall19,
                        data_DWP = NULL, 
                        model_SE = search_efficiency, 
                        model_CP = carcass_persistence,
                        unitCol = "Building",
                        frac=((6/51)*(21/45)), #6 buildings/total #buildings (51) * 21/total of 45 days in fall 2019.
                        COdate = "Date", 
                        nsim = 10000) 

summary(all_mort_Fall19)
#    median      5%     95% 
#   1349.43 1088.35 1639.04 

plot(all_mort_Fall19)



#FALL 2020
all_mort_Fall20 <- estM(data_CO = mort_observations_Fall20, 
                        data_SS = search_schedule_Fall20,
                        data_DWP = NULL, 
                        model_SE = search_efficiency, 
                        model_CP = carcass_persistence,
                        unitCol = "Building",
                        frac=((6/51)*(21/45)), #6 buildings/total #buildings (51) * 21/total of 45 days in fall 2019.
                        COdate = "Date", 
                        nsim = 10000) 
summary(all_mort_Fall20)
#   median      5%     95% 
#   788.24  592.64 1006.51 

plot(all_mort_Fall20)


#Extract the simulated values
sims_mort_Fall19 <- colSums(all_mort_Fall19$Mhat)
sims_mort_Fall20 <- colSums(all_mort_Fall20$Mhat)

allMortSimsFall <- c(sims_mort_Fall19, sims_mort_Fall20)
MortFallResult <- data.frame(meanValue = mean(allMortSimsFall), medianValue=median(allMortSimsFall),
                             q025 = quantile(allMortSimsFall, probs = 0.025),
                             q975 = quantile(allMortSimsFall, probs = 0.975), row.names = 'Fall')

#       meanValue medianValue     q025     q975
# Fall  1072.829    1045.808 592.6422 1639.041



#EARLY WINTER 2021
all_mort_EarlyWinter <- estM(data_CO = mort_observations_EarlyWinter, 
                             data_SS = search_schedule_EarlyWinter,
                             data_DWP = NULL, 
                             model_SE = search_efficiency, 
                             model_CP = carcass_persistence,
                             unitCol = "Building",
                             frac=((6/51)*(21/68)), #6 buildings/total #buildings (51) * 21/total # days in earlyWinter (68).
                             COdate = "Date", 
                             nsim = 10000) 

summary(all_mort_EarlyWinter)
#   median     5%    95% 
#   267.83 126.23 434.77 

plot(all_mort_EarlyWinter)

sims_mort_EarlyWinter <- colSums(all_mort_EarlyWinter$Mhat)

MortEarlyWinterResult <- data.frame(meanValue = mean(sims_mort_EarlyWinter), medianValue=median(sims_mort_EarlyWinter),
                                    q025 = quantile(sims_mort_EarlyWinter, probs = 0.025),
                                    q975 = quantile(sims_mort_EarlyWinter, probs = 0.975), row.names = 'EarlyWinter')

#             meanValue medianValue     q025     q975
# EarlyWinter  273.4233    267.8316 116.2801 470.9685



#LATE WINTER 2020
all_mort_LateWinter20 <- estM(data_CO = mort_observations_LateWinter20, 
                              data_SS = search_schedule_LateWinter20,
                              data_DWP = NULL, 
                              model_SE = search_efficiency, 
                              model_CP = carcass_persistence,
                              unitCol = "Building",
                              frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                              COdate = "Date", 
                              nsim = 10000) 

summary(all_mort_LateWinter20)
#    median     5%    95% 
#   183.82  72.03 337.61 

plot(all_mort_LateWinter20)



#LATE WINTER 2021
all_mort_LateWinter21 <- estM(data_CO = mort_observations_LateWinter21, 
                              data_SS = search_schedule_LateWinter21,
                              data_DWP = NULL, 
                              model_SE = search_efficiency, 
                              model_CP = carcass_persistence,
                              unitCol = "Building",
                              frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                              COdate = "Date", 
                              nsim = 10000) 

summary(all_mort_LateWinter21)
# median     5%     95% 
# 110.53  34.70 225.17 

#We don't have splits for this season because there is only one sampling period.
plot(all_mort_LateWinter21)



# 2) LATE WINTER 2022
all_mort_LateWinter22 <- estM(data_CO = mort_observations_LateWinter22, 
                              data_SS = search_schedule_LateWinter22,
                              data_DWP = NULL, 
                              model_SE = search_efficiency, 
                              model_CP = carcass_persistence,
                              unitCol = "Building",
                              frac=((6/51)*(21/84)), #6 buildings/total #buildings (51) * 21/total # days in LateWinter (84).
                              COdate = "Date", 
                              nsim = 10000) 

summary(all_mort_LateWinter22)
# median     5%     95% 
# 147.35  37.31 282.92 

#We don't have splits for this season because there is only one sampling period.
plot(all_mort_LateWinter22)



#Extract the simulated values
sims_mort_Winter20 <- colSums(all_mort_LateWinter20$Mhat)
sims_mort_Winter21 <- colSums(all_mort_LateWinter21$Mhat)
sims_mort_Winter22 <- colSums(all_mort_LateWinter22$Mhat)

allMortSimsWinter <- c(sims_mort_Winter20, sims_mort_Winter21, sims_mort_Winter22)
MortWinterResult <- data.frame(meanValue = mean(allMortSimsWinter), medianValue=median(allMortSimsWinter),
                               q025 = quantile(allMortSimsWinter, probs = 0.025),
                               q975 = quantile(allMortSimsWinter, probs = 0.975), row.names = 'LateWinter')

#             meanValue medianValue     q025     q975
# LateWinter   150.439    146.5175 34.53632 329.9392



MortalityResults <- rbind(MortFallResult, MortEarlyWinterResult, MortWinterResult)

#             meanValue medianValue      q025      q975
# Fall        1072.8291   1045.8076 592.64217 1639.0414
# EarlyWinter  273.4233    267.8316 116.28006  470.9685
# LateWinter   150.4390    146.5175  34.53632  329.9392


write.csv(MortalityResults, paste0(dir, 'Results/Mortality_Campus_Season_Extrapolation-', run_date, '.csv'))


# join simulation results

allMortSimsWinterLong <- data.frame(value = allMortSimsWinter, 
                                    season = rep('C_LateWinter', length(allMortSimsWinter)))

sims_mort_EarlyWinterLong <- data.frame(value = sims_mort_EarlyWinter, 
                                       season = rep('B_EarlyWinter', length(sims_mort_EarlyWinter)))

allMortSimsFallLong <- data.frame(value = allMortSimsFall, 
                                  season = rep('A_Fall', length(allMortSimsFall)))


ResultSeasonAllMort <- rbind(sims_mort_EarlyWinterLong, 
                             allMortSimsWinterLong,
                             allMortSimsFallLong)


birdMortalityTotal <- ggplot(ResultSeasonAllMort, aes(x = season, y = value)) +
  geom_boxplot(fill = '#238A8DFF', alpha = 0.7,
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
       y = "Campus- and season-wide bias-corrected estimate",
       title = 'A) Bird collision mortality (carcasses)') +
  scale_x_discrete(labels = c("A_Fall" = "Fall",
                              "B_EarlyWinter" = "Ealy Winter", 
                              "C_LateWinter" = "Late Winter")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14, margin = margin(t = 20)),      # Increase x-axis label size
        axis.title.y = element_text(size = 14),      # Increase y-axis label size
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),          # Remove major gridlines
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))


#arrange a grid with mortality and collision estimates into two plots and export pdf
pdf(paste0(dir, 'Results/estimates_campus-season-wide-', run_date, '.pdf'), 
    width = 5, height = 8)

gridExtra::grid.arrange(birdMortalityTotal, birdCollisionTotal, nrow=2)

dev.off()




# histogram with all results - Mortality

#Early Winter
median_mortEW <- median(sims_mort_EarlyWinterLong$value)
ciEW <- quantile(sims_mort_EarlyWinterLong$value, probs = c(0.025, 0.975))

earlywinterplotM <- ggplot(sims_mort_EarlyWinterLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#238A8DFF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_mortEW + 200, y = 0.004, 
           label = paste0("Median: ", round(median_mortEW, 2), "\n95% CI: [", 
                          round(ciEW[1], 1), ", ", round(ciEW[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Mortality", y = "Probability",
       title = 'Campus- and season-wide bird mortality\n Early winter (Nov-Dec)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))


#Late Winter
median_mortLW <- median(allMortSimsWinterLong$value)
ciLW <- quantile(allMortSimsWinterLong$value, probs = c(0.025, 0.975))

latewinterplotM <- ggplot(allMortSimsWinterLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#238A8DFF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_mortLW + 300, y = 0.007, 
           label = paste0("Median: ", round(median_mortLW, 2), "\n95% CI: [", 
                          round(ciLW[1], 1), ", ", round(ciLW[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Mortality", y = "Probability",
       title = 'Campus- and season-wide bird mortality\n Late winter (Jan-Mar)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))


#Fall
median_mortF <- median(allMortSimsFallLong$value)
ciF <- quantile(allMortSimsFallLong$value, probs = c(0.025, 0.975))

fallplotM <- ggplot(allMortSimsFallLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#238A8DFF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_mortF + 500, y = 0.0013, 
           label = paste0("Median: ", round(median_mortF, 2), "\n95% CI: [", 
                          round(ciF[1], 1), ", ", round(ciF[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Mortality", y = "Probability",
       title = 'Campus- and season-wide bird mortality\n Fall (Aug-Oct)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))



# histogram with all results - Collisions

#Early Winter
median_colEW <- median(sims_col_EarlyWinterLong$value)
ciEWcol <- quantile(sims_col_EarlyWinterLong$value, probs = c(0.025, 0.975))

earlywinterplotC <- ggplot(sims_col_EarlyWinterLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#482677FF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_colEW + 300, y = 0.002, 
           label = paste0("Median: ", round(median_colEW, 2), "\n95% CI: [", 
                          round(ciEWcol[1], 1), ", ", round(ciEWcol[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Collisions", y = "Probability",
       title = 'Campus- and season-wide bird collisions\n Early winter (Nov-Dec)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))


#Late Winter
median_colLW <- median(allCollSimsWinterLong$value)
ciLWcol <- quantile(allCollSimsWinterLong$value, probs = c(0.025, 0.975))

latewinterplotC <- ggplot(allCollSimsWinterLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#482677FF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_colLW + 700, y = 0.0025, 
           label = paste0("Median: ", round(median_colLW, 2), "\n95% CI: [", 
                          round(ciLWcol[1], 1), ", ", round(ciLWcol[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Collisions", y = "Probability",
       title = 'Campus- and season-wide bird collisions\n Late winter (Jan-Mar)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))


#Fall
median_colF <- median(allCollSimsFallLong$value)
ciFcol <- quantile(allCollSimsFallLong$value, probs = c(0.025, 0.975))

fallplotC <- ggplot(allCollSimsFallLong, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#482677FF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median_colF + 500, y = 0.001, 
           label = paste0("Median: ", round(median_colF, 2), "\n95% CI: [", 
                          round(ciFcol[1], 1), ", ", round(ciFcol[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Collisions", y = "Probability",
       title = 'Campus- and season-wide bird collisions\n Fall (Aug-Oct)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16, hjust = 0.5))



#arrange a grid with mortality and collision estimates into two plots and export pdf
pdf(paste0(dir, 'Results/estimates_campus-season-wide-histog-', run_date, '.pdf'), 
    width = 10, height = 10)

gridExtra::grid.arrange(fallplotM, fallplotC, 
                        earlywinterplotM, earlywinterplotC, 
                        latewinterplotM, 
                        latewinterplotC, ncol=2)

dev.off()





# Histogram with the Sum of all seasons

# collisions
winter_means_coll <- cbind(winter2020 = sims_col_Winter20, 
                           winter2021 = sims_col_Winter21, 
                           winter2022 = sims_col_Winter22)

winter_means_coll <- rowMeans(winter_means_coll)


fall_means_coll <- cbind(fall2019 = sims_col_Fall19,
                         fall2020 = sims_col_Fall20)

fall_means_coll <- rowMeans(fall_means_coll)

allmeans_coll <- data.frame(cbind(fall = fall_means_coll, 
                                  winter = winter_means_coll, 
                                  earlywinter = sims_col_EarlyWinter))


# Get the total sum using the three seasons
allmeans_coll$totals <- rowSums(allmeans_coll)
ci_col <- quantile(allmeans_coll$totals, probs = c(0.025, 0.975))

total_collisions <- ggplot(allmeans_coll, aes(x = totals)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#482677FF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median(allmeans_coll$totals) + 200, y = 0.0017, 
           label = paste0("Median: ", round(median(allmeans_coll$totals), 2), "\n95% CI: [", 
                          round(ci_col[1], 1), ", ", round(ci_col[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Collisions", y = "Probability",
       title = 'B) Total bird collisions (including live birds and feather smears)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))



# mortality
winter_means_mort <- cbind(winter2020 = sims_mort_Winter20, 
                           winter2021 = sims_mort_Winter21, 
                           winter2022 = sims_mort_Winter22)

winter_means_mort <- rowMeans(winter_means_mort)


fall_means_mort <- cbind(fall2019 = sims_mort_Fall19,
                         fall2020 = sims_mort_Fall20)

fall_means_mort <- rowMeans(fall_means_mort)

allmeans_mort <- data.frame(cbind(fall = fall_means_mort, 
                                  winter = winter_means_mort, 
                                  earlywinter = sims_mort_EarlyWinter))


# Get the total sum using the three seasons
allmeans_mort$totals <- rowSums(allmeans_mort)
ci_mort <- quantile(allmeans_mort$totals, probs = c(0.025, 0.975))

total_mortality <- ggplot(allmeans_mort, aes(x = totals)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "#238A8DFF", color = "black", 
                 alpha = 0.4, size = 0.3) +
  geom_density(color = "black", size = 1) +
  annotate("text", x = median(allmeans_mort$totals) + 200, y = 0.0025, 
           label = paste0("Median: ", round(median(allmeans_mort$totals), 2), "\n95% CI: [", 
                          round(ci_mort[1], 1), ", ", round(ci_mort[2], 1), "]"), 
           hjust = 0) +
  labs(x = "Mortality", y = "Probability",
       title = 'A) Total bird collision mortality (carcasses)') +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),      # Increase x-axis label size
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12),       # Increase x-axis tick labels size
        axis.text.y = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        plot.title = element_text(size = 16))


#arrange a grid with mortality and collision estimates into two plots and export pdf
pdf(paste0(dir, 'Results/estimates_total-histog-', run_date, '.pdf'), 
    width = 10, height = 5)

gridExtra::grid.arrange(total_mortality, 
                        total_collisions,
                        ncol=2)

dev.off()


