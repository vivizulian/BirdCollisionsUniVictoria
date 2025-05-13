
#######################
## Join data sets for abundance/vulnerability estimates
## Winter (early and late)
#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
data_run_date <- 'XXX'
dir <- 'XXX'



# load packages -----------------------------------------------------------

library(dplyr)
library(tidyr)
library(reshape2)



# read in data -----------------------------------------------------------

#point counts
point_count_data <- read.csv(paste0(dir, 'Data/L1/point_counts-', data_run_date, '.csv'))

#flocking information
flock <- read.csv(paste0(dir, 'Data/L1/flock_data-', data_run_date, '.csv'))

#collisions
collision_data <- read.csv(paste0(dir, 'Data/L1/collisions_data_per_species-', data_run_date, '.csv'))


#filter data for fall to identify the species to keep for FALL only
#CRITERIA: Species that were detected at least once on point 
#counts OR on collisions data

#point count
PointDataWinter <- subset(point_count_data, Season == 'EarlyWinter' | Season == 'LateWinter')
PointSpecies <- PointDataWinter %>%
  dplyr::select(AmericanCrow:YellowWarbler) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

#collisions data
CollisionsDataWinter <- subset(collision_data, Season == 'LateWinter' | Season == 'EarlyWinter')
CollisionsSpecies <- unique(CollisionsDataWinter$SPECIES_ID)


#make a list of species to keep for fall 
SpeciesKeepWinter <- sort(unique(c(PointSpecies, CollisionsSpecies)))


#filter the point to include only the species of interest

PointDataWinterFilter <- PointDataWinter %>%
  dplyr::select(Date, Location, Year, Month, 
                Season, Effort,
                all_of(SpeciesKeepWinter))

effortPointWinter <- PointDataWinterFilter %>%
  dplyr::select(Date, Location, Year, Month,
                Season, Effort) %>%
  dplyr::distinct(Location, Year, Season, .keep_all = TRUE)


#36 species in total for winter


# for each season, create the 3D data frame with sites, visits and species

#create a column with year and season aggregated
PointDataWinterFilter <- PointDataWinterFilter %>%
  dplyr::mutate(SeasonYear = paste0(Season, Year)) %>%
  dplyr::relocate(SeasonYear, .after = Season)


#convert data to long format
PointDataLateWinter2020 <- PointDataWinterFilter %>%
  dplyr::filter(SeasonYear == 'LateWinter2020') %>%
  pivot_longer(cols = AmericanCrow:VariedThrush,
               names_to = "Species",
               values_to = "Count")

PointDataEarlyWinter2021 <- PointDataWinterFilter %>%
  dplyr::filter(SeasonYear == 'EarlyWinter2021') %>%
  pivot_longer(cols = AmericanCrow:VariedThrush,
               names_to = "Species",
               values_to = "Count")

PointDataLateWinter2021 <- PointDataWinterFilter %>%
  dplyr::filter(SeasonYear == 'LateWinter2021') %>%
  pivot_longer(cols = AmericanCrow:VariedThrush,
               names_to = "Species",
               values_to = "Count")

PointDataLateWinter2022 <- PointDataWinterFilter %>%
  dplyr::filter(SeasonYear == 'LateWinter2022') %>%
  pivot_longer(cols = AmericanCrow:VariedThrush,
               names_to = "Species",
               values_to = "Count")


# Convert to 3D array: [Location, Date, Species]
PointDataLateWinter2020Array <- reshape2::acast(PointDataLateWinter2020, Location ~ Date ~ Species, 
                                                value.var = "Count", fill = 0)

PointDataEarlyWinter2021Array <- reshape2::acast(PointDataEarlyWinter2021, Location ~ Date ~ Species, 
                                                value.var = "Count", fill = 0)

PointDataLateWinter2021Array <- reshape2::acast(PointDataLateWinter2021, Location ~ Date ~ Species, 
                                                value.var = "Count", fill = 0)

PointDataLateWinter2022Array <- reshape2::acast(PointDataLateWinter2022, Location ~ Date ~ Species, 
                                                value.var = "Count", fill = 0)


#effort for each year

#match banding date order as the array order
#since all values for point counts are the same, the order do not matter
effortPointLateWinter2020 <- effortPointWinter %>%
  dplyr::filter(Year == 2020 & Season == 'LateWinter')

effortPointEarlyWinter2021 <- effortPointWinter %>%
  dplyr::filter(Year == 2021 & Season == 'EarlyWinter')

effortPointLateWinter2021 <- effortPointWinter %>%
  dplyr::filter(Year == 2021 & Season == 'LateWinter')

effortPointLateWinter2022 <- effortPointWinter %>%
  dplyr::filter(Year == 2022 & Season == 'LateWinter')


#flocking winter

flockWinter <- flock %>% 
  dplyr::select(-X.1) %>%
  dplyr::filter(Common.Name %in% SpeciesKeepWinter) %>%
  dplyr::select(Common.Name, FlockingWinter)



#collisions data

species_df <- data.frame(SPECIES_ID = SpeciesKeepWinter)

CollisionsDataLateWinter2020 <- CollisionsDataWinter %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'LateWinter' & Year == 2020) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)

CollisionsDataEarlyWinter2021 <- CollisionsDataWinter %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'EarlyWinter' & Year == 2021) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)

CollisionsDataLateWinter2021 <- CollisionsDataWinter %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'LateWinter' & Year == 2021) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)

CollisionsDataLateWinter2022 <- CollisionsDataWinter %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'LateWinter' & Year == 2022) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)




# write out -----------------------------------------------------------

write.csv(flockWinter, paste0(dir, 'Data/L2/flockWinter-', run_date, '.csv'))

#point counts + collisions + effort for each data set in each year

#2020 - Late winter (jan/feb)
saveRDS(PointDataLateWinter2020Array, paste0(dir, 'Data/L2/PointDataLateWinter2020Array-', run_date, '.rds'))
write.csv(effortPointLateWinter2020, paste0(dir, 'Data/L2/effortPointLateWinter2020-', run_date, '.csv'))
write.csv(CollisionsDataLateWinter2020, paste0(dir, 'Data/L2/CollisionsDataLateWinter2020-', run_date, '.csv'))


#2021 - Early winter (nov/dec)
saveRDS(PointDataEarlyWinter2021Array, paste0(dir, 'Data/L2/PointDataEarlyWinter2021Array-', run_date, '.rds'))
write.csv(effortPointEarlyWinter2021, paste0(dir, 'Data/L2/effortPointEarlyWinter2021-', run_date, '.csv'))
write.csv(CollisionsDataEarlyWinter2021, paste0(dir, 'Data/L2/CollisionsDataEarlyWinter2021-', run_date, '.csv'))


#2021 - Late winter (jan/feb)
saveRDS(PointDataLateWinter2021Array, paste0(dir, 'Data/L2/PointDataLateWinter2021Array-', run_date, '.rds'))
write.csv(effortPointLateWinter2021, paste0(dir, 'Data/L2/effortPointLateWinter2021-', run_date, '.csv'))
write.csv(CollisionsDataLateWinter2021, paste0(dir, 'Data/L2/CollisionsDataLateWinter2021-', run_date, '.csv'))


#2022 - Late winter (jan/feb)
saveRDS(PointDataLateWinter2022Array, paste0(dir, 'Data/L2/PointDataLateWinter2022Array-', run_date, '.rds'))
write.csv(effortPointLateWinter2022, paste0(dir, 'Data/L2/effortPointLateWinter2022-', run_date, '.csv'))
write.csv(CollisionsDataLateWinter2022, paste0(dir, 'Data/L2/CollisionsDataLateWinter2022-', run_date, '.csv'))



