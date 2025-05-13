
#######################
# Create data sets of mortality and collisions

#COLLISIONS = all evidence, including father piles, carcasses, feather smears, and carcasses         
#MORTALITY = known deaths (feather piles + carcasses)
#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
data_run_date <- '2025-04-15'
dir <- 'XXX'


# load packages -----------------------------------------------------------

library(dplyr)


# read in data -----------------------------------------------------------

#search efficiency
search_efficiency <- read.csv(paste0(dir, 'Data/L0/searcher_efficiency_data/SearcherEfficiencyData.csv'))

#carcass persistence
carcass_persistence <- read.csv(paste0(dir, 'Data/L0/carcass_persistence_data/CarcassPersistenceData.csv'))

#search schedule
search_schedule <- read.csv(paste0(dir, 'Data/L0/searcher_efficiency_data/SearcherScheduleData.csv'))
search_schedule$Date <- as.Date(search_schedule$SearchDate, format = '%m/%d/%y')

#collisions data
collisions_data <- read.csv(paste0(dir, 'Data/L1/all_collisions_data-', data_run_date, '.csv'))



# split data per type (collisions or mortality)

unique(collisions_data$TYPE_OF_COLLISION)
# "feather smear"  "carcass"        "feather pile"   "collision_live"

#unknown: COLLISIONS = all evidence, including father piles, carcasses, feather smears, and carcasses
all_collisions <- subset(collisions_data, EVIDENCE_FOUND == 'Y')
all_collisions <- all_collisions %>%
  dplyr::mutate(SeasonYear = paste0(Season, Year))

#known deaths: MORTALITY (feather piles + carcasses)
all_mortality <- subset(collisions_data, TYPE_OF_COLLISION != 'feather smear' &
                          TYPE_OF_COLLISION != "collision_live")
all_mortality <- all_mortality %>%
  dplyr::mutate(SeasonYear = paste0(Season, Year))



#create a collisions data set

collision_observations = data.frame(carcID = paste0('x',(rownames(all_collisions))[(all_collisions$EVIDENCE_FOUND)=='Y']),
                                    Year = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'Year'],
                                    Season = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'Season'],
                                    SeasonYear = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'SeasonYear'],
                                    Building = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'BUILDING_CODE'],
                                    Facade = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'FACADE'],
                                    Date = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'Date'],
                                    Species = all_collisions[(all_collisions$EVIDENCE_FOUND)=='Y', 'SPECIES_ID'])

collision_observations$carcID = paste0('x',(rownames(collision_observations)))



#create a mortality data set

mortality_observations = data.frame(carcID = paste0('x',(rownames(all_mortality))[(all_mortality$EVIDENCE_FOUND)=='Y']),
                                    Year = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'Year'],
                                    Season = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'Season'],
                                    SeasonYear = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'SeasonYear'],
                                    Building = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'BUILDING_CODE'],
                                    Facade = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'FACADE'],
                                    Date = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'Date'],
                                    Species = all_mortality[(all_mortality$EVIDENCE_FOUND)=='Y', 'SPECIES_ID'])

mortality_observations$carcID = paste0('x',(rownames(mortality_observations)))


# write out files
write.csv(mortality_observations, 
          file = paste0(dir, 'Data/L1/mortality_observations-', run_date, '.csv'),
          row.names = FALSE)

write.csv(collision_observations, 
          file = paste0(dir, 'Data/L1/collision_observations-', run_date, '.csv'),
          row.names = FALSE)

