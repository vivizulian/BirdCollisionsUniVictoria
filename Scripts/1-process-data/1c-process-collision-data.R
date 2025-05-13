#######################
## Process University of Victoria Collision data

#Remove all collisions with no identification of species

#######################



# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
dir <- 'XXX'



# load packages -----------------------------------------------------------

library(dplyr)



# read in data -----------------------------------------------------------

#fall 
coll_fall <- read.csv(paste0(dir, 'Data/L0/collisions_data/collisionsData_Fall.csv'))

#winter
coll_winter <- read.csv(paste0(dir, 'Data/L0/collisions_data/collisionsData_Winter.csv'))


coll_fall <- coll_fall %>%
  dplyr::mutate(Date = as.Date(SURVEY_DATE, format = "%Y-%m-%d")) 

coll_winter <- coll_winter %>%
  dplyr::mutate(Date = as.Date(SURVEY_DATE, format = "%m/%d/%y")) 


collisions_data <- rbind(coll_fall, coll_winter)


collisions_data <- collisions_data %>%
  dplyr::filter(EVIDENCE_FOUND == 'Y') %>%
  dplyr::mutate(Year = substr(Date, 1, 4)) %>%
  dplyr::mutate(Month = as.numeric(substr(Date, 6, 7))) 

collisions_data <- collisions_data %>%
  dplyr::mutate(Season = case_when(Month == 9 | Month == 10  ~ "Fall",
                                   Month >= 11  ~ "EarlyWinter",
                                   Month == 01 | Month == 02  ~ "LateWinter")) 


collisions_data_per_season <- collisions_data %>%
  dplyr::group_by(Year, Season) %>%
  dplyr::summarise(Ncollisions = n())
  
# Year  Season        Ncollisions
# 1 2019  Fall                117
# 2 2020  Fall                108
# 3 2020  LateWinter            6
# 4 2021  EarlyWinter          50
# 5 2021  LateWinter           27
# 6 2022  LateWinter            7


mortality_data_per_season <- collisions_data %>%
  dplyr::filter(TYPE_OF_COLLISION == 'carcass' | TYPE_OF_COLLISION == 'feather pile') %>%
  dplyr::group_by(Year, Season) %>%
  dplyr::summarise(Nmortality = n())

# Year  Season         Nmortality
# 1 2019  Fall                69
# 2 2020  Fall                44
# 3 2020  LateWinter           5
# 4 2021  EarlyWinter         11
# 5 2021  LateWinter           3
# 6 2022  LateWinter           5
  

collisions_data <- collisions_data %>%
  dplyr::mutate(SPECIES_ID = recode(SPECIES_ID, "unknown" = NA_character_,
                                    "GCSP" = "GoldenCrownedSparrow", 
                                    "SOSP" = "SongSparrow",  
                                    "ORJU" = "DarkEyedJunco", 
                                    "GCKI" = "GoldenCrownedKinglet",  
                                    "SAVS" = "SavannahSparrow",   
                                    "HETH" = "HermitThrush",
                                    "YEWA" = "YellowWarbler",
                                    "LISP" = "LincolnsSparrow",  
                                    "EMBERIZIDAE" = NA_character_,
                                    "SWTH" = "SwainsonsThrush",
                                    "PARULIDAE" = NA_character_,
                                    "FOSP" = "FoxSparrow",  
                                    "YRWA" = "YellowRumpedWarbler",
                                    "OCWA" = "OrangeCrownedWarbler", 
                                    "Thrush sp." = NA_character_, 
                                    "ZONOTRICHIA" = NA_character_, 
                                    "REGULUS" = NA_character_,
                                    "VATH" = "VariedThrush", 
                                    "ANHU" = "AnnasHummingbird", 
                                    "MELOSPIZA" = NA_character_, 
                                    "AMPI" = "AmericanPipit", 
                                    "RCKI" = "RubyCrownedKinglet", 
                                    "CATHARUS" = NA_character_,
                                    "AMRO" = "AmericanRobin",
                                    "UNKNOWN" = NA_character_,
                                    "PSFL" = "PacificSlopeFlycatcher", 
                                    "PISI" =  "PineSiskin", 
                                    "CEDW" = "CedarWaxwing",
                                    "CBCH" = "ChestnutBackedChickadee",
                                    "HOFI" = "HouseFinch")) 

collisions_data_species <- collisions_data %>%
  dplyr::filter(!is.na(SPECIES_ID))


write.csv(collisions_data_species, paste0(dir, 'Data/L1/collisions_data_per_species-', run_date, '.csv'))
write.csv(collisions_data, paste0(dir, 'Data/L1/all_collisions_data-', run_date, '.csv'))

