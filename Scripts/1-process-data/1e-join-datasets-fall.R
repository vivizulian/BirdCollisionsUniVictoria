
#######################
## Join data sets for abundance/vulnerability estimates

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

#banding counts
banding_data <- read.csv(paste0(dir, 'Data/L1/banding_counts-', data_run_date, '.csv'))

#point counts
point_count_data <- read.csv(paste0(dir, 'Data/L1/point_counts-', data_run_date, '.csv'))

#flocking information
flock <- read.csv(paste0(dir, 'Data/L1/flock_data-', data_run_date, '.csv'))

#collisions
collision_data <- read.csv(paste0(dir, 'Data/L1/collisions_data_per_species-', data_run_date, '.csv'))


#check species

bandingSpNames <- colnames(banding_data)[10:76]
spName <- colnames(point_count_data)[8:61]

spName[!spName %in% bandingSpNames]
#Species that are not captured on nets:
# [1] "AmericanCrow"         "AmericanPipit"        "CanadaGoose"          "CommonRaven"         
# [5] "EurasianCollaredDove" "GlaucousWingedGull"   "HouseSparrow"         "Mallard"             
# [9] "Merlin"               "RedCrossbill"         "TownsendsSolitaire"   "TurkeyVulture"

bandingSpNames[!bandingSpNames %in% spName]
# [1] "BandTailedPigeon"         "BarredOwl"                "BeltedKingfisher"        
# [4] "BlackHeadedGrosbeak"      "BlackThroatedGrayWarbler" "BrewersBlackbird"        
# [7] "BrownHeadedCowbird"       "CassinsVireo"             "ChippingSparrow"         
# [10] "CommonYellowthroat"       "HammondsFlycatcher"       "HouseWren"               
# [13] "HuttonsVireo"             "LincolnsSparrow"          "MacGillivraysWarbler"    
# [16] "NorthernSawWhetOwl"       "OliveSidedFlycatcher"     "RedBreastedSapsucker"    
# [19] "RufousHummingbird"        "TennesseeWarbler"         "VioletGreenSwallow"      
# [22] "WarblingVireo"            "WesternWoodPewee"         "WhiteThroatedSparrow"    
# [25] "WilsonsWarbler"


#add species 
#from banding to point counts
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- NA
  data
}

point_count_data <- fncols(point_count_data, c(bandingSpNames))
point_count_data[is.na(point_count_data)] <- 0


#from point counts to banding
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- NA
  data
}

banding_data <- fncols(banding_data, c(spName))
banding_data[is.na(banding_data)] <- 0


#filter data for fall to identify the species to keep for FALL only
#CRITERIA: Species that were detected at least once on banding data OR on point 
#counts OR on collisions data

#banding data for both years for fall
BandingDataFall <- subset(banding_data, BANDING_MONTH == 9 | 
                            BANDING_MONTH == 10)
BandingSpecies <- BandingDataFall %>%
  dplyr::select(AmericanGoldfinch:TurkeyVulture) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

#point count
PointDataFall <- subset(point_count_data, Season == 'Fall')
PointSpecies <- PointDataFall %>%
  dplyr::select(AmericanCrow:WilsonsWarbler) %>%
  colSums() %>%
  .[. > 0] %>%
  names()

#collisions data
CollisionsDataFall <- subset(collision_data, SEASON == 'Fall')
CollisionsSpecies <- unique(CollisionsDataFall$SPECIES)


#make a list of species to keep for fall 
SpeciesKeepFall <- sort(unique(c(BandingSpecies, PointSpecies, CollisionsSpecies)))


#filter the banding and point to include only the species of interest

BandingDataFallFilter <- BandingDataFall %>%
  dplyr::select(BANDING_DAY, BANDING_MONTH, BANDING_YEAR, 
                DATE, NET_ID, effortNhoursNet, 
                effortTotalHoursNet, effort_str,
                all_of(SpeciesKeepFall))

effortBanding <- BandingDataFallFilter %>%
  dplyr::select(BANDING_DAY, BANDING_MONTH, BANDING_YEAR, 
                DATE, effortNhoursNet, 
                effortTotalHoursNet, effort_str) %>%
  dplyr::distinct(DATE, .keep_all = TRUE)


PointDataFallFilter <- PointDataFall %>%
  dplyr::select(Date, Location, Year, Month, 
                Season, Effort,
                all_of(SpeciesKeepFall))

effortPointFall <- PointDataFallFilter %>%
  dplyr::select(Date, Location, Year, Month,
                Season, Effort) %>%
  dplyr::distinct(Location, Year, .keep_all = TRUE)


#63 species in total for fall



# for each season, create the 3D data frame with sites, visits and species

#convert data to long format
BandingDataFall2019 <- BandingDataFallFilter %>%
  dplyr::filter(BANDING_YEAR == 2019) %>%
  tidyr::pivot_longer(cols = AmericanCrow:YellowWarbler,
                      names_to = "Species",
                      values_to = "Count")

BandingDataFall2020 <- BandingDataFallFilter %>%
  dplyr::filter(BANDING_YEAR == 2020) %>%
  tidyr::pivot_longer(cols = AmericanCrow:YellowWarbler,
                      names_to = "Species",
                      values_to = "Count")

# Convert to 3D array: [Location, Date, Species]
BandingDataFall2019Array <- reshape2::acast(BandingDataFall2019, NET_ID ~ DATE ~ Species, 
                                  value.var = "Count", fill = 0)

BandingDataFall2020Array <- reshape2::acast(BandingDataFall2020, NET_ID ~ DATE ~ Species, 
                                  value.var = "Count", fill = 0)


#create a column with year and season aggregated
PointDataFallFilter <- PointDataFallFilter %>%
  dplyr::mutate(SeasonYear = paste0(Season, Year)) %>%
  dplyr::relocate(SeasonYear, .after = Season)

#convert data to long format
PointDataFall2019 <- PointDataFallFilter %>%
  dplyr::filter(Year == 2019) %>%
  pivot_longer(cols = AmericanCrow:YellowWarbler,
               names_to = "Species",
               values_to = "Count")

PointDataFall2020 <- PointDataFallFilter %>%
  dplyr::filter(Year == 2020) %>%
  pivot_longer(cols = AmericanCrow:YellowWarbler,
               names_to = "Species",
               values_to = "Count")


# Convert to 3D array: [Location, Date, Species]
PointDataFall2019Array <- reshape2::acast(PointDataFall2019, Location ~ Date ~ Species, 
                                value.var = "Count", fill = 0)

PointDataFall2020Array <- reshape2::acast(PointDataFall2020, Location ~ Date ~ Species, 
                                value.var = "Count", fill = 0)


#make sure that the species order is the same in both datasets
identical(dimnames(BandingDataFall2019Array)[3][[1]], 
          dimnames(PointDataFall2019Array)[3][[1]])


#effort for each year

#match banding date order as the array order
effortBandingFall2019 <- effortBanding %>%
  dplyr::filter(BANDING_YEAR == 2019) %>%
  dplyr::mutate(DATE = factor(DATE, levels = colnames(BandingDataFall2019Array))) %>%
  dplyr::arrange(DATE)

effortBandingFall2020 <- effortBanding %>%
  dplyr::filter(BANDING_YEAR == 2020) %>%
  dplyr::mutate(DATE = factor(DATE, levels = colnames(BandingDataFall2020Array))) %>%
  dplyr::arrange(DATE)


#since all values for point counts are the same, the order do not matter
effortPointFall2019 <- effortPointFall %>%
  dplyr::filter(Year == 2019)
  
effortPointFall2020 <- effortPointFall %>%
  dplyr::filter(Year == 2020)
  


#flocking data

flockFall <- flock %>% 
  dplyr::select(-X.1) %>%
  dplyr::filter(Common.Name %in% SpeciesKeepFall) %>%
  dplyr::select(Common.Name, FlockingFall)
  


#collisions data

species_df <- data.frame(SPECIES_ID = SpeciesKeepFall)

CollisionsDataFall2019 <- CollisionsDataFall %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'Fall' & Year == 2019) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)

CollisionsDataFall2020 <- CollisionsDataFall %>%
  dplyr::select(Date, Year, Season, SPECIES_ID) %>%
  dplyr::filter(Season == 'Fall' & Year == 2020) %>%
  dplyr::group_by(SPECIES_ID) %>%
  dplyr::summarise(Ncollisions = n()) %>%
  dplyr::right_join(species_df, by = "SPECIES_ID") %>%
  dplyr::mutate(Ncollisions = replace_na(Ncollisions, 0)) %>%
  dplyr::arrange(SPECIES_ID)




# write out -----------------------------------------------------------

write.csv(flockFall, paste0(dir, 'Data/L2/flockFall-', run_date, '.csv'))

#banding + point counts + collisions + effort for each data set in each year
#2019
saveRDS(BandingDataFall2019Array, paste0(dir, 'Data/L2/BandingDataFall2019Array-', run_date, '.rds'))

saveRDS(PointDataFall2019Array, paste0(dir, 'Data/L2/PointDataFall2019Array-', run_date, '.rds'))

write.csv(effortBandingFall2019, paste0(dir, 'Data/L2/effortBandingFall2019-', run_date, '.csv'))

write.csv(effortPointFall2019, paste0(dir, 'Data/L2/effortPointFall2019-', run_date, '.csv'))

write.csv(CollisionsDataFall2019, paste0(dir, 'Data/L2/CollisionsDataFall2019-', run_date, '.csv'))

#2020
saveRDS(BandingDataFall2020Array, paste0(dir, 'Data/L2/BandingDataFall2020Array-', run_date, '.rds'))

saveRDS(PointDataFall2020Array, paste0(dir, 'Data/L2/PointDataFall2020Array-', run_date, '.rds'))

write.csv(effortBandingFall2020, paste0(dir, 'Data/L2/effortBandingFall2020-', run_date, '.csv'))

write.csv(effortPointFall2020, paste0(dir, 'Data/L2/effortPointFall2020-', run_date, '.csv'))

write.csv(CollisionsDataFall2020, paste0(dir, 'Data/L2/CollisionsDataFall2020-', run_date, '.csv'))


