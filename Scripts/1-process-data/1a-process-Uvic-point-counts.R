#######################
## Process University of Victoria Point Counts

#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
dir <- 'XXX'


# load packages -----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(reshape)



# read in data -----------------------------------------------------------

UVic_counts <- read.csv(paste0(dir, 'Data/L0/count_data/UVic_PointCountData.csv'))
flock <- read.csv(paste0(dir, 'Data/L0/count_data/FlockingData.csv'))


#filter data
UVic_counts <- UVic_counts %>%
  dplyr::filter(Location != 'University of Victoria')

UVic_counts$Date <- as.Date(UVic_counts$Date, "%m/%d/%y")


#flock data - transform to categorical variable - non-flocking = 0; flocking = 1
flock_data <- flock %>%
  dplyr::mutate(across(c(FlockingWinter, FlockingFall), 
                       ~ recode(.x, "N" = 0, "Y" = 1, .default = NA_real_))) %>%
  dplyr::arrange(Common.Name)



#create community matrix
ComMatrixCounts <- reshape::cast(UVic_counts, Date + Location ~ Common.Name, 
                                 value='Count', fun.aggregate = sum)

#popular name in alphabetical order
ComMatrixCounts <- ComMatrixCounts %>%
  dplyr::rename("AmericanCrow" = "American Crow", 
                "AmericanGoldfinch" = "American Goldfinch", 
                "AmericanPipit" = "American Pipit", 
                "AmericanRobin" = "American Robin", 
                "AnnasHummingbird" = "Anna's Hummingbird",
                "BewicksWren" = "Bewick's Wren", 
                "BrownCreeper" = "Brown Creeper", 
                "Bushtit" = "Bushtit", 
                "CanadaGoose" = "Canada Goose", 
                "CedarWaxwing" = "Cedar Waxwing",
                "ChestnutBackedChickadee" = "Chestnut-backed Chickadee", 
                "CommonRaven" = "Common Raven", 
                "CoopersHawk" = "Cooper's Hawk", 
                "DarkEyedJunco" = "Dark-eyed Junco", 
                "DownyWoodpecker" = "Downy Woodpecker", 
                "EurasianCollaredDove" = "Eurasian Collared-Dove", 
                "EuropeanStarling" = "European Starling", 
                "FoxSparrow" = "Fox Sparrow", 
                "GlaucousWingedGull" = "Glaucous-winged Gull", 
                "GoldenCrownedKinglet" = "Golden-crowned Kinglet", 
                "GoldenCrownedSparrow" = "Golden-crowned Sparrow", 
                "HairyWoodpecker" = "Hairy Woodpecker", 
                "HermitThrush" = "Hermit Thrush", 
                "HouseFinch" = "House Finch", 
                "HouseSparrow" = "House Sparrow", 
                "Mallard" = "Mallard", 
                "Merlin" = "Merlin", 
                "NorthernFlicker" = "Northern Flicker", 
                "OrangeCrownedWarbler" = "Orange-crowned Warbler", 
                "PacificWren" = "Pacific Wren", 
                "PacificSlopeFlycatcher" = "Pacific-slope Flycatcher", 
                "PileatedWoodpecker" = "Pileated Woodpecker", 
                "PineSiskin" = "Pine Siskin", 
                "PurpleFinch" = "Purple Finch", 
                "RedCrossbill" = "Red Crossbill", 
                "RedBreastedNuthatch" = "Red-breasted Nuthatch", 
                "RedTailedHawk" = "Red-tailed Hawk", 
                "RedWingedBlackbird" = "Red-winged Blackbird", 
                "RubyCrownedKinglet" = "Ruby-crowned Kinglet", 
                "SavannahSparrow" = "Savannah Sparrow", 
                "SharpShinnedHawk" = "Sharp-shinned Hawk", 
                "SongSparrow" = "Song Sparrow",
                "SpottedTowhee" = "Spotted Towhee", 
                "StellersJay" = "Steller's Jay", 
                "SwainsonsThrush" = "Swainson's Thrush", 
                "TownsendsSolitaire" = "Townsend's Solitaire",
                "TownsendsWarbler" = "Townsend's Warbler", 
                "TurkeyVulture" = "Turkey Vulture", 
                "VariedThrush" = "Varied Thrush",
                "WesternTanager" = "Western Tanager", 
                "WhiteCrownedSparrow" = "White-crowned Sparrow",
                "WillowFlycatcher" = "Willow Flycatcher", 
                "YellowWarbler" = "Yellow Warbler", 
                "YellowRumpedWarbler" = "Yellow-rumped Warbler") 

ComMatrixCounts <- ComMatrixCounts %>%
  dplyr::relocate(all_of(sort(names(ComMatrixCounts)))) %>%
  dplyr::relocate(Date, Location, .before = 'AmericanCrow') 



#get the year and month
ComMatrixCounts <- within(ComMatrixCounts, Year <- substr(ComMatrixCounts$Date, 1, 4))
ComMatrixCounts <- within(ComMatrixCounts, Month <- substr(ComMatrixCounts$Date, 6, 7))

##Fill the season
ComMatrixCounts$Month <- as.numeric(ComMatrixCounts$Month)

ComMatrixCounts <- ComMatrixCounts %>%
  dplyr::mutate(Season = case_when(Month == 9 | Month == 10  ~ "Fall",
                                   Month >= 11  ~ "EarlyWinter",
                                   TRUE       ~ "LateWinter")) %>%
  dplyr::mutate(Effort = rep(0.004)) %>% #area (km2) sampled - 3927 m2 = 0.004 km2
  dplyr::relocate(c(Year, Month, Season, Effort), .after = Location)


#summaries:

table(ComMatrixCounts$Year, ComMatrixCounts$Month)
#       1  2  9 10 11 12
# 2019  0  0 24 12  0  0
# 2020 24 12 29  6  0  0
# 2021 24 12  0  0 18 18
# 2022 24 12  0  0  0  0


#mean counts per species per season

MeanCountsSeason <- ComMatrixCounts %>%
  dplyr::group_by(Season, Year) %>%
  dplyr::summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  dplyr::select(-Month)


MeanCountsSeason <- MeanCountsSeason %>%
  pivot_longer(cols = -c(Season, Year), 
               names_to = "Species", 
               values_to = "Count") %>%
  pivot_wider(names_from = c(Season, Year), 
              values_from = Count, 
              values_fill = list(Count = 0))


# write out -----------------------------------------------------------

write.csv(ComMatrixCounts, paste0(dir, 'Data/L1/point_counts-', run_date, '.csv'))
write.csv(flock_data, paste0(dir, 'Data/L1/flock_data-', run_date, '.csv'))



