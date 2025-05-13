#######################
## Process Rocky Point Banding Data 
## Data sent to Louise Blight

#######################


# set dirs ----------------------------------------------------------------

run_date <- Sys.Date()
dir <- 'XXX'


# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)


# read in data -----------------------------------------------------------

#banding records (counts)
banding_raw_data <- read.csv(paste0(dir, 'Data/L0/count_data/RPBO_BandingData_2019-2022.csv'))

#effort data (counts)
effort_data <- read.csv(paste0(dir, 'Data/L0/count_data/RockyPointEffortData.csv'))


#filter banding data
banding_raw_data <- banding_raw_data %>%
  dplyr::select('BAND_NUMBER', 'SPECIES', 'BANDING_MONTH',
                'BANDING_DAY', 'BANDING_YEAR', 'BANDER_LOCATION_ID',
                'CAPTURE_TIME', 'NET_NEST_CAVITY_NUMBER') %>%
  dplyr::filter(BANDER_LOCATION_ID == "RPBO") %>%
  dplyr::mutate(DATE = as.Date(paste(BANDING_YEAR, BANDING_MONTH, BANDING_DAY, sep="-"), 
                               format = "%Y-%m-%d")) %>%
  dplyr::mutate(COUNT = 1)


#filter effort data
effort_data <- effort_data %>%
  dplyr::filter(AREA.C.10 == "RPBO") %>%
  dplyr::mutate(DATE = as.Date(DATE.D, format = "%d/%m/%Y")) %>%
  dplyr::select('Year', 'AREA.C.10', 'OBSERVERS.N.5.1', 
                'SBNETS.N.5.1', 'START', 'END', 'NHOURS',
                'DATE')

ComMatrixBand <- tidyr::pivot_wider(banding_raw_data,
                                    id_cols = c(BANDING_DAY, BANDING_MONTH, 
                                                BANDING_YEAR, DATE, NET_NEST_CAVITY_NUMBER),
                                    names_from = SPECIES,
                                    values_from = COUNT,
                                    values_fn = sum,
                                    values_fill = 0)


sort(colnames(ComMatrixBand))



ComMatrixBand <- ComMatrixBand %>%
  dplyr::rename("AlderFlycatcher" = "ALFL", "AmericanGoldfinch" = "AMGO",          
                "AmericanRedstart" = "AMRE", "AmericanRobin" = "AMRO",
                "AnnasHummingbird" = "ANHU", "AudubonsWarbler" = "AUWA",          
                "BarredOwl" = "BDOW", "BeltedKingfisher" = "BEKI",
                "BewicksWren" = "BEWR", "BrownHeadedCowbird" = "BHCO",
                "BlackHeadedGrosbeak" = "BHGR", "BrewersBlackbird" = "BRBL",
                "BrownCreeper" = "BRCR", "BrownShrike" = "BRSH",
                "BandTailedPigeon" = "BTPI", "BlackThroatedGrayWarbler" = "BTYW", 
                "Bushtit" = "BUSH", "CassinsVireo" = "CAVI", 
                "ChestnutBackedChickadee" = "CBCH", "ClayColoredSparrow" = "CCSP", 
                "CedarWaxwing" = "CEDW", "ChippingSparrow" = "CHSP", 
                "CoopersHawk" = "COHA", "CommonYellowthroat" = "COYE", 
                "ChestnutSidedWarbler" = "CSWA", 
                "DownyWoodpecker" = "DOWO", "DuskyFlycatcher" = "DUFL", 
                "EuropeanStarling" = "EUST", "NorthernFlicker" = "FLIN", 
                "FoxSparrow" = "FOSP", "GoldenCrownedKinglet" = "GCKI", 
                "GoldenCrownedSparrow" = "GCSP", "GrayCatbird" = "GRCA", 
                "GambelsWhiteCrownedSparrow" = "GWCS", "HammondsFlycatcher" = "HAFL", 
                "HairyWoodpecker" = "HAWO", "HermitThrush" = "HETH", 
                "HouseFinch" = "HOFI", "HouseWren" = "HOWR", 
                "HuttonsVireo" = "HUVI", "LeastFlycatcher" = "LEFL", 
                "LincolnsSparrow" = "LISP", "MarshWren" = "MAWR", 
                "MacGillivraysWarbler" = "MGWA", "MyrtleWarbler" = "MYWA", 
                "NashvilleWarbler" = "NAWA", "NorthernWaterthrush" = "NOWA", 
                "NorthernSawWhetOwl" = "NSWO", "OrangeCrownedWarbler" = "OCWA", 
                "OregonJunco" = "ORJU", "OrchardOriole" = "OROR", 
                "OliveSidedFlycatcher" = "OSFL", "PacificWren" = "PAWR", 
                "PineSiskin" = "PISI", "PileatedWoodpecker" = "PIWO", 
                "PacificSlopeFlycatcher" = "PSFL", "PugetSoundWhiteCrowned" = "PSWS", 
                "PurpleFinch" = "PUFI", "RedBreastedNuthatch" = "RBNU", 
                "RedBreastedSapsucker" = "RBSA", "RubyCrownedKinglet" = "RCKI", 
                "RedShaftedFlicker" = "RSFL", "RedTailedHawk" = "RTHA", 
                "RufousHummingbird" = "RUHU", "RedWingedBlackbird" = "RWBL", 
                "SavannahSparrow" = "SAVS", "SlateColoredJunco" = "SCJU",
                "Sora" = "SORA", "SongSparrow" = "SOSP", 
                "SpottedTowhee" = "SPTO", "SharpShinnedHawk" = "SSHA", 
                "StellersJay" = "STJA", "SwampSparrow" = "SWSP", 
                "SwainsonsThrush" = "SWTH", "TennesseeWarbler" = "TEWA", 
                "TownsendsWarbler" = "TOWA", "UnkYellowRumpedWarbler" = "UYRW", 
                "VariedThrush" = "VATH", "VioletGreenSwallow" = "VGSW", 
                "WarblingVireo" = "WAVI", "WhiteCrownedSparrow" = "WCSP",
                "WesternTanager" = "WETA", "WesternWoodPewee" = "WEWP",
                "WillowFlycatcher" = "WIFL", "WilsonsWarbler" = "WIWA", 
                "WesternPalmWarbler" = "WPWA", "WhiteThroatedSparrow" = "WTSP", 
                "YellowWarbler" = "YEWA",
                "NET_ID" = "NET_NEST_CAVITY_NUMBER") 


ComMatrixBand <- within(ComMatrixBand, effortNhoursNet <- effort_data[match(ComMatrixBand$DATE, effort_data$DATE), 7])
ComMatrixBand <- within(ComMatrixBand, effortTotalHoursNet <- effort_data[match(ComMatrixBand$DATE, effort_data$DATE), 4])
#divided by the max effort (78) to standardize
ComMatrixBand <- within(ComMatrixBand, effort_str <- ComMatrixBand$effortTotalHoursNet/78)

#select data form 2019 and 2020 and months 9 and 10:
banding_count <- subset(ComMatrixBand, BANDING_YEAR == 2020 | BANDING_YEAR == 2019)
banding_count <- subset(banding_count, BANDING_MONTH == 9 | BANDING_MONTH == 10)


#remove entries for NA in the net number information
banding_count <- banding_count %>% 
  dplyr::filter(!is.na(NET_ID))


#join species, remove species that are rare or do not occur at campus
banding_count <- banding_count %>%
  dplyr::select(!c("AlderFlycatcher", "AmericanRedstart", "ClayColoredSparrow", "DuskyFlycatcher",
                   "GrayCatbird", "LeastFlycatcher", "MarshWren", "NashvilleWarbler", "NorthernWaterthrush",
                   "OrchardOriole", "Sora", "SwampSparrow", "WesternPalmWarbler"))

banding_count <- banding_count %>% 
  dplyr::mutate(YellowRumpedWarbler = AudubonsWarbler + UnkYellowRumpedWarbler + MyrtleWarbler) %>%
  dplyr::select(-AudubonsWarbler, -UnkYellowRumpedWarbler, -MyrtleWarbler) %>%
  
  dplyr::mutate(WhiteCrownedSparrow = WhiteCrownedSparrow + GambelsWhiteCrownedSparrow) %>%
  dplyr::select(-GambelsWhiteCrownedSparrow) %>%
  
  dplyr::mutate(DarkEyedJunco = OregonJunco + SlateColoredJunco) %>%
  dplyr::select(-OregonJunco, -SlateColoredJunco) %>%
  
  dplyr::mutate(WhiteCrownedSparrow = WhiteCrownedSparrow + PugetSoundWhiteCrowned) %>%
  dplyr::select(-PugetSoundWhiteCrowned) %>%
  
  dplyr::mutate(NorthernFlicker = RedShaftedFlicker + NorthernFlicker) %>%
  dplyr::select(-RedShaftedFlicker) %>%
  
  dplyr::select(-BrownShrike, -ChestnutSidedWarbler)


#reorder columns
banding_count <- banding_count %>%
  dplyr::relocate(all_of(sort(names(banding_count)))) %>%

  dplyr::relocate(BANDING_DAY, BANDING_MONTH,
                  BANDING_YEAR, DATE, NET_ID,
                  effortNhoursNet, effortTotalHoursNet,
                  effort_str,
                  .before = AmericanGoldfinch)


write.csv(banding_count, paste0(dir, 'Data/L1/banding_counts-', run_date, '.csv'))


