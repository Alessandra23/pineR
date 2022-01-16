listWeatherData <- list.files("Data/Weather Stations", pattern = "*.xlsx", full.names = TRUE)
readWeatherData <- lapply(listWeatherData, dataFrameData)
WeatherDataFiles <- lapply(readWeatherData, weatherData)
names(WeatherDataFiles) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(listWeatherData))
WeatherDataFiles

#data <- weightedTemp(WeatherDataFiles, distances = c(2,4,6,8))


# distances
library(Hmisc)
getDistance <- function(data, site, list){
  target <- names(list)
  target <- gsub(site, "", target)
  target <- capitalize(target)
  target <- gsub("([a-z])([A-Z])", "\\1 \\2", target)
  dis <- data %>% filter(Site == site) %>%
    select(`Weather Station`, Distance) %>%
    arrange(factor(`Weather Station` , levels = target))
  return(dis$Distance)
}

# get weather stations for each site

getWeatherStation <- function(site){
  grepl(site, names(WeatherDataFiles)) %>%
    keep(WeatherDataFiles, .)
}

# weighted data

# Hortland ----------------------------------------------------------------

auxHortland <- getWeatherStation("Hortland")
auxHortland <- list(ballinlaHouseHortland = auxHortland$ballinlaHouseHortland,
                    derrygreenaghHortland = auxHortland$derrygreenaghHortland,
                    dunsanyHortland = auxHortland$dunsanyHortland)
getDistance(summaryData, "Hortland", auxHortland)

hortlandData <- weightedTemp(auxHortland, getDistance(summaryData, "Hortland", auxHortland))
save(hortlandData,file="Data/Weighted Data/hortlandData.Rda")

# Ballinagee --------------------------------------------------------------

ballinageeData <- weightedTemp(getWeatherStation("Ballinagee"),
                               getDistance(summaryData, "Ballinagee", getWeatherStation("Ballinagee")))
save(ballinageeData,file="Data/Weighted Data/ballinageeData.Rda")


# Oakwood -----------------------------------------------------------------

OakwoodData <- weightedTemp(getWeatherStation("Oakwood"),
                            getDistance(summaryData, "Oakwood", getWeatherStation("Oakwood")))
save(OakwoodData,file="Data/Weighted Data/OakwoodData.Rda")

# Glendine1 ---------------------------------------------------------------

Glendine1Data <- weightedTemp(getWeatherStation("Glendine1"),
                              getDistance(summaryData, "Glendine1", getWeatherStation("Glendine1")))
save(Glendine1Data,file="Data/Weighted Data/Glendine1Data.Rda")

# Glendine2 ---------------------------------------------------------------

Glendine2Data <- weightedTemp(getWeatherStation("Glendine2"),
                              getDistance(summaryData, "Glendine2", getWeatherStation("Glendine2")))
save(Glendine2Data,file="Data/Weighted Data/Glendine2Data.Rda")

# Lackenrea 1  -------------------------------------------------------------

Lackenrea1Data <- weightedTemp(getWeatherStation("Lackenrea"),
                              getDistance(summaryData, "Lackenrea 1", getWeatherStation("Lackenrea")))
save(Lackenrea1Data,file="Data/Weighted Data/Lackenrea1Data.Rda")


# Lackenrea 2 -------------------------------------------------------------

Lackenrea2Data <- weightedTemp(getWeatherStation("Lackenrea"),
                               getDistance(summaryData, "Lackenrea 2", getWeatherStation("Lackenrea")))
save(Lackenrea2Data,file="Data/Weighted Data/Lackenrea2Data.Rda")


# Summerhill --------------------------------------------------------------

SummerhillData <- weightedTemp(getWeatherStation("Summerhill")$dunsanySummerhill, c(8.1))
save(SummerhillData,file="Data/Weighted Data/SummerhillData.Rda")

# Deerpark ----------------------------------------------------------------

DeerparkData <- weightedTemp(getWeatherStation("Deerpark"),
                               getDistance(summaryData, "Deerpark",
                                           getWeatherStation("Deerpark")))
save(DeerparkData,file="Data/Weighted Data/DeerparkData.Rda")

# Ballymacshaneboy --------------------------------------------------------

BallymacshaneboyData <- weightedTemp(getWeatherStation("Ballymacshaneboy")$mountRussellBallymacshaneboy,2.5)
save(BallymacshaneboyData,file="Data/Weighted Data/BallymacshaneboyData.Rda")

# Kilduff -----------------------------------------------------------------

KilduffData <- weightedTemp(getWeatherStation("Kilduff")$derrygreenaghKilduff, 7.03)
save(KilduffData,file="Data/Weighted Data/KilduffData.Rda")


# Rossnagad ---------------------------------------------------------------

RossnagadData <- weightedTemp(getWeatherStation("Rossnagad"),
                              getDistance(summaryData, "Rossnagad",
                                          getWeatherStation("Rossnagad")))
save(RossnagadData,file="Data/Weighted Data/RossnagadData.Rda")

# Ballyroan1  -------------------------------------------------------------

Ballyroan1Data <- weightedTemp(getWeatherStation("Ballyroan1"),
                               getDistance(summaryData, "Ballyroan1",
                                           getWeatherStation("Ballyroan1")))
save(Ballyroan1Data,file="Data/Weighted Data/Ballyroan1Data.Rda")


# Ballyroan2 --------------------------------------------------------------

Ballyroan2Data <- weightedTemp(getWeatherStation("Ballyroan2"),
                               getDistance(summaryData, "Ballyroan2",
                                           getWeatherStation("Ballyroan2")))
save(Ballyroan2Data,file="Data/Weighted Data/Ballyroan2Data.Rda")

# Donadea -----------------------------------------------------------------

DonadeaData <- weightedTemp(getWeatherStation("Donadea"),
                            getDistance(summaryData, "Donadea",
                                        getWeatherStation("Donadea")))
save(DonadeaData,file="Data/Weighted Data/DonadeaData.Rda")

# Cloondara ---------------------------------------------------------------

CloondaraData <- weightedTemp(getWeatherStation("Cloondara")$mountdillonCloondara, 5.46)
save(CloondaraData,file="Data/Weighted Data/CloondaraData.Rda")

# Knockaville -------------------------------------------------------------

KnockavilleData <- weightedTemp(getWeatherStation("Knockaville"),
                                getDistance(summaryData, "Knockaville",
                                            getWeatherStation("Knockaville")))
save(KnockavilleData,file="Data/Weighted Data/KnockavilleData.Rda")

# Kilurney ----------------------------------------------------------------

KilurneyData <- weightedTemp(getWeatherStation("Kilurney"),
                             getDistance(summaryData, "Kilurney",
                                         getWeatherStation("Kilurney")))
save(KilurneyData,file="Data/Weighted Data/KilurneyData.Rda")

# Doon --------------------------------------------------------------------

DoonData <- weightedTemp(getWeatherStation("Doon"),
                         getDistance(summaryData, "Doon",
                                     getWeatherStation("Doon")))
save(DoonData,file="Data/Weighted Data/DoonData.Rda")


# Clonoghil* ---------------------------------------------------------------

ClonoghilData <- weightedTemp(getWeatherStation("Clonoghil")$nealstownClonoghil, 5.46)
save(ClonoghilData,file="Data/Weighted Data/ClonoghilData.Rda")

# Tigroney* ----------------------------------------------------------------

TigroneyData <- weightedTemp(getWeatherStation("Tigroney")$glenealyTigroney, 6.48)
save(TigroneyData,file="Data/Weighted Data/TigroneyData.Rda")

# Gurtnapisha* -------------------------------------------------------------

GurtnapishaData <- weightedTemp(getWeatherStation("Gurtnapisha")$fethardGurtnapisha, 9.63)
save(GurtnapishaData,file="Data/Weighted Data/GurtnapishaData.Rda")

# Rickardstown ------------------------------------------------------------

RickardstownData <- weightedTemp(getWeatherStation("Rickardstown"),
                                 getDistance(summaryData, "Rickardstown",
                                             getWeatherStation("Rickardstown")))
save(RickardstownData,file="Data/Weighted Data/RickardstownData.Rda")

# Longfordpass ------------------------------------------------------------

LongfordpassData <- weightedTemp(getWeatherStation("Longfordpass"),
                                 getDistance(summaryData, "Longfordpass",
                                             getWeatherStation("Longfordpass")))
save(LongfordpassData,file="Data/Weighted Data/LongfordpassData.Rda")

# Cashelduff* --------------------------------------------------------------

CashelduffData <- weightedTemp(getWeatherStation("Cashelduff")$knockAirportCashelduff,6.97)
save(CashelduffData,file="Data/Weighted Data/CashelduffData.Rda")

# Woodford ----------------------------------------------------------------

WoodfordData <- weightedTemp(getWeatherStation("Woodford"),
                             getDistance(summaryData, "Woodford",
                                         getWeatherStation("Woodford")))
save(WoodfordData,file="Data/Weighted Data/WoodfordData.Rda")


# Ballybrittas ------------------------------------------------------------

BallybrittasData <- weightedTemp(getWeatherStation("Ballybrittas"),
                                 getDistance(summaryData, "Ballybrittas",
                                             getWeatherStation("Ballybrittas")))
save(BallybrittasData,file="Data/Weighted Data/BallybrittasData.Rda")


# Corracloon --------------------------------------------------------------

CorracloonData <- weightedTemp(getWeatherStation("Corracloon"),
                               getDistance(summaryData, "Corracloon",
                                           getWeatherStation("Corracloon")))
save(CorracloonData,file="Data/Weighted Data/CorracloonData.Rda")


# Corrakyle ---------------------------------------------------------------

CorrakyleData <- weightedTemp(getWeatherStation("Corrakyle"),
                              getDistance(summaryData, "Corrakyle",
                                          getWeatherStation("Corrakyle")))
save(CorrakyleData,file="Data/Weighted Data/CorrakyleData.Rda")












