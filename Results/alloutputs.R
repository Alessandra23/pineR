## Script for a presentation

# Description of the data

summaryData <- read_excel("Data/summaryData.xlsx")
summaryData <- summaryData %>% select(c(1:9, 11:12) ) %>%
  dplyr::rename("Site ID" = "Site",
                "Site" = "Trial site emergence",
                "Slope" = "Slope %") %>%
  filter(!(Site == "Ballybrittas" & `Weather Station` == "Durrow"))
tableSummary <- summaryData %>% gt::gt()



summaryData %>% gt() %>%
  tab_style(
    style = list(
      cell_fill(color = met.brewer("Hiroshige", 1))
    ),
    locations = cells_body(
      #columns = vars(V1, V2), # not needed if coloring all columns
      rows = Distance <= 10)
  )


# Select variables --------------------------------------------------------

# Select weather stations that the distance is smaller than 10 km (use them alone)
summaryData %>% filter(Distance <= 10) %>% gt()

# some plots

p1 <- summaryData %>% ggplot(aes(Altitude)) +
  geom_histogram(bins = 6, color = "black", fill=met.brewer("Hiroshige", 1)) +
  theme_bw() + labs(y = "Frequency")
#summaryData %>% ggplot(aes(y = Altitude)) + geom_boxplot() + theme_bw()


p2 <- summaryData %>% group_by(Species) %>%
  dplyr::summarise(Frequency = n()) %>% ggplot(aes(x=Species, y = Frequency)) +
  geom_bar(stat="identity", fill = met.brewer("Hiroshige", 1), color = "black") + theme_bw()


p3 <- summaryData %>% group_by(Soil) %>%
  dplyr::summarise(Frequency = n()) %>% ggplot(aes(x=Soil, y = Frequency)) +
  geom_bar(stat="identity", fill = met.brewer("Hiroshige", 1), colour = "black") + theme_bw()


p4 <- summaryData %>% group_by(Aspect) %>%
  dplyr::summarise(Frequency = n()) %>% ggplot(aes(y=reorder(Aspect, - Frequency), x = Frequency)) +
  geom_bar(stat="identity", fill = met.brewer("Hiroshige", 1), colour = "black") + theme_bw()+
  ylab("Aspect")

p5 <- summaryData %>% mutate_at(11, as.numeric) %>% na.omit(Slope) %>%
  ggplot(aes(Slope)) +
  geom_histogram(bins = 6, color = "black", fill=met.brewer("Hiroshige", 1)) +
  theme_bw() + labs(y = "Frequency")


p6 <- summaryData %>% mutate_at(11, as.numeric) %>% na.omit(Slope) %>% group_by(Site) %>%
  ggplot(aes(x = Slope, y = reorder(Site, - Slope))) +
  geom_point(colour = met.brewer("Hiroshige", 1), fill = "black", size = 3) +
  labs(x = "Slope (%)", y = "Site") + theme_bw()


#gridExtra::grid.arrange(p1,p2,p3,p4, p6)

gridExtra::grid.arrange(p1,p2,p3, nrow = 1)
gridExtra::grid.arrange(p4, p6, nrow = 1)


# Weather data
which(is.na(DeerparkData[, "temp"]))


hortlandW <- piner(npop = 100, ngen = 1, ntimes = 100, data = hortlandData,  species = 1, depth = 1)
ballinageeDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = ballinageeData,  species = 2, depth = 1)
OakwoodDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = OakwoodData,  species = 2, depth = 1)

Glendine1Data[762,"max_temp"] <- Glendine1Data[761,"max_temp"]
Glendine1Data[762,"temp"] <- Glendine1Data[761,"temp"]
Glendine1DataW<- piner(npop = 100, ngen = 1, ntimes = 100, data = Glendine1Data,  species = 1, depth = 1)

Glendine2DataW<- piner(npop = 100, ngen = 1, ntimes = 100, data = Glendine2Data,  species = 1, depth = 1)

Lackenrea1Data[212,"max_temp"] <- Lackenrea1Data[211,"max_temp"]
Lackenrea1Data[212,"temp"] <- Lackenrea1Data[211,"temp"]
Lackenrea1DataW<- piner(npop = 100, ngen = 1, ntimes = 100, data = Lackenrea1Data,  species = 1, depth = 1)

Lackenrea2Data[212,"max_temp"] <- Lackenrea2Data[211,"max_temp"]
Lackenrea2Data[212,"temp"] <- Lackenrea2Data[211,"temp"]
Lackenrea2DataW<- piner(npop = 100, ngen = 1, ntimes = 100, data = Lackenrea2Data,  species = 2, depth = 1)

SummerhillData$year <- as.numeric(SummerhillData$year)
SummerhillDataW<- piner(npop = 100, ngen = 1, ntimes = 100, data = SummerhillData,  species = 1, depth = 1)

DeerparkData[273,"max_temp"] <- DeerparkData[272,"max_temp"]
DeerparkData[273,"temp"] <- DeerparkData[272,"temp"]
DeerparkDataW <- piner(npop = 1, ngen = 1, ntimes = 1, data = DeerparkData,  species = 2, depth = 2)


BallymacshaneboyData$year <- as.numeric(BallymacshaneboyData$year)
BallymacshaneboyDataW <- piner(npop = 1, ngen = 1, ntimes = 4, data = BallymacshaneboyData,  species = 1, depth = 1)

KilduffData$year <- as.numeric(KilduffData$year)
KilduffDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = KilduffData,  species = 1, depth = 1)

RossnagadDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = RossnagadData,  species = 1, depth = 1)

Ballyroan1Data[1012,"max_temp"] <- Glendine1Data[1011,"max_temp"]
Ballyroan1Data[1012,"temp"] <- Glendine1Data[1011,"temp"]
Ballyroan1DataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = Ballyroan1Data,  species = 1, depth = 1)

Ballyroan2Data[647,"max_temp"] <- Glendine1Data[646,"max_temp"]
Ballyroan2Data[647,"temp"] <- Glendine1Data[646,"temp"]
Ballyroan2DataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = Ballyroan2Data,  species = 1, depth = 1)

DonadeaDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = DonadeaData,  species = 1, depth = 1)

CloondaraData$year <- as.numeric(CloondaraData$year)
CloondaraDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = CloondaraData,  species = 1, depth = 1)

KnockavilleDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = KnockavilleData,  species = 1, depth = 1)

KilurneyDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = KilurneyData,  species = 1, depth = 1)

DoonDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = DoonData,  species = 1, depth = 1)

ClonoghilData$year <- as.numeric(ClonoghilData$year)
ClonoghilDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = ClonoghilData,  species = 1, depth = 1)

TigroneyData$year <- as.numeric(TigroneyData$year)
TigroneyDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = TigroneyData,  species = 1, depth = 1)

GurtnapishaData$year <- as.numeric(GurtnapishaData$year)
GurtnapishaDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = GurtnapishaData,  species = 1, depth = 1)

RickardstownDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = RickardstownData,  species = 1, depth = 1)

#LongfordpassDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = LongfordpassData)
#which(is.na(LongfordpassData[, "temp"]))

CashelduffData$year <- as.numeric(CashelduffData$year)
CashelduffDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = CashelduffData,  species = 1, depth = 1)

WoodfordDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = WoodfordData,  species = 1, depth = 1)
BallybrittasDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = BallybrittasData,  species = 1, depth = 1)
CorracloonDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = CorracloonData,  species = 1, depth = 1)
CorrakyleDataW <- piner(npop = 100, ngen = 1, ntimes = 100, data = CorrakyleData,  species = 1, depth = 1)






