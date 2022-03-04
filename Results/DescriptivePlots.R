# Descriptive plots
library(ggthemes)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(MetBrewer)
library(googledrive)
library(gt)

# Read weighted data
listWeightedData <- list.files("Data/Weighted Data", pattern = "*.Rda", full.names = TRUE)
readWeightedData <- lapply(listWeightedData, loadRData)
names(readWeightedData) <- gsub('Data.Rda','',basename(listWeightedData))
readWeightedData

WeightedData <-  reshape2::melt(readWeightedData, id = c("day", "month", "year", "min_temp", "max_temp", "temp"))
WeightedData <- WeightedData %>%  transform(year = as.factor(year),
                                            month = as.factor(month))

# Read summary table

# downloadData(name = "summaryData",
#              link = "https://docs.google.com/spreadsheets/d/1Xekodvbzysn-5iy82gzK_OlbkrfyN3aooBk9YsWoXq0/edit#gid=0",
#              path = "Data/",
#              type = "xlsx", gdrive = TRUE)



summaryData <- read_excel("Data/summaryData.xlsx")
summaryData <- summaryData %>% dplyr::select(c(1:16) ) %>%
  dplyr::rename("Site ID" = "Site",
                "Site" = "Trial site emergence",
                "Slope" = "Slope %",
                "Soil Type" = "Soil") %>%
  dplyr::filter(!(Site == "Ballybrittas" & `Weather Station` == "Durrow"))

summaryTable <- summaryData %>% gt() %>%
  tab_style(
    style = list(
      cell_fill(color = 'steelblue')
    ),
    locations = cells_body(
      #columns = vars(V1, V2), # not needed if coloring all columns
      rows = Distance <= 10)
  )

# Select weather stations that the distance is smaller than 10 km (use them alone)
coClimate <- summaryData %>% filter(Distance <= 10)
coClimate %>% gt()
coClimate$Site

# Box plot

WeightedData %>%
  ggplot(aes(y=L1, x=temp)) +
  labs(x = "Weighted mean temperature", y = "Site")+
  geom_boxplot(fill = 'steelblue' , color = "black") +
  xlim(-10, 30) +
  theme_bw(base_size = 14)
  # theme_few() +
  # scale_fill_few()


WeightedDataTidy <- WeightedData %>%
  group_by(L1) %>%
  mutate(dayYear = 1:length(temp))

WeightedDataTidy %>%
  ggplot(aes(x = dayYear, y = temp)) +
  #geom_smooth()+
  geom_line(color = "gray40", alpha = 0.8, size = 0.2) +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white")) +
  # theme_few() +
  # scale_fill_few() +
  scale_colour_brewer(palette="Set1") +
  labs(x = "Day of the year", y = expression(paste("Temperature (",degree,"C)"))) +
  geom_rect(data = subset(WeightedDataTidy, L1 %in% coClimate$Site), fill = NA,
           colour = "red", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, size = 1.2) +
  facet_wrap(~L1)

# Plots for each covariate

# Histograms

AltitudeHist <- summaryData %>% ggplot(aes(Altitude)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) + labs(y = "Frequency")
  # theme_few() +
  # scale_fill_few()

DistanceHist <- summaryData %>% ggplot(aes(Distance)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) + labs(y = "Frequency") +
  xlab("Distance (km)")
# theme_few() +
# scale_fill_few()

gridExtra::grid.arrange(AltitudeHist, DistanceHist, nrow = 1)
ggsave("Plots/histAltDist.pdf")


SlopeHist <- summaryData %>%  na.omit(Slope) %>%
  ggplot(aes(Slope)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) +
  # theme_few() +
  # scale_fill_few() +
  labs(y = "Frequency")

SlopeAngleHist <- summaryData %>%
  ggplot(aes(`Slope Angle`)) +
  geom_histogram(bins = 6, color = "black", fill='steelblue') +
  theme_bw(base_size = 14) +
  # theme_few() +
  # scale_fill_few() +
  labs(y = "Frequency", x = expression(paste("Slope (",degree, ")")))


# Covariate vs Site

SlopeAngleSite <- summaryData %>%  group_by(Site) %>%
  ggplot(aes(x = `Slope Angle`, y = reorder(Site,  `Slope Angle`))) +
  geom_point(colour = 'steelblue', fill = "black", size = 2) +
  geom_segment(aes(x = 0, xend=`Slope Angle`, y=Site, yend=Site), color='steelblue') +
  labs(x = expression(paste("Slope (",degree, ")")), y = "Site") +
  theme_bw(base_size = 14)
  # theme_few() +
  # scale_fill_few()


SlopePercSite <- summaryData %>%  group_by(Site) %>%
  ggplot(aes(x = Slope, y = reorder(Site,  Slope))) +
  geom_point(colour = 'steelblue', fill = "black", size = 2) +
  geom_segment(aes(x = 0, xend=Slope, y=Site, yend=Site), color='steelblue') +
  labs(x = "Slope (%)", y = "Site") +
  theme_bw(base_size = 14)

gridExtra::grid.arrange(SlopeAngleSite, SlopePercSite, nrow = 1)
ggsave("Plots/SlopeSite.pdf")


## Aspect

AspectFreq <- summaryData %>% group_by(Aspect) %>%
  dplyr::summarise(Frequency = n()) %>% ggplot(aes(y=reorder(Aspect, - Frequency), x = Frequency)) +
  geom_bar(stat="identity", fill = 'steelblue', colour = "black") +
  theme_bw(base_size = 14)+
  ylab("Aspect")
ggsave("Plots/AspectFreq.pdf")


# SlopeTypeSite <- summaryData %>%
#   group_by(Site) %>%
#   filter(`Slope Type`!="N/A") %>%
#   ggplot(aes(x = `Slope Type`, y = reorder(Site,  `Slope Type`))) +
#   geom_point(colour = 'steelblue', fill = "black", size = 3) +
#   #geom_segment(aes(x = 0, xend=`Slope Type`, y=Site, yend=Site), color='steelblue') +
#   labs(x = "Slope Type", y = "Site") +
#   theme_bw(base_size = 14)
#   # theme_few() +
#   # scale_fill_few()

# SpeciesSite <- summaryData %>%  group_by(Site) %>%
#   ggplot(aes(x = Species, y = reorder(Site,  Species))) +
#   geom_point(colour = 'steelblue', fill = "black", size = 3) +
#   #geom_segment(aes(x = 0, xend= Species, y=Site, yend=Site), color='steelblue') +
#   labs(x = "Species", y = "Site") +
#   theme_bw(base_size = 14)


# Plot all categorical variables

summaryDataSelect <- summaryData %>%
  select(Site, Species, `Soil Type`, Conditions, Roughness)  %>%
   mutate_if(is.character,as.factor)
summaryDataSelect <-  summaryDataSelect %>% as.data.frame()
df <- reshape::melt(summaryDataSelect, id = "Site")

CategSite <- ggplot(df, aes(value, Site)) +
  geom_point(size = 2, col = 'steelblue') +
  facet_grid(~ variable, scale="free", space="free_x") +
  xlab(" ") +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white"))
# theme_few() +
# scale_fill_few()

CategSite
ggsave("Plots/CategSite.pdf")

