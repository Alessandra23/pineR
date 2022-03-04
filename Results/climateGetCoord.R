library(tidyverse)
library(raster)
library(sf)
library(sp)
library(magrittr)

eobs <- raster::brick("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/mean001_2014")  # read in netcdf file
my_data <- raster::coordinates(eobs) %>% as_tibble()

# save each day as a new column
eobs_data <- eobs %>%
  values() %>%
  as_tibble()

# adding lat and long information
eobs_data$x <- my_data$x
eobs_data$y <- my_data$y

# removing mysterious "X" from all dates
names(eobs_data) <- names(eobs_data) %>% str_remove_all("X")
eobs_data %<>% drop_na() # remove nans

eobs001 <- eobs_data %>%
  tidyr::pivot_longer(c(-x, -y), names_to = "date", values_to = "temp")



myPoint <- tibble(
  sites = c(
    "Hortland", "Ballinagee","Oakwood", "Glendine", "Lackenrea 1", "Lackenrea 2", "Summerhill","Deerpark",
    "Ballymacshaneboy","Kilduff","Rossnagad", "Ballyroan 1", "Ballyroan 2","Ballybrittas","Emo","Donadea",
    "Glendalough", "Glendine trial 1", "Glendine trial 2", "Knockeen","The Rodneys","Killnaconnigan",
    "Cloondara","Knockaville","Killurney","Doon", "Clonoghil","Tigroney", "Gurtnapisha","Rickardstown",
    "Longfordpass", "Cashelduff","Woodford", "Corracloon","Corrakyle", "Annalecka"
  ),
  Easting = c(
    276187,320402,303812, 224303, 213024,213024,283758,326609,160106,244952,243350,248305,248305, 255415,
    253231, 283467,313063,224303, 224303,257434,136307,266278, 207204,250306, 227148,211437,225136, 321000,
    230028,253640,224426, 152830,170183,159162,162844, 305109
  ),
  Northing = c(
    237482, 214375,201215,199898, 101205,101205,246294,211019, 118416,233062,203335,199495,199495,236791,
    205382,232596,195655,199898,199898,106876,111288,255198,275580,248333,129158,231856,192099,182996,
    132387,263408,160680,298829,197082,191153,190777, 201042
  )
)


findPoints <- function(df, points){
  p1 <- max(df[df$x <= points[1],]$x)
  p2 <- max(df[df$y <= points[2],]$y)
  pointsSelec <- df[(df$x==p1) & (df$y==p2),]
  return(pointsSelec)
}

findPoints(my_data, c(276187, 237482))


values <- apply(myPoint, 1, function(x) findPoints(my_data, c(x[2], x[3])))
values <- do.call(rbind.data.frame, values)
xyGrid <- cbind(myPoint, values)

save(xyGrid,file="Data/xyGrid.Rda")

# todos os 1999

Ballyroan11 <- eobs001[(eobs001$x == xyGrid$x[7])& (eobs001$y == xyGrid$y[7]), ] %>%
  mutate(dayYear = 1:length(temp))


# todos os 2011

tempS1 <- eobs001[(eobs001$x == xyGrid$x[7])& (eobs001$y == xyGrid$y[7]), ] %>%
  mutate(dayYear = 1:length(temp))


# todos os 2012



# todos os 2013
temoClondara1 <- eobs001[(eobs001$x == xyGrid$x[23])& (eobs001$y == xyGrid$y[23]), ] %>%
  mutate(dayYear = 1:length(temp))

tempKnockaville1 <- eobs001[(eobs001$x == xyGrid$x[24])& (eobs001$y == xyGrid$y[24]), ] %>%
  mutate(dayYear = 1:length(temp))

tempKilurney1 <- eobs001[(eobs001$x == xyGrid$x[25])& (eobs001$y == xyGrid$y[25]), ] %>%
  mutate(dayYear = 1:length(temp))


# todos os 2014

temoClondara2 <- eobs001[(eobs001$x == xyGrid$x[23])& (eobs001$y == xyGrid$y[23]), ] %>%
  mutate(dayYear = 1:length(temp))

tempKnockaville2 <- eobs001[(eobs001$x == xyGrid$x[24])& (eobs001$y == xyGrid$y[24]), ] %>%
  mutate(dayYear = 1:length(temp))

tempKilurney2 <- eobs001[(eobs001$x == xyGrid$x[25])& (eobs001$y == xyGrid$y[25]), ] %>%
  mutate(dayYear = 1:length(temp))


# save data

Glendine1CD <- rbind(tempGlendine13, tempGlendine12, tempGlendine11) %>% mutate(dayYear = 1:length(temp))

save(Glendine1CD,file="Data/Climate Data/Glendine1CD.Rda")


## comp temp

load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/glendine1Data.Rda")


hortlandData <- Glendine1Data %>% mutate(dayYear = 1:length(temp))

hortlandData %>%
  ggplot() + geom_line(aes(x = dayYear, y = temp, color =  "Weighted")) +
  geom_line(data = Glendine1CD, aes(x = dayYear, y = temp, color = "Eobs")) +
  scale_color_manual(values = c("Weighted" = "steelblue",
                     "Eobs" = "firebrick")) +
  labs(x = "Day of the Year", y = "Temperature", color = "Data") +
  theme_bw(base_size = 14)


