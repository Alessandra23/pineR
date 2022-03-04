library(tidyverse)
library(raster)
library(sf)
library(sp)
library(magrittr)

# convert the nc file into a tibble, saving it as a rda file, and get the mean from min and max temp.
readeobs <- function(path){
  eobs <- raster::brick(path)  # read in netcdf file
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

  eobs_tibble <- eobs_data %>%
    tidyr::pivot_longer(c(-x, -y), names_to = "date", values_to = "temp")

  return(eobs_tibble)
}

mineobs <- readeobs("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/min001_2014.nc")
maxeobs <- readeobs("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/max001_2014")

mean001 <- cbind(mineobs, maxeobs$temp) %>%
  dplyr::rename("min_temp" = "temp",
                "max_temp" = "maxeobs$temp")

mean001$temp <- rowMeans(mean001[,c("min_temp", "max_temp")])

mean001_2014 <- mean001
save(mean001_2014, file = "Data/Climate Data/mean001_2014.Rda")



##

load("Data/xyGrid.Rda")

xy <- xyGrid[xyGrid$sites == "Hortland", c("x", "y")]

load("Data/Climate Data/mean001_2010.Rda")
load("Data/Climate Data/mean001_2011.Rda")
load("Data/Climate Data/mean001_2012.Rda")

l1 <- mean001_2010
l2 <- mean001_2011
l3 <- mean001_2012

# -7.601761  52.4137
l1 %>% filter(str_detect(x, "-7.60") & str_detect(y, "52.41"))
str(l1$x)

m1 <- l1[(l1$x == xy$x)& (l1$y == xy$y), ]
m2 <- l2[(l2$x == xy$x)& (l2$y == xy$y), ]
m3 <- l3[(l3$x == xy$x)& (l3$y == xy$y), ]

site <- rbind(m1, m2, m3) %>% mutate(dayYear = 1:length(temp))


data <- l1 %>% distinct(x, y)
coordinates(data) <- ~x+y
# Setting default projection
proj4string(data) <- CRS('+init=epsg:4326')
xy_utm <- spTransform(data, CRS('+init=epsg:29902'))
xy_utm <- xy_utm %>% as.data.frame()


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
  p1 <- max(which(df$x <= points[1]))
  p2 <- max(which(df$y >= points[2]))
  df[(df$x==p1),]
  df[(df$y==p2),]
  pointsSelec <- df[(df$x==p1) & (df$y==p2),]
  return(pointsSelec)
}

my_data <- xy_utm

findPoints(df = my_data, points = c(276187, 237482))


values <- apply(myPoint, 1, function(x) findPoints(my_data, c(x[2], x[3])))
values <- do.call(rbind.data.frame, values)
xyGrid <- cbind(myPoint, values)
