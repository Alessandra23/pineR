#Climate data big file

####### process EOBS data and save as csv in "processed_data"
library(raster) # package for netcdf manipulation
library(tidyverse)
library(magrittr)

#setwd("~Data/Climate Data") # set working direcory

# ---- read in and crop E-OBS data
eobs <- raster::brick("~/Desktop/pineRold2/Data/Climate Data/meanLarge.nc") # read in netcdf file

# we only want irish data
ROI <- extent(-11, -5.5, 51.2, 55.5)
eobs <- crop(eobs, ROI) # takes a minute

# create tibble and store lat/long
my_data <- raster::coordinates(eobs) %>% as_tibble()
names(my_data) <- c("Long", "Lat")

# save each day as a new column
eobs_data <- eobs %>%
  values() %>%
  as_tibble()

# adding lat and long information
eobs_data$Long <- my_data$Long
eobs_data$Lat <- my_data$Lat

# removing mysterious "X" from all dates
names(eobs_data) <- names(eobs_data) %>% str_remove_all("X")
eobs_data %<>% drop_na() # remove nans


txx <- eobs_data %>%
  tidyr::pivot_longer(c(-Long, -Lat), names_to = "date", values_to = "temp") %>% # Making each row an observation
  dplyr::mutate(date = date %>% lubridate::ymd()) %>% # format data nicely
  dplyr::mutate(id = group_indices(., Long, Lat)) %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(.by_group = T) %>%
  ungroup()

txx %>%
  write_csv("~/Desktop/pineRold2/Data/Climate Data/meanLarge.csv") # save data




# Read climate data
#devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")
library(ggplot2)
library(readr)
library(sf)
library(colorspace)

tx = read_csv("~/Desktop/pineRold2/Data/Climate Data/meanLarge.csv")
tx

# sf of Ireland, used for plots of ... Ireland
ireland_sf <- ne_countries(type = "map_units",
                           scale='large',
                           returnclass = 'sf',
                           continent = "europe") %>%
  .[.$name %in% c('Ireland', 'N. Ireland'),] # dont forget northern ireland


tx_sites = txx %>%
  dplyr::select(Long, Lat, id)%>%
  unique()

df <- tibble(
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


#> Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.3
lat_long <- df %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 29900) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(sites = df$sites) %>%
  rename(c(Latitude = X, Longitude = Y))

lat_long
lat_long <- lat_long %>% dplyr::filter(sites %in% c("Hortland", "Ballinagee","Oakwood", "Glendine", "Lackenrea 1", "Lackenrea 2", "Summerhill","Deerpark",
                                                    "Ballymacshaneboy","Kilduff","Rossnagad", "Ballyroan 1", "Ballyroan 2","Ballybrittas","Donadea",
                                                    "Glendine trial 1", "Glendine trial 2", "Cloondara","Knockaville","Killurney","Doon", "Clonoghil","Tigroney", "Gurtnapisha","Rickardstown",
                                                    "Longfordpass", "Cashelduff","Woodford", "Corracloon","Corrakyle", "Annalecka"))


tx_sites %>%
  ggplot()+
  geom_sf(data=ireland_sf, alpha = 0)+
  geom_point(aes(Long, Lat))+
  geom_point(data = tibble(x = -6.8, y = 53.3), aes(x,y), col = 'red')+
  theme_minimal()+
  labs(title = "Hortland site in red", x = "Longitude", y = "Latitude")



pp <- tx_sites %>%
  ggplot()+
  geom_sf(data=ireland_sf, alpha = 0)+
  geom_point(aes(Long, Lat), size = 1.2, alpha = 0.5)+
  geom_point(data = tibble(x = lat_long$Latitude, y = lat_long$Longitude, site = lat_long$sites),
             aes(x,y,fill = site),  pch = 21, size = 1.2, color = "blue")+
  # scale_color_discrete_sequential(palette = "Red-Blue")+
  theme_bw()+
  labs(title = "EOBS coordinates x Sites coordinates", x = "Longitude", y = "Latitude")+
  geom_text(data = tibble(x = lat_long$Latitude, y = lat_long$Longitude, site = lat_long$sites),
            aes(x = x, y = y, label=site), hjust=0, vjust=0, size = 2) +
  theme(legend.position = "none")
pp

tx_sites %>%
  ggplot()+
  geom_sf(data=ireland_sf, alpha = 0)+
  geom_point(aes(Long, Lat))+
  geom_point(data = tibble(x = lat_long$Latitude, y = lat_long$Longitude, site = lat_long$sites),
             aes(x,y,color = site),   size = 2)+
  scale_shape_manual(values=seq(0,length(lat_long$sites)))+
  theme_bw()+
  labs(title = "EOBS coordinates x Sites coordinates", x = "Longitude", y = "Latitude")

