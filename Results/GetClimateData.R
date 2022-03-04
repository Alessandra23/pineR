####### process EOBS data and save as csv in "processed_data"
library(raster) # package for netcdf manipulation
library(tidyverse)
library(magrittr)
library(sf)

#setwd("~Data/Climate Data") # set working direcory

# ---- read in and crop E-OBS data
eobs <- raster::brick("/Users/alessalemos/Documents/GitHub/pineR/Data/Climate Data/mean001.nc") # read in netcdf file

# we only want irish data
# ROI <- extent(-11, -5.5, 51.2, 55.5)
# eobs <- crop(eobs, ROI) # takes a minute

# create tibble and store lat/long
my_data <- raster::coordinates(eobs) %>% as_tibble()
#names(my_data) <- c("Long", "Lat")
my_data <- my_data %>%
  st_as_sf(coords = c("x", "y"), crs = 29902) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(c(Longitude = X, Latitude = Y))


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

eobs001 <- eobs_data %>%
  tidyr::pivot_longer(c(-Long, -Lat), names_to = "date", values_to = "temp")

save(eobs001,file="Data/Climate Data/eobs001.Rda")


hortland <- txx %>% filter(Long ==-6.875 & Lat == 53.375) %>%
  filter(date>="2010.01.01" & date<="2012.12.31")

eobs_data %>%
  tidyr::pivot_longer(c(-Long, -Lat), names_to = "date", values_to = "temp") %>% # Making each row an observation
  mutate(date = date %>% lubridate::ymd()) %>% # format data nicely
  dplyr::mutate(id = group_indices(., Long, Lat)) %>%
  group_by(id) %>%
  dplyr::arrange(.by_group = T) %>%
  ungroup() %>%
  write_csv("mean.csv") # save data



