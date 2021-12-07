####### process EOBS data and save as csv in "processed_data"
library(raster) # package for netcdf manipulation
library(tidyverse)
library(magrittr)

#setwd("~Data/Climate Data") # set working direcory

# ---- read in and crop E-OBS data
eobs <- raster::brick("mean.nc") # read in netcdf file

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


eobs_data %>%
  tidyr::pivot_longer(c(-Long, -Lat), names_to = "date", values_to = "temp") %>% # Making each row an observation
  mutate(date = date %>% lubridate::ymd()) %>% # format data nicely
  dplyr::mutate(id = group_indices(., Long, Lat)) %>%
  group_by(id) %>%
  dplyr::arrange(.by_group = T) %>%
  ungroup() %>%
  write_csv("mean.csv") # save data
