# Read limate data
library(readr)
library(tidyverse)

meanTemp <- read_csv("Data/Climate Data/mean.csv")
#View(meanTemp)
meanTemp %>% head()

hortlandCoordinates <- c(53.38224, -6.85567)
meanTemp %>% filter(Long == hortlandCoordinates[2] & Lat == hortlandCoordinates[1])
