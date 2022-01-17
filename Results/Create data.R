# Create data sets
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(MetBrewer)

# downloadData(name = "summaryData",
#              link = "https://docs.google.com/spreadsheets/d/1Xekodvbzysn-5iy82gzK_OlbkrfyN3aooBk9YsWoXq0/edit#gid=0",
#              path = "Data/",
#              type = "xlsx", gdrive = TRUE)



summaryData <- read_excel("Data/summaryData.xlsx")
summaryData <- summaryData %>% select(c(1:9, 11) ) %>%
  rename("SiteNo" = "Site",
         "Site" = "Trial site emergence") %>%
  filter(!(Site == "Ballybrittas" & `Weather Station` == "Durrow"))





# Select variables --------------------------------------------------------

# Select weather stations that the distance is smaller than 10 km (use them alone)
summaryData %>% filter(Distance <= 10)



# Plots

p1 <- summaryData %>% ggplot(aes(Altitude)) + geom_histogram(bins = 6, color = "black", fill="#E69F00") +
  theme_bw() + labs(y = "Frequency")
#summaryData %>% ggplot(aes(y = Altitude)) + geom_boxplot() + theme_bw()


p2 <- summaryData %>% group_by(Species) %>%
  summarise(Frequency = n()) %>% ggplot(aes(x=Species, y = Frequency)) +
  geom_bar(stat="identity", fill = "#E69F00", color = "black") + theme_bw()


p3 <- summaryData %>% group_by(Soil) %>%
  summarise(Frequency = n()) %>% ggplot(aes(x=Soil, y = Frequency)) +
  geom_bar(stat="identity", fill = "#E69F00", colour = "black") + theme_bw()


p4 <- summaryData %>% group_by(Aspect) %>%
  summarise(Frequency = n()) %>% ggplot(aes(y=Aspect, x = Frequency)) +
  geom_bar(stat="identity", fill = "#E69F00", colour = "black") + theme_bw()



gridExtra::grid.arrange(p1,p2,p3,p4)


# Frequency tables




# Create fake data frame --------------------------------------------------



data <- tibble::tibble(mintemp = round(runif(3*365 + 1, -1, 10), 2),
                       maxtemp = mintemp +  round(runif(3*365 + 1, 1, 10), 2),
                       date = seq(ymd('2002-01-01'), ymd('2004-12-31'), by = '1 day'))

data <- data %>% mutate(
  #temp = purrr::map2_dbl(mintemp, maxtemp, ~ mean(c(.x, .y)))
  year = lubridate::year(date),
  month = lubridate::month(date),
  day = lubridate::day(date)
) %>%
  mutate(year = recode(year,
                           `2002` =  1,
                           `2003` =  2,
                           `2004` =  3))


