load("Data/Climate Data/eobs025Mean.Rda")
load("Data/Climate Data/HortlandCD.Rda")
load("Data/Climate Data/Glendine1CD.Rda")
load("Data/Climate Data/KilduffCD.Rda")
load("Data/Climate Data/RossnagadCD.Rda")

load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/hortlandData.Rda")
load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/glendine1Data.Rda")
load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/kilduffData.Rda")
load("/Users/alessalemos/Documents/GitHub/pineR/Data/Weighted Data/rossnagadData.Rda")

hortlandData <- RossnagadData  %>% ungroup() %>% mutate(dayYear = 1:length(temp))

# hCD <- eobs025 %>% filter((Long == -6.875)&(Lat==53.375)) %>%
#   filter(date>="2010.01.01" & date<="2012.12.31") %>%
#   mutate(dayYear = 1:length(temp))
l1 %>% filter((x == -6.85)&(y==53.38333))
sort(unique(l1$x))
sort(unique(l1$y))
latLongSites[25,]

l2[26155,]

data %>%
  tibble::rownames_to_column() %>%
  filter(str_detect(x, "-7.6") & str_detect(y, "52.4"))

HortlandCD
Glendine1CD
KilduffCD
RossnagadCD

hortlandData %>%
  ggplot() + geom_line(aes(x = dayYear, y = temp, color =  "Weighted")) +
  geom_line(data = RossnagadCD, aes(x = dayYear, y = temp, color = "Eobs (0.01)")) +
  scale_color_manual(values = c("Weighted" = "steelblue",
                                "Eobs (0.01)" = "firebrick")) +
  labs(x = "Day of the Year", y = "Temperature", color = "Data") +
  theme_bw(base_size = 14)
