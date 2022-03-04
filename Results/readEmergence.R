listFiles <- list.files("Data/Sites", pattern = "*.xlsx", full.names = TRUE)
readFiles <- lapply(listFiles, dataFrameData, range = "A:I")
emergence <- lapply(readFiles, readEmergence)
names(emergence) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(listFiles))


names(emergence) <- sub("_emergence", "", names(emergence))
names(emergence) <- paste(toupper(substr(names(emergence), 1, 1)), substr(names(emergence), 2, nchar(names(emergence))), sep="")
emergence

library (plyr)
library(ggplot2)
library(tidyverse)
library(MetBrewer)
dfEmergence <- ldply(emergence)


# Plots of the cumulative emergence
#option 1
dfEmergence %>% ggplot(aes(x = day, y = prob, colour = .id)) + geom_line() +
  geom_point() + theme_bw(base_size = 14) +
  labs(x = "Days", y = "Cumulative probability", colour = "Site")
ggsave("Plots/emergCumOp1.pdf")

#option 2
dfEmergence %>% ggplot(aes(x = day, y = prob)) + geom_line(colour = met.brewer("Hiroshige", 1)) +
  geom_point(colour = met.brewer("Hiroshige", 1)) +
  labs(x = "Days", y = "Cumulative probability") +
  facet_wrap(~.id) +
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="white"))
ggsave("Plots/emergCumOp2.pdf")

