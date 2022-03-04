listWeightedData <- list.files("Data/Weighted Data", pattern = "*.Rda", full.names = TRUE)
readWeightedData <- lapply(listWeightedData, loadRData)
names(readWeightedData) <- gsub('Data.Rda','',basename(listWeightedData))
readWeightedData

WeightedData <-  reshape2::melt(readWeightedData, id = c("day", "month", "year", "min_temp", "max_temp", "temp"))
WeightedData <- WeightedData %>%  transform(year = as.factor(year),
                  month = as.factor(month))





# loadRData <- function(fileName){
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
