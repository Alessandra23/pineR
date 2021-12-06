listWeatherData <- list.files("Data/Weather Stations", pattern = "*.xlsx", full.names = TRUE)
readWeatherData <- lapply(listWeatherData, dataFrameData)
WeatherDataFiles <- lapply(readWeatherData, weatherData)
names(WeatherDataFiles) <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(listWeatherData))
WeatherDataFiles

data <- weightedTemp(WeatherDataFiles, distances = c(2,4,6,8))

