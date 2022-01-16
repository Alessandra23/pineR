##
#' @title Dowload data
#' @description Download data from google drive or other site.
#'
#' @import googledrive
#' @import RCurl
#'
#' @return Returns a file saved in the designated folder.
#' @param name A name for your file
#' @param link The data link.
#' @param path Location on the computer where the data will be saved.
#' @param type Type of file. Default is .xlsx.
#' @export
downloadData <- function(name, link, path, type = "xlsx", gdrive = TRUE) {
  localName <- paste0(path, name,".", type)
  if(gdrive){
    drive_download(
      as_id(link),
      path = localName,
      overwrite = TRUE,
      type = type)
  }else{
    download.file(link, destfile = localName, method = "curl")
  }
}

#'
#' @title Convert to data frame
#'
#' @description Convert a excel file (or other extension) in a data frame.
#'
#' @param path File location.
#' @return A data frame object.
#' @export
dataFrameData <- function(path){
  readFile <- read_excel(path, range = cell_cols("A:D"))
  dataDf <- as.data.frame(readFile)
  return(dataDf)
}

#'
#' Read Emergence
#' @description  Create a data frame with two columns: day of emergence and cumulative percentage
#' @import readxl
#'@param data Data frame of emergence
#'@param colIndex Columns of the days and the cumulative percentage
#'@param percentual If it is percentual or not
#'
#'@export
readEmergence <- function(data, colIndex = c(4,6), percentual = TRUE) {
  if(percentual){
    emergence <- data.frame(
      day = data[,colIndex[1]],
      prob = data[,colIndex[2]]/100
    )
  }else{
    emergence <- data.frame(
      day = data[,colIndex[1]],
      prob = data[,colIndex[2]]
    )
  }
  return(emergence)
}



#'
#' @title Organize weather data
#'
#' @description Prepares the data in the format to be used in the function.
#' @import tidyverse
#' @param data Weather data data frame
#' @export
weatherData <- function(data){
  weatherTemp <- data %>%
    mutate(
      temp = purrr::map2_dbl(min_temp, max_temp, ~ mean(c(.x, .y))),
      trueYear = lubridate::year(date),
      month = lubridate::month(date),
      dayMonth = lubridate::day(date)
    )
  # year as factor and get levels
  levelsYear <- levels(as.factor(weatherTemp$trueYear))
  # recode levels of years and add name of the station
  weatherTemp <- weatherTemp %>%
    mutate(year = fct_recode(as.factor(trueYear),
                             `1` = levelsYear[1],
                             `2` = levelsYear[2],
                             `3` = levelsYear[3]
    )) %>%
    group_by(trueYear) %>%
    mutate(
      day = seq(trueYear)
    )
  return(weatherTemp)
}


#'
#' @title  Weight the data
#'
#' @description  Inputting weighted averages
#'
#' @param data list of data frames
#' @param  distances  vector of distances associated with each element of the data frame list.
#' @return A data frame with the date, minimum, maximum and average temperatures.
#' @export
weightedTemp <- function(data, distances) {

  n <- length(distances)
  td <- sum(distances)
  tw <- n * td - td
  sites_weights <- lapply(1:n, function(i) (td - distances[i]) / tw) %>% unlist()

  max_temp <- lapply(1:n, function(i) data[[i]]$max_temp * sites_weights[i])
  max_temp <- Reduce(`+`, max_temp)

  min_temp <- lapply(1:n, function(i) data[[i]]$min_temp * sites_weights[i])
  min_temp <- Reduce(`+`, min_temp)
  weightedTemp <- data.frame(min_temp, max_temp)
  meanTemp <- apply(weightedTemp, 1, mean)

  temperatureData <- data.frame(day = data[[1]]$day, month = data[[1]]$month, year = as.numeric(data[[1]]$year),
                                min_temp = weightedTemp$min_temp, max_temp = weightedTemp$max_temp,
                                temp = meanTemp)

  return(temperatureData)
}

