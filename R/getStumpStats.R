#' getStumpStats
#'
#' Calculate Stump Temperature Statistics.
#' This function calculates various statistics related to stump temperature,
#' such as mean temperatures, dispersal days, emergence days, and overwintering days.
#'
#' @param nd A single numeric value representing the number of days.
#' @param temp A numeric vector of length 'nd' containing daily temperature data.
#' @param maxtemp A numeric vector of length 'nd' containing daily maximum temperature data.
#' @param day A numeric vector of length 'nd' containing the day of the year for each day.
#' @param year A numeric vector of length 'nd' containing the year for each day .
#' @param firstdisp A single numeric value representing the mean first dispersal day, updated during each iteration.
#' @param lastdisp A single numeric value representing the mean last dispersal day, updated during each iteration.
#' @param firstemrg A single numeric value representing the mean first emergence day, updated during each iteration.
#' @param firstow A single numeric value representing the mean first overwintering day, updated during each iteration.
#' @param noow A single numeric value representing the number of years with no overwintering, updated during each iteration.
#' @param cold A numeric vector of length 'nd' containing cold-related information for each day.
#' @param params A list containing various model parameters, such as threshold temperatures and time limits.
#' @param cumcold A numeric vector of length 'nd' containing cumulative cold-related information for each day.
#' @param i A single numeric value representing the current iteration of the outer loop.
#'
#' @return A list containing updated values for various stump temperature statistics, including:
#' \itemize{
#' \item Site mean temperature.
#' \item Cumulative cold-related information.
#' \item Cold day count (ccd).
#' \item Cold week count (cwd).
#' \item Mean first and last dispersal days.
#' \item Mean first emergence day.
#' \item Year, day, and cumulative cold information for calculating overwintering days.
#' \item Temporary date variable for overwintering calculations.
#' \item Number of years with no overwintering.
#' \item Mean first overwintering day.
#' }
#'
#' @export


getStumpStats <- function(nd,
                          temp,
                          maxtemp,
                          day,
                          year,
                          firstdisp,
                          lastdisp,
                          firstemrg,
                          firstow,
                          noow,
                          cold,
                          params,
                          cumcold,
                          i){



  SiteMeanTemp <- mean(temp)
  lastdisp <- ((i - 1) * lastdisp + mean(as.numeric(tapply(day[maxtemp >=  params$th8], year[maxtemp >= params$th8], max)))) / i
  firstdisp <- ((i - 1) * firstdisp + mean(as.numeric(tapply(day[maxtemp >=  params$th8], year[maxtemp >=  params$th8], min)))) / i
  ccd <- 0
  cwd <- 0
  cumcold[1] <- 0
  for (j in 1:nd) {
    if (cold[j] == -1) {
      ccd <- ccd + 1
      cumcold[j] <- -ccd
    } else if (cold[j] == 1) {
      cwd <- cwd + 1
      ccd <- 0
      cumcold[j] <- cwd
    } else {
      ccd <- 0
      cwd <- 0
    }
  }
  firstemrg <- ((i - 1) * firstemrg + mean(as.numeric(tapply(day[cumcold >= params$t6Edays],
                                                             year[cumcold >= params$t6Edays], min)))) / i

  Jyear <- year[1:(nd - 366)]
  Jday <- day[1:(nd - 366)]
  Jcumcold <- cumcold[173:(nd - 194)]
  tmpdate <- as.numeric(tapply(Jday[Jcumcold <= (-params$t6Odays)],
                                Jyear[Jcumcold <= (-params$t6Odays)], min))
  noow <- noow + (29 - length(tmpdate))
  firstow <- ((i - 1) * firstow + mean(tmpdate)) / i

  return(list(
    SiteMeanTemp = SiteMeanTemp,
    cumcold = cumcold,
    ccd = ccd,
    cwd = cwd,
    lastdisp = lastdisp,
    firstdisp = firstdisp,
    firstemrg = firstemrg,
    Jyear = Jyear,
    Jday = Jday,
    Jcumcold = Jcumcold,
    tmpdate = tmpdate,
    noow = noow,
    firstow = firstow
  ))
}
