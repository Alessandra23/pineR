#' module3
#'
#' Module 3: Calculate Adult Emergence and Overwintering.
#' This function calculates the emergence times and overwintering status of
#' the pine weevil.
#'
#' @param npop A single numeric value representing the population size.
#' @param params A list containing various model parameters, such as degree days for emergence and overwintering.
#' @param stump_temp A list containing stump temperature and cold data.
#' @param today A single numeric value representing the current day.
#' @param prep A numeric vector of length 'npop' containing the pre-pupal development times for each individual.
#' @param start A single numeric value representing the starting day of the time series.
#' @param stagepp A numeric vector of length 'npop' containing the pre-pupal stage duration for each individual.
#' @param stagepl A numeric vector of length 'npop' containing the pupal stage duration for each individual.
#' @param owr A logical vector of length 'npop' containing the overwintering status for each individual.
#' @param emerge A numeric vector of length 'npop' containing the emergence times for each individual.
#' @param i A single numeric value representing the current iteration of the outer loop.
#' @param j A single numeric value representing the current iteration of the inner loop.
#' @param gemtime A numeric vector of length 'npop' containing the emergence times relative to the initial time (t0) for each individual.
#' @param gemtime_new A numeric vector of length 'npop' containing the new emergence times for each individual.
#' @param gemwinter A numeric vector of length 'npop' containing the number of winters passed before emergence for each individual.
#' @param gemmonth A numeric vector of length 'npop' containing the month of emergence for each individual.
#' @param year A numeric vector containing the year for each day.
#' @param gstyear A numeric vector of length 'npop' containing the generation start year for each individual.
#' @param month A numeric vector containing the month for each day.
#' @param t0 A numeric vector of length 'npop' containing the initial time for each individual.
#' @param rweevil A numeric vector of length 'npop' containing the re-emergence status for overwintering individuals.
#'
#' @return A list containing updated values for various insect development times and overwintering status, including:
#' \itemize{
#' \item Current day (today).
#' \item Re-emergence status for overwintering individuals (rweevil).
#' \item Emergence times relative to the initial time (t0) for each individual (gemtime).
#' \item Number of winters passed before emergence for each individual (gemwinter).
#' \item Month of emergence for each individual (gemmonth).
#' \item Emergence times for each individual (emerge).
#' }
#'
#' @export

module3 <- function(npop,
                    params,
                    stump_temp,
                    today,
                    prep,
                    start,
                    stagepp,
                    stagepl,
                    #cold,
                    owr,
                    emerge,
                    i,
                    j,
                    gemtime,
                    gemtime_new,
                    gemwinter,
                    gemmonth,
                    year,
                    gstyear,
                    month,
                    t0,
                    rweevil){


# Set up parameters -------------------------------------------------------

  t5days <-  params$t5days
  t6Edays <- params$t6Edays
  t6Odays <- params$t6Odays
  cold <- stump_temp$cold



# -------------------------------------------------------------------------

  k <- (i - 1) * npop + j
  stagepp[k] <- prep[j] - start
  stagepl[k] <- today - prep[j]
  start <- today
  ## Adult in pupil cell - melenation
  today <- today + t5days
  ## Check for immediate emergance
  k <- today - t6Edays + 1
  cwd <- sum(cold[k:today])
  if (cwd < t6Edays) {
    ## Emergance and overwintering
    if (owr[j]) {
      cwd <- 0
      ccd <- 0
      while (cwd < t6Edays) {
        if (ccd == t6Odays) {
          owr[j] <- FALSE
          k <- (i - 1) * npop + j
          rweevil[k] <- 1
        }
        {
          if (cold[today] == 1) {
            cwd <- cwd + 1
            ccd <- 0
          } else if (cold[today] == -1) {
            ccd <- ccd + 1
            cwd <- 0
          } else {
            cwd <- 0
            ccd <- 0
          }
        }
        today <- today + 1
      }
    } else {
      cwd <- 0
      while (cwd < t6Edays) {
        if (cold[today] == 1) {
          cwd <- cwd + 1
        } else {
          cwd <- 0
        }
        today <- today + 1
      }
    }
  }
  emerge[j] <- today
  k <- (i - 1) * npop + j
  gemtime[k] <- today - t0[j]
  gemtime_new[k] <- today
  gemwinter[k] <- year[today] - gstyear[k]
  gemmonth[k] <- month[today]
  ## Check for overwintering
  if (owr[j]) {
    ccd <- 0
    while (ccd < t6Odays) {
      if (cold[today] == -1) {
        ccd <- ccd + 1
      } else {
        ccd <- 0
      }
      today <- today + 1
    }
    ## Re-emergence
    cwd <- 0
    while (cwd < t6Edays) {
      today <- today + 1
      if (cold[today] == 1) {
        cwd <- cwd + 1
      } else {
        cwd <- 0
      }
    }
    k <- (i - 1) * npop + j
    rweevil[k] <- 2
  }

  return(list(
    today = today,
    rweevil = rweevil,
    gemtime = gemtime,
    gemwinter = gemwinter,
    gemmonth = gemmonth,
    emerge = emerge
  ))

}
