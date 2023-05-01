#' module4
#'
#' Module 4: Calculate Maturation Feeding and Hibernation.
#' This function calculates the maturation feeding times and hibernation
#' status of the pine weevil.
#'
#' @param t7 A numeric vector of length 'npop' containing the required feeding times for each individual.
#' @param today A single numeric value representing the current day.
#' @param params A list containing various model parameters, such as degree days for emergence and overwintering.
#' @param stump_temp A list containing stump temperature and cold data.
#' @param month A numeric vector containing the month for each day.
#' @param j A single numeric value representing the current iteration of the inner loop.#'
#' @return A list containing updated values for various insect maturation and hibernation times, including:
#' \itemize{
#' \item Current day (today).
#' \item Number of days spent feeding (feeddays).
#' }
#'
#' @export

module4 <- function(t7,
                    today,
                    params,
                    stump_temp,
                    month,
                    j){


# Set parameters ----------------------------------------------------------
  t6Odays <- params$t6Odays
  nfv <-     params$nfv
  t6Edays <- params$t6Edays
  cold <- stump_temp$cold

# -------------------------------------------------------------------------




  ## Maturation feeding / hibernation
  rdd <- t7[j]
  feeddays <- 0
  ccd <- 0
  start <- today
  ## Feed while checking for hbernation
  while (feeddays < rdd && ccd < t6Odays) {
    today <- today + 1
    if (cold[today] == 1) {
      feeddays <- feeddays + nfv[month[today]]
      ccd <- 0
    } else if (cold[today] == -1) {
      ccd <- ccd + 1
    }
  }
  ## Check if feeding not complete (hibernation)
  if (feeddays < rdd) {
    ## Check for re-rmergance
    cwd <- 0
    while (cwd < t6Edays) {
      today <- today + 1
      if (cold[today] == 1) {
        cwd <- cwd + 1
      } else {
        cwd <- 0
      }
    }
    ## Continue feeding
    while (feeddays < rdd) {
      today <- today + 1
      if (cold[today] == 1) {
        feeddays <- feeddays + nfv[month[today]]
      }
    }
  }
  #mature[j] <- today



  return(list(
    today = today,
   # mature = mature,
    feeddays = feeddays
  ))

}
