#' module5
#'
#' Module 5: Calculate Dispersal, Oviposition, and Egg Development.
#' This function calculates the dispersal, oviposition, and egg development times
#' of the pine weevil.
#'
#' @param params A list containing various model parameters, such as degree days for emergence and overwintering.
#' @param today A single numeric value representing the current day.
#' @param maxtemp A numeric vector containing the maximum daily temperatures.
#' @param stump_temp A list containing stump temperature and cold data.
#' @param t1 A numeric vector of length 'npop' containing the required egg development times for each individual.
#' @param j A single numeric value representing the current iteration of the inner loop.
#'
#' @return A list containing updated values for various insect dispersal, oviposition, and egg development times, including:
#' \itemize{
#' \item Current day (today).
#' \item Day of viable egg development (eggdev).
#' \item Index of selected viable egg (eggsel).
#' \item Vector of days for each egg (eggdays).
#' \item Vector of sorted indices for egg distribution (eggdist).
#' \item Vector of egg weights (eggwts).
#' }
#'
#' @importFrom stats runif
#'
#' @export
module5 <- function(params,
                    today,
                    maxtemp,
                    stump_temp,
                    t1,
                    j){

# Set up parameters -------------------------------------------------------

th8 <- params$th8
t6Odays <- params$t6Odays
eggdays <- params$eggdays
th1 <- params$th1
t1lim <- params$t1lim
t9d <- params$t9d
cold <- stump_temp$cold
s10 <- stump_temp$s10


# -------------------------------------------------------------------------



  ## Dispersal, Oviposition and egg development
  eggsel <- 0
  while (eggsel == 0) {
    while (maxtemp[today] < th8) {
      today <- today + 1
    }
    l <- 0
    ccd <- 0
    while (l < t9d && ccd < t6Odays) {
      today <- today + 1
      if (cold[today] == 1) {
        ccd <- 0
        l <- l + 1
        eggdays[l] <- today
      } else if (cold[today] == -1) {
        ccd <- ccd + 1
      }
    }
    ## Select and check for viable egg development
    if (l > 0) {
      if (l > 1) {
        eggwts <- l - c(1:l) / (l + 1)
        eggdist <- sort(runif(l) * eggwts, decreasing = T, index.return = T)
      }
      if (l == 1) {
        eggdist <- c(1)
      }
      k <- 0
      while (k < l && eggsel == 0) {
        k <- k + 1
        {
          if (l > 1) {
            eggi <- eggdist$ix[k]
          } else {
            eggi <- 1
          }
        }
        est <- eggdays[eggi]
        edd <- 0
        dd <- 0.0
        rdd <- t1[j]
        while (dd < rdd && edd < t1lim) {
          edd <- edd + 1
          dd <- dd + max((s10[edd + est] - th1), 0)
        }
        if (dd >= rdd) {
          eggdev <- edd + est
          eggsel <- eggi
        }
      }
    }
  }

  return(list(
    today = today,
    eggdev = eggdev,
    eggsel = eggsel,
    eggdays = eggdays,
    eggdist = eggdist,
    eggwts = eggwts
  ))
}
