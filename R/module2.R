#' module2
#'
#' Module 2: Calculate Pre-pupal and Pupal Development Times.
#' This function calculates the development times for the pre-pupal and pupal stages of
#' the pine weevil.
#'
#' @param prep A numeric vector of length 'npop' containing the pre-pupal development times for each individual.
#' @param today A single numeric value representing the current day.
#' @param stump_temp A list containing stump temperature data.
#' @param params A list containing various model parameters, such as threshold temperatures and time limits.
#' @param t3 A numeric vector of length 'npop' containing the required degree days for pre-pupal development for each individual.
#' @param t4 A numeric vector of length 'npop' containing the required degree days for pupal development for each individual.
#' @param j A single numeric value representing the current iteration of the inner loop.
#'
#' @return A list containing updated values for various insect development times, including:
#' \itemize{
#' \item Current day (today).
#' \item Pre-pupal development times (prep).
#' \item Completion status for pupal development (cpupal).
#' }
#'
#' @export

module2 <- function(prep,
                    today,
                    stump_temp,
                    params,
                    t3,
                    t4,
                    j){



# Set up variables --------------------------------------------------------

  t3a <- params$t3a
  t3b <- params$t3b
  t3c <- params$t3c
  t3m <- params$t3m
  t4i <- params$t4i
  t4lim <- params$t4lim
  th4 <- params$th4
  t3ldays = params$t3ldays
  t3udays <- params$t3udays
  stumpT <- stump_temp$stumpT

# -------------------------------------------------------------------------

    ## Pre-pupal development
    sum <- 0
    rdd <- t3ldays - 1
    for (k in 1:rdd) {
      today <- today + 1
      sum <- sum + max(stumpT[today], 0)
    }
    dd <- rdd
    err <- t3[j]
    rdd <- t3ldays
    while (dd < rdd && dd < t3udays) {
      dd <- dd + 1
      today <- today + 1
      sum <- sum + max(stumpT[today], 0)
      meant <- sum / dd
      rdd <- err * exp(t3a + t3c / (1 + exp(t3b * (meant - t3m))))
    }
    ## Pupal Development cycle
    cpupal <- 0
    ## Delay in pupal
    while (cpupal == 0) {
      while (stumpT[today] < t4i) {
        today <- today + 1
      }
      prep[j] <- today
      ## Pupal Development
      cpupal <- 1
      laps <- 0
      dd <- 0
      rdd <- t4[j]
      while (dd < rdd && laps < t4lim) {
        today <- today + 1
        laps <- laps + 1
        dd <- dd + max((stumpT[today] - th4), 0)
      }
      if (dd < rdd) {
        cpupal <- 0
        today <- prep[j] + 1
      }
    }



  return(list(
    today = today,
    prep = prep,
    cpupal = cpupal
  ))

}
