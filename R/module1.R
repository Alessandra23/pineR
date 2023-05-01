#' module1
#'
#' Module 1: Calculate Development Times for for Eggs and Larvae
#' This function calculates the time for eggs and larvae development of
#' the pine weevil.
#'
#' @param npop A single numeric value representing the number of individuals in the population.
#' @param data A data frame containing daily climate data.
#' @param params A list containing various model parameters, such as threshold temperatures and time limits.
#' @param stump_temp A list containing stump temperature data.
#' @param first_generation A list containing information about the first generation of insects.
#' @param k A single numeric value representing the index for storing data in output vectors.
#' @param year A numeric vector of length 'nd' containing the year for each day.
#' @param i A single numeric value representing the current iteration of the outer loop.
#' @param j A single numeric value representing the current iteration of the inner loop.
#' @param t0 A numeric vector of length 'npop' containing initial times for each individual in the population.
#' @param gstyear A numeric vector of length 'npop' containing the year of the first generation for each individual.
#' @param stagegg A numeric vector of length 'npop' containing the egg development time for each individual.
#' @param rweevil A numeric vector of length 'npop' containing the reproduction time for each individual.
#' @param laval A numeric vector of length 'npop' containing the larval development time for each individual.
#' @param stagel A numeric vector of length 'npop' containing the stage of development for each individual.
#' @param t2 A numeric vector of length 'npop' containing the required degree days for larval development for
#' each individual.
#'
#' @importFrom stats runif
#'
#' @return A list containing updated values for various insect development times, including:
#' \itemize{
#' \item Index values for storing data in output vectors (k and j).
#' \item Current day (today).
#' \item Larval development times (laval).
#' \item Start day for larval development (start).
#' \item Year of the first generation for each individual (gstyear).
#' \item Egg development time for each individual (stagegg).
#' \item Reproduction time for each individual (rweevil).
#' }
#'
#' @export

module1 <- function(npop,
                    data,
                    params,
                    stump_temp,
                    first_generation,
                    k,
                    year,
                    i,
                    j,
                    t0,
                    gstyear,
                    stagegg,
                    rweevil,
                    laval,
                    stagel,
                    t2) {
  # Set up parameters --------------------------------------------------------
  th2 <- params$th2
  s10 <- stump_temp$s10
  s30 <- stump_temp$s30
  lavalst <- first_generation$lavalst

  # -------------------------------------------------------------------------



  ## Store egg development time
  k <- (i - 1) * npop + j
  gstyear[k] <- year[t0[j]]
  stagegg[k] <- lavalst[j] - t0[j]
  rweevil[k] <- 0


  ## Larval Development
  today <- lavalst[j]
  start <- today
  dd <- 0
  rdd <- t2[j]
  if (runif(1) > 0.5) {
    while (dd < rdd) {
      today <- today + 1
      dd <- dd + max((s10[today] - th2), 0)
    }
  } else {
    while (dd < rdd) {
      today <- today + 1
      dd <- dd + max((s30[today] - s10), 0)
    }
  }
  laval[j] <- today

  k <- (i - 1) * npop + j
  stagel[k] <- today - start
  start <- today



  return(list(
    j = j,
    k = k,
    today = today,
    laval = laval,
    start = start,
    gstyear = gstyear,
    stagegg = stagegg,
    rweevil = rweevil
  ))
}
