#' getFirstGeneration
#'
#' Calculate First Generation Timings.
#' This function calculates the timings for the first generation of a
#'  population based on initial conditions, thermal requirements, and temperature data.
#'
#' @param npop A single numeric value representing the population size.
#' @param t0 A numeric vector of length 'npop' containing the initial starting day for each individual.
#' @param t1 A numeric vector of length 'npop' containing the thermal requirement (in degree days) for each individual.
#' @param params A list containing various model parameters, such as threshold temperatures and time limits.
#' @param stump_temp A list containing stump temperature data, including 10-day moving averages (s10).
#' @param lavalst A numeric vector of length 'npop' initialized with zeros, to store the calculated day when each individual reaches the larval stage.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{today}: A single numeric value representing the last calculated day for the final individual in the population.
#' \item \code{lavalst}: A numeric vector of length 'npop' containing the calculated day when each individual reaches the larval stage.
#' }
#'
#' @export

getFirstGeneration <- function(npop,
                               t0,
                               t1,
                               params,
                               stump_temp,
                               lavalst){

  for (j in 1:npop) {
    today <- round(t0[j])
    start <- today
    dd <- 0.0
    rdd <- t1[j]
    while (dd < rdd) {
      today <- today + 1
      dd <- dd + max((stump_temp$s10[today] - params$th1), 0)
    }
    if ((today - start) > params$t1lim) {
      today <- start + params$t1lim
    }
    lavalst[j] <- today
  }


  return(list(
    today = today,
    lavalst = lavalst
  ))

}
