#' getStumpTemp
#'
#' Calculate Stump Temperatures
#'
#' This function calculates stump temperatures at specified depths and returns a list of attributes.
#'
#' @param depth A single numeric value indicating the depth at which to calculate stump temperatures.
#' Only 1 and 3  are accepted.
#' @param nd A single numeric value representing the number of days for which the calculation will be performed.
#' @param temp A numeric vector of length 'nd' containing daily temperature values in degrees Celsius.
#' @param th6 A single numeric value representing the threshold temperature (in degrees Celsius)
#' below which the stump is considered cold.
#'
#' @return A list with the following components:
#' \itemize{
#' \item \code{s10}: A numeric vector of length 'nd' representing daily 10-day moving averages of temperature at 10 cm depth.
#' \item \code{s30}: A numeric vector of length 'nd' representing daily 30-day moving averages of temperature at 30 cm depth.
#' \item \code{cold}: A numeric vector of length 'nd' containing values of -1, 0, or 1, indicating whether the stump is cold
#' or not based on the 10-day moving average and threshold temperature.
#' \item \code{cumcold}: A numeric vector of length 'nd' containing cumulative cold values.
#' \item \code{stumpT}: A numeric vector of length 'nd' containing calculated stump temperatures based on the specified depth.
#' }
#' @examples
#' depth <- 1
#' nd <- 10
#' temp <- c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
#' th6 <- 6
#' getStumpTemp(depth, nd, temp, th6)
#' @export

getStumpTemp <- function(depth, nd, temp, th6){

  d10 <- 5.0
  d30 <- 5.0
  s10 <- numeric(nd)
  s30 <- numeric(nd)
  cold <- numeric(nd)
  cumcold <- numeric(nd)

  for (j in 1:nd) {
    d10 <- 0.77 * d10 + 0.23 * max(temp[j], 0)
    d30 <- 0.91 * d30 + 0.09 * max(temp[j], 0)
    s10[j] <- d10
    s30[j] <- d30
    cold[j] <- 0 - 1 * ((s10[j] < th6) && (temp[j] < th6)) + 1 * ((s10[j] > th6) && (temp[j] > th6))
  }

  if (depth == 1) {
    stumpT <- s10
  } else if (depth == 3) {
    stumpT <- s30
  } else {
    stumpT <- (s10 + s30) / 2
  }

  return(list(s10 = s10,
              s30 = s30,
              cold = cold,
              cumcold = cumcold,
              stumpT = stumpT))

}
