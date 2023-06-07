#' piner
#'
#' Difference between the observed and the predicted at a quantile
#'
#' @description
#' The diffOP function calculates the difference between two points of a cumulative curve at a given quantile.
#'
#' @param observed The observed value at the quantile
#' @param predicted The predicted value at the quantile
#' @param quantile The quantile where the difference should be calculated
#'
#' @return It returns a numeric value.
#'
#' @export
diffOP <- function(observed, predicted, quantile = 0.5){
  dayPredicted <- predicted[which.min(abs(predicted$qm - quantile)), 1]
  dayObserved <- observed[which.min(abs(observed$prob - quantile)), 1]
  daysDiff <-   dayObserved - dayPredicted
  daysDiff <- round(daysDiff)
  return(daysDiff)
}
