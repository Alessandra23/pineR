#' plotEmergence
#' Plot Emergence Curve with Confidence Intervals.
#' This function creates a ggplot object visualizing the observed
#' and predicted emergence curves along with the confidence intervals
#' for the quantiles.
#'
#' @param quantiles A data frame containing quantile information
#' for the observed emergence curve.
#'  It should have columns 'day', 'qm' (median), 'ql' (lower quantile), and 'qu' (upper quantile).
#' @param emergence A data frame containing emergence probability
#' information for the predicted emergence curve. It should have columns 'day' and 'prob'.
#' @param pal A vector of length two representing the colours used to display the
#' observed (default: 'firebrick') and predicted (default: 'steelblue') values.
#'
#' @return A ggplot object representing the emergence curve plot.
#' @export

plotEmergence <- function(quantiles, emergence, pal = c('steelblue', 'firebrick')){

  # Declare global vars
  day <- qm <- prob <- ql <- qu <- NULL

  ecdf_var <- ggplot() +
    geom_line(data = quantiles, aes(x = day, y = qm, colour = pal[1]),  linewidth = 1) +
    geom_line(data = emergence, aes(x = day, y = prob, colour =pal[2]), linewidth = 1) +
    scale_color_manual(name = "Emergence",
                       labels = c("Observed", "Predicted"),
                       values = c(pal[2], pal[1])) +
    geom_ribbon(data = quantiles, aes(x = day, ymin = ql, ymax = qu),
                alpha=0.1,
                linetype="dashed",
                color = pal[1],
                fill  = pal[1]) +
    labs(x = "Days",
         y = "Probability") +
    xlim(c(0,400))+
    ylim(0, 1) +
    theme_bw(base_size = 14)

  suppressWarnings(
    print(ecdf_var)
  )
}
