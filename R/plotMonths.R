#' plotMonths
#'
#' Create a Stacked Area Plot of Monthly Probabilities for Different Stages.
#' This function takes a simulation table and a data frame name as input, and
#' returns a ggplot object representing a stacked area plot of monthly probabilities
#' for different stages (L, P, E, M, and O) with a viridis color palette.
#'
#' @param model Output from the \code{piner} function.
#' @param pal The colour palette used to display the months.
#'
#' @import ggplot2
#' @importFrom grDevices rainbow
#' @importFrom stats reshape
#'
#' @return A ggplot object representing the stacked area plot of monthly probabilities
#' for different stages.
#'
#' @export

plotMonths <- function(model, pal = rainbow(12)){

  # Declare global vars
  Stage <- prob <- Months <- NULL

    # Extract info
    mon <- model$dfMonths

    # Convert the data frame to long format using base R's reshape function
    dff_long <- reshape(mon, idvar = "mname", varying = c("monthL", "monthP", "monthE", "monthM", "monthO"),
                         direction = "long", timevar = "Stage", v.names = "prob", new.row.names = NULL)

    # Create Months column
    dff_long$Months <- factor(dff_long$mname, levels = month.abb)

    # Sort the data frame by the month column
    sorted_df <- dff_long[order(dff_long$Months), ]
    # Set row names
    rownames(sorted_df) <- c(1:nrow(sorted_df))
    # Index stage column and set as character
    sorted_df$Stage <- c("monthL", "monthP", "monthE", "monthM", "monthO")[sorted_df$Stage]
    sorted_df$Stage <- as.character(sorted_df$Stage)
    # Turn into data frame
    sdf <- as.data.frame(sorted_df)
    # Remove unnecessary attributes
    one_entry <- function(x) {
      for (i in length(x)) attr(x[[i]], "names") <- NULL
      return(x)
    }
    sdf <- lapply(sdf, FUN=one_entry)
    dat_long <- as.data.frame(sdf)




# Plot --------------------------------------------------------------------


  p_months <- dat_long |>
    ggplot(aes(x = as.numeric(factor(Stage, levels = c("monthL", "monthP", "monthE", "monthM", "monthO"))),
               y = prob, fill = Months)) +
    geom_area(alpha=0.6 , size=.5) +
    theme_bw() +
    scale_fill_manual(values = pal,
                      labels = unique(dat_long$Months)) +
    labs(x = "Stage",
         y = "Probability",
         fill = ' ') +
    scale_x_discrete(limits= c("L", "P", "E", "M", "O"))


  return(p_months)


}
