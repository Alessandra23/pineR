#' plotStumpTemp
#'
#' Plot Stump Temperature Time Series.
#' This function plots stump temperature data over time, either in days or months.
#' The data can be grouped by month and/or averaged over each month.
#'
#' @param model Output from the \code{piner} function.
#' @param type A character vector specifying the type of time series plot.
#' Can be 'days' or 'month'. Default is 'days'.
#' @param startDate A Date object representing the starting date of the time series.
#' Default is "2000-01-01". Only used when displaying Months (see details).
#' @param groupMonth A logical value. If TRUE, the data will be grouped by
#' Month and displayed as a single point for each group. Default is FALSE.
#' @param avgMonth A logical value. If TRUE, the data will be averaged for
#' each month across the entire time series. Default is FALSE.
#'
#' @return A ggplot object displaying the stump temperature time series.
#'
#' @details The 'startDate', 'groupMonth', and 'avgMonth' arguments are only used when
#' selecting \code{type = 'month'}. The 'startDate' argument is used to set the starting
#' date of the data and should be in the form \code{as.Date(myDate)}. However, if \code{type = 'days'}
#' then the 'startDate', 'groupMonth', and 'avgMonth' arguments are ignored.
#'
#' @import ggplot2
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' data(weatherData)
#' mod <- piner(data = weatherData,
#'              npop = 100,
#'              ntimes = 10,
#'              species = 2,
#'              output = 0)
#'
#' plotStumpTemp(model = mod, startDate = as.Date("2020-01-01"), type = "month", avgMonth = TRUE)


plotStumpTemp <- function(model,
                          type = c('days', 'month'),
                          startDate = as.Date("2000-01-01"),
                          groupMonth = F,
                          avgMonth = F){

  # Declare global vars
  days <- stumpTemp <- MonthAbb <- Year <- MonthOrder <- MonthYear <- mean_stumpTemp <-  NULL

  if(avgMonth == TRUE && groupMonth == T){
    stop('Both \'groupMonth\' and \'avgMonth\' arguments can not be set to TRUE. Change one of these arguments to FALSE to continue.')
  }
  # Extract information
  temperatures <- model$stumpT

  # turn into data frame
  df <- data.frame(stumpTemp = temperatures,
                   days = c(1:length(temperatures)))

  # Match the argument type
  type <- match.arg(type)
  switch(type,
         "days" = {
           p <- ggplot(df, aes(x = days, y = stumpTemp)) +
             geom_line(color = 'steelblue') +
             theme_bw() +
             xlab('Days') +
             ylab('Stump Temperature')
         },
         "month" = {
           # Calculate the new date column by adding 'days' to the startDate
           df$Date <- startDate + df$days

           # Extract the associated month number and convert it to the month name
           df$Month <- month.abb[as.POSIXlt(df$Date)$mon + 1]

           # Extract the year from the Date column
           df$Year <- as.numeric(format(df$Date, "%Y"))

           # Calculate the year number
           startDateYear <- as.numeric(format(startDate, "%Y"))
           df$YearNumber <- df$Year - startDateYear + 1

           # Combine the month name and year number with 'year' prefix
           df$MonthYear <- paste0(df$Month, "_Yr_", df$YearNumber)

           # Add a MonthOrder column to maintain the correct order and rename
           df$Month <- as.numeric(format(df$Date, "%m"))
           df$MonthOrder <-  (df$Year - min(df$Year)) * 12 + df$Month
           df$Month <- month.abb[df$Month]

           # Group by 'MonthYear' and 'MonthOrder', and calculate the mean of 'stumpTemp'
           summary_df <- aggregate(stumpTemp ~ MonthYear + MonthOrder, data = df, FUN = mean)

           # Arrange the summary data frame based on 'MonthOrder'
           summary_df <- summary_df[order(summary_df$MonthOrder),]

           # Remove the 'MonthOrder' column from the summary data frame
           summary_df <- summary_df[, !(names(summary_df) %in% c("MonthOrder"))]

           # Rename
           names(summary_df) <- c('MonthYear', 'mean_stumpTemp')

           # Name plot label
           lab = 'Month'

           # Group by 'MonthYear' and calculate the mean of 'stumpTemp'
           if(groupMonth){

             # Create new month order
             month_order_df <- data.frame(
               MonthAbb = month.abb,
               MonthOrder = 1:12)

             # Split 'MonthYear' column into 'MonthAbb' and 'Year' columns
             split_month_year <- strsplit(summary_df$MonthYear, split = "_Yr_")
             df <- data.frame(MonthAbb = sapply(split_month_year, "[[", 1),
                              Year = sapply(split_month_year, "[[", 2))

             # Convert the 'Year' column to numeric
             df$Year <- as.numeric(df$Year)

             df$mean_stumpTemp <- summary_df$mean_stumpTemp

             # Join the month order data frame with the split names data frame
             df <- merge(df, month_order_df, by = "MonthAbb")

             # Arrange the combined data frame based on the month order and year
             ordered_rows <- order(df$MonthOrder, df$Year)
             df <- df[ordered_rows,]

             # Recreate the MonthYear column in the correct order
             df$MonthYear <- paste(df$MonthAbb, "Yr", df$Year, sep = "_")

             # Remove the 'MonthAbb', 'Year', and 'MonthOrder' columns from the data frame
             summary_df <- subset(df, select = -c(MonthAbb, Year, MonthOrder))

             # Name plot label
             lab = 'Month Grouped'
           }
           if(avgMonth){
             # Use aggregate function to group the 'df' data frame by 'Month' and calculate the mean of 'stumpTemp' for each group
             dfm <- aggregate(stumpTemp ~ Month, data = df, FUN = mean)

             # Sort based on the order of months in the 'month.abb' vector
             dfm <- dfm[order(factor(dfm$Month, levels = unique(month.abb))),]

             # Convert to a factor
             dfm$Month <- factor(dfm$Month, levels = month.abb)

             # Rename columns
             names(dfm) <- c('MonthYear', 'mean_stumpTemp')

             # Reassign
             summary_df <- dfm

             # Name plot label
             lab = 'Month Averaged'
           }
           # Factor column
           summary_df$MonthYear <- factor(summary_df$MonthYear, levels = summary_df$MonthYear)

           # plot
           p <- ggplot(summary_df, aes(x = MonthYear, y = mean_stumpTemp)) +
             geom_point(color = 'steelblue') +
             geom_line(group = 1, color = 'steelblue') +
             theme_bw() +
             xlab(lab) +
             ylab('Stump Temperature') +
             theme(axis.text.x=element_text(angle=55,hjust=1))
         })
  return(p)
}
