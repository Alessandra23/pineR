#' collectStatistics
#'
#' Collect and Process Life Cycle Statistics.
#' This function collects and processes life cycle statistics from simulation outputs,
#' calculating the averages and proportions of various life cycle events.
#'
#' @param ntimes A single numeric value representing the number of simulation runs.
#' @param npop A single numeric value representing the population size.
#' @param monthL A numeric value representing the sum of all individuals in the
#' larval stage across all months and simulation runs.
#' @param monthP A numeric value representing the sum of all individuals in the
#'  pupal stage across all months and simulation runs.
#' @param monthE A numeric value representing the sum of all individuals in the
#'  adult (emerging) stage across all months and simulation runs.
#' @param monthR A numeric value representing the sum of all individuals in the
#' reproducing stage across all months and simulation runs.
#' @param monthM A numeric value representing the sum of all individuals in the
#' migrating stage across all months and simulation runs.
#' @param monthO A numeric value representing the sum of all individuals in the
#' overwintering stage across all months and simulation runs.
#' @param SiteMeanTemp A single numeric value representing the sum of the mean
#' temperatures across all simulation runs.
#' @param count A list containing various counts of life cycle events from the simulation runs.
#' @param gemmonth A numeric vector containing the month of initial emergence for each individual.
#' @param gemwinter A numeric vector containing the number of winters before initial
#' emergence for each individual.
#' @param geewinter A numeric vector containing the number of winters before initial
#' oviposition for each individual.
#' @param geemonth A numeric vector containing the month of initial oviposition for each individual.
#' @param firstdisp Mean first dispersal day.
#' @param lastdisp Mean last dispersal day.
#' @param firstemrg Mean first emergence day.
#' @param firstow Mean first overwintering day.
#' @param noow Number of overwintering years.
#'
#' @return A list containing the processed statistics and proportions of various life cycle events, including:
#' \itemize{
#' \item Average proportion of individuals in each stage by month.
#' \item Mean temperature.
#' \item Mean days for various life cycle events (first dispersal, last dispersal, first emergence, first overwintering).
#' \item Percentage of the year with no overwintering.
#' \item Data frame with average proportions of individuals in each stage by month.
#' \item Proportions of initial emergence, initial oviposition, and emergence-oviposition across years.
#' \item Proportions of individuals completing the life cycle in 0, 1, 2, 3, or more than 3 years.
#' }
#'
#' @export

collectStatistics <- function(ntimes,
                              npop,
                              monthL,
                              monthP,
                              monthE,
                              monthR,
                              monthM,
                              monthO,
                              SiteMeanTemp,
                              count,
                              gemmonth,
                              gemwinter,
                              geewinter,
                              geemonth,
                              firstdisp,
                              lastdisp,
                              firstemrg,
                              firstow,
                              noow){

  monthL <- monthL / (ntimes * npop)
  monthP <- monthP / (ntimes * npop)
  monthE <- monthE / (ntimes * npop)
  monthR <- monthR / (ntimes * npop)
  monthM <- monthM / (ntimes * npop)
  monthO <- monthO / (ntimes * npop)

  smt <- SiteMeanTemp
  mean_temperature <- round(smt*100)/100
  mean_first_disp_day <- firstdisp
  mean_last_disp_day <-  lastdisp
  mean_first_emer_day <- firstemrg

  mean_first_overw_day <- round(firstow) + 172
  pnoow <- 100 * noow / (30 * ntimes) # Percentage of year with no overwintering

  mname <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
    "Oct", "Nov", "Dec"
  )
  df_months <- data.frame(mname, monthL, monthP, monthE, monthM, monthO)

  initial_emergence <- 1000 * table(gemmonth, gemwinter) / length(gemwinter) / 10
  p_initial_oviposition <- 1000 * table(geemonth, geewinter) / length(geewinter) / 10
  years_emergence_oviposition <- 1000 * table(geewinter, gemwinter) / length(geewinter) / 10

  p0_year_life_cycle <- round(sum(geewinter == 0) / (ntimes * npop), 3)
  p1_year_life_cycle  <- round(sum(geewinter == 1) / (ntimes * npop), 3)
  p2_year_life_cycle  <- round(sum(geewinter == 2) / (ntimes * npop), 3)
  p3_year_life_cycle  <- round(sum(geewinter == 3) / (ntimes * npop), 3)
  p4_year_life_cycle  <- round(sum(geewinter > 3) / (ntimes * npop), 3)

  return(list(
    monthL = monthL,
    monthP = monthP,
    monthE = monthE,
    monthR = monthR,
    monthM = monthM,
    monthO = monthO,
    mean_temperature  = mean_temperature,
    mean_first_disp_day = mean_first_disp_day,
    mean_last_disp_day  = mean_last_disp_day,
    mean_first_emer_day = mean_first_emer_day,
    mean_first_overw_day = mean_first_overw_day,
    pnoow = pnoow,
    df_months = df_months,
    initial_emergence = initial_emergence,
    p_initial_oviposition = p_initial_oviposition,
    years_emergence_oviposition = years_emergence_oviposition,
    p0_year_life_cycle = p0_year_life_cycle,
    p1_year_life_cycle = p1_year_life_cycle,
    p2_year_life_cycle = p2_year_life_cycle,
    p3_year_life_cycle = p3_year_life_cycle,
    p4_year_life_cycle = p4_year_life_cycle
  ))

}
