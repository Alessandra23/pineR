#' setParameters
#'
#' Set Parameters for Insect Life Cycle Model.
#' This function sets various parameters for running in the \code{piner} function.
#'
#' @param species A character string representing the species of interest.
#' Currently supported species are "spruce" and "pine".
#'
#' @return A list containing various model parameters for different life stages,
#'  such as mean and standard deviation values for the duration of each stage,
#'  temperature thresholds, and feeding values. See associated vignette for a detailed
#'  list of all the parameters.
#'
#' @examples
#' # Set parameters for the spruce species
#' spruce_params <- setParameters(species = "spruce")
#'
#' # Set parameters for the pine species
#' pine_params <- setParameters(species = "pine")
#'
#' @export

setParameters <- function(species = NULL){

  ## Stage 0: initial oviposition
  t0m <- 171 # mean
  t0sd <- 10 # standard deviation

  ## Stage 1: egg development
  t1m <- 110 # mean
  t1sd <- 5.09 # standard deviation
  th1 <- 8.0 # threshold (temperature)
  t1lim <- 40 # limit for number of days

  ## Stage 2: larval development
  if (species == "spruce") {
    t2m <- 767 # mean
    t2sd <- 126 # standard deviation
  }
  if (species == "pine") {
    t2m <- 660 # mean
    t2sd <- 101 # standard deviation
  }

  th2 <- 4.5 # threshold (temperature)

  ## Stage 3: prepupal development
  t3ldays <- 10 # lower limit
  t3udays <- 190 # upper limit
  t3a <- 3.1799 # parameter related to the logistic distribution
  t3b <- 2.461 # parameter related to the logistic distribution
  t3c <- 1.3503 # parameter related to the logistic distribution
  t3m <- 18.678 # mean
  t3sd <- 0.19 # standard deviation

  ## Stage 4: pupal development
  t4i <- 12.5 # 12.5 degree transition from prepupae to pupa
  t4m <- 219 # day degrees mean
  t4sd <- 22.5 # day degrees standard deviation
  th4 <- 7.3 # threshold (temperature)
  t4lim <- 60 # threshold limit to pass to next Stage. to maintain pop.size (see paper methods)

  ## Stage 5: melanisation
  t5days <- 21 # length of days for melanisation to take place for weevil (this is the darkening of the insect)

  ## Stage 6: emergence / hibernation / overwintering
  th6 <- 9 # threshold temperature for progressing to next Stage
  t6Edays <- 5 # referring to 5 consec. days to intiate emergence
  t6Odays <- 10 # variable for threshold of days to overwintering intiated when mean and air rs temp. is less than 9 degrees for 10 days

  ## Stage 7: maturation feeding
  t7m <- 20 # mean
  t7sd <- 0.06 # standard deviation
  t7days <- 12 # minimum number of days before initiating maturation feeding

  ## Stage 8: dispersion
  th8 <- 18.5 # threshold temperature for progressing to next Stage and moving onto ovipostion again for second year

  ## Stage 9: oviposition
  t9d <- 78 # refering to oviposition starting after dispersal. Occurs over a total of 78 days when mean air and 10cm rs temperature is over 9 degrees
  t9sur <- 40 # relating to mortality. More than 40 days, eggs do not develop
  eggdays <- numeric(t9d)

  ## feeding value
  nfv <- c(0.25, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 0.5, 0.25, 0.25) # one per month of the year, notional feeding values

  return(list(
    t0m = t0m,
    t0sd = t0sd,
    t1m = t1m,
    t1sd = t1sd,
    th1 = th1,
    t1lim = t1lim,
    t2m = t2m,
    t2sd = t2sd,
    th2 = th2,
    t3ldays = t3ldays,
    t3udays = t3udays,
    t3a = t3a,
    t3b = t3b,
    t3c = t3c,
    t3m = t3m,
    t3sd = t3sd,
    t4i = t4i,
    t4m = t4m,
    t4sd = t4sd,
    th4 = th4,
    t4lim = t4lim,
    t5days = t5days,
    th6 = th6,
    t6Edays = t6Edays,
    t6Odays = t6Odays,
    t7m = t7m,
    t7sd = t7sd,
    t7days = t7days,
    th8 = th8,
    t9d = t9d,
    t9sur = t9sur,
    eggdays = eggdays,
    nfv = nfv
  ))
}
