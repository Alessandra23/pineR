#' Counters
#'
#' @description  Setting up the counters
#'
#' @param npop population size
#' @param ntimes number of times, simulations
#' @return Empty vectors
#' @export
#'

setCounters <- function(npop, ntimes) {

  laval <- numeric(npop) # larval
  lavalst <- numeric(npop) # larval at root stump
  prep <- numeric(npop) # pre-pupal
  emerge <- numeric(npop) # emergence (very important in relation to overwintering with cold and warm days parameters)
  mature <- numeric(npop) # maturation stage
  ovipos <- numeric(npop) # oviposition stage

  gstmonth <- numeric(npop * ntimes) # oviposition month for the start of the final generation after the model is run
  gstyear <- numeric(npop * ntimes) # oviposition year for the start of the final generation after the model is run
  geetime <- numeric(npop * ntimes) # number of days to start oviposition
  geewinter <- numeric(npop * ntimes) # number of winters until start of oviposition
  geemonth <- numeric(npop * ntimes) # month when oviposition starts
  gemtime <- numeric(npop * ntimes) # number of days to emergence above ground
  gemtime_new <- numeric(npop * ntimes)
  gemmonth <- numeric(npop * ntimes) # month of emergence above ground
  gemwinter <- numeric(npop * ntimes) # number of winters to emergence above ground
  rweevil <- numeric(npop * ntimes) # overwintering status of weevil
  # 0 = no overwintering
  # 1 = overwintering before emergence (i.e. in the pupal cell/stage)
  # 2 = overwintering after emergence

  stagegg <- numeric(npop * ntimes) # number of days in egg stage
  stagel <- numeric(npop * ntimes) # number of days in larval stage
  stagepp <- numeric(npop * ntimes) # number of days in prepupal stage
  stagepl <- numeric(npop * ntimes) # number of days in pupal stage
  stageov <- numeric(npop * ntimes) # number of days from completion of Overwintering to the start of ovipostion
  debug <- numeric(npop * ntimes) # debug variable

  monthL <- rep(0, 12) # larval
  monthP <- rep(0, 12) # pupal
  monthE <- rep(0, 12) # emergence
  monthR <- rep(0, 12) # re-emergence
  monthM <- rep(0, 12) # maturation
  monthO <- rep(0, 12) # oviposition

  firstdisp <- 0
  lastdisp <- 0
  firstemrg <- 0
  firstow <- 0
  noow <- 0

  return(list(
    laval = laval,
    lavalst = lavalst,
    prep = prep,
    emerge = emerge,
    mature = mature,
    ovipos = ovipos,
    gstmonth = gstmonth,
    gstyear = gstyear,
    geetime = geetime,
    geewinter = geewinter,
    geemonth = geemonth,
    gemtime = gemtime,
    gemtime_new = gemtime_new,
    gemmonth = gemmonth,
    gemwinter = gemwinter,
    rweevil = rweevil,
    stagegg = stagegg,
    stagel = stagel,
    stagepp = stagepp,
    stagepl = stagepl,
    stageov = stageov,
    debug = debug,
    monthL = monthL,
    monthP = monthP,
    monthE = monthE,
    monthR = monthR,
    monthM = monthM,
    monthO = monthO,
    firstdisp = firstdisp,
    lastdisp = lastdisp,
    firstemrg = firstemrg,
    firstow = firstow,
    noow = noow
  ))
}
