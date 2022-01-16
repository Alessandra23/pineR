#' Setting parameters
#'
#' @description   Function to setting up the population parameters
#'
#' @param species Tree species: pine or spruce
#' @return A list of hyperparameters
#' @export
setParameters <- function(species) {

  ## stage 0: initial oviposition
  t0m <- 171 # mean
  t0sd <- 10 # standard deviation

  ## stage 1: egg development
  t1m <- 110 # mean
  t1sd <- 5.09 # standard deviation
  th1 <- 8.0 # threshold (temperature)
  t1lim <- 40 # limit for number of days

  ## stage 2: larval development
  if (species == "spruce") {
    t2m <- 767 # mean
    t2sd <- 126 # standard deviation
  }
  if (species == "pine") {
    t2m <- 660 # mean
    t2sd <- 101 # standard deviation
  }

  th2 <- 4.5 # threshold (temperature)

  ## stage 3: prepupal development
  t3ldays <- 10 # lower limit
  t3udays <- 190 # upper limit
  t3a <- 3.1799 # parameter related to the logistic distribution
  t3b <- 2.461 # parameter related to the logistic distribution
  t3c <- 1.3503 # parameter related to the logistic distribution
  t3m <- 18.678 # mean
  t3sd <- 0.19 # standard deviation

  ## stage 4: pupal development
  t4i <- 12.5 # 12.5 degree transition from prepupae to pupa
  t4m <- 219 # day degrees mean
  t4sd <- 22.5 # day degrees standard deviation
  th4 <- 7.3 # threshold (temperature)
  t4lim <- 60 # threshold limit to pass to next stage. to maintain pop.size (see paper methods)

  ## stage 5: melanisation
  t5days <- 21 # length of days for melanisation to take place for weevil (this is the darkening of the insect)

  ## stage 6: emergence / hibernation / overwintering
  th6 <- 9 # threshold temperature for progressing to next stage
  t6Edays <- 5 # referring to 5 consec. days to intiate emergence
  t6Odays <- 10 # variable for threshold of days to overwintering intiated when mean and air rs temp. is less than 9 degrees for 10 days

  ## stage 7: maturation feeding
  t7m <- 20 # mean
  t7sd <- 0.06 # standard deviation
  t7days <- 12 # minimum number of days before initiating maturation feeding

  ## stage 8: dispersion
  th8 <- 18.5 # threshold temperature for progressing to next stage and moving onto ovipostion again for second year

  ## stage 9: oviposition
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


#' Counters
#'
#' @description  Setting up the counters
#'
#' @param npop population size
#' @param ntimes number of times, simulations
#' @return Empty vectors
#' @export
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

  ## Initialize whether generator statistics
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


#' getStumpTemp
#' @description  Get the stump temperatures
#' @param depth depth
#' @param nd length of temperature
#' @param temp mean temperature
#' @param th6 param module 6
#' @export
getStumpTemp <- function(depth, nd, temp, th6){

st10 <- numeric(nd)
st30 <- numeric(nd)
cold <- numeric(nd)
st10[1] <- st30[1] <- 5

for (l in 1:nd) {
  st10[l] <- 0.77 * st10[l] + 0.23 * max(temp[l], 0)
  st30[l] <- 0.91 * st30[l] + 0.09 * max(temp[l], 0)
  cold[l] <- 0 - 1 * ((st10[l] < th6) && (temp[l] < th6)) + 1 * ((st10[l] > th6) && (temp[l] > th6))
}


if (depth == 1) {
  st <- st10
} else if (depth == 3) {
  st <- st30
} else {
  st <- (st10 + st30) / 2
}

return(list(st = st,
            st10 = st10,
            st30 = st30,
            cold = cold))

}


#'
#' Stump stats
#' @description jvsjvjsj
#' @param nd data frame
#' @param temp values of the parameters
#' @param maxtemp jcnrjcr
#' @param day jcrnjr
#' @param year jjrv
#' @param firstdisp jnvjv
#' @param lastdisp cnencrej
#' @param firstemrg ncvenke
#' @param firstow vnevne
#' @param noow ncrenvje
#' @param cold vnrvnje
#' @param params vjnvjr
#' @param counter1 jnvjrjnv
#'
#' @return Stump temperature information
#'
#' @export


getStumpStats <- function(nd, temp, maxtemp, day, year,  firstdisp, lastdisp, firstemrg,
                          firstow, noow, cold, params, counter1){

  th8 <-  params$th8
  t6Odays <-  params$t6Odays
  t6Edays <-  params$t6Edays

  cumcold <- numeric(nd)
  ccd <- 0 # cumulative cold days
  cwd <- 0 # cumulative warm days
  cumcold[1] <- 0

  for (j in 1:nd) {
    if (cold[j] == -1) {
      ccd <- ccd + 1
      cumcold[j] <- -ccd
    } else if (cold[j] == 1) {
      cwd <- cwd + 1
      ccd <- 0
      cumcold[j] <- cwd
    } else {
      ccd <- 0
      cwd <- 0
    }
  }

  lastdisp <- ((counter1 - 1) * lastdisp + mean(as.numeric(tapply(day[maxtemp >= th8], year[maxtemp >= th8], max)))) / counter1
  firstdisp <- ((counter1 - 1) * firstdisp + mean(as.numeric(tapply(day[maxtemp >= th8], year[maxtemp >= th8], min)))) / counter1
  firstemrg <- ((counter1 - 1) * firstemrg + mean(as.numeric(tapply(day[cumcold >= t6Edays], year[cumcold >= t6Edays], min))))/ counter1

  Jyear <- year[1:(nd - 366)]
  Jday <- day[1:(nd - 366)]
  Jcumcold <- cumcold[173:(nd - 194)]
  tmpdate <- as.numeric(tapply(Jday[Jcumcold <= (-t6Odays)], Jyear[Jcumcold <= (-t6Odays)], min))
  noow <- noow + (29 - length(tmpdate))
  firstow <- ((counter1 - 1) * firstow + mean(tmpdate)) / counter1

  return(list(
    cumcold = cumcold,
    ccd = ccd,
    cwd = cwd,
    lastdisp = lastdisp,
    firstdisp = firstdisp,
    firstemrg = firstemrg,
    Jyear = Jyear,
    Jday = Jday,
    Jcumcold = Jcumcold,
    tmpdate = tmpdate,
    noow = noow,
    firstow = firstow
  ))

}

#' First Geeneration
#'
#' @description  Get first generation information.
#'
#' @param npop population size
#' @param params values of parameters
#' @param stumpTemp stump temperature information
#' @return First Generation information
#' @export
getFirstGeneration <- function(npop, params, count, stumpTemp){

  t0m <- params$t0m
  t0sd <- params$t0sd
  t1m <- params$t1m
  t1sd <- params$t1sd
  t1lim <- params$t1lim
  th1 <- params$th1
  st10 <- stumpTemp$st10
  lavalst <- count$lavalst

  ## Iniital egg laying
  t0 <- rnorm(npop, t0m, t0sd)
  ## Initial egg development
  t1 <- rnorm(npop, t1m, t1sd)

  ## for each individual weevil
  for (j in 1:npop) {
    today <- round(t0[j]) # first day, using round function to round off estimate.
    start <- today
    dd <- 0 # number of day degrees
    rdd <- t1[j] # random value of day degrees threshold
    while (dd < rdd) { # update number of day degrees until dd is larger than rdd
      (today <- today + 1)
      (dd <- dd + max((st10[today] - th1), 0))
    }
    # implementing day threshold (if it took more than t1lim days, stop at t1lim days)
    if ((today - start) > t1lim) {
      today <- start + t1lim
    }

    lavalst[j] <- today ### in relation to laval stump temperature.
  }

  return(list(
    today = today,
    lavalst = lavalst
  ))

}

#' Collect statistics
#'
#' @description  Collect statistics
#' @param ntimes ntimes
#' @param npop npop
#' @param monthL monthL
#' @param monthP monthP
#' @param monthE monthE
#' @param monthR monthR
#' @param monthM monthM
#' @param monthO monthO
#' @param smt smt
#' @param count count
#' @param gemmonth gemmonth
#' @param gemwinter gemwinter
#' @param geewinter geewinter
#' @param geemonth geemonth
#' @export
collectStatistics <- function(ntimes, npop, monthL, monthP, monthE, monthR, monthM, monthO, smt, count,
                               gemmonth, gemwinter, geewinter, geemonth ){

  monthL <- monthL / (ntimes * npop)
  monthP <- monthP / (ntimes * npop)
  monthE <- monthE / (ntimes * npop)
  monthR <- monthR / (ntimes * npop)
  monthM <- monthM / (ntimes * npop)
  monthO <- monthO / (ntimes * npop)

  mean_temperature <- round(smt*100)/100
  mean_first_disp_day <- count$firstdisp
  mean_last_disp_day <- count$lastdisp
  mean_first_emer_day <- count$firstemrg

  mean_first_overw_day <- round(count$firstow) + 172
  pnoow <- 100 * count$noow / (30 * ntimes) # Percentage of year with no overwintering

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






