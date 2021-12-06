#' @export
#'
#' @description Larval Development
#'

runModule1 <- function(npop, data, params, count, stump_temp, first_generation, k, counter3){

  year <- data$year
  st10 <- stump_temp$st10
  st30 <- stump_temp$st30
  t0 <- params$t0
  th2 <- params$th2
  laval <- count$laval
  stagel <- count$stagel
  gstyear <- count$gstyear
  stagegg <- count$stagegg
  #rweevil <- count$rweevil

  gstyear[k] <- year[t0[counter3]]
  stagegg[k] <- first_generation$lavalst[counter3] - t0[counter3] # number of days in the egg stage
  #rweevil[k] <- 0                  # overwintering status (0 = no; 1 = before emergence; 2 = after emergence)

  today <- first_generation$lavalst[counter3]
  # get when larva emerged for insect j (weevil)
  start <- today
  dd <- 0             # day degrees is 0
  rdd <- params$t2[counter3]        # random day degrees requirement drawn from normal distribution

  if(runif(1) > 0.5) {  # with probability 0.5 run this loop (stump depth of 10cm)
    while(dd < rdd) {
      today <- today + 1
      dd <- dd + max((st10[today] - th2), 0)
    }
  } else {
    while(dd < rdd) {    # with probability 0.5 run this other loop (stump depth of 30cm)
      today <- today + 1
      dd <- dd + max((st30[today] - th2), 0)
    }
  }

  laval[counter3] <- today
  stagel[k] <- today - start # number of days in the larval stage
  start <- today


  return(list(
    k = k,
    today = today,
    laval = laval,
    start = start,
    gstyear = gstyear,
    stagegg = stagegg
    #rweevil = rweevil
  ))
}


#' @export
#'
#' @description Pupal Development
#'
#' @param params
#' @param today
#' @param stump_temp
#' @param counter3
#'
#'
runModule2 <- function(params, prep, today, stump_temp, counter3){

  t3ldays <- params$t3ldays
  t3udays <- params$t3udays
  t3 <- params$t3
  t3a <- params$t3a
  t3b <- params$t3b
  t3c <- params$t3c
  t3m <- params$t3m
  t4 <- params$t4
  th4 <- params$th4
  t4i <- params$t4i
  t4lim <- params$t4lim
  st <- stump_temp$st


  sum <- 0
  rdd <- t3ldays - 1
  for(k in 1:rdd) {                 # in relation to random number of day degrees.
    today <- today + 1
    sum <- sum + max(st[today], 0)
  }

  dd <- rdd
  err <- t3[counter3]
  rdd <- t3ldays

  while(dd < rdd && dd < t3udays) {
    dd <- dd + 1
    today <- today + 1
    sum <- sum + max(st[today],0)
    meant <- sum/dd
    rdd <- err*exp(t3a+t3c/(1+exp(t3b*(meant-t3m)))) # exponential logistic function as in appendice for prepupal development.

  }

  ###

  ## Pupal Development cycle
  cpupal <- 0
  ## Delay in pupal
  while(cpupal == 0) {
    while(st[today] < t4i) {
      today <- today + 1
    }
    prep[counter3] <- today
    ## Pupal Development
    cpupal <- 1
    laps <- 0          ##laps value.
    dd <- 0
    rdd <- t4[counter3]
    while(dd < rdd && laps < t4lim) {  ####in relation to days limit
      today <- today + 1
      laps <- laps + 1
      dd <- dd + max((st[today] - th4), 0)   ###max requirement for transition to stage
    }
    if(dd < rdd) {
      cpupal <- 0
      today <- prep[counter3] + 1
    }
  }

  return(list(
    today = today,
    prep = prep,
    cpupal = cpupal
  ))

}


#' @export
#'
#' @description Emergence
#'
#'
#'
#'

runModule3 <- function(npop, data, today, count, params, stump_temp, owr, counter1, counter3){

  month <- data$month
  year <- data$year

  t0 <- params$t0
  t1 <- params$t1
  t5days <- params$t5days
  t6Edays <- params$t6Edays
  t6Odays <- params$t6Odays
  cold <- stump_temp$cold

  rweevil <- count$rweevil
  gemtime <- count$gemtime
  gemwinter <- count$gemwinter
  gemmonth <- count$gemmonth
  emerge <- count$emerge
  gstyear <- count$gstyear

  ## Adult in pupal cell - melanisation       ###length of darknening period for weevil development.
  today <- today + t5days
  ## Check for immediate emergance
  k <- today - t6Edays + 1
  cwd <- sum(cold[k:today])
  if(cwd < t6Edays) {

    ## Emergence and overwintering

    if(owr[counter3]) {                     # in relation to overwintering requirements for insect (j)
      cwd <- 0                       # cumulative warm days
      ccd <- 0                       # cumulative cold days
      while(cwd < t6Edays) {               ###setting conditions in relation to 5 consecutative warm days
        if(ccd == t6Odays) {                ###and here in relation to 10 consecutative cold days (<9 degrees)
          owr[counter3] <- FALSE
          k <- (counter1 - 1) * npop + counter3
          rweevil[k] <- 1			  ###in relation to overwintering status of individual weevil as seen earlier.
        }
        if(cold[today] == 1) {
          cwd <- cwd + 1
          ccd <- 0
        } else if(cold[today] == -1) {
          ccd <- ccd + 1
          cwd <- 0
        } else {
          cwd <- 0
          ccd <- 0
        }
        today <- today + 1
      }
    } else {
      cwd <- 0
      while(cwd < t6Edays) {
        if(cold[today] == 1) {
          cwd <- cwd + 1
        } else {
          cwd <- 0
        }
        today <- today + 1
      }
    }
  }

  emerge[counter3] <- today              ###in relation to emergence of insect
  k <- (counter1 - 1) * npop + counter3
  gemtime[k] <- today - t0[counter3]        ### number of days to emergence above ground
  gemwinter[k] <- year[today] - gstyear[k]  ### number of winters until start of ovipositon and also related to ovipositon year for final generation.
  gemmonth[k] <- month[today]      ### month of emergence above ground.

  ## Check for overwintering
  if(owr[counter3]) {
    ccd <- 0
    while(ccd < t6Odays){
      if(cold[today] == -1) {
        ccd <- ccd + 1
      } else {
        ccd <- 0
      }
      today <- today + 1
    }		                   ##validating previous part of loop
    ## Re-emergence
    cwd <- 0
    while(cwd < t6Edays) {
      today <- today + 1
      if(cold[today]==1) {
        cwd <- cwd + 1
      } else {
        cwd <- 0
      }
    }
    k <- (counter1 - 1) * npop + counter3
    rweevil[k] <- 2
  }

  return(list(
    today = today,
    rweevil = rweevil,
    gemtime = gemtime,
    gemwinter = gemwinter,
    gemmonth = gemmonth,
    emerge = emerge
  ))

}


#' @export
#'
#' @description Maturation
#'
#' @import tidyverse
#'

runModule4 <- function(data, count, today, params, stumpTemp, counter3){

  t7 <- params$t7
  t6Edays <- params$t6Edays
  t6Odays <- params$t6Odays
  cold <- stump_temp$cold
  nfv <- params$nfv
  month <- data$month %>% as.numeric()
  mature <- count$mature

  rdd <- t7[counter3]
  feeddays <- 0      ####number of feed days specified by notional feeding value depending on time of year.
  ccd <- 0
  start <- today

  ## Feed while checking for hibernation
  while(feeddays < rdd && ccd < t6Odays) {      ### also in relation to cold and warm days for hibernations status and feeding.
    today <- today + 1
    if(cold[today] == 1) {
      feeddays <- feeddays + nfv[month[today]] ### month dependant on NFV inputted (see appendices)
      ccd <- 0
    } else if(cold[today] == -1) {
      ccd <- ccd + 1
    }
  }

  ## Check if feeding not complete (hibernation)
  if(feeddays < rdd) {                ###feedays less than random number of day degrees.
    ## Check for re-rmergance
    cwd <- 0
    while(cwd < t6Edays) {     ###less than 5 consecutative warm days to trigger emergence/re emergence. 9 degrees value importance.
      today <- today + 1
      if(cold[today] == 1) {
        cwd <- cwd + 1
      } else {
        cwd <- 0
      }
    }

    ## Continue feeding
    while(feeddays < rdd) {
      today <- today + 1
      if(cold[today] == 1) {
        feeddays <- feeddays + nfv[month[today]]
      }
    }
  }

  mature[counter3] <- today

  return(list(
    today = today,
    mature = mature,
    feeddays = feeddays
  ))

}


#' @export
#'
#' @description Oviposition
#'
#'
#'
#'
#'

runModule5 <- function(data, today, count, params, stump_temp, counter3){

  maxtemp <- data$maxtemp
  t1 <- params$t1
  t1lim <- params$t1lim
  th1 <- params$th1
  th8 <- params$th8
  t9d <- params$t9d
  t6Odays <- params$t6Odays
  eggdays <- params$eggdays
  st10 <- stump_temp$st10
  cold <- stump_temp$cold

  eggsel <- 0
  while(eggsel == 0) {          ###in relation to egg selection
    while(maxtemp[today] < th8) {
      today <- today + 1
    }
    l <- 0
    ccd <- 0
    while(l < t9d && ccd < t6Odays) {          ###t9d referring to 78 days of development.
      ####Occurs over a total of 78 days when mean air and 10 cm r-s temperature >9C (egg laying)
      today <- today + 1
      if(cold[today] == 1) {
        ccd <- 0
        l <- l + 1
        eggdays[l] <- today          ###egg days in relation to egg days of development.
      } else if(cold[today] == -1) {
        ccd <- ccd + 1
      }
    }
    ## Select and check for viable egg development
    if(l > 0) {
      if(l > 1) {
        eggwts <- l - c(1:l)/(l + 1)          ##egg weights
        eggdist <- sort(runif(l) * eggwts, decreasing = TRUE, index.return = TRUE) ###distribution of egg locations.
      }
      if(l == 1) {
        eggdist <- 1
      }
      k <- 0
      while(k < l && eggsel == 0) {
        k <- k + 1
        if(l > 1) {
          eggi <- eggdist$ix[k]
        } else {
          eggi <- 1
        }
        est <- eggdays[eggi]
        edd <- 0
        dd <- 0
        rdd <- t1[counter3]
        while(dd < rdd && edd < t1lim) {
          edd <- edd + 1
          dd <- dd + max((st10[edd + est] - th1), 0)
        }
        if(dd >= rdd) {
          eggdev <- edd + est
          eggsel <- eggi
        }
      }
    }
  }

  return(list(
    today = today,
    eggdev = eggdev,
    eggsel = eggsel,
    eggdays = eggdays,
    eggdist = eggdist,
    eggwts = eggwts
  ))
}



