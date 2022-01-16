
#' PineR
#' @description A function to predict the life cycle of the Hylobius Abietis (Pine weevil).
#'
#' @param npop Population size.
#' @param ntimes Number of weather generator runs.
#' @param ngen Number of generations.
#' @param data Data containing the minimum, maximum and average temperatures, as well as day, month and year.
#' @param species Species of the three.
#' @param depth Stump depth (1 = 10cm, 2 = 20cm, 3 = 30cm)
#' @param owp Percentage that require overwintering
#' @param seed Seed for random number generator
#'
#' @return A list of objects.
#' @export
piner <- function(npop = 100,
                  ntimes = 1,
                  ngen = 5,
                  data,
                  species = "pine",
                  depth = 2,
                  owp = 100,
                  params = TRUE,
                  seed = 2021){

  if (seed > 1) {
    set.seed(seed)
  }

  if (owp < 0 | owp > 100) {
    stop("Overwintering percentage ('owp') should be bounded between 0 and 100")
  }

  if(params){
    params <- setParameters(species = species)
  }else{
    params <- params
  }

  count <- setCounters(npop = npop, ntimes = ntimes)

  # Read data

  temp <- data$temp
  maxtemp <- data$max_temp # NOTE HERE
  day <- data$day
  month <- data$month
  year <- data$year
  nd <- length(temp)
  nwarn <- 0.9 * nd
  smt <- mean(temp)

  # Initialize vectors
  lavalst <- count$lavalst
  gstmonth <- count$gstmonth
  stageov <- count$stageov
  ovipos <- count$ovipos
  geetime <- count$geetime
  geewinter <- count$geewinter
  geemonth <- count$geemonth
  monthL <- count$monthL
  monthP <- count$monthP
  monthE <- count$monthE
  monthR <- count$monthR
  monthM <- count$monthM
  monthO <- count$monthO

  firstdisp <- 0
  lastdisp <- 0
  firstemrg <- 0
  firstow <- 0
  noow <- 0

  for (i in 1:ntimes) {

   # stump_temp <- getStumpTemp(data = data, params = params, depth = depth, count = count, counter1 = i)
    stump_temp <-  getStumpTemp(depth = depth, nd = nd, temp = temp, th6 = params$th6)

    count$firstdisp <- stump_temp$firstdisp
    count$lastdisp <- stump_temp$lastdisp
    count$firstemrg <- stump_temp$firstemrg
    count$firstow <- stump_temp$firstow
    count$noow <- stump_temp$noow

    first_generation <- getFirstGeneration(npop = npop, params = params, count = count, stumpTemp = stump_temp)

    for (j in 2:ngen) {
      t0 <- params$t0
      t1 <- params$t1
      t2 <- params$t2
      t3 <- params$t3
      t4 <- params$t4
      t5 <- params$t5
      t6 <- params$t6
      t7 <- params$t7

      if (owp == 0) {
        owr <- rep(FALSE, npop) # nobody overwinters
      } else if (owp < 100) {
        owr <- (runif(npop) <= owp / 100) # some % overwinter
      } else {
        owr <- rep(TRUE, npop)
      } # all overwinter

      for (l in 1:npop) {

        ## Store egg development time
        k <- (i-1) * npop + l

        module1 <- module1(npop = npop, data = data, params = params, count = count,
                               stump_temp = stump_temp, first_generation = first_generation,
                               k = k, counter3 = l)
        start <- module1$start
        today <- module1$today
        laval <- module1$laval
        gstyear <- module1$gstyear
        stagegg <- module1$stagegg
        rweevil <- module1$rweevil

        module2 <- run_module2(params = params, prep = count$prep, today = today, stump_temp = stump_temp, counter3 = l)
        today <- module2$today
        prep <- module2$prep

        count$stagepp[k] <- prep[l] - start       ####in relation to number of days in prepupal stage
        count$stagepl[k] <- today - prep[l]       ### in relation to number of days in pupal stage
        start <- today

        module3 <- run_module3(npop = npop, data = data, today = today, count = count, params = params,
                               stump_temp = stump_temp, owr = owr, counter1 = i, counter3 = l)
        today <- module3$today
        rweevil <- module3$rweevil
        gemtime <- module3$gemtime
        gemwinter <- module3$gemwinter
        gemmonth <- module3$gemmonth
        emerge <- module3$emerge

        module4 <- run_module4(data = data, count = count, today = today, params = params, stump_temp = stump_temp, counter3 = l)
        today <- module4$today
        mature <- module4$mature
        feeddays <- module4$feeddays

        module5 <- run_module5(data = data, today = today, count = count, params = params, stump_temp = stump_temp, counter3 = l)
        today <- module5$today
        eggdev <- module5$eggdev
        eggsel <- module5$eggsel
        eggdays <- module5$eggdays
        eggdist <- module5$eggdist
        eggwts <- module5$eggwts

        lavalst[l] <- eggdev
        gstmonth[k] <- month[t0[l]]          ###in relation to the oviposition month for start of final generation.
        geetime[k] <- eggdays[1] - t0[l]     ####number of days to emergence above ground.
        geewinter[k] <- year[eggdays[1]] - gstyear[k] ### in relation to number of winters until start of ovipostion and year of of oviposition for final weevil generation.
        geemonth[k] <- month[eggdays[1]] ### in relation to month of emergence above ground
        stageov[k] <- eggdays[1] - start ### in relation to number of days from completion of overwintering to start of oviposition.
        ovipos[l] <- eggdays[1]      ###oviposition and egg days required
        t0[l] <- eggdays[eggsel]    ### threshold 0 for insect and egg days on egg selection.

        if (max(t0) > nwarn) {
          print("WARNING - near end of temperature data", quote = FALSE)
        }

      }

      for (l in 1:npop) {
        k <- month[laval[l]]
        monthL[k] <- monthL[k] + 1
        k <- month[prep[l]]
        monthP[k] <- monthP[k] + 1
        monthE[k] <- monthE[k] + 1
        k <- month[mature[l]]
        monthM[k] <- monthM[k] + 1
        k <- month[ovipos[l]]
        monthO[k] <- monthO[k] + 1
      }

    }


  }

  #browser()
  stats <- collect_statistics(ntimes = ntimes, npop = npop, monthL = monthL, monthP = monthP, monthE = monthE,
                              monthR = monthR, monthM = monthM, monthO = monthO, smt = smt, count = count,
                              gemmonth = gemmonth, gemwinter = gemwinter, geewinter = geewinter, geemonth = geemonth)

  return(list(
    stats = stats,
    mature = mature,
    lavalst = lavalst,
    gstmonth =  gstmonth,
    geetime = geetime,
    geewinter = geewinter,
    geemonth = geemonth,
    stageov = stageov,
    rweevil = rweevil
  ))

}



