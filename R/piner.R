#' piner
#'
#' Pine Weevil Simulation Model
#'
#' @description The piner function simulates the life cycle of the pine weevil,
#' Hylobius abietis, using a weather generator and user-defined parameters.
#' The model simulates population dynamics, including overwintering, emergence,
#' dispersal, and oviposition. It returns summary statistics and can be used to
#' understand how the life cycle of the pine weevil is affected by climate and other factors.
#'
#' @param data A data frame containing daily weather data with columns for
#' minimum temperature, maximum temperature, day, month, and year.
#' @param npop An integer specifying the number of individuals in the population (default: 1000).
#' @param ntimes An integer specifying the number of iterations for the simulation (default: 10).
#' @param ngen An integer specifying the number of generations to simulate (default: 1).
#' @param species A character string or an integer specifying the tree species.
#' Where 1 and 2 represent, "pine" (default) and "spruce", respectively.
#' @param depth A numeric value representing the depth of the tree stump in meters (default: 1).
#' @param owp A numeric value between 0 and 100 specifying the overwintering percentage (default: 100).
#' @param output An integer specifying the output. Where 0 is no output, 1 prints summary statistics,
#' and 2 prints summary statistics along with outputting egg emergence time plots.
#' @param seed An integer for the random seed to ensure reproducibility of the simulation.
#'
#' @return A list of summary statistics, including mean temperature, mean first dispersal day,
#' mean last dispersal day, mean first emergence day, mean first overwintering day,
#' and the percentage of the year with no overwintering.
#' Additionally, summary statistics for life cycle stages are provided, such as the
#' proportion of individuals with 0, 1, 2, 3, and 4 or more years in their life cycle.
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom graphics par
#' @importFrom graphics hist
#'
#'
#' @examples
#' \dontrun{
#' # Load weather data
#' weather_data <- read.csv("path/to/weather_data.csv")
#'
#' # Run the pine weevil simulation with default parameters
#' piner(data = weather_data)
#'
#' # Run the pine weevil simulation with custom parameters
#' piner(data = weather_data, npop = 500, ntimes = 5, species = "spruce", owp = 75)
#' }
#'
#' @export


piner <- function(npop = 1000,
                  ntimes = 10,
                  ngen = 1,
                  data,
                  species = 1,
                  depth = 1,
                  owp = 100,
                  output = 1,
                  seed = 1) {


  ##########################################
  ## Initialization/setting up parameters ##
  ##########################################


  ## Set up empty vectors
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

  # -------------------------------------------------------------------------

  # Set seed
  if (seed > 1) {
    set.seed(seed)
  }

  # Warning message
  if (owp < 0 | owp > 100) {
    stop("Overwintering percentage ('owp') should be bounded between 0 and 100")
  }

  # Select species
  if(is.numeric(species)){
    if (species == 1) {
      species <- "pine"
    } else if (species == 2) {
      species <- "spruce"
    }
  }

  # Set the parameters
  params <- setParameters(species = species)

  # Initialize weather stats
  firstdisp <- 0
  lastdisp <- 0
  firstemrg <- 0
  firstow <- 0
  noow <- 0

  ###################
  ## End of set up ##
  ###################

  # -------------------------------------------------------------------------

  ###################################
  ## Loop over weather simulations ##
  ###################################
  pb <- txtProgressBar(min = 0, max = ntimes, style = 3) # set progress bar
  for (i in 1:ntimes) {
    setTxtProgressBar(pb, i) # set progress bar
    temp <- (data$min_temp + data$max_temp) / 2
    maxtemp <- data$max_temp
    day <- data$day
    month <- data$month
    year <- data$year
    nd <- length(temp)
    nwarn <- 0.9 * nd

    ## Compute stump temperatures
    stump_temp <- getStumpTemp(depth = depth, nd = nd, temp = temp, th6 = params$th6)

    ## Initial oviposition and egg development (Stages 0 and 1)

    # Iniital egg laying
    t0 <- rnorm(npop, params$t0m, params$t0sd)
    # Initial egg development
    t1 <- rnorm(npop, params$t1m, params$t1sd)

    # 1st generation
    first_generation <- getFirstGeneration(
      npop = npop,
      t0 = t0,
      t1 = t1,
      params = params,
      stump_temp = stump_temp,
      lavalst = lavalst
    )


    ###########################
    ## Loop over generations ##
    ###########################

    for (gen in 1:ngen) {

      ## Generate population values
      t1 <- rnorm(npop, params$t1m, params$t1sd)
      t2 <- rnorm(npop, params$t2m, params$t2sd)
      t3 <- rnorm(npop, params$t3m, params$t3sd)
      t3 <- ((1 / runif(npop)) - 1)**params$t3sd
      t4 <- rnorm(npop, params$t4m, params$t4sd)
      t7 <- params$t7m * ((1 / runif(npop)) - 1)**params$t7sd
      t7 <- params$t7days * (t7 < params$t7days) + t7 * (t7 >= params$t7days)

      ## Set up overwinter requirement
      if (owp <= 0) {
        owr <- rep(F, npop)  # nobody overwinters
      } else if (owp < 100) {
        owr <- (runif(npop) <= owp / 100) # some % overwinter
      } else {
        owr <- rep(T, npop) # all overwinter
      }

      ##########################
      ## Loop over population ##
      ##########################

      for (j in 1:npop) {

        # Run module 1 (Egg and Larval development - Stage 1 and 2)
        mod1 <- module1(
          npop = npop,
          data = data,
          params = params,
          stump_temp = stump_temp,
          first_generation = first_generation,
          year = year,
          i = i,
          j = j,
          t0 = t0,
          gstyear = gstyear,
          stagegg = stagegg,
          rweevil = rweevil,
          laval = laval,
          stagel = stagel,
          t2 = t2)

        # Return values
        start <- mod1$start
        today <- mod1$today
        laval <- mod1$laval
        gstyear <- mod1$gstyear
        stagegg <- mod1$stagegg
        rweevil <- mod1$rweevil




        ## Run module 2 (Prepupal development - Stage 3 and 4)
        mod2 <- module2(
          prep = prep,
          today = today,
          stump_temp = stump_temp,
          params = params,
          t3 = t3,
          t4 = t4,
          j = j
        )

        # Return values
        prep <- mod2$prep
        today <- mod2$today


        ## Run modeule 3 (Emergence - Stage 5)
        mod3 <- module3(
          npop = npop,
          params = params,
          stump_temp = stump_temp,
          today = today,
          prep = prep,
          start = start,
          stagepp = stagepp,
          stagepl = stagepl,
          owr = owr,
          emerge = emerge,
          i = i,
          j = j,
          gemtime = gemtime,
          gemtime_new = gemtime_new,
          gemwinter = gemwinter,
          gemmonth = gemmonth,
          year = year,
          gstyear = gstyear,
          month = month,
          t0 = t0,
          rweevil = rweevil)

        # Return values
        today <- mod3$today
        rweevil <- mod3$rweevil
        gemtime <- mod3$gemtime
        gemwinter <- mod3$gemwinter
        gemmonth <- mod3$gemmonth
        emerge <- mod3$emerge
        ## NOTE: End of overwintering


        # Run module 4 (Maturation - Stage 6)
        mod4 <- module4(
          t7 = t7,
          params = params,
          stump_temp = stump_temp,
          today = today,
          month = month,
          j = j
        )

        # Return values
        today <- mod4$today
        mature[j] <- today


        # Run module 5 (Oviposition - Stage 7,8, and 9)
        mod5 <- module5(
          today = today,
          params = params,
          maxtemp = maxtemp,
          stump_temp = stump_temp,
          t1 = t1,
          j = j
        )

        # Return values
        today <- mod5$today
        eggdev <- mod5$eggdev
        eggsel <- mod5$eggsel
        eggdays <- mod5$eggdays
        eggdist <- mod5$eggdist
        eggwts <- mod5$eggwts

        #########################################################
        ## End of dispersal and ovipostion and egg development ##
        #########################################################

        ## Set results
        lavalst[j] <- eggdev
        k <- (i - 1) * npop + j
        gstmonth[k] <- month[t0[j]]
        geetime[k] <- eggdays[1] - t0[j]
        geewinter[k] <- year[eggdays[1]] - gstyear[k]
        geemonth[k] <- month[eggdays[1]]
        stageov[k] <- eggdays[1] - start
        ovipos[j] <- eggdays[1]
        t0[j] <- eggdays[eggsel]
      }
      ##############################################
      ## End of individual population (npop) loop ##
      ##############################################


      # Warning
      if (max(t0) > nwarn) {
        print("WARNING - near end of temperature data", quote = FALSE)
      }
    }
    ##################################
    ## End of generation (gen) loop ##
    ##################################

    # Fill in month vectors with results
    for (j in 1:npop) {
      k <- month[laval[j]]
      monthL[k] <- monthL[k] + 1
      k <- month[prep[j]]
      monthP[k] <- monthP[k] + 1
      k <- month[emerge[j]]
      monthE[k] <- monthE[k] + 1
      k <- month[mature[j]]
      monthM[k] <- monthM[k] + 1
      k <- month[ovipos[j]]
      monthO[k] <- monthO[k] + 1
    }
  }
  close(pb)

  #######################################
  ##  End of simulations (ntimes) loop ##
  #######################################
  #######################################
  #######################################

  # -------------------------------------------------------------------------

  ####################
  ## Prepare output ##
  ####################

  # get stump statistics
  stump_stats <- getStumpStats(
    nd = nd,
    temp = temp,
    maxtemp = maxtemp,
    day = day,
    year = year,
    firstdisp = firstdisp,
    lastdisp = lastdisp,
    firstemrg = firstemrg,
    firstow = firstow,
    noow = noow,
    cold = stump_temp$cold,
    params = params,
    cumcold = stump_temp$cumcold,
    i = i)

  # Get general statistics
  stats <- collectStatistics(ntimes = ntimes,
                              npop = npop,
                              monthL = monthL,
                              monthP = monthP,
                              monthE = monthE,
                              monthR = monthR,
                              monthM = monthM,
                              monthO = monthO,
                              SiteMeanTemp = stump_stats$SiteMeanTemp,
                              gemmonth = gemmonth,
                              gemwinter = gemwinter,
                              geewinter = geewinter,
                              geemonth = geemonth,
                              firstdisp =  firstdisp,
                              lastdisp =   lastdisp,
                              firstemrg =  firstemrg,
                              firstow = firstow,
                              noow = noow)

  # What to output to screen (0 = no output, 1 = text, 2 = text and plot)

  # Small function to control what is printed on screen
  printFun <- function(){
    for(z in 1:4){
      tmp <- round(sum(geewinter == (z-1)) / (ntimes * npop), 3)
      print(paste("Propn", z-1,  "yr life cycle = ", tmp), quote = FALSE)
    }
    tmp <- round(sum(geewinter > 3) / (ntimes * npop), 3)
    print(paste("Propn 4 or more yr life cycle = ", tmp), quote = FALSE)
  }

  # Set output
  if(output == 1){
    printFun()
  }else if(output == 2){
    printFun()
    par(mfrow = c(1, 2))
    hist(gemtime, main = "Egg to Emergance Time")
    hist(geetime, main = "Egg to Egg Time")
    par(mfrow = c(1, 1))
  }

  # Update values
  geetime_new <- geetime + t0
  gemtime_new_t0 <- gemtime + t0


# -------------------------------------------------------------------------

  ####################
  ## Return results ##
  ####################

  my_list <- list(
    stats = stats,
    gstyear = gstyear,
    gstmonth = gstmonth,
    geetime = geetime,
    geemonth = geemonth,
    geewinter = geewinter,
    gemtime = gemtime,
    gemmonth = gemmonth,
    gemwinter = gemwinter,
    rweevil = rweevil,
    stagegg = stagegg,
    stagel = stagel,
    stagepp = stagepp,
    stagepl = stagepl,
    stageov = stageov,
    geetime_new = geetime_new,
    gemtime_new = gemtime_new,
    gemtime_new_t0 = gemtime_new_t0,
    dfMonths = stats$df_months,
    stumpT = stump_temp$stumpT
  )

  return(my_list)
}
