piner.eobs <- function(coords,
                       profileTemp,
                       npop = 100,
                       ntimes = 1,
                       ngen = 5,
                       data,
                       species = "pine",
                       depth = NULL,
                       owp = 100,
                       params = TRUE,
                       seed = 2021){

  if(profileTemp<9){
    profile <- normal
  }else{
    if(profileTemp > 20){
      profile <- warm
    }else{
      profile <- normal
    }
  }

  if(depth = NULL){
    model <- piner(npop = npop,
                   ntimes = ntimes,
                   ngen = ngen,
                   data = data,
                   species = species,
                   depth = depth,
                   owp = owp,
                   params = params,
                   seed = seed)
  }else{
    modelDepth1 <- piner(npop = npop,
                   ntimes = ntimes,
                   ngen = ngen,
                   data = data,
                   species = species,
                   depth = 1,
                   owp = owp,
                   params = params,
                   seed = seed)

    modelDepth2 <- piner(npop = npop,
                         ntimes = ntimes,
                         ngen = ngen,
                         data = data,
                         species = species,
                         depth = 2,
                         owp = owp,
                         params = params,
                         seed = seed)

    modelDepth3 <- piner(npop = npop,
                         ntimes = ntimes,
                         ngen = ngen,
                         data = data,
                         species = species,
                         depth = 3,
                         owp = owp,
                         params = params,
                         seed = seed)
  }


  modelRegression <- lm(model$gemtime~ soil+apect+depth+altitute)

  return(modelCorrected)


}
