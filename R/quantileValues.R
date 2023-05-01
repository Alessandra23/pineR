#' quantile_values
#'
#' Calculate Quantile Values with Optional Correction. This function calculates the quantile values
#' for a given dataset and set of parameters, with the option to apply a correction.
#' It returns a data frame containing the quantile values for each day.
#'
#' @param data A data frame containing the input data.
#' @param N An integer representing the number of replicates to use in the simulation.
#' @param npop An integer representing the population size.
#' @param ntimes An integer representing the number of time steps in the simulation.
#' @param ngen An integer representing the number of generations in the simulation (default is 1).
#' @param species A character string specifying the species.
#' @param depth A numeric value representing the depth.
#' @param daysDiff An integer representing the difference in days between the start of the data and the start of the simulation.
#' @param correction A logical value indicating whether to apply a correction (default is TRUE).
#'
#' @return A data frame containing the quantile values for each day with columns: day, ql (lower quantile), qu (upper quantile), and qm (median quantile).
#'
#' @importFrom stats ecdf
#' @importFrom stats quantile
#'
#' @export
quantileValues <- function(data,
                           N,
                           npop,
                           ntimes,
                           ngen = 1,
                           species,
                           depth,
                           daysDiff,
                           correction = TRUE){



  if(correction){
    class(data) <- c('data.frame', 'correction')
  }else{
    class(data) <- c('data.frame', 'noCorrection')
  }


  final_values <- val_method(data = data,
                             N = N,
                             npop = npop,
                             ntimes = ntimes,
                             ngen = ngen,
                             species = species,
                             depth = depth,
                             daysDiff = daysDiff,
                             correction = correction)

  return(final_values)
}


# -------------------------------------------------------------------------


# Main method function:
val_method <- function(data,
                 N, npop, ntimes, ngen = 1, species, depth, daysDiff, correction = TRUE,
                 ...) {
  UseMethod("val_method", data)
}


# -------------------------------------------------------------------------



# Applying correction -----------------------------------------------------

val_method.correction <- function(data, N, npop, ntimes, ngen = 1, species, depth, daysDiff, correction) {
  rep <- replicate(N, piner(
    npop = npop, ngen = ngen, ntimes = ntimes, data = data,
    species = species, depth = depth, owp = 100,
    output = 0,
    seed = 0
  ))

  # 1095 or 1096 (we have 3 years)
  nRow <- nrow(data)
  list_rep <- rep["gemtime_new_t0", ]
  list_rep <- lapply(list_rep, function(x) (x - nRow + daysDiff))

  # n_row <- range(emer_true$day)
  n_row <- range(list_rep)
  matrix_edcf <- matrix(NA, ncol = N, nrow = n_row[2] - n_row[1] + 1)
  for (i in 1:N) {
    ecdf_list <- ecdf(list_rep[[i]])
    matrix_edcf[, i] <- ecdf_list(n_row[1]:n_row[2])
  }

  matrix_quantile <- apply(matrix_edcf, 1, quantile, c(0.025, 0.5, 0.975))
  quantiles_df <- data.frame(
    day = seq(n_row[1], n_row[2], 1),
    ql = matrix_quantile[1, ],
    qu = matrix_quantile[3, ],
    qm = matrix_quantile[2, ]
  )
  return(quantiles_df)
}


# No correction -----------------------------------------------------------

val_method.noCorrection <- function(data, N, npop, ntimes, ngen = 1, species, depth, daysDiff, correction) {
  rep <- replicate(N, piner(
    npop = npop, ngen = ngen, ntimes = ntimes, data = data,
    species = species, depth = depth, owp = 100,
    output = 0,
    seed = 0
  ))

  # 1095 or 1096 (we have 3 years)
  nRow <- nrow(data)
  list_rep <- rep["gemtime_new_t0", ]
  list_rep <- lapply(list_rep, function(x) (x - nRow))

  # n_row <- range(emer_true$day)
  n_row <- range(list_rep)
  matrix_edcf <- matrix(NA, ncol = N, nrow = n_row[2] - n_row[1] + 1)
  for (i in 1:N) {
    ecdf_list <- ecdf(list_rep[[i]])
    matrix_edcf[, i] <- ecdf_list(n_row[1]:n_row[2])
  }

  matrix_quantile <- apply(matrix_edcf, 1, quantile, c(0.025, 0.5, 0.975))
  quantiles_df <- data.frame(
    day = seq(n_row[1], n_row[2], 1),
    ql = matrix_quantile[1, ],
    qu = matrix_quantile[3, ],
    qm = matrix_quantile[2, ]
  )
  return(quantiles_df)
}
