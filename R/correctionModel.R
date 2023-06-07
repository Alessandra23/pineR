#' Correction Model Function
#'
#' This function modifies the class of the input data.frame to match the
#' model specified and then attempts to generate predictions.
#'
#' @param data A data.frame containing the dataset to be used.
#' @param model Character. The model to be used for correction.
#'        The available options are 'ranger', 'bart', 'xgboost', 'nn', 'svm', 'lasso', and 'lm'.
#'        The default model is 'ranger'.
#' @param seed Numeric. The seed for reproducibility. Not used in this function but might be used in the 'getPredicts' function.
#' @param ... Additional arguments to be passed to the model's function.
#'
#' @return This function returns the result of calling 'getPredicts' on the input data.
#'
#' @examples
#' \dontrun{
#' corrected_data <- correctionModel(data = mtcars, model = "xgboost", seed = 123)
#' }
#'
#' @importFrom ranger ranger
#' @importFrom dbarts bart
#' @importFrom xgboost xgb.DMatrix
#' @importFrom xgboost xgboost
#' @importFrom neuralnet neuralnet
#' @importFrom neuralnet compute
#' @importFrom e1071 svm
#' @importFrom glmnet glmnet
#' @importFrom glmnet cv.glmnet
#'
#'
#' @export
correctionModel <- function(data,
                            model = c('ranger', 'bart', 'xgboost', 'nn', 'svm', 'lasso', 'lm'),
                            seed, ...){

  modelArg <- match.arg(model)

  switch(modelArg,
         "ranger" = {
           class(data) <- c('data.frame', 'ranger')
         },
         "bart" = {
           class(data) <- c('data.frame', 'bart')
         },
         "xgboost" = {
           class(data) <- c('data.frame', 'xgboost')
         },
         "nn" = {
           class(data) <- c('data.frame', 'nn')
         },
         "svm" = {
           class(data) <- c('data.frame', 'svm')
         },
         "lasso" = {
           class(data) <- c('data.frame', 'lasso')
         },
         "lm" = {
           class(data) <- c('data.frame', 'lm')
         })

  p <-  getPredicts(data = data, seed = seed)

  return(p)
}



getPredicts <- function(data, seed, ...){
  UseMethod('getPredicts', data)
}


# Ranger
getPredicts.ranger <- function(data, seed = 022, num.trees = 100, mtry = 2,
                               importance = 'permutation'){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- ranger::ranger(y~.,
                            data = training_set,
                            num.trees = num.trees,
                            mtry = mtry,
                            importance = importance)
    predictions[i] <- predict(model, data = validation_set[, -1])$predictions
  }
  return(predictions)
}


# BART
getPredicts.bart <- function(data, seed = 022, ntree = 100){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- dbarts::bart(x.train = training_set[, -1],
                          y.train = training_set[, 1],
                          ntree = ntree,
                          keeptrees = TRUE)

    predictions[i] <- mean(predict(model, newdata = validation_set[, -1]))
  }
  return(predictions)
}

# xgboost
getPredicts.xgboost <- function(data, seed, max.depth, nrounds){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    training_x = data.matrix(training_set[, -1])
    training_y = training_set[,1]

    validation_x = data.matrix(validation_set[, -1])
    validation_y = validation_set[, 1]

    xgb_training = xgboost::xgb.DMatrix(data = training_x, label = training_y)
    xgb_validation = xgboost::xgb.DMatrix(data = validation_x, label = validation_y)

    model <- xgboost::xgboost(data = xgb_training, max.depth = max.depth, nrounds = nrounds)

    predictions[i] <- predict(model, newdata = xgb_validation)
  }
  return(predictions)
}

# Neural Networks
getPredicts.nn <- function(data, seed = 022, hidden = 4, algorithm = "rprop+"){

  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    col_list <- paste(c(colnames(training_set[,-1])),collapse="+")
    col_list <- paste(c("y~",col_list),collapse="")
    f <- formula(col_list)

    set.seed(seed)
    model <- neuralnet::neuralnet(f, data = training_set,
                                  hidden = hidden,
                                  algorithm = algorithm)

    predictions[i] <- neuralnet::compute(model, validation_set[,-1])$net.result[1,1]
  }
  return(predictions)
}


# Support vector machine
getPredicts.svm <- function(data, seed = 022){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- e1071::svm(y ~ ., data = data)

    predictions[i] <- predict(model, newdata = validation_set[, -1])
  }
  return(predictions)
}

# Lasso regression
getPredicts.lasso <- function(data, seed = 022){

  y <- data[,1]
  x <- data.matrix(data[,-1])

  cv <- glmnet::cv.glmnet(x, y, alpha = 1)
  lambda <- cv$lambda.min

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_x = x[-i, ]
    training_y = y[-i]

    validation_x = x[i, ]
    validation_y = y[i]

    model <- glmnet::glmnet(training_x, training_y, alpha = 1, lambda = lambda)

    predictions[i] <- predict(model, newx = validation_x, s = lambda)
  }
  return(predictions)

}

# Linear regression
getPredicts.lm <- function(data, seed = 022){

  set.seed(seed)
  predictions <- numeric(nrow(data))
  for (i in 1:nrow(data)) {

    training_set <- data[-i, ]
    validation_set <- data[i, ]

    model <- lm(y ~ ., data = data)

    predictions[i] <- mean(predict(model, newdata = validation_set[, -1]))
  }
  return(predictions)
}



