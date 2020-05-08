library(forecast)
library(Metrics)

################################ MODELS ######################################


# fit standard naive
fit_naive <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fcast <- naive(train[,column], h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}

# fit seasonal naive (frequency is important!)
fit_snaive <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data[,column], frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(data)-horizon))
  test <- tail(ts, n=horizon)
  fcast <- snaive(train, h=horizon)
  actual <- as.numeric(test)
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}

# fit arima no xreg
fit_arima <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fit <- auto.arima(train[,column])
  #arimaorder(fit)
  fcast <- forecast(fit, h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}

# fit ets model
fit_ets <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fit <- ets(train[,column])
  fcast <- forecast(fit, h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}

# fit TBATS method
fit_tbats <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fit <- tbats(train[,column])
  fcast <- forecast(fit, h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}

# regression with arima errors (ARIMAX)
fit_arima_errors <- function(data, horizon, frequency,column) {
  
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  
  test <- tail(ts, n=horizon)
  # get covariates names
  reg <- names(data) 
  covariates <- reg[3:length(reg)]
  xreg_train <- data.frame(train[,covariates])
  names(xreg_train) <- covariates
  fit <- auto.arima(train[,column], xreg=as.matrix(xreg_train))
  xreg_test <- data.frame(test[,covariates])
  names(xreg_test) <- covariates
  fcast <- forecast(fit, h=horizon, xreg=as.matrix(xreg_test))
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}


# neural network autoregression
fit_nnetar <- function(data, horizon, frequency,column) {
  set.seed(123)
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fit <- nnetar(train[,column])
  fcast <- forecast(fit, h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(model=fit, actual=actual, predicted=predicted))
}

# neural network autoregression with lagged variables and PROMO_FLAG as predictors
fit_nnetar_xreg <- function(data, horizon, frequency,column) {
  set.seed(123)
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  # get covariates names
  reg <- names(data) 
  # get covariates names
  reg <- names(data) 
  covariates <- reg[2:length(reg)]
  xreg_train <- data.frame(train[,covariates])
  names(xreg_train) <- covariates
  fit <- nnetar(train[,column], xreg=xreg_train, MaxNWts=84581)
  xreg_test <- data.frame(test[,covariates])
  names(xreg_test) <- covariates
  fcast <- forecast(fit, h=horizon, xreg=xreg_test)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(model=fit, actual=actual, predicted=predicted))
}


library(quadprog)

# solve the optimization problem
combine_with_cls <- function(X, y) {
  
  # y is the vector of actual values
  # X is the matrix where x_ij is the forecast at the time step i obtained with method j
  
  n_forecasts <- nrow(X)
  n_methods <- ncol(X)
  
  A <- matrix(NA, nrow=n_methods+1, ncol=n_methods)
  
  # b_1 + ... + b_(n_methods) = 1
  eq_constraint <- rep(1, n_methods)
  
  A[1, ] <- eq_constraint
  
  methods_fact <- factor(1:n_methods)
  # b_1 >= 0, ..., b_(n_methods) >= 0
  neq_contraints <- model.matrix(~methods_fact+0)
  
  A[2:nrow(A), ] <- neq_contraints
  
  b <- c(1, rep(0, n_methods))
  D <- t(X) %*% X
  A <- t(A)
  d <- as.vector(t(y) %*% X)
  
  sol <- solve.QP(D, d, A, b, meq=1)
  #sol$solution to obtain weights
  return (sol$solution)
  
}

# Combination of auto.arima, ets and nnetar, @mode must be "average" or "cls"
# The training set will be used to train the individual models. The test set is again split
# into two parts, the first part out of the two is used to train the combination scheme, save
# for the simple average which does not need any training
fit_combination <- function(data, horizon, frequency, mode,column) {
  set.seed(123)
  
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  
  methods <- 3
  
  arima_model <- auto.arima(train[,column])
  ets_model <- ets(train[,column])
  nnetar_model <- nnetar(train[,column])
  
  if (mode == "cls") {
    
    train <- head(train, n=(nrow(train)-horizon))
    val <- tail(train, n=horizon)
    
    arima_model_val <- auto.arima(train[,column])
    ets_model_val <- ets(train[,column])
    nnetar_model_val <- nnetar(train[,column])
    
    # constrained least squares
    arima_forecast_val <- as.numeric(forecast(arima_model_val, h=horizon)$mean)
    ets_forecast_val <- as.numeric(forecast(ets_model_val, h=horizon)$mean)
    net_forecast_val <- as.numeric(forecast(nnetar_model_val, h=horizon)$mean)
    
    X <- matrix(c(arima_forecast_val, ets_forecast_val, net_forecast_val), horizon, methods)
    y <- as.numeric(val[,"Y"])
    weights <- tryCatch(combine_with_cls(X, y), error=function(err) {print(err) ; return (rep(1/methods, methods))})
  }
  
  else if (mode == "average") {
    
    # average combination
    weights <- rep(1/methods, methods)
    
  }
  
  else stop("Invalid combination mode!")
  
  print(weights)
  
  arima_forecast <- as.numeric(forecast(arima_model, h=horizon)$mean)
  ets_forecast <- as.numeric(forecast(ets_model, h=horizon)$mean)
  net_forecast <- as.numeric(forecast(nnetar_model, h=horizon)$mean)
  
  forecast_matrix <- matrix(c(arima_forecast, ets_forecast, net_forecast), horizon, methods)
  
  result <- apply(forecast_matrix, 1, function(row) { return (t(row) %*% weights)})
  
  actual <- as.numeric(test[,"Y"])
  predicted <- as.numeric(result)
  return (list(actual=actual, predicted=predicted))
}

# Combination of arima and neural network with xreg, @mode must be "average" or "cls"
# The training set will be used to train the individual models. The test set is again split
# into two parts, the first part out of the two is used to train the combination scheme, save
# for the simple average which does not need any training
fit_combination_xreg <- function(data, horizon, frequency, mode, column) {
  
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  
  # combine arima and neural network with xreg
  methods <- 2
  
  # get covariates (external regessors) names
  reg <- names(data) 
  # get covariates names
  reg <- names(data) 
  covariates <- reg[3:length(reg)]
  xreg_train <- data.frame(train[,covariates])
  names(xreg_train) <- covariates
  
  nnetar_model <- nnetar(train[,column], xreg=as.matrix(xreg_train), MaxNWts=84581)
  arima_model <- auto.arima(train[,column], xreg=as.matrix(xreg_train))
  
  xreg_test <- data.frame(test[,covariates])
  names(xreg_test) <- covariates
  
  if (mode == "cls") {
    
    train <- head(train, n=(nrow(train)-horizon))
    val <- tail(train, n=horizon)
    
    xreg_train <- data.frame(train[,covariates])  
    names(xreg_train) <- covariates
    xreg_val <- data.frame(val[,covariates])
    names(xreg_val) <- covariates
    
    arima_model_val <- auto.arima(train[,column], xreg=as.matrix(xreg_train), MaxNWts=84581)
    nnetar_model_val <- nnetar(train[,column], xreg=as.matrix(xreg_train))
    
    # constrained least squares
    arima_forecast_val <- as.numeric(forecast(arima_model_val, h=horizon, xreg=as.matrix(xreg_val))$mean)
    net_forecast_val <- as.numeric(forecast(nnetar_model_val, h=horizon, xreg=as.matrix(xreg_val))$mean)
    
    X <- matrix(c(arima_forecast_val, net_forecast_val), horizon, methods)
    y <- as.numeric(val[,"Y"])
    weights <- tryCatch(combine_with_cls(X, y), error=function(err) {print(err) ; return (rep(1/methods, methods))})
  }
  
  else if (mode == "average") {
    
    # average combination
    weights <- rep(1/methods, methods)
    
  }
  
  else stop("Invalid combination mode!")
  
  print(weights)
  
  arima_forecast <- as.numeric(forecast(arima_model, h=horizon, xreg=as.matrix(xreg_test))$mean)
  net_forecast <- as.numeric(forecast(nnetar_model, h=horizon, xreg=as.matrix(xreg_test))$mean)
  
  forecast_matrix <- matrix(c(arima_forecast, net_forecast), horizon, methods)
  
  result <- apply(forecast_matrix, 1, function(row) { return (t(row) %*% weights)})
  
  actual <- as.numeric(test[,"Y"])
  predicted <- as.numeric(result)
  return (list(actual=actual, predicted=predicted))
  
}

fit_combination_xreg_cls <- function(data, horizon, frequency, column){
  return (fit_combination_xreg(data, horizon, frequency, mode="cls",column  ))
}

fit_combination_cls <- function(data, horizon, frequency, column){
  return (fit_combination(data, horizon, frequency, mode="cls",column))
}

fit_combination_xreg_average <- function(data, horizon, frequency, column){
  return (fit_combination_xreg(data, horizon, frequency, mode="average",column))
}

fit_combination_average <- function(data, horizon, frequency, column){
  return (fit_combination(data, horizon, frequency, mode="average",column))
}
