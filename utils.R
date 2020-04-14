library(reshape2)
library(forecast)
library(Metrics)


rmsse<-function(actual, predicted , step_size = 1){
  
  #Root Mean Squared Scaled Error (RMSSE),
  #which is a variant of the well-known Mean Absolute Scaled Error (MASE)
  
  naive_start <- step_size + 1
  n <- as.numeric(length(actual))
  naive_end <- n - step_size
  sum_errors <- sum((ae(actual, predicted))^2)
  naive_errors <- sum((ae(actual[naive_start:n], actual[1:naive_end]))^2)
  return( sqrt(sum_errors/(n * naive_errors/naive_end)) )
}

rmsse2<-function(actual,actual_h, predicted_h , step_size = 1){
  
  #Root Mean Squared Scaled Error (RMSSE),
  #which is a variant of the well-known Mean Absolute Scaled Error (MASE)
  
  naive_start <- step_size + 1  
  n <- as.numeric(length(actual))   #size of ts
  h <- as.numeric(length(actual_h)) #horizon
  
  naive_end  <- n - step_size
  sum_errors <- sum((ae(actual_h, predicted_h))^2)
  naive_errors <- sum((ae(actual[naive_start:n], actual[1:naive_end]))^2)
  return(sqrt((1/h)* (sum_errors/(naive_errors/naive_end))) )
}


# expanding window: df is a time series dataframe ('Y', 'XREG_1', 'XREG_2', ..., 'XREG_M')
expanding <- function(df, initial_window, step) {
  frames <- list()
  i <- 1
  end <- initial_window
  while (TRUE) {
    if (end > nrow(df))
      break
    frames[[i]] <- df[1:end, ]
    end <- end + step
    i <- i + 1
  }
  return (frames)
}

# calculate metrics
calculate_error <- function(df_actual, df_predicted, actual_full) {
  rows <- nrow(df_actual)
  results <- matrix(NA, rows, 5)
  colnames(results) <- c("SMAPE", "MASE", "RMSE", "MAE","RMSSE")
  for (i in 1:rows) {
    a <- as.numeric(df_actual[i, ])
    p <- as.numeric(df_predicted[i, ])
    # smape is between 0% and 200%
    results[i, 1] <- smape(a, p)
    results[i, 2] <- mase(a, p, step_size=1)
    results[i, 3] <- rmse(a, p)
    results[i, 4] <- mae(a, p)
    results[i, 5] <- rmsse2(actual_full,a, p, step_size=1)
  }
  #print("eccola")
  print(results)
  # average for each column
  apply(results, 2, mean, na.rm=TRUE)
}

cross_validation <- function(list_of_frames, horizon, frequency,column, model, plot=FALSE) {
  # model is a function in the catalog
  m_actual <- matrix(NA, length(list_of_frames), horizon)
  m_predicted <- matrix(NA, length(list_of_frames), horizon)
  for (i in 1:length(list_of_frames)) {
    # get i data frame
    #print(paste0("Split ", i))
    data <- list_of_frames[[i]]
    values <- model(data, horizon, frequency,column)
    actual <- as.numeric(values$actual)
    predicted <- as.numeric(values$predicted)
    m_actual[i, ] <- actual
    m_predicted[i, ] <- predicted
  }
  if (plot) {
    par(mfrow=c(1, 1))
    plot(actual, type="l",ylab="sales", xlab="day", main=paste(" TX Series predicted with",as.character(substitute(model)) ))
    lines(predicted, col="red")
    legend(0,14000,legend = c("actual","predicted"), col = c("black","red"), lty = 1:1, cex = 0.7)
  }
  df_actual <- as.data.frame(m_actual)
  df_predicted <- as.data.frame(m_predicted)
  error <- calculate_error(df_actual, df_predicted,data[,column])
  return (list(df_actual=df_actual, df_predicted=df_predicted, error=error, last_actual=actual, last_predicted=predicted))
}

compute_all_forecasts<- function(data, horizon, frequency, model, plot=FALSE) {
  
  df_predicted = data.frame(matrix(NA, nrow = 28, ncol = 0))
  #df_actual = data.frame(matrix(NA, nrow = 28, ncol = 0))
  name_series<-colnames(data)
  errors<-vector()
  i=1
  for (x in name_series) {
    print(paste0("series ", i))
    values    <- model(data, horizon, frequency,x)
    actual    <- as.numeric(values$actual)
    predicted <- as.numeric(values$predicted)
    actual_n  <-as.numeric(data[,x])
    df_predicted<-cbind(df_predicted,predicted)
    #df_actual   <-cbind(df_actual,actual)
    errors[i]<-rmsse2(actual_n,actual, predicted, step_size=1)
    print(errors[i])
    i<-i+1
  }
  colnames(df_predicted)<-name_series
  return (list(df_predicted=df_predicted, errors=errors))
}


map<-function(df_predicted_level,df_actaul_level, map_func, param){
  
  
  col_names<-list()
  col_names<-names(df_predicted_level)
  
  map_predicted <-data.frame(df_predicted_level)
  map_actual    <-data.frame(df_actaul_level)
  groupedcols   <-map_func(param,"", col_names)
  #rinomino le colonne
  colnames(map_predicted)<-groupedcols
  colnames(map_actual)<-groupedcols
  
  return (list(map_predicted=map_predicted, map_actual=map_actual))
}


aggregate_level<-function(map_predicted,map_actual){
  
  #aggregate predicted 
  grouped<-data.frame(sapply(unique(names(map_predicted)), function(x) rowSums( map_predicted[ , grep(x, names(map_predicted)), drop=FALSE]) ))
  
  #aggregate actual
  grouped_actual<-data.frame(sapply(unique(names(map_actual)), function(x) rowSums( map_actual[ , grep(x, names(map_actual)), drop=FALSE]) ))
  
  return (list(agg_predicted=grouped, agg_actual=grouped_actual))
}

compute_rmsse<-function(agg_predicted,agg_actual,horizon){
  
  errors<-vector()
  i=1
  for(x in names(agg_actual)){
    errors[i]<-rmsse2(agg_actual[,x],agg_actual[(1913-horizon+1):1913,x],agg_predicted[,x])
    i<-i+1
  }
  #rmsse_mean<-mean(errors)
  #rmsse_min<-min(errors)
  #rmsse_max<-max(errors)
  #return(list(rmsse_mean=rmsse_mean,rmsse_min=rmsse_min,rmsse_max=rmsse_max))
  return(errors)
}

compute_wrmsse<-function(list_weights, list_rmsse){
  
  wrmsse<-0
  for(i in 1:12){
    wrmsse <-wrmsse + sum( as.vector(unlist(Map('*',list_weights[i],list_rmsse[i] ))))
  }
  return(wrmsse/12)
}


compute_weights<-function( df_sales, df_sell_prices, df_calendar, period=1886:1913){

  # Get the last 28 days for weight
  cols<- paste("d", period ,sep = "_")
  data = data.frame(matrix(NA, nrow = 0, ncol = 0))
  data<- cbind(df_sales["id"],df_sales["store_id"],df_sales["item_id"],df_sales[,cols])
  
  data  <-melt(data, id.vars =c("id",'store_id','item_id'), variable.name = "d", value.name = "sale")
  data  <-merge(data, df_calendar, by.x = "d", by.y = "d",all.x=TRUE)
  data  <-data[,c("id", 'store_id', 'item_id', "sale", "wm_yr_wk")]
  data  <-merge(data, df_sell_prices, by = c('store_id', 'item_id', 'wm_yr_wk'), all.x =TRUE, sort = FALSE)
  
  result_list<-list()
  
  #aggregate lower level
  lower_amount<-data
  lower_amount$amount = data['sale'] * data['sell_price']
  result12<-aggregate(lower_amount$amount, by=list(lower_amount$id), FUN=sum)
  result12<-c(result12$sale)
  
  #aggregate 11 level
  amount_11<-data
  amount_11$id<-gsub('.{13}$', '', amount_11$id)
  amount_11$amount = amount_11['sale'] * amount_11['sell_price']
  result11<-aggregate(amount_11$amount, by=list(amount_11$id), FUN=sum)
  result11<-c(result11$sale)
  
  #aggregate 10 level
  amount_10<-amount_11
  amount_10$id<-gsub('.{3}$', '', amount_10$id)
  amount_10$amount = amount_10['sale'] * amount_10['sell_price']
  result10<-aggregate(amount_10$amount, by=list(amount_10$id), FUN=sum)
  result10<-c(result10$sale)
  
  #aggregate 9 level
  amount_9<-data
  amount_9$id<-gsub("_\\d{3}", '', amount_9$id)
  amount_9$amount = amount_9['sale'] * amount_9['sell_price']
  result9<-aggregate(amount_9$amount, by=list(amount_9$id), FUN=sum)
  result9<-c(result9$sale)
  
  #aggregate 8 level
  amount_8<-data
  amount_8$id<-gsub("_\\d+_\\d+", '', amount_8$id)
  amount_8$amount = amount_8['sale'] * amount_8['sell_price']
  result8<-aggregate(amount_8$amount, by=list(amount_8$id), FUN=sum)
  result8<-c(result8$sale)
  
  #aggregate 7 level
  amount_7<-amount_11
  amount_7$id<-gsub("_\\d{3}", '', amount_7$id)
  amount_7$amount = amount_7['sale'] * amount_7['sell_price']
  result7<-aggregate(amount_7$amount, by=list(amount_7$id), FUN=sum)
  result7<-c(result7$sale)
  
  #aggregate 6 level
  amount_6<-amount_8
  amount_6$id<-gsub(".{12}$", '', amount_6$id)
  amount_6$amount = amount_6['sale'] * amount_6['sell_price']
  result6<-aggregate(amount_6$amount, by=list(amount_6$id), FUN=sum)
  result6<-c(result6$sale)
  
  #aggregate 5 level
  amount_5<-amount_7
  amount_5$id<-gsub(".{3}$", '', amount_5$id)
  amount_5$amount = amount_5['sale'] * amount_5['sell_price']
  result5<-aggregate(amount_5$amount, by=list(amount_5$id), FUN=sum)
  result5<-c(result5$sale)

  #aggregate 4 level
  amount_4<-amount_5
  amount_4$id<-gsub(".{2}$", '', amount_4$id)
  amount_4$amount = amount_4['sale'] * amount_4['sell_price']
  result4<-aggregate(amount_4$amount, by=list(amount_4$id), FUN=sum)
  result4<-c(result4$sale)

  #aggregate 3 level
  amount_3<-amount_9
  amount_3$id<-gsub("^.*?\\_\\d_", '', amount_3$id)
  amount_3$amount = amount_3['sale'] * amount_3['sell_price']
  result3<-aggregate(amount_3$amount, by=list(amount_3$id), FUN=sum)
  result3<-c(result3$sale)
  
  #aggregate 2 level
  amount_2<-amount_3
  amount_2$id<-gsub(".{13}$", '', amount_2$id)
  amount_2$amount = amount_2['sale'] * amount_2['sell_price']
  result2<-aggregate(amount_2$amount, by=list(amount_2$id), FUN=sum)
  result2<-c(result2$sale)
  
  #aggregate 1 level
  amount_1<-amount_2
  amount_1$id<-gsub(".{2}$", 'Y', amount_1$id)
  amount_1$amount = amount_1['sale'] * amount_1['sell_price']
  result1<-aggregate(amount_1$amount, by=list(amount_1$id), FUN=sum)
  result1<-c(result1$sale)
  
  list_weights <-list(result1,result2,result3,result4,result5,result6,result7,result8,result9, result10, result11,result12)
  
  for(i in 1:12){
    amount_sum        <- sum(as.vector(unlist(list_weights[i])))
    list_weights[[i]] <- (as.vector(unlist(list_weights[i])))/amount_sum
  }
  
  return(list_weights)
}
