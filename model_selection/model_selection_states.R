source("models.R")

sales_states <- read.csv("data/sales_states.csv")
df_sales_states<- as.data.frame(sales_states)

calendar <- read.csv("data/calendar.csv",header = TRUE)
df_calendar<- as.data.frame(calendar)
df_calendar<-df_calendar[1:1913,]

topLevel <- read.csv("data/TS_topLevel.csv",header = FALSE)
df_topLevel<- as.data.frame(topLevel)
names(df_topLevel) <- c("Date","Y")
df_topLevel_train<-head(df_topLevel, n=(nrow(df_topLevel)-28))
df_topLevel_test<-tail(df_topLevel, n=28)

df_sales_states<-cbind(df_sales_states,df_calendar$date)
df_sales_states_train<-head(df_sales_states, n=(nrow(df_sales_states)-28))
df_sales_states_test<-tail(df_sales_states, n=28)

name_series<-colnames(df_sales_states[,2:4])

# fit seasonal naive (frequency is important!)
fit_snaive <- function(data, horizon, frequency,column) {
  # build ts object
  ts <- ts(data, frequency=frequency)
  # train-test split
  train <- head(ts, n=(nrow(ts)-horizon))
  test <- tail(ts, n=horizon)
  fcast <- snaive(train[,column], h=horizon)
  actual <- as.numeric(test[,column])
  predicted <- as.numeric(fcast$mean)
  return (list(actual=actual, predicted=predicted))
}






par(mfrow = c(1, 1))
plot(df_sales_states_train$CA, type="l")
lines(df_sales_states_train$TX, col="red")
lines(df_sales_states_train$WI, col="yellow")


par(mfrow = c(1, 1))
acf(df_topLevel_train$Y, lag.max=200, main="ACF campionaria (h=0,1,...,200)")
pacf(df_topLevel_train$Y, lag.max=200, main="PACF campionaria (h=0,1,...,200)")

par(mfrow = c(1, 1))
acf(df_sales_states_train$CA, lag.max=200, main="ACF campionaria (h=0,1,...,200)")
pacf(df_sales_states_train$CA, lag.max=200, main="PACF campionaria (h=0,1,...,200)")

par(mfrow = c(1, 1))
acf(df_sales_states_train$TX, lag.max=200, main="ACF campionaria (h=0,1,...,200)")
pacf(df_sales_states_train$TX, lag.max=200, main="PACF campionaria (h=0,1,...,200)")

par(mfrow = c(1, 1))
acf(df_sales_states_train$WI, lag.max=200, main="ACF campionaria (h=0,1,...,200)")
pacf(df_sales_states_train$WI, lag.max=200, main="PACF campionaria (h=0,1,...,200)")


par(mar=c(4,4,2,2))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
plot(df_sales_states_train$CA, type="l")
plot(df_sales_states_train$TX, type="l")
par(mar=c(2,14,2,14))
plot(df_sales_states_train$WI, type="l")
