library(tidyverse)
library(caret)
library(xgboost)
library("dummies")
library(Matrix)
library(RcppRoll)
library(zoo)

calendar <-read.csv("data/calendar.csv",stringsAsFactors = FALSE)
df_calendar <-data.frame(calendar)
df_calendar <-df_calendar[1:1913,]
rm(calendar)
topLevel <- read.csv("data/TS_topLevel.csv",header = FALSE)
df_topLevel<- as.data.frame(topLevel)
names(df_topLevel) <- c("Date","Y")
rm(topLevel)
x_reg = data.frame(matrix(NA, nrow = 1913, ncol = 0))
rm(x_reg_wday)
rm(x_reg_month)
df_topLevel<-df_topLevel[,1:2]
### dummy 
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
x_reg <-cbind(x_reg,x_reg_wday,x_reg_month)
df_topLevel<-cbind(df_topLevel,x_reg)


x_reg <-cbind(x_reg,df_calendar["snap_CA"],df_calendar["snap_TX"],df_calendar["snap_WI"])

#Create input for xgboost
label = df_topLevel[1:1885,"Y"]
trainMatrix <-data.matrix(x_reg[1:1885,], rownames.force = NA)
testMatrix <- data.matrix(x_reg[1886:1913,], rownames.force = NA)
trainDMatrix <- xgb.DMatrix(data = trainMatrix, label = label)



#Set parameters of model
params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.6, gamma=0, eval_metric = "mae")
#Cross-validation
xgb.tab <- xgb.cv(data = trainDMatrix, param = params, evaluation = "mae", nrounds = 1000, nthreads = 10, nfold = 24, early_stopping_round = 10)

#Number of rounds
num_iterations = xgb.tab$best_iteration
model <- xgb.train(data = trainDMatrix, param = params, evaluation = 'mae', nrounds = num_iterations)

#Predict values for a given day
pred <- predict(model, testMatrix)
pred

par(mfrow = c(1, 1))
plot(df_topLevel[1885:1913,"Y"], type="l")
lines(pred, col="red")


