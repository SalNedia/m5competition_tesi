#source("models.R")
#source("utils.R")
library("TTR")
library("dummies")
library("gbm")

sales_train_validation <- read.csv("data/sales_train_validation.csv",stringsAsFactors = FALSE)
df_sales <-data.frame(sales_train_validation)
rm(sales_train_validation)
calendar <-read.csv("data/calendar.csv",stringsAsFactors = FALSE)
df_calendar <-data.frame(calendar)
df_calendar <-df_calendar[1:1913,]
rm(calendar)

topLevel <- read.csv("data/TS_topLevel.csv",header = FALSE)
df_topLevel<- as.data.frame(topLevel)
names(df_topLevel) <- c("Date","sales")
rm(topLevel)


df_topLevel <- cbind(df_topLevel, df_calendar$wday, df_calendar$weekday, df_calendar$month, df_calendar$year )
#df_topLevel_train<-head(df_topLevel, n=(nrow(df_topLevel)-28))
#df_topLevel_test<-tail(df_topLevel, n=28)


df_topLevel$days <- as.factor(df_topLevel$`df_calendar$wday`)
df_topLevel$weeks <- as.factor(df_topLevel$`df_calendar$weekday`)
df_topLevel$month <- as.factor(df_topLevel$`df_calendar$month`)
df_topLevel$year <- as.factor(df_topLevel$`df_calendar$year`)



set.seed(150)
df_topLevel$ma <- EMA(df_topLevel$sales, 28)
df_topLevel <- df_topLevel[-(1:28),]
train <- df_topLevel[(1:1885),]
val <- df_topLevel[(1886:1913),]

formula <- sales~(days + weeks + month + year)*ma

fit <- gbm(formula, data = train, n.trees = 100000)













