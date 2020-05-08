source("models.R")
source("utils.R")
library("dummies")


sales_train_validation <- read.csv("data/sales_train_validation.csv",stringsAsFactors = FALSE)
df_sales <-data.frame(sales_train_validation)
rm(sales_train_validation)
calendar <-read.csv("data/calendar.csv",stringsAsFactors = FALSE)
df_calendar <-data.frame(calendar)
df_calendar <-df_calendar[1:1913,]
rm(calendar)
sell_prices<- read.csv("data/sell_prices.csv",stringsAsFactors = FALSE)
df_sell_prices <-data.frame(sell_prices)  
rm(sell_prices)

topLevel <- read.csv("data/TS_topLevel.csv",header = FALSE)
df_topLevel<- as.data.frame(topLevel)
names(df_topLevel) <- c("Date","Y")
rm(topLevel)


df_topLevel<-cbind(df_topLevel, df_calendar$month)


dev.new(width=10, height=20)
par(mfrow=c(1, 1))
plot(df_topLevel[df_topLevel["df_calendar$weekday"]=="Saturday","Y"], ylab="sales", xlab="day")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Monday","Y"], col="red")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Tuesday","Y"], col="yellow")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Wednesday","Y"], col="blue")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Thursday","Y"], col="green")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Friday","Y"], col="brown")
points(df_topLevel[df_topLevel["df_calendar$weekday"]=="Sunday","Y"], col="purple")
df_topLevel[df_topLevel["df_calendar$weekday"]=="Thuersday",]
#legend(0,55000,legend = c("actual","predicted"), col = c("black","red"), lty = 1:1, cex = 0.7)





x_reg = data.frame(matrix(NA, nrow = 1913, ncol = 0))
rm(x_reg_wday)
rm(x_reg_month)
df_topLevel<-df_topLevel[,1:2]

### with dummy && event type1,2
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
#x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,4,5,6,7)],x_reg_month[,c(1,2,3,4,5,7,8,9,10,11,12)])
x_reg <-cbind(x_reg,x_reg_wday,x_reg_month, df_calendar$event_type_1, df_calendar$event_type_2)


### with dummy && event name1,2
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
#x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,4,5,6,7)],x_reg_month[,c(1,2,3,4,5,7,8,9,10,11,12)])
x_reg <-cbind(x_reg,x_reg_wday,x_reg_month, df_calendar$event_name_1, df_calendar$event_name_2)


### with dummy && event name1,2 no wednesday no june
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,4,5,6,7)],x_reg_month[,c(1,2,3,4,5,7,8,9,10,11,12)])
x_reg <-cbind(x_reg, df_calendar$event_name_1, df_calendar$event_name_2)

### with dummy && event name1,2 no monday no october
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
x_reg <-cbind(x_reg,x_reg_wday[,c(2,3,4,5,6,7)],x_reg_month[,c(1,2,3,4,5,6,7,8,9,11,12)])
x_reg <-cbind(x_reg, df_calendar$event_name_1, df_calendar$event_name_2)

### with dummy && event name1,2 no giovedi no october
x_reg_wday  <-data.frame(dummy(df_calendar$wday))
x_reg_month <-data.frame(dummy(df_calendar$month))
x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,3,5,6,7)],x_reg_month[,c(1,2,3,4,5,6,7,8,9,11,12)])
x_reg <-cbind(x_reg, df_calendar$event_name_1, df_calendar$event_name_2)

df_topLevel<-cbind(df_topLevel,x_reg)


list_of_frames <- expanding(df_topLevel, 1240, step=28) #1210=circa 3 anni e mezzo
results_narx   <-cross_validation(list_of_frames, horizon=28, frequency=7, "Y" ,fit_nnetar_xreg, plot=TRUE)
results_arimax <-cross_validation(list_of_frames, horizon=28, frequency=7, "Y" , fit_arima_errors, plot=TRUE)
results_avg_arima_nar<-cross_validation(list_of_frames, horizon=28, frequency=7, "Y", fit_combination_xreg_average, plot=TRUE)

results_narx$error
results_arimax$error
results_avg_arima_nar$error



type1<-unique(df_calendar$event_name_1)
type2<-unique(df_calendar$event_name_2)
typen1<-unique(df_calendar$event_type_1)
typen2<-unique(df_calendar$event_name_2)
dummy(df_calendar$event_name_1)


