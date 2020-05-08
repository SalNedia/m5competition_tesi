source("models.R")
source("utils.R")
library("dummies")


sales_train_validation <- read.csv("data/sales_train_validation.csv",stringsAsFactors = FALSE)
df_sales <-data.frame(sales_train_validation)
rm(sales_train_validation)
calendar <-read.csv("data/calendar.csv",stringsAsFactors = FALSE)
df_calendar <-data.frame(calendar)
rm(calendar)
sell_prices<- read.csv("data/sell_prices.csv",stringsAsFactors = FALSE)
df_sell_prices <-data.frame(sell_prices)  
rm(sell_prices)

##calcola tutti i pesi w per la metrica WRMSSE( per tutti e 12 i livelli)
list_weights<-compute_weights(df_sales, df_sell_prices, df_calendar)

#riorganizzo il dataframe per applicare il forecast
df_sales$item_store_id <- apply( df_sales[ ,c('item_id','store_id')],1,paste,collapse = "_")
df_sales <- df_sales[,7:ncol(df_sales)]
ts_sales <-data.frame(t(df_sales))
colnames(ts_sales)<-df_sales[,'item_store_id']
ts_sales<-data.frame(ts_sales[1:1913,])
#to retransfrom all values in integer
ts_sales <- data.frame(lapply(ts_sales, function(x) as.numeric(as.character(x))))


#generate hierarchy 
hierarchy<-compute_hierarchy(ts_sales, period=1:1913)

#Average historical proportions
list_proportions<-compute_proportions(hierarchy)

top_level       <- data.frame(hierarchy$series1)
names(top_level)<- c("Y")

### with dummy && event name1,2 no giovedi no october
x_reg = data.frame(matrix(NA, nrow = 1913, ncol = 0))
rm(x_reg_wday)
rm(x_reg_month)
x_reg_wday  <-data.frame(dummy(df_calendar[1:1913,]$wday))
x_reg_month <-data.frame(dummy(df_calendar[1:1913,]$month))
x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,3,5,6,7)],x_reg_month[,c(1,2,3,4,5,6,7,8,9,11,12)])
x_reg <-cbind(x_reg, df_calendar[1:1913,]$event_name_1, df_calendar[1:1913,]$event_name_2)

top_level<-cbind(top_level,x_reg)

#faccio il forecast della serie top-level(totale)
output <- fit_nnetar_xreg(top_level, horizon=28, frequency=7,"Y")
top_level_predicted       <-data.frame(output$predicted)
names(top_level_predicted)<- c("Y")
write.csv(top_level_predicted, file = 'data/toplevel_predicted.csv',row.names=FALSE)
#calcolo gli rmsse per ogni livello
list_rmsse <-compute_topdown_rmsse(hierarchy, top_level_predicted, list_proportions)
compute_wrmsse( list_weights, list_rmsse)

##codice per scrive il file di sottomissione!!
actual_temp_series<-data.frame(hierarchy[[12]][,])
pred_temp = data.frame(matrix(NA, nrow = 28, ncol = 30490 ))

for(j in 1:30490){
  pred_temp[,j]<-top_level_predicted[,]*list_proportions[[12]][[j]]
  names(pred_temp) <- names(actual_temp_series)
}

subs = data.frame(matrix(NA, nrow = 30490, ncol = 29 ))
c_ids<-paste0(names(pred_temp), "_validation")
ids<-c("id",sprintf("F%d", 1:28))
colnames(subs) <-ids
subs$id<-c_ids

for(j in 1:30490){
  subs[j,2:29]<-pred_temp[,j]
}

subs2 = data.frame(matrix(0, nrow = 30490, ncol = 29 ))
c_ids2<-paste0(names(pred_temp), "_evaluation")
colnames(subs2) <-ids
subs2$id<-c_ids2
sub<-rbind(subs,subs2)
rm(subs)
rm(subs2)
write.csv(sub, "submission.csv", row.names=FALSE)

########################## calcolo 2-level CA ########################


CA_level2       <- data.frame(hierarchy$series2$CA)
names(CA_level2)<- c("CA")
### with dummy && event name1,2 no giovedi no october
x_reg = data.frame(matrix(NA, nrow = 1913, ncol = 0))
x_reg_wday  <-data.frame(dummy(df_calendar[1:1913,]$wday))
x_reg_month <-data.frame(dummy(df_calendar[1:1913,]$month))
x_reg <-cbind(x_reg,x_reg_wday[,c(1,2,3,5,6,7)],x_reg_month[,c(1,2,3,4,5,6,7,8,9,11,12)])
x_reg <-cbind(x_reg, df_calendar[1:1913,]$event_name_1, df_calendar[1:1913,]$event_name_2)

#faccio il forecast della serie
CA_level2<-cbind(CA_level2,x_reg)
output <- fit_nnetar_xreg(CA_level2, horizon=28, frequency=7,"CA")

par(mfrow = c(1, 1))
plot(output$actual, type="l")
lines(output$predicted, col="red")

score<-rmsse2(CA_level2[,1],output$actual,output$predicted)

