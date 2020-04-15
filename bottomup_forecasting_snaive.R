source("models.R")
source("utils.R")

sales_train_validation <- read.csv("data/sales_train_validation.csv",stringsAsFactors = FALSE)
df_sales <-data.frame(sales_train_validation)
rm(sales_train_validation)
sell_prices<- read.csv("data/sell_prices.csv",stringsAsFactors = FALSE)
df_sell_prices <-data.frame(sell_prices)  
rm(sell_prices)
calendar <-read.csv("data/calendar.csv",stringsAsFactors = FALSE)
df_calendar <-data.frame(calendar)
rm(calendar)

##calcola tutti i w per la metrica WRMSSE( per tutti e 12 i livelli)
list_weights<-compute_weights(df_sales, df_sell_prices, df_calendar)

#riorganizzo il dataframe per applicare il forecast
df_sales$item_store_id <- apply( df_sales[ ,c('item_id','store_id')],1,paste,collapse = "_")
df_sales <- df_sales[,7:ncol(df_sales)]
ts_sales <-data.frame(t(df_sales))
colnames(ts_sales)<-df_sales[,'item_store_id']
ts_sales<-data.frame(ts_sales[1:1913,])
#to retransfrom all values in integer
ts_sales <- data.frame(lapply(ts_sales, function(x) as.numeric(as.character(x))))

par(mfrow = c(1, 1))
plot(ts_sales$HOBBIES_1_001_CA_2, type="l")


#level 12 (bottom)  
#30490 series
output<- compute_all_forecasts(ts_sales, horizon=28, frequency=7, fit_snaive)

#level11
#9147 series
# HOBBIES_1_001_CA_1 -> HOBBIES_1_001_CA 
result11 <-map(output$df_predicted, ts_sales, gsub, '.{2}$')    
grouped11<-aggregate_level(result11$map_predicted, result11$map_actual)
errors11 <-compute_rmsse(grouped11$agg_predicted, grouped11$agg_actual, horizon=28)

#level10
#3049 series
# HOBBIES_1_001_CA -> HOBBIES_1_001
result10 <-map(grouped11$agg_predicted, grouped11$agg_actual, gsub, '.{3}$')
grouped10<-aggregate_level(result10$map_predicted, result10$map_actual)
errors10 <-compute_rmsse(grouped10$agg_predicted, grouped10$agg_actual, horizon=28)

#level9
# 70 series
# HOBBIES_1_001_CA_1 -> HOBBIES_1_CA_1
result9 <-map(output$df_predicted, ts_sales, gsub, "_\\d{3}")
grouped9<-aggregate_level(result9$map_predicted, result9$map_actual)
errors9 <-compute_rmsse(grouped9$agg_predicted, grouped9$agg_actual, horizon=28)

#level8
# 30 series
# HOBBIES_1_001_CA_1 -> HOBBIES_CA_1
result8 <-map(output$df_predicted, ts_sales, gsub, "_\\d+_\\d+")
grouped8<-aggregate_level(result8$map_predicted, result8$map_actual)
errors8 <-compute_rmsse(grouped8$agg_predicted, grouped8$agg_actual, horizon=28)

#level7
# 21 series
# HOBBIES_1_001_CA -> HOBBIES_1_CA
result7 <-map(grouped11$agg_predicted, grouped11$agg_actual, gsub, "_\\d{3}")
grouped7<-aggregate_level(result7$map_predicted, result7$map_actual)
errors7 <-compute_rmsse(grouped7$agg_predicted, grouped7$agg_actual, horizon=28)

#level6
# 9 series
# HOBBIES_1_001_CA -> HOBBIES_CA
result6 <-map(grouped11$agg_predicted, grouped11$agg_actual, gsub, "_\\d+_\\d+")
grouped6<-aggregate_level(result6$map_predicted, result6$map_actual)
errors6 <-compute_rmsse(grouped6$agg_predicted, grouped6$agg_actual, horizon=28)

#level5
# 7 series
# HOBBIES_1_001_CA -> HOBBIES_1
result5 <-map(grouped11$agg_predicted, grouped11$agg_actual, gsub, ".{7}$")
grouped5<-aggregate_level(result5$map_predicted, result5$map_actual)
errors5 <-compute_rmsse(grouped5$agg_predicted, grouped5$agg_actual, horizon=28)

#level4
# 3 series
# HOBBIES_1 -> HOBBIES
result4 <-map(grouped5$agg_predicted, grouped5$agg_actual, gsub, '.{2}$')
grouped4<-aggregate_level(result4$map_predicted, result4$map_actual)
errors4 <-compute_rmsse(grouped4$agg_predicted, grouped4$agg_actual, horizon=28)

#level3
# 10 series
# HOBBIES_1_CA_1 -> CA_1
result3 <-map(grouped9$agg_predicted, grouped9$agg_actual, gsub,"^.*?\\_\\d_")
grouped3<-aggregate_level(result3$map_predicted, result3$map_actual)
errors3 <-compute_rmsse(grouped3$agg_predicted, grouped3$agg_actual, horizon=28)

#level2
# 3 series
# CA_1 -> CA
result2 <-map(grouped3$agg_predicted, grouped3$agg_actual, gsub, '.{2}$')
grouped2<-aggregate_level(result2$map_predicted, result2$map_actual)
errors2 <-compute_rmsse(grouped2$agg_predicted, grouped2$agg_actual, horizon=28)

#level1 (top)
#grouped1<-list()
agg_predicted1<-data.frame(apply(grouped2$agg_predicted,1,sum))
agg_actual1   <-data.frame(apply(grouped2$agg_actual,1,sum))
names(agg_predicted1) <- c("Y")
names(agg_actual1)    <- c("Y")
errors1 <-compute_rmsse(agg_predicted1, agg_actual1, horizon=28)


list_rmsse <-list(errors1,errors2,errors3,errors4,errors5,errors6,errors7,errors8,errors9, errors10, errors11,output$errors)
compute_wrmsse( list_weights, list_rmsse)
