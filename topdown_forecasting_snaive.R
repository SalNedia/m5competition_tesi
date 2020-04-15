source("models.R")
source("utils.R")

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
#faccio il forecast della serie top-level(totale)
output <- fit_snaive(df_top_level, horizon=28, frequency=7,"Y")

top_level_predicted       <-data.frame(output$predicted)
names(top_level_predicted)<- c("Y")
#calcolo gli rmsse per ogni livello
list_rmsse <-compute_topdown_rmsse(hierarchy, top_level_predicted, list_proportions)

compute_wrmsse( list_weights, list_rmsse)
