library(fpp2)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)
library(Metrics)
# move to data directory for loading the data
setwd("/home/barath/codespace/afcs_proj/AFCS_Group_4/data")

# read in all the data
calendar <- read_csv("calendar_afcs2022.csv")
days <- c(1:1969)
# Set a new Column for 'd' in calendar
calendar$d <- sub("^","d_", days)
sales_train <- read_csv("sales_train_validation_afcs2022.csv")
price <- data.frame(read_csv("sell_prices_afcs2022.csv"))
sample_org <- data.frame(read_csv("sample_submission_afcs2022.csv"))

# extra_data
total <- data.frame(read_csv("sales_test_validation_afcs2022.csv"))

validation <- total[,c(0:29)]
h=28

# set sample for the output
sample <- sample_org
for(num in c(1:823)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 11)
  print(name)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # combine the data with calendar data
  sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
  sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))
  
  # combine the data with sale price
  price_select <- price
  price_select <- filter(price_select, item_id == name)
  train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  # select all the wanted columns for the forecast
  train <- select(train_draft, sales)

  observ <- as.numeric(train$sales)

  # forecast model could be changed to wanted model
  model <- arima(observ, order=c(0,0,1))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
autoplot(fcast) + labs(title="Forecast using MA(1) for the Product FOODS_3_827")

rmse_total = c()
MAE_total = c()
smape_total = c()
for(num in c(1:823)){
  o = as.list(as.data.frame(t(validation[num,2:29])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  print(rmse(m,o))
  rmse_total <- c(rmse_total, rmse(m,o))
  print(rmse_total)
  MAE_total <- c(MAE_total, mae(m,o))
  smape_total <- c(smape_total, smape(m,o))
}
mean(rmse_total)
mean(MAE_total)
mean(smape_total)

write.csv(sample, ".moving_avg1.csv", row.names = F)


################# RESULT 
# > mean(rmse_total)
# [1] 2.052775 
# > mean(MAE_total)
# [1] 1.635956
# > mean(smape_total)
# [1] 1.289486

################################# AUTOREGRESSIVE MODEL ######################

# AR-1

# set sample for the output
sample <- sample_org
for(num in c(1:823)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 11)
  print(name)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # combine the data with calendar data
  sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
  sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))
  
  # combine the data with sale price
  price_select <- price
  price_select <- filter(price_select, item_id == name)
  train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  # select all the wanted columns for the forecast
  train <- select(train_draft, sales)
  
  observ <- as.numeric(train$sales)
  
  # forecast model could be changed to wanted model
  model <- arima(observ, order=c(1,0,0))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
autoplot(fcast) + labs(title="Forecast using AR(1) for the Product FOODS_3_827")
rmse_total = c()
MAE_total = c()
smape_total = c()
for(num in c(1:823)){
  o = as.list(as.data.frame(t(validation[num,2:29])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  print(rmse(m,o))
  rmse_total <- c(rmse_total, rmse(m,o))
  print(rmse_total)
  MAE_total <- c(MAE_total, mae(m,o))
  smape_total <- c(smape_total, smape(m,o))
}
mean(rmse_total)
mean(MAE_total)
mean(smape_total)

write.csv(sample, "/home/barath/codespace/afcs_proj/AFCS_Group_4/output/auto_regressive1.csv", row.names = F)

# > mean(rmse_total)
# [1] 2.041194
# > mean(MAE_total)
# [1] 1.621103
# > mean(smape_total)
# [1] 1.288392

######################################################################
##################### AUTO ARIMA ###################################


# Have to change the one method within the loop to get result

# set sample for the output
sample <- sample_org
for(num in c(1:823)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 11)
  print(name)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  # combine the data with calendar data
  sales_days <- merge.data.frame(s_train, calendar, by.x = 0, by.y = "d")
  sales_days$d <- as.integer(gsub('d_', '', sales_days$Row.names))
  
  # combine the data with sale price
  price_select <- price
  price_select <- filter(price_select, item_id == name)
  train_draft <- merge.data.frame(sales_days, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  # select all the wanted columns for the forecast
  train <- select(train_draft, sales)

  observ <- as.numeric(train$sales)

  # forecast model could be changed to wanted model
  model <- auto.arima(as.numeric(train$sales))
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
autoplot(fcast) + labs(title="Forecast using AUTO-ARIMA for the Product FOODS_3_827")

rmse_total = c()
MAE_total = c()
smape_total = c()
for(num in c(1:823)){
  o = as.list(as.data.frame(t(validation[num,2:29])))
  m = as.list(as.data.frame(as.numeric(t(sample[num,2:29]))))
  o <- as.numeric(as.character(unlist(o[[1]])))
  m <- as.numeric(as.character(unlist(m[[1]])))
  print(rmse(m,o))
  rmse_total <- c(rmse_total, rmse(m,o))
  print(rmse_total)
  MAE_total <- c(MAE_total, mae(m,o))
  smape_total <- c(smape_total, smape(m,o))
}
mean(rmse_total)
mean(MAE_total)
mean(smape_total)

write.csv(sample, "/home/barath/codespace/afcs_proj/AFCS_Group_4/output/auto_arima.csv", row.names = F)
################# RESULT
# > mean(rmse_total)
# [1] 1.837828
# > mean(MAE_total)
# [1] 1.416719
# > mean(smape_total)
# [1] 1.289395