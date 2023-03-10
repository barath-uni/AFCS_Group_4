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


calendar$date <- as.Date(calendar$date, format = '%m/%d/%Y')

# set sample for the output
sample <- sample_org
for(num in c(1:823)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 11)
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
  train <- select(train_draft, sales, sell_price, snap_TX)
  train <- ts(train)
  
  # validation
  new_data <- merge.data.frame(calendar, price_select, by.x = "wm_yr_wk", by.y = "wm_yr_wk")
  new_data$d <- as.integer(gsub('d_', '', new_data$d))
  new_data <- filter(new_data, d > 1913)
  new_data <- filter(new_data, d< 1942)
  new_data <- select(new_data, sell_price, snap_TX)
  new_data$sell_price =  as.numeric(as.character(new_data$sell_price))
  
  # forecast model could be changed to wanted model
  model <- tslm(sales ~ sell_price + snap_TX, data = train)
  print(model)
  fcast <- forecast(model, newdata = new_data)
  
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
autoplot(fcast) + labs(title="Forecast using Linear Method for the Product FOODS_3_827")

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
write.csv(sample, "/home/barath/codespace/afcs_proj/AFCS_Group_4/output/linear_model_out.csv", row.names = F)

############ RESULT ################
############ mean(rmse_total) ##########
################## 3.751081
############# mean(MAE_total)
################## 3.461215
############### mean(smape_total)
#################### 1.276803
########################################


######### SNAIVE


# set sample for the output
sample <- sample_org
for(num in c(1:823)){
  # select from train file only the selected row
  s_train <- t(sales_train[num,])
  name <- substr(as.character(s_train[1,]), start = 1, stop = 11)
  s_train <- data.frame(s_train[-1,])
  colnames(s_train) = "sales" #name
  
  train <- ts(s_train, frequency = 365, start = c(2011,29))
  # forecast model could be changed to wanted model
  model <- snaive(train)
  fcast <- forecast(model, h=h)
  autoplot(fcast)
  print(fcast)
  # create wanted output format
  row <- as.numeric(t(data.frame(fcast))[1,])
  new_row <- c(sample[num,1], row)
  sample[num,] <- new_row
}
autoplot(fcast) + labs(title="Forecast using SNAIVE for the Product FOODS_3_827")

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

write.csv(sample, "/home/barath/codespace/afcs_proj/AFCS_Group_4/output/snaive.csv", row.names = F)

# > mean(rmse_total)
# [1] 4.842787
# > mean(MAE_total)
# [1] 3.876063
# > mean(smape_total)
# [1] 1.317562

