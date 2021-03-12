library(data.table)
data <- fread("AirPassengers.csv")


library(dplyr)

data %>% glimpse()

data %>% head()

names(data) <- names(data) %>% gsub("`#Passengers`","Passengers",.) %>% 
                          gsub("\\#","",.) 

data %>% dim()

data$Month[3]

new_date = c()

for(i in 1:length(data$Month)){
  #print(data$Month[i])
  a = paste(data$Month[i],"01",sep = "-")
  new_date[i] = a  
}

new_date

data$Month <- new_date

data$Month <- data$Month %>% as.Date(.,"%Y-%m-%d")

data$Month %>% head(20)

data %>% glimpse()

library(inspectdf)

data %>% inspect_na()


library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)


interactive <- FALSE

library(rsample)

splits <- initial_time_split(data, prop = 0.8)

data %>% names()

library(forecast)

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Passengers ~ Month + as.numeric(Month) + factor(month(Month, label = TRUE), ordered = F),
      data = training(splits))

# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Passengers ~ Month, data = training(splits))

# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Passengers ~ Month, data = training(splits))


models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )
#Boosted Arima has the lowest RMSE


calibration_tbl <- model_fit_arima_boosted %>%
  modeltime_calibrate(new_data = testing(splits))



calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  ) 
