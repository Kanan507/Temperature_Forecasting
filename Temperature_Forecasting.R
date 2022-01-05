# Import libraries & dataset ----
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)
library(timetk)
library(highcharter)
library(h2o)
library(tidymodels)
library(modeltime)

df <- fread('daily-minimum-temperatures-in-me (1).csv')
names(df)[2] <- 'Temp'
df$Date <- as.Date(df$Date , format = '%m/%d/%Y')
df$Temp
df$Temp <- gsub('\\?', '', df$Temp)
df$Temp <- as.numeric(df$Temp)

# timetk package ----
df %>% 
  plot_time_series(
    Date, Temp, 
    # .color_var = lubridate::year(Date),
    # .color_lab = "Year",
    .interactive = T)

# Seasonality plots
df %>%
  plot_seasonal_diagnostics(
    Date, Temp, .interactive = F)

df_timetk <- df %>% tk_augment_timeseries_signature()

df_timetk %>% glimpse()

df_timetk <- df_timetk %>%
  select(-contains(c('hour','minute','second','am.pm'))) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)


# ------------------------------------ H2O ------------------------------------
h2o.init()    

train <- df_timetk %>% filter(year < 1990) %>% as.h2o()
test <- df_timetk %>% filter(year == 1990) %>% as.h2o()

y <- "Temp" 
x <- df_timetk %>% select(-Temp) %>% names()

model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train, 
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  seed = 123, 
  nfolds = 10,
  exclude_algos = c("GLM","DRF","GBM", 'XGBoost'),
  max_runtime_secs = 60) 


model_h2o@leaderboard %>% as.data.frame() 
h2o_leader <- model_h2o@leader

pred_h2o <- h2o_leader %>% h2o.predict(test) 

h2o_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)

error_tbl <- df_timetk %>% 
  filter(lubridate::year(Date) == 1990) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = Temp) %>% 
  select(Date,actual,pred)


highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')


# New data (next 2 years) ----
next_year <- seq(as.Date("1991-01-01"), as.Date("1991-12-31"), "days") %>%
  as_tibble() %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains(c('hour','minute','second','am.pm')))  %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)


# Forcaste ----
new_h2o <- next_year %>% as.h2o()

next_year_predictions <- h2o_leader %>% 
  h2o.predict(new_h2o) %>% 
  as_tibble() %>%
  add_column(Date=next_year$Date) %>% 
  select(Date,predict) %>% 
  rename(Temp=predict)

df %>% 
  bind_rows(next_year_predictions) %>% 
  mutate(categories=c(rep('Actual',nrow(df)),rep('Predicted',nrow(next_year_predictions)))) %>% 
  hchart("line", hcaes(Date, Temp, group = categories)) %>% 
  hc_title(text='Forecast') %>% 
  hc_colors(colors = c('red','green'))


# -------------------------------- Tidy models --------------------------------

train_2 <- df %>% filter(Date < "1990-01-01")
test_2 <- df %>% filter(Date >= "1990-01-01")

# 1.Auto ARIMA
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(Temp  ~ Date, train_2)



# calibration
calibration <- modeltime_table(
  model_fit_arima ) %>%
  modeltime_calibrate(test_2)


# Forecast ----
calibration %>% 
  #filter(.model_id == 3) %>% 
  modeltime_forecast(actual_data = df) %>%
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T)


# Accuracy ----
calibration %>% modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = F)


# Forecast Forward ----
calibration %>%
  filter(.model_id %in% 1) %>% # best model
  modeltime_refit(df) %>%
  modeltime_forecast(h = "2 year", 
                     actual_data = df) %>%
  select(-contains("conf")) %>% 
  plot_modeltime_forecast(.interactive = T,
                          .plotly_slider = T,
                          .legend_show = F)




