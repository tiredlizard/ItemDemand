library(vroom)
library(tidymodels)
library(tidyverse)
library(forecast)
library(yardstick)
library(modeltime)
library(timetk)

store_train <- vroom('train.csv')
store_test <- vroom('test.csv')  

storeItemTrain <- store_train %>%
  filter(store==8, item==11)

storeItemTest <- store_test %>%
  filter(store==8, item==11)

# train <- store_train %>% filter(store==6,item==13)
cv_split <- time_series_split(storeItemTrain,assess = "3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales ~ ., data = storeItemTrain) %>% # linear model part
  step_date(date, features=c("month", "dow", "doy", "year")) %>%
  step_mutate(date_month = as.numeric(date_month)) %>%
  step_mutate(date_dow = as.numeric(date_dow)) %>%
  step_range(date_month, min=0,max=pi) %>%
  step_mutate(sinMonth = sin(date_month), cosMonth = cos(date_month)) %>%
  step_range(date_dow, min=0, max=pi) %>%
  step_mutate(sinDOW = sin(date_dow), cosMonth = cos(date_dow))



arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, #default max p to tune
                         non_seasonal_ma=5, #default max q to tune
                         seasonal_ar=2, #default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences = 2, #default max d to tune
                         seasonal_differences = 2 #default mox D to tune
                         ) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(cv_split))

##Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))


## Visualize results
p3 = cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = training(cv_split),
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)


## Now that you have calibrated (tuned), refit to whole dataset
fullfit <- cv_results %>%
  modeltime_refit(data = storeItemTrain)


## Predict for all the observations in storeItemTest
p4 = fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest,
    actual_data = storeItemTrain
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE)

# p1 = plot_modeltime_forecast()

plotly::subplot(p1,p3,p2,p4,nrows = 2)

library(gridExtra)
grid.arrange(p1,p2,p3,p4, ncol = 2)

