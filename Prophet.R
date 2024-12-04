library(tidymodels)
library(tidyverse)
library(vroom)

store_train <- vroom('train.csv')
store_test <- vroom('test.csv')

storeRecipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features=c("month", "dow", "doy", "year")) %>%
  step_mutate(date_month = as.numeric(date_month)) %>%
  step_mutate(date_dow = as.numeric(date_dow)) %>%
  step_range(date_month, min=0,max=pi) %>%
  step_mutate(sinMonth = sin(date_month), cosMonth = cos(date_month)) %>%
  step_range(date_dow, min=0, max=pi) %>%
  step_mutate(sinDOW = sin(date_dow), cosMonth = cos(date_dow))

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))