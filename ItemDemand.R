library(vroom)
library(tidymodels)
library(tidyverse)
library(forecast)

store_train <- vroom('train.csv')
store_test <- vroom('test.csv')
# ctrl + space to view data
# cmd + click to view in different tab

storeItem1 <- store_train %>%
  filter(store==6, item==13)

series_plot1 <- storeItem1 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

ACF_month1 <- storeItem1 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

ACF_years1 <- storeItem1 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)


storeItem2 <- store_train %>%
  filter(store==5, item==47)

series_plot2 <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

ACF_month2 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

ACF_years2 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

library(gridExtra)

# Assume p1, p2, ..., p6 are your ggplot objects
grid.arrange(series_plot1, ACF_month1, ACF_years1, series_plot2, ACF_month2, ACF_years2, ncol = 3)

