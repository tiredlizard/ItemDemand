library(vroom)
library(tidymodels)
library(tidyverse)
library(forecast)
library(yardstick)

store_train <- vroom('train.csv')
store_test <- vroom('test.csv')  

storeItem <- store_train %>%
  filter(store==6, item==13)

storeItem <- storeItem %>%
  mutate(sales = as.numeric(sales))

storeRecipe <- recipe(sales ~ ., data = storeItem) %>%
  step_date(date, features=c("month", "dow", "doy", "year")) %>%
  step_mutate(date_month = as.numeric(date_month)) %>%
  step_mutate(date_dow = as.numeric(date_dow)) %>%
  step_range(date_month, min=0,max=pi) %>%
  step_mutate(sinMonth = sin(date_month), cosMonth = cos(date_month)) %>%
  step_range(date_dow, min=0, max=pi) %>%
  step_mutate(sinDOW = sin(date_dow), cosMonth = cos(date_dow))


knn_model <- nearest_neighbor(neighbors = tune()) %>% # set or tune
  set_mode("regression") %>%
  set_engine("kknn")

knn_wf <- workflow() %>%
  add_recipe(storeRecipe) %>%
  add_model(knn_model)

knn_grid <- grid_regular(neighbors(range = c(1,10)),
                         levels = 10)

# ## Split data for CV
# folds_knn <- vfold_cv(storeItem, v = 5, repeats = 2)
# 
# smape <- function(data, truth, estimate, ...) {
#   smape_val <- 100 * mean(abs(truth - estimate) / ((abs(truth) + abs(estimate)) / 2))
#   tibble(.metric = "smape", .estimator = "standard", .estimate = smape_val)
# }
# smape_metric <- metric_set(smape)
# 
# ## Run the CV
# CV_results_knn <- knn_wf %>%
#   tune_grid(resamples=folds_knn,
#             grid=knn_grid,
#             metrics=metric_set(smape_metric))

## Custom SMAPE function and metric

smape <- function(data, truth, estimate, ...) {
  truth <- as.numeric(truth)
  estimate <- as.numeric(estimate)
  smape_val <- 100 * mean(abs(truth - estimate) / ((abs(truth) + abs(estimate)) / 2))
  smape_val
}

# Register as a numeric metric
smape_metric <- yardstick::new_numeric_metric(
  metric_impl = smape,
  direction = "minimize"  # Lower SMAPE is better
)

## Run the CV
CV_results_knn <- knn_wf %>%
  tune_grid(
    resamples = folds_knn,
    grid = knn_grid,
    metrics = smape_metric
  )


# Select the best parameters
bestTune_knn <- CV_results_knn %>%
  select_best(metric = "smape")

# Report the cross-validated error for the best model
best_error <- CV_results_knn %>%
  filter_parameters(parameters = bestTune_knn) %>%  # Explicitly specify parameters
  collect_metrics()

print(best_error)


# ## Finalize the Workflow & fit it
# final_knn_wf <-
#   knn_wf %>%
#   finalize_workflow(bestTune_knn) %>%
#   fit(data=amazon_train)
# 
# knn_preds <- predict(final_knn_wf, new_data=amazon_test, type="prob")
# 
# ## kaggle
# kag_knn <- knn_preds %>%
#   bind_cols(amazon_test) %>%
#   rename(ACTION=.pred_1) %>%
#   select(id, ACTION)
# 
# vroom_write(kag_knn, "kag_knn.csv", delim = ",")

