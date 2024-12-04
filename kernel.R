library(tidymodels)
library(tidyverse)
library(vroom)

store_train <- vroom('train.csv')
store_test <- vroom('test.csv')

nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- store_train %>%
      filter(store==s, item==i)
    storeItemTest <- store_test %>%
      filter(store==s, item==i)
    
    ## Fit storeitme models
    ## predict storeitem sales
    ##save storeitem predictions
    
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,preds)
    }
    
  }
}

vroom_write(all_preds, file=,delim=)