library(caret)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)

xgb_grid_1 <- expand.grid(
  nrounds= 500,
  eta=c(0.01,0.001,0.0001),
  gamma = 0,               #default=0
  colsample_bytree = 1,    #default=1
  min_child_weight = 1,     #default=1
  max_depth=c(3,5,10),
  subsample=1
  )

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")

clase <- factor(ifelse(train_t$click=='0','NO','SI'))

xgb_train_1 <- train(
  x = as.matrix(train_t[,variables]),
  y= clase,
  trControl = fitControl,
  method="xgbTree"
)
