#train v5

library(dplyr)
library(caret)
library(randomForest)
library(miscTools)
library(ggplot2)
library(rpart)
library(rattle)
library(pROC)
library(funModeling)
library(ranger)
library(RPostgreSQL)
library(DBI)
library(xgboost)

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "DM_CUP",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

train_v5 <- dbGetQuery(con, "SELECT * from train_v5")

set.seed(307)

variables <- c( 'competitorPrice', 'price','click',
                'rrp','dia_semana','compar_compet','compar_ref','price_changed',
                'orden_dia_producto','avg_comp_price','avg_price','lag_avg_comp_price','lag_avg_price','grupo_prod',
                'cajas','kmeans_g','perc_adflag','perc_generic','perc_generic','perc_unit','perc_sales','perc_campaign','up_price',
                'multiple','flag_avail','flag_generic','avail_generic')
train_variables <- df_status(train_v5)

train_v5$multiple <- factor(train_v5$multiple)
train_v5$flag_avail <- factor(train_v5$flag_avail)
train_v5$flag_generic <- factor(train_v5$flag_generic)
train_v5$avail_generic <- factor(train_v5$avail_generic)

train_t <- train_v5 %>% filter(day >= 1 & day< 61)
test_t <- train_v5 %>% filter(day >= 61 & day< 92)

head(train_t[,variables])


sum(train_variables$variable %in% variables)

train_t$click <- factor(train_t$click)
fit.rpart <- rpart(click ~ .,data = train_t[,variables],control = rpart.control(cp=0.001,minsplit = 10,minbucket = round(20/3),maxdepth = 20),parms = list(split='gini'))

fancyRpartPlot(fit.rpart)


pred.rpart_click.v2 <- predict(fit.rpart,test_t[,variables],type = "prob")

pred.rpart_click.v2 <- ifelse(pred.rpart_click.v2[,2] >= .57, 1,0)

table("predicho"=pred.rpart_click.v2, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rpart_click.v2, data = test_t[,variables])
plot(g, col="red") 
g
probpred.training <- predict(fit.rpart, type=c("prob"))
probpred.training <- ifelse(probpred.training[,2] >= .5, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

#curva roc 61,59%, mejoro con respecto a modelo anterior

#random forest

set.seed(222)

train_t$competitorPrice[is.na(train_t$competitorPrice)] <- 0
train_t$compar_compet[is.na(train_t$compar_compet)] <- 0
train_t$avg_comp_price[is.na(train_t$avg_comp_price)] <- 0
train_t$lag_avg_comp_price[is.na(train_t$lag_avg_comp_price)] <- 0
train_t$lag_avg_price[is.na(train_t$lag_avg_price)] <- 0
train_t$dia_semana <- factor(train_t$dia_semana)


fit_ranger <- ranger(click ~ ., data = train_t[,variables] , num.trees=100, min.node.size=10, probability=TRUE,importance = 'impurity' ) 
sort(fit_ranger$variable.importance)

var_ranger <- as.data.frame(fit_ranger$variable.importance)

colnames(var_ranger) <- 'importancia'
var_ranger$variable <- rownames(var_ranger)

ggplot(var_ranger %>% arrange(desc(importancia)) %>% filter(importancia>18000),aes(x=variable,y=importancia))+geom_col()

#fit.rf_click <- randomForest(click ~ .,data = train_t[,variables],ntree=100,importance=TRUE,maxnodes=25)


test_t$competitorPrice[is.na(test_t$competitorPrice)] <- 0
test_t$compar_compet[is.na(test_t$compar_compet)] <- 0
test_t$avg_comp_price[is.na(test_t$avg_comp_price)] <- 0
test_t$lag_avg_comp_price[is.na(test_t$lag_avg_comp_price)] <- 0
test_t$lag_avg_price[is.na(test_t$lag_avg_price)] <- 0
test_t$dia_semana <- factor(test_t$dia_semana)

pred.rf_click <- predict(fit_ranger,test_t[,variables],type = "response")

pred.rf_click <- ifelse(pred.rf_click$predictions[,2] >= .55, 1,0)

table("predicho"=pred.rf_click, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rf_click, data = test_t)
plot(g, col="red") 
g

probpred.training <- predict(fit_ranger,train_t[,variables],type = "response")
probpred.training <- ifelse(probpred.training$predictions[,2] >= .57, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

remove(list = c("g.training","g","fit_ranger","pred.rpart_click","pred.rpart_click.v2","probpred.training","fit.rpart"))

#65% CURVA roc, igual que la anterior prueba


#----xgboost-----

library(xgboost)

drat:::addRepo("dmlc")
library(caret)
library(readr)
library(dplyr)
library(tidyr)

loglossobj <- function(preds, dtrain) {
  # dtrain is the internal format of the training data
  # We extract the labels from the training data
  labels <- getinfo(dtrain, "label")
  # We compute the 1st and 2nd gradient, as grad and hess
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  # Return the result as a list
  return(list(grad = grad, hess = hess))
}

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)


clase <- as.numeric(ifelse(train_t$click=="1",1,0))
data_xg <- data.matrix(train_t[,variables])


variables <- c( 'competitorPrice', 'price',
                'rrp','dia_semana','compar_compet','compar_ref','price_changed',
                'orden_dia_producto','avg_comp_price','avg_price','lag_avg_comp_price','lag_avg_price','grupo_prod',
                'cajas','kmeans_g','perc_adflag','perc_generic','perc_generic','perc_unit','perc_sales','perc_campaign','up_price',
                'multiple','flag_avail','flag_generic','avail_generic')

# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = data_xg,
                label = clase,
                params = xgb_params_1,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                         # stop if no improvement within 10 trees
)

xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = data_xg,
                  label = clase,
                  nrounds = 100, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10,missing = NA
)


#plot the AUC for the training and testing samples
library(tidyr)

with(xgb_cv_1$evaluation_log,plot(iter,train_auc_mean,type = 'l'))
with(xgb_cv_1$evaluation_log,lines(iter,test_auc_mean,type = 'l',col='red'))

max(xgb_cv_1$evaluation_log$test_auc_mean)

#auc max 67%