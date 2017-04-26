library(caret)
library(randomForest)
library(miscTools)
library(ggplot2)
library(dplyr)
library(rpart)
library(rattle)
library(funModeling)
library(caret)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(ranger)
library(pROC)

train_v3 <- train_v3 %>% left_join(items_v2[,c(1,12)],by="pid")

train_t <- train_v3 %>% filter(day >= 1 & day< 61)
test_t <- train_v3 %>% filter(day >= 61 & day< 92)

set.seed(307)

variables <- c( 'pid', 'adFlag', 'availability', 'competitorPrice', 'price','click','unit','genericProduct',
               'salesIndex','campaignIndex','rrp','dia_semana','compar_compet','compar_ref','price_changed',
               'orden_dia_producto','avg_comp_price','avg_price','lag_avg_comp_price','lag_avg_price','grupo_prod',
               'cajas','kmeans_g')
train_variables <- df_status(train_v3)

head(train_t[,variables])


sum(train_variables$variable %in% variables)

train_t$click <- factor(train_t$click)
fit.rpart <- rpart(click ~ .,data = train_t[,variables],control = rpart.control(cp=0.01,minsplit = 10,minbucket = round(20/3),maxdepth = 20),parms = list(split='gini'))

fancyRpartPlot(fit.rpart)


pred.rpart_click <- predict(fit.rpart,test_t,type = "prob")

pred.rpart_click <- ifelse(pred.rpart_click[,2] >= .5, 1,0)

table("predicho"=pred.rpart_click, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rpart_click, data = test_t)
plot(g, col="red") 

probpred.training <- predict(fit.rpart, type=c("prob"))
probpred.training <- ifelse(probpred.training[,2] >= .5, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )


#random forest

set.seed(222)

train_t$competitorPrice[is.na(train_t$competitorPrice)] <- 0
train_t$compar_compet[is.na(train_t$compar_compet)] <- 0
train_t$avg_comp_price[is.na(train_t$avg_comp_price)] <- 0
train_t$lag_avg_comp_price[is.na(train_t$lag_avg_comp_price)] <- 0
train_t$lag_avg_price[is.na(train_t$lag_avg_price)] <- 0


fit_ranger <- ranger(click ~ ., data = train_t[,variables] , num.trees=100, min.node.size=10, probability=TRUE,importance = 'impurity' ) 
fit_ranger$variable.importance

var_ranger <- as.data.frame(fit_ranger$variable.importance)

colnames(var_ranger) <- 'importancia'
var_ranger$variable <- rownames(var_ranger)

ggplot(var_ranger %>% arrange(desc(importancia)) %>% filter(importancia>18000),aes(x=variable,y=importancia))+geom_col()

#fit.rf_click <- randomForest(click ~ .,data = train_t[,variables],ntree=100,importance=TRUE,maxnodes=25)

varImpPlot(fit.rr)

test_t$competitorPrice[is.na(test_t$competitorPrice)] <- 0
test_t$compar_compet[is.na(test_t$compar_compet)] <- 0
test_t$avg_comp_price[is.na(test_t$avg_comp_price)] <- 0
test_t$lag_avg_comp_price[is.na(test_t$lag_avg_comp_price)] <- 0
test_t$lag_avg_price[is.na(test_t$lag_avg_price)] <- 0

pred.rf_click <- predict(fit_ranger,test_t,type = "response")

pred.rf_click <- ifelse(pred.rf_click$predictions[,2] >= .5, 1,0)

table("predicho"=pred.rf_click, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rf_click, data = test_t)
plot(g, col="red") 
g

probpred.training <- predict(fit_ranger,train_t,type = "response")
probpred.training <- ifelse(probpred.training$predictions[,2] >= .5, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

remove(list = c("g.training","g"))

#65% CURVA roc, mejoro