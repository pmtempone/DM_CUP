library(funModeling)
library(caret)
library(dplyr)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggplot2)
library(pROC)

# predecir clicks

head(freq(train, "group"), 10)

freq(train,"click") #bastante balanceadas las clases

freq(train,"basket") #bastante balanceadas las clases

table(train$click,train$basket)

cross_click_basket=cross_plot(train, str_input="click", str_target="basket")

cross_plot(train, str_input="adFlag", str_target="click")

cross_plot(train, str_input="availability", str_target="click")

cross_plot(train, str_input="unit", str_target="click")

cross_plot(train, str_input="genericProduct", str_target="click")

cross_plot(train, str_input="campaignIndex", str_target="click")

cross_plot(train, str_input="competitorPrice", str_target="click")

cross_plot(train, str_input="price", str_target="click")

cross_plot(train, str_input="rrp", str_target="click")

#boxplots

plotar(data=train, str_input="competitorPrice", str_target="click", plot_type = "boxplot")

plotar(data=train, str_input="price", str_target="click", plot_type = "boxplot")

quantile(train$competitorPrice, 0.99,na.rm = TRUE)


#densidad

plotar(data=train, str_input="competitorPrice", str_target="click", plot_type = "histdens")

plotar(data=train, str_input="price", str_target="click", plot_type = "histdens")


#modelo base de click

variables <- c('pid','adFlag','availability','competitorPrice','price','manufacturer','unit','genericProduct','salesIndex','campaignIndex','rrp','click')

train_t$click <- factor(train_t$click)
fit.rpart <- rpart(click ~ .,data = train_t[,variables],control = rpart.control(cp=0.01,minsplit = 10,minbucket = round(20/3),maxdepth = 20),parms = list(split='gini'))

fancyRpartPlot(fit.rpart)


pred.rpart_click <- predict(fit.rpart,test_t,type = "prob")

pred.rpart_click <- ifelse(pred.rpart_click[,2] >= .5, 1,0)

table("predicho"=pred.rpart_click, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rpart_click, data = test_t)
plot(g, col="red") 
g
probpred.training <- predict(fit.rpart, type=c("prob"))
probpred.training <- ifelse(probpred.training[,2] >= .5, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )


#random forest

fit.rf_click <- randomForest(click ~ .,data = train_t[,variables],ntree=100,importance=TRUE,maxnodes=10)

varImpPlot(fit.rf_click)


pred.rf_click <- predict(fit.rf_click,test_t,type = "prob")

pred.rf_click <- ifelse(pred.rf_click[,2] >= .5, 1,0)

table("predicho"=pred.rf_click, "observado"=test_t[,"click"])


g <- roc(click ~ pred.rf_click, data = test_t)
plot(g, col="red") 
g

probpred.training <- predict(fit.rf_click, type=c("prob"))
probpred.training <- ifelse(probpred.training[,2] >= .5, 1,0)

g.training <- roc(click ~ probpred.training, data = train_t)
lines(g.training, col="blue") 
legend("bottomright", c("entrenamiento","prueba"), col = c("blue", "red"), lty = 1 )

remove(list = c("g.training","g"))

#peor auc en rf que en rpart


