library(dplyr)
library(caret)
library(randomForest)
library(miscTools)
library(ggplot2)

#train y test los parto segun lo defindo (1 a 60 test, el resto train)
#reemplazo NA con 0
train_t$competitorPrice[is.na(train_t$competitorPrice)] <- 0 
#train_t$category <- as.character(train_t$category)
#train_t$category[is.na(train_t$category)] <- 'No_hay'
#train_t$pharmForm <- as.character(train_t$pharmForm)
#train_t$content <- as.character(train_t$content)
#train_t$group <- as.character(train_t$group)


set.seed(307)

variables <- c('day', 'pid', 'adFlag', 'availability', 'competitorPrice', 'click', 'basket', 'order', 'price','revenue')
head(train_t[,variables])

sum(data_profile$variable %in% variables)

fit.rf <- randomForest(revenue ~ ., data = train_t[,variables],ntree=100,importance=TRUE,maxnodes=10)

pred.rf <- predict(fit.rf,test_t) #36363 valores no pudo predecir


 
(r2 <- rSquared(test_t$revenue[which(!is.na(pred.rf))], test_t$revenue[which(!is.na(pred.rf))] - pred.rf[which(!is.na(pred.rf))]))
# [1] 0.6887012
(mse <- mean((test_t$revenue[which(!is.na(pred.rf))] - pred.rf[which(!is.na(pred.rf))])^2))
# [1] 32.84105

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test_t$revenue[which(!is.na(pred.rf))], pred=pred.rf[which(!is.na(pred.rf))]))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
