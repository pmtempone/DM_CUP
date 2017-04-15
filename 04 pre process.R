library(caret)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)

#dummy variables

dummies <- dummyVars(revenue ~ ., data = train)
head(predict(dummies, newdata = train))

nzv <- nearZeroVar(train, saveMetrics= TRUE)
nzv[nzv$nzv,]

library(AppliedPredictiveModeling)

row.names(train) <- train$lineID


pp_hpc <- preProcess(train[,c(6:27)], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc


# correlation

variables <- data_profile$variable[data_profile$type %in% c("integer","numeric")][2:16]

descrCor <-  cor(train_t %>% select(day,pid,adFlag,availability,competitorPrice,click,basket,order,price,revenue,manufacturer,genericProduct,salesIndex,rrp),use = "pairwise.complete.obs")
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

#scalar variables

library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

plotSubset <- data.frame(scale(train_t[, c("price", "competitorPrice")])) 
xyplot(price ~ competitorPrice,
       data = plotSubset,
       groups = train_t$unit, 
       auto.key = list(columns = 2))  

transformed <- spatialSign(plotSubset[!is.na(plotSubset$competitorPrice),])
transformed <- as.data.frame(transformed)
xyplot(price ~ competitorPrice, 
       data = transformed, 
       groups = train_t$unit[!is.na(train_t$competitorPrice)], 
       auto.key = list(columns = 2)) 

remove(plotSubset)
