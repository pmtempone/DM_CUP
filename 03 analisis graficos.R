library(ggplot2)
library(dplyr)
library(dtw)
library(caret)
library(gridExtra)

by_day <- train %>% group_by(day) %>% summarise(n=sum(revenue))
plot(by_day$day,by_day$n,type = 'l')

table(train$basket,train$click)

ggplot(data = by_day,aes(x=day,y=n))+geom_line()+ylab("revenue total")

ggplot(train, aes(x = day, y = revenue)) + 
  geom_line() + 
  facet_wrap(~ unit, scales = 'free_y', ncol = 1)

train_t <- train %>% filter(day >= 1 & day< 61)
test_t <- train %>% filter(day >= 61 & day< 92)

featurePlot(x = iris[, 1:4], 
           y = iris$Species, 
           plot = "pairs",
           ## Add a key at the top
           auto.key = list(columns = 3))

#density plot


p1 <- ggplot(data=train,aes(x=competitorPrice))+geom_density(data=train, aes(fill=factor(genericProduct)))
p2 <- ggplot(data=train,aes(x=price))+geom_density(data=train, aes(fill=factor(genericProduct)))
p3 <- ggplot(data=train,aes(x=revenue))+geom_density(data=train, aes(fill=factor(genericProduct)))

grid.arrange(p1,p2,p3)

remove(list = c("p1","p2","p3"))

#pareciera que los productos genericos generan mas revenue y tienen precios más altos

ggplot(data=train,aes(x=units))+geom_density(data=train, aes(fill=factor(genericProduct)))

#boxplots

p1 <- ggplot(data=train,aes(factor(genericProduct),competitorPrice))+geom_boxplot()
p2 <- ggplot(data=train,aes(factor(genericProduct),price))+geom_boxplot()
p3 <- ggplot(data=train,aes(factor(genericProduct),revenue))+geom_boxplot()

grid.arrange(p1,p2,p3)

remove(list = c("p1","p2","p3"))


#scatter

p1 <- ggplot(data=train,aes(price,competitorPrice,colour=factor(genericProduct)))+geom_point()
p2 <- ggplot(data=train,aes(factor(genericProduct),price))+geom_boxplot()
p3 <- ggplot(data=train,aes(factor(genericProduct),revenue))+geom_boxplot()

grid.arrange(p1,p2,p3)

remove(list = c("p1","p2","p3"))