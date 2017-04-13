#analisis de los datos

library(dplyr)
library(ggplot2)


# Vemos que los precios de la competencia toman varios valores, 4 veces es la moda
comp_price_by_product <- train %>% filter(!is.na(competitorPrice)) %>% group_by(pid) %>% summarise(Unique_Elements = n_distinct(competitorPrice))

comp_price_by_product_n <- comp_price_by_product %>% group_by(Unique_Elements) %>% summarise( n = n())

comp_price_by_product_n[order(-n),]

# Para ver si se venió algo ver si bajo el precio antes. 
#ventas_dia <- train %>% group_by(day) %>% summarise(media = mean(revenue))

#ggplot(ventas_dia, aes(y=media, x = day)) + geom_line()

#Construir units
train <- train %>%
  mutate(units = revenue/price)

# GRÁFICOS
plot(train$lineID[train$pid==99],train$price[train$pid==99])

# Contruir lags sobre el precio anterior
#Contruir diferencia porcentual con el competidor
train <- train %>%
  mutate(compar_compet = (competitorPrice - price) / price)


