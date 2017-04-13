library(ggplot2)
library(dplyr)
library(data.table)
library(dtplyr)
library(funModeling)
library(doMC)


train <- read.delim("/Volumes/Disco_SD/Datamining Cup/DMC_2017_task/train.csv",header = TRUE,sep = "|")

items <- read.delim("/Volumes/Disco_SD/Datamining Cup/DMC_2017_task/items.csv",header = TRUE,sep = "|")


check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}

train <-
  train %>% left_join(items, by = "pid")

# Tabla con los NA
na_count <- data.frame(sapply(train, function(y) sum(length(which(is.na(y))))))
data_profile <- df_status(train)
# na_count 
#100.687

# CONSTRUCCIÓN DE VARIABLES
#Construir units
train <- train %>%
  mutate(units = revenue/price)

#Contruir día de la semana
train <- train %>%
  mutate(dia_semana = day %% 7)

#Contruir diferencia porcentual con el competidor
train <- train %>%
  mutate(compar_compet = (competitorPrice - price) / price)

#Contruir la comparación con el precio de referencia

train <- train %>%
  mutate(compar_ref = (rrp - price) / price)


train <- train %>%
  mutate(price_changed = !(check.integer(train$revenue/train$price)))

distinct_days = train %>% distinct(day)
train$orden_dia_producto <- 0

#Porcentaje de acciones que terminan en compra
prop.table(table(train$order))

#Ad Flag presente significa que se vende más
prop.table(table(train$adFlag,train$order),1)

#Availability: Availability 4 casi no se vende!!! A menor avail. más se vende
prop.table(table(train$availability,train$order),1)

#La comparación con el precio del competidor no parece influyente
prop.table(table(train$compar_compet > 0,train$order),1)

#unit. Parece determinante
prop.table(table(train$unit,train$order),1)

#generico. Se vende mucho más.
prop.table(table(train$genericProduct,train$order),1)

#sales index. No se qué es.
prop.table(table(train$salesIndex,train$order),1)

#category. Es factor. Demasiados levels.
train$category <- as.factor(train$category)
prop.table(table(train$category,train$order),1)

#Campaign index. NA se parece mucho a B. 
prop.table(table(train$campaignIndex,train$order),1)

# Podría ser significativa.
prop.table(table(train$dia_semana,train$order),1)

#La comparación con el precio del competidor no parece influyente
prop.table(table(train$orden_dia_producto,train$order),1)

#Campaign index. NA se parece mucho a B. 
prop.table(table(train$campaignIndex,train$order),1)



