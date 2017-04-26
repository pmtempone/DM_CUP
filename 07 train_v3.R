#levantar la base v3

library(RPostgreSQL)
library(DBI)

# query the data from postgreSQL 
train_v3 <- dbGetQuery(con, "SELECT * from train_v3")
train_v3$row.names <- NULL

library(ggplot2)
library(funModeling)

tail(freq(train_v3,str_input = "group"),20)

train_v3$grupo_prod <- substr(train_v3$group,1,2)

head(freq(train_v3,str_input = "category"),20)

coincidencias <- (grepl("X",train_v3$content))

train_v3$cajas <- ifelse(coincidencias==TRUE,1,0)