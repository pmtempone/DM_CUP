#grabar base en postgre

----#librerias----
library(RPostgreSQL)
library(DBI)

---#carga archivo -----

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "saga305"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "DM_CUP",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

dbWriteTable(con, "train_v1", value = train, append = TRUE)

dbWriteTable(con, "items", value = items, append = TRUE)


# check for the cartable
dbExistsTable(con, "abril_v4")
# TRUE

#leer tabla

# query the data from postgreSQL 
abril <- dbGetQuery(con, "SELECT * from abril_v4")

#crear tabla predicciones 15 octubre

sql_command <- "CREATE TABLE pred_15_octubre_v1
(
  numero_de_cliente integer,
  seed integer,
  BAJA_1 real,
  BAJA_2 real,  
  CONTINUA real)
  WITH (
  OIDS=FALSE
  );
  "
  # sends the command and creates the table
  dbGetQuery(con, sql_command)
  
  #crear tabla ganancias 15 octubre
  
  sql_command <- "CREATE TABLE ganancias_modelos_15_octubre_v1
  (
  date timestamp,
  duracion real,
  seed integer,
  algoritm character(20),
  cp real,  
  minsplit integer,
  minbucket integer,
  maxdepth integer,
  ganancias real)
  WITH (
  OIDS=FALSE
  );
  "
  # sends the command and creates the table
  dbGetQuery(con, sql_command)
  
  #test insert to db
  
  # writes df to the PostgreSQL database "postgres", table "..." 
  dbWriteTable(con, "pred_15_octubre_v1", value = cbind(104000,as.data.frame(abril_testing_prediccion)), append = TRUE)