library(ggplot2)
library(dplyr)
library(data.table)
library(dtplyr)

train <- read.delim("/Volumes/Disco_SD/Datamining Cup/DMC_2017_task/train.csv",header = TRUE,sep = "|")

items <- read.delim("/Volumes/Disco_SD/Datamining Cup/DMC_2017_task/items.csv",header = TRUE,sep = "|")

