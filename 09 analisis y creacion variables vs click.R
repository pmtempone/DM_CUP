library(funModeling) ## contains heart_disease data
library(minerva) ## contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gridExtra) ## allow us to plot two plots in a row

library(RPostgreSQL)
library(DBI)

options(scipen=999) # disable cientific notation


variables <- c( 'pid', 'adFlag', 'availability', 'competitorPrice', 'price','click','genericProduct',
                'salesIndex','rrp','dia_semana','compar_compet','compar_ref',
                'orden_dia_producto','avg_comp_price','avg_price','lag_avg_comp_price','lag_avg_price','grupo_prod',
                'cajas','kmeans_g')

correlation_table(data=train_t[,variables], str_target="click")

'Variable click
1               click  1.00
2          salesIndex  0.13
3        availability  0.12
4               price  0.11
5           avg_price  0.11
6       lag_avg_price  0.11
7                 pid  0.09
8     competitorPrice  0.09
9      avg_comp_price  0.09
10 lag_avg_comp_price  0.09
11                rrp  0.08
12      compar_compet  0.02
13           kmeans_g  0.00
14         dia_semana -0.04
15         compar_ref -0.14'


categ_analysis(data=train_t, input="adFlag", target = "click")

categ_analysis(data=train_t, input="availability", target = "click")

categ_analysis(data=train_t, input="genericProduct", target = "click")

categ_analysis(data=train_t, input="unit", target = "click")

categ_analysis(data=train_t, input="salesIndex", target = "click")

categ_analysis(data=train_t, input="campaignIndex", target = "click")

train_v4 <- train_v3 %>% mutate(perc_adflag=ifelse(adFlag==1,0.572,0.380),
                              perc_availability=ifelse(availability==1,0.461,ifelse(availability==2,0.255,
                                                                                    ifelse(availability==3,0.230,
                                                                                           0.001))),
                              perc_generic=ifelse(genericProduct==1,0.615,0.42),
                              perc_unit=ifelse(unit=='M',0.479,ifelse(unit=='ML',0.456,ifelse(unit=='G',0.442,
                                                                                              ifelse(unit=='ST',0.433,
                                                                                                     ifelse(unit=='KG',0.4,
                                                                                                            ifelse(unit=='L',0.387,
                                                                                                                   ifelse(unit=='P',0.282,0.185))))))),
                              perc_sales=ifelse(salesIndex==40,0.509,ifelse(salesIndex==52,0.396,ifelse(salesIndex==53,0.371,0.337))),
                              perc_campaign=ifelse(campaignIndex=='B',0.651,ifelse(campaignIndex=='A',0.522,ifelse(campaignIndex=='',0.415,0.404))))


dbWriteTable(con,"train_v4",train_v4,row.names=FALSE)


train_t <- train_v4 %>% filter(day >= 1 & day< 61)
test_t <- train_v4 %>% filter(day >= 61 & day< 92)
