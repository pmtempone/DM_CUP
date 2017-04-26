library(funModeling) ## contains heart_disease data
library(minerva) ## contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gridExtra) ## allow us to plot two plots in a row
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
