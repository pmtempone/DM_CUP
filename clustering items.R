#creacion de variables

grupos <- toupper(items$group)

length(unique(grupos))

length(unique(items$group=='kal'))

length(unique(grupos=='kal'))


library(FactoMineR)



FactoMineR::MCA(items_v2[,c(1,2,7,8,9,11)],quanti.sup = c(2:6))


items_v2 <- items
items_v2$category[is.na(items_v2$category)] <- 0

pc_items <- princomp(items_v2[,c(2,7,8,9,11)],cor=TRUE)


summary(pc_items)

plot(pc_items)

library(rgl)

plot3d(pc_items$scores[,1:3])

# Determine number of clusters
wss <- (nrow(items_v2[,c(2,7,8,9,11)])-1)*sum(apply(items_v2[,c(2,7,8,9,11)],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(items_v2[,c(2,7,8,9,11)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k <- kmeans(pc_items$scores[,1:3], 4, nstart=5, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(pc_items$scores, col=k$clust, pch=16)

items_v2$kmeans_g <- k$cluster

plot3d(pc_items$scores[,1:3],col=k$cluster)

