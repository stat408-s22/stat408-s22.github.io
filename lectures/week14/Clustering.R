## Clustering Code
library(mnormt) # for rmnorm

## Simulate data from three clusters
set.seed(10)
cluster1 <- rmnorm(n=25,mean=c(.3,.2), varcov=diag(2)*.025)
cluster2 <- rmnorm(n=25,mean=c(.15,.75),varcov=diag(2)*.025)
cluster3 <- rmnorm(n=50, mean=c(.75,.6),varcov=diag(2)*.03)
combined <- rbind(cluster1,cluster2,cluster3)
plot(combined,type='n',axes=F,xlab='', ylab='')
points(cluster1,pch=16,col='dodgerblue')
points(cluster2,pch=16,col='forestgreen')
points(cluster3,pch=16,col='firebrick4')

## k-means clustering
?kmeans
km <- kmeans(combined, 3)
km
plot(combined,type='n',axes=F, xlab='',ylab='')
box()
# Plot points: color is from simulated cluster (truth) and number
#              is assigned cluster.
points(combined,pch=as.character(km$cluster), col=c(rep('dodgerblue',25),rep('forestgreen',25),rep('firebrick4',50)))

# Scree plot
## Evaluate within cluster sum of squares for 1 to 15 clusters
wss <- rep(0,15)
for (i in 1:15) wss[i] <- sum(kmeans(combined,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


## Hierarchical Clustering
## Use animal dataset
animals <- cluster::animals
colnames(animals) <- c("warm-blooded", 
                       "can fly",
                       "vertebrate",
                       "endangered",
                       "live in groups",
                       "have hair")
# Delete a few variables and clean up data
animals.cluster <- animals[,-(5)]
animals.cluster <- animals.cluster[-c(4,5,12,16,18),]
animals.cluster[10,4] <- 2
animals.cluster[14,4] <- 1

# Distance calculations and MDS
d <- dist(animals.cluster) 
?cmdscale

fit <- cmdscale(d, k=2) 

plot(fit[,1], fit[,2], xlab="", ylab="",   main="",	type="n",axes=F)
box()
text(fit[,1], fit[,2], labels = row.names(animals.cluster), cex=1)

# Hierarchical Clustering
?hclust
# complete: maximum difference between elements in clusters
# single: minimum difference
hc <- hclust(dist(animals.cluster))
plot(hc, hang=-1)

# obtaining cluster assignments
cutree(hc, 4)
