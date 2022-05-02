### Classification and Cross-Validation
set.seed(10)
cluster1 <- rmnorm(n=25,mean=c(.3,.2), varcov=diag(2)*.025)
cluster2 <- rmnorm(n=25,mean=c(.15,.75),varcov=diag(2)*.025)
cluster3 <- rmnorm(n=50, mean=c(.75,.6),varcov=diag(2)*.03)
plot(rbind(cluster1,cluster2,cluster3),type='n',axes=F,xlab='', ylab='')
points(cluster1,pch='1',col='dodgerblue')
points(cluster2,pch='1',col='dodgerblue')
points(cluster3,pch='0',col='firebrick4')
box()
pred.points <- cbind(c(.2,.45,.48,.9),c(.7,.35,.3,.4))

colnames(pred.points) <- c('x','y')
points(pred.points, pch='*', cex=2.5)

# Logistic Regression
labels <- (rep(c(1,0),each=50))
combined <- rbind(cluster1, cluster2, cluster3)
supervised <- as.data.frame(cbind(labels,combined))
colnames(supervised)[2:3] <- c('x','y')
?glm
logistic <- glm(labels ~ x + y, data = supervised, family='binomial')
summary(logistic)
library(knitr)
kable(cbind(pred.points,round(predict(logistic,as.data.frame(pred.points), type='response'),3)),col.names=c('x','y','Prob[Val = 1]'))

# Decision Tree
library(rpart)
tree1 <- rpart(labels ~., data=supervised, method = 'class')
print(tree1)
plot(tree1)
text(tree1)
kable(cbind(pred.points,
            round(predict(tree1,as.data.frame(pred.points))[,2],3)),
      col.names=c('x','y','Prob[Val = 1]'))

## Cross-Validation with logistic regression
Seattle <- read.csv('http://www.math.montana.edu/ahoegh/teaching/stat408/data/SeattleHousing.csv')
Seattle$million <- as.numeric(Seattle$price > 1000000)
num.folds <- 3
num.houses <- nrow(Seattle)
pred.vector <- rep(0,num.houses)
# create folds
fold.ids <- sample(num.folds,num.houses,replace=T)

for (i in 1:num.folds){
  train <- Seattle[fold.ids != i,]
  test <- Seattle[fold.ids == i,]
  logistic <- glm(million ~ bedrooms + bathrooms + sqft_living + sqft_lot + waterfront + floors, data = train, family='binomial')
  pred.vector[fold.ids == i] <- predict(logistic,as.data.frame(test), type='response')
}

CE <- 1 - mean(round(pred.vector) == Seattle$million)

