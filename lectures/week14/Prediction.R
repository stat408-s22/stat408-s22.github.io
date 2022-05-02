## Predictive Modeling R Script

# Download King County Data
Seattle <- read.csv('http://www.math.montana.edu/ahoegh/teaching/stat408/data/SeattleHousing.csv')
head(Seattle)

# Test / Training Construction
set.seed(03252017)
num.houses <- nrow(Seattle)
# need zipcode to be factor for modeling and clustering
Seattle$zipcode <- as.factor(Seattle$zipcode)
test.ids <- base::sample(1:num.houses, size=round(num.houses*.3))
test.set <- Seattle[test.ids,]
train.set <- Seattle[(1:num.houses)[!(1:num.houses) %in% 
                                      test.ids],]

# Regression
lm.1 <- lm(price ~ bedrooms + bathrooms + sqft_living + zipcode +
             waterfront, data=train.set)
summary(lm.1)

# Evaluation
rmse.lm1 <- sqrt(mean((test.set$price - predict(lm.1,test.set))^2))
rmse.lm1
mad.lm1 <- mean(abs(test.set$price - predict(lm.1,test.set)))
mad.lm1

# Polynomial Regression
train.set$sqft_living2 <- train.set$sqft_living^2
test.set$sqft_living2 <- test.set$sqft_living^2

lm.2 <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_living2 + 
             zipcode + waterfront, data=train.set)
summary(lm.2)

rmse.lm2 <- sqrt(mean((test.set$price - predict(lm.2,test.set))^2)) 
rmse.lm1
rmse.lm2
mad.lm2 <- mean(abs(test.set$price - predict(lm.2,test.set)))
mad.lm1
mad.lm2

# Decision Tree
library(rpart)
tree1 <- rpart(price ~ ., data=train.set, method = 'anova')
plot(tree1)
text(tree1)
print(tree1)
?print.rpart

rmse.tree1 <- sqrt(mean((test.set$price - predict(tree1,test.set))^2)) 
rmse.tree1
rmse.lm2

mad.tree1 <- mean(abs(test.set$price - predict(tree1,test.set)))
mad.tree1
mad.lm2

# Random Forest
library(randomForest)
rf1 <- randomForest(price~., data=Seattle)
plot(rf1)
rmse.rf <- sqrt(mean((test.set$price - predict(rf1,test.set))^2)) 
rmse.rf
rmse.lm2
mad.rf <- mean(abs(test.set$price - predict(rf1,test.set)))
mad.rf
mad.lm2
