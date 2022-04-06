library(tidyverse)
library(openintro)

# Original data
data(elmhurst)

# Goal: Write a function that will take our original
# sample and create a 95% bootstrap confidence interval
# for the true population slope.

set.seed(408)

# A single bootstrap sample:
index <- sample(1:50, size = 50, replace = TRUE)
resamp <- elmhurst[index, 1:2]

# Calculate slope from resample
lm(gift_aid ~ family_income, data = resamp)$coef[2]

# Create a loop to do this 10,000 times
reps <- 10000
slopes <- vector("numeric", length = reps)
for(i in 1:reps) {
  index <- sample(1:50, size = 50, replace = TRUE)
  resamp <- elmhurst[index, 1:2]
  slopes[i] <- lm(gift_aid ~ family_income, data = resamp)$coef[2]
}
hist(slopes)

# Use bootstrapped slopes to calculate a 95% CI
quantile(slopes, c(0.025, 0.975))


# Put all our code into a function:
# Inputs?
# - reps (number of bootstrap replicates)
# - In the future: data set (1st column is explanatory variable; 2nd column response)

bootstrap_elmhurst <- function(reps = 10000, conf.level = 0.95){
  slopes <- vector("numeric", length = reps)
  for(i in 1:reps) {
    index <- sample(1:50, size = 50, replace = TRUE)
    resamp <- elmhurst[index, 1:2]
    slopes[i] <- lm(gift_aid ~ family_income, data = resamp)$coef[2]
  }
  hist(slopes)
  # Use bootstrapped slopes to calculate a 95% CI
  quantile(slopes, c((1 - conf.level)/2, conf.level + (1 - conf.level)/2))
}
