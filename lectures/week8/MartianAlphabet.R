# Solutions to Martian Alphabet Activity (STAT 408 version)
library(tidyverse)

# Plots -------------------------------------------------------------------

## Plot observed class data
n <- 40 # sample size
x <- 32 # number correct

### Plotting frequencies from raw data
obs_dat <- data.frame(
  Guess = rep(c("Correct", "Incorrect"), c(x, n-x))
)
obs_dat %>% ggplot(aes(x = Guess)) + 
  geom_bar() + 
  labs(title = "Frequency Bar Plot of Class Data", 
       x = "Guess",  
       y = "Frequency")

### Plotting proportions from raw data
obs_dat %>% ggplot(aes(x = Guess)) + 
  geom_bar(aes(y = ..prop.., group = 1)) + 
  labs(title = "Frequency Bar Plot of Class Data", 
       x = "Guess",  
       y = "Frequency")

### Plotting proportions directly
sum_dat <- data.frame(
  Guess = c("Correct", "Incorrect"),
  Proportion = c(x/n, 1-x/n)
)
sum_dat %>% ggplot(aes(x = Guess, y = Proportion)) + 
  geom_col() +
  labs(title = "Relative Frequency Bar Plot of Class Data", 
       x = "Guess",  
       y = "Proportion")
  
# Simulations -------------------------------------------------------------

## Simulating a single student's guess
sample(c("correct", "incorrect"), size = 1, prob = c(0.5, 0.5))

## Simulating a single class's guess
n <- 40 # sample size
p <- 0.5 # hypothesized probability of "correct" guess
dat <- sample(c("correct", "incorrect"), size = n, replace = TRUE, prob = c(p, 1-p))

# Proportion correct
mean(dat == "correct")

## Simulating 1,000 class's guesses
reps <- 1000
props <- NULL
for(i in 1:reps){
  dat <- sample(c("correct", "incorrect"), size = n, 
                replace = TRUE, prob = c(p, 1-p))
  props[i] <- mean(dat == "correct")
}

# Plot proportions
props <- data.frame(props)
props %>% ggplot(aes(x = props)) + 
  geom_dotplot(dotsize = 0.2) +
  scale_y_continuous(NULL, breaks = NULL) + # Hide y-axis
  labs(x = "Proportion Correct",
       title = "Simulated proportion correct for 1,000 classes")
