# Load required libraries
library(tidyverse)
library(rvest)
library(robotstxt)

# First, check if this is allowed
paths_allowed("https://www.imdb.com/chart/top")

# Read in the whole page
page <- read_html("https://www.imdb.com/chart/top")

# Scrape the titles
titles <- page %>% html_elements(".titleColumn a") %>% html_text()

# Scrape the years
years <- page %>% 
  html_elements(".secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>%
  str_remove("\\)") %>%
  strtoi()  # as.numeric()

# Scrape the ratings
ratings <- page %>% 
  html_elements("strong") %>% 
  html_text() %>% 
  as.numeric()

# Put it all into a data frame
imdb_top_250 <- tibble(
  title = titles,
  year = years,
  rating = ratings
)
