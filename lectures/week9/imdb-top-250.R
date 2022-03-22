# Load required libraries
library(tidyverse) # includes stringr
library(robotstxt) # check if we're allowed to scrape
library(rvest) # reads html

# Webpage source: http://www.imdb.com/chart/top

# Are we allowed to scrape?.... (should we scrape? is a different question)
paths_allowed("http://www.imdb.com")

# Read in the whole page
page <- read_html("https://www.imdb.com/chart/top/")
page
typeof(page)
class(page)

# Save titles
titles <- page %>%
  html_elements(".titleColumn a") %>% # Scrape nodes
  html_text() # Remove text from nodes
titles

# Save years
years <- page %>%
  html_elements(".secondaryInfo") %>%
  html_text() %>%
  str_remove("\\(") %>% # remove (
  str_remove("\\)") %>% # remove )
  as.numeric()
years

# Save ratings
ratings <- page %>%
  html_elements("strong") %>%
  html_text() %>%
  as.numeric()
ratings

# Create data frame
imdb_top_250 <- tibble(
  title = titles, 
  year = years, 
  rating = ratings
)
imdb_top_250

# Add a new "rank" variable (since we didn't scrape that variable)
imdb_top_250 <- imdb_top_250 %>%
  mutate(rank = 1:nrow(imdb_top_250)) %>%
  relocate(rank) # Move to be first variable in data fram

## Explore

# Which years have the most movies on the list?
imdb_top_250 %>% 
  count(year, sort = TRUE)

# Which 1995 movies made the list?
imdb_top_250 %>% 
  filter(year == 1995) %>%
  print(n = 8)

# Visualize the average yearly rating for movies that made it on the top 250 list over time.
imdb_top_250 %>% 
  group_by(year) %>%
  summarise(avg_score = mean(rating)) %>%
  ggplot(aes(y = avg_score, x = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "Average score")

# Write to a csv file
write_csv(imdb_top_250, file = "imdb_top_250.csv")
