# Load required libraries
library(tidyverse)
library(rvest)
library(lubridate)

# Function to scrape a speech from a single url
scrape_speech <- function(url) {
  
  speech_page <- read_html(url)
  
  title <- speech_page %>%
    html_element(".article-header__title") %>%
    html_text()
  
  date <- speech_page %>%
    html_element(".content-data__list:nth-child(1) strong") %>%
    html_text() %>%
    dmy()
  
  location <- speech_page %>%
    html_element(".content-data__list+ .content-data__list strong") %>%
    html_text()
  
  abstract <- speech_page %>%
    html_element(".leader--first-para p") %>%
    html_text()
  
  text <- speech_page %>%
    html_elements("#preamble p") %>%
    html_text() %>%
    list()
  
  tibble(
    title = title, date = date, location = location,
    abstract = abstract, text = text, url = url
  )
}

# Now we want to iterate this function through all speech urls

## All URLs
all_speeches_page <- read_html("https://www.gov.scot/collections/first-ministers-speeches/")

covid_speech_urls <- all_speeches_page %>%
  html_nodes(".collections-list a") %>%
  html_attr("href") %>% # Relative links
  str_subset("covid-19") %>% # Subset
  str_c("https://www.gov.scot", .) # . makes the input from the pipeline go to the second argument


## Go to each page, scrape speech
# This takes a while!
covid_speeches <- map_dfr(covid_speech_urls, scrape_speech)

# Write to RDS file
#write_rds(covid_speeches, file = "covid_speeches.rds")
