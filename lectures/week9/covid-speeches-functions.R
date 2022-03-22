# Load required libraries
library(tidyverse)
library(rvest)
library(lubridate)

# Iterate through what you want the function to do for a single speech webpage:

## Read page for 26 Oct speech
url <- "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-26-october/"
speech_page <- read_html(url)

## Extract title
title <- speech_page %>%
  html_element(".article-header__title") %>%
  html_text()
title

## Extract date
date <- speech_page %>%
  html_element(".content-data__list:nth-child(1) strong") %>%
  html_text() %>%
  dmy()
date

## Extract location
location <- speech_page %>%
  html_element(".content-data__list+ .content-data__list strong") %>%
  html_text()
location

## Extract abstract
abstract <- speech_page %>%
  html_element(".leader--first-para p") %>%
  html_text()
abstract

## Extract text
text <- speech_page %>% 
  html_elements("#preamble p") %>%
  html_text() %>%
  list()
text

## Put it all in a data frame
oct_26_speech <- tibble(
  title    = title,
  date     = date,
  location = location,
  abstract = abstract,
  text     = text,
  url      = url
)
oct_26_speech
  

# Function to repeat this sequence of code
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

## Function in action

scrape_speech(url = "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-26-october/") %>%
  glimpse()

scrape_speech(url = "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-23-october/") %>%
  glimpse()

scrape_speech(url = "https://www.gov.scot/publications/coronavirus-covid-19-update-first-ministers-speech-22-october/") %>%
  glimpse()
