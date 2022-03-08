library(tidyverse)
library(rvest)
library(fredr)

# shortest women in the world table from Wikipedia
short_women_table <- read_html("https://en.wikipedia.org/wiki/List_of_the_verified_shortest_people") %>% # read_html() reads the text of the website
  html_nodes("#mw-content-text > div.mw-parser-output > table:nth-child(13)") %>% 
  '[['(1) %>%
  html_table(fill=TRUE)

# FRED data on ethereum values

eth_val <- fredr(
  series_id = "CBETHUSD",
  observation_start = as.Date("2017-03-08"),
  observation_end = as.Date("2022-03-07")
)