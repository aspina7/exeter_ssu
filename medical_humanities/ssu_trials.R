library(rvest)
library(dplyr)
library(stringr)

## define the website
epidemic_list <- read_html("https://en.wikipedia.org/wiki/List_of_epidemics") %>% 
  ## define which bit want to pull out (in this case its called a wikitable after inspecting)
  html_nodes(xpath='//table[contains(@class, "wikitable")]') %>%
  ## pull in to a table and fill in empty rows with NA
  html_table(fill = TRUE) 


## get it out of a list 
epidemic_list <- epidemic_list[[1]]


## get start year 
epidemic_list <- mutate(epidemic_list, 
                        start_yr = str_remove(
                          str_sub(epidemic_list$Date, 1, 4), "-"), 
                        start_yr = as.numeric(start_yr))


## filter for after 1900 
epidemic_list <- filter(epidemic_list, 
                        start_yr >= 1900)
