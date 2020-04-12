# Gina
# created 4/12/2020
# purpose: get andy's data all in one place/same format


library(tidyverse)
library(janitor)
library(lubridate)

# read in data ------------------------------------------------------------


wea <- 
  tibble(files = list.files("data/raw/")) %>% 
  filter(str_detect(files, "ames")) %>% 
  mutate(filepath = paste0("data/raw/", files),
         data = filepath %>% map(read_csv)) %>% 
  unnest(cols = data) %>% 
  clean_names() %>% 
  group_by(c_year, c_month, c_day) %>% 
  summarise(precip_mm = sum(precip_mm)) %>%
  mutate(date = ymd(paste(c_year, c_month, c_day)),
         yday = yday(date)) 
           


# write to tidy folder ----------------------------------------------------

wea %>% write_csv("data/tidy/data_ames-wea-from-andy.csv")
