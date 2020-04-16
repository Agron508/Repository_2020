# Gina
# created 4/12/2020
# purpose: get andy's data all in one place/same format


library(tidyverse)
library(janitor)
library(lubridate)

# read in data ------------------------------------------------------------


rawwea <- 
  tibble(files = list.files("data/raw/")) %>% 
  filter(str_detect(files, "ames"),
         !str_detect(files, "pet")) %>% 
  mutate(filepath = paste0("data/raw/", files),
         data = filepath %>% map(read_csv)) %>% 
  unnest(cols = data) %>% 
  clean_names() %>% 
  select(c_year, c_month, c_day, precip_mm) %>% 
  group_by(c_year, c_month, c_day) %>% 
  summarise(precip_mm = sum(precip_mm, na.rm = T)) %>% 
  mutate(date = ymd(paste(c_year, c_month, c_day))) %>% 
  ungroup() %>% 
  select(date, precip_mm)

rawwea %>% 
  ggplot(aes(date, precip_mm)) +
  geom_point()

# something is wrong w/2018 and 2019

wea12_17 <- 
  rawwea %>%
  mutate(year = year(date)) %>% 
  filter(year < 2018)

#--just do 2019 for now
wea19 <- 
  read_csv("data/raw/ames_2019.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(paste(c_year, c_month, c_day))) %>% 
  group_by(date, doy) %>% 
  summarise(precip_mm = sum(precip_mm)) %>% 
  mutate(year = year(date))

#--just do 2018 for now
wea18 <- 
  read_csv("data/raw/ames_2018.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(paste(c_year, c_month, c_day))) %>% 
  group_by(date, doy) %>% 
  summarise(precip_mm = sum(precip_mm)) %>% 
  mutate(year = year(date))



# put them tog ------------------------------------------------------------

wea_all <- 
  wea12_17 %>% 
  bind_rows(wea18) %>% 
  bind_rows(wea19) %>% 
  select(-doy)

wea_all %>% 
  ggplot(aes(date, precip_mm)) +
  geom_point()



# write to tidy folder ----------------------------------------------------

wea_all %>% write_csv("data/tidy/data_ames-wea.csv")


