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
  mutate(date = ymd(paste(c_year, c_month, c_day)),
         doy = yday(date)) %>% 
  group_by(date, doy) %>% 
  summarise(precip_mm = sum(precip_mm, na.rm = T)) 


#--something is wrong. 
wea %>%
  mutate(year = year(date)) %>% 
  filter(year == 2019) %>% 
  ggplot(aes(doy, precip_mm)) + 
  geom_point()


#--just do 2019 for now
wea19 <- 
  read_csv("data/raw/ames_2019.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(paste(c_year, c_month, c_day))) %>% 
  group_by(date, doy) %>% 
  summarise(precip_mm = sum(precip_mm))

wea19 %>%
  ggplot(aes(doy, precip_mm)) + 
  geom_point()


# write to tidy folder ----------------------------------------------------

#wea %>% write_csv("data/tidy/data_ames-wea.csv")

wea19 %>% write_csv("data/tidy/data_ames-wea-2019.csv")


wea %>% 
  ggplot(aes(yday, precip_mm)) + 
  geom_point(aes(color = c_year))
