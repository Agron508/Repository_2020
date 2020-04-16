# Gina
# created 4/16/2020
# purpose: get josh's data all in one place/same format


library(tidyverse)
library(janitor)
library(lubridate)

# read in data ------------------------------------------------------------


pet <- 
  tibble(files = list.files("data/raw/")) %>% 
  filter(str_detect(files, "pet")) %>% 
  mutate(filepath = paste0("data/raw/", files),
         data = filepath %>% map(read_csv)) %>% 
  unnest(cols = data) %>% 
  clean_names() %>%
  mutate(year = year(date)) %>% 
  group_by(date, year, doy) %>% 
  summarise(pet_mm = sum(pet_mm, na.rm = T)) 



# look at it --------------------------------------------------------------

pet %>% 
  ggplot(aes(doy, pet_mm)) + 
  geom_point(aes(color = year))

#--there are a 50 instances where it is 0. In April that seems weird. 
pet %>% 
  filter(pet_mm == 0)

# write to tidy folder ----------------------------------------------------

pet %>% write_csv("data/tidy/data_ames-pet.csv")


