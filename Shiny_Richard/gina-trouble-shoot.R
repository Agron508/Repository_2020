#This is my app to edit

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
library(shinythemes)



dat2 <- read_csv("data/tidy/condensed.csv") %>%
  filter(mol_pho < 0.002) %>%
  filter(hour >= 6, hour <= 20) %>%
  mutate(
    wl_bins = (wavelength %/% 10) * 10,
    hour2 = trunc(hour / 2) * 2,
    energy = mol_pho * 6.022E23 * (6.63E-19 * 3E8) / wavelength / 1E-9,
    month_lab = month(month, label = T)
  ) %>%
  select(month_lab, wl_bins, hour2, mol_pho, energy) %>%
  pivot_longer(mol_pho:energy) %>%
  group_by(month_lab, hour2, wl_bins, name) %>%
  summarise(val_sum = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(hour2 = paste0(hour2, "-", hour2+2)) %>% 
  mutate(mycolor = ifelse(
    wl_bins < 380, "thistle",
    ifelse(wl_bins < 440, "purple4",
           ifelse(wl_bins < 490, "blue",
                  ifelse(wl_bins < 500, "lightblue",
                         ifelse(wl_bins < 570, "green4",
                                ifelse(wl_bins < 590, "yellow",
                                       ifelse(wl_bins < 630, "orange",
                                              ifelse(wl_bins < 740, "red", "darkred")
                                       )
                                )
                         )
                  )
           )
    )
  )
)

mycolor_vct <- 
  dat2 %>% 
  select(mycolor) %>% 
  unique() %>% 
  pull()

dat2 %>% 
  mutate(mycolor = factor(mycolor, levels = mycolor_vct)) %>% 
    filter(month_lab == "Jul") %>% 
  filter(name == "mol_pho") %>% 
  ggplot(aes(x = wl_bins,y = val_sum)) +
      geom_col(aes(fill = mycolor)) +
      facet_wrap(~hour2) + 
      labs(x = "wavelength (nm)",
           y = bquote('photon flux '(number/sm^2)),
           title = "Photon Flux Over Next Two Hours") +
  scale_fill_manual(values = mycolor_vct)
  