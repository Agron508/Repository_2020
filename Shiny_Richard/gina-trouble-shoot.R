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
  mutate(hour2 = paste0(hour2, "-", hour2+2))


      ggplot(aes(x = wl_bins,y = mol_pho_sum)) +
      geom_col() +
      facet_wrap(~hour2) + 
      labs(x = "wavelength (nm)",
           y = bquote('photon flux '(number/sm^2)),
           title = "Photon Flux Over Next Two Hours")
  