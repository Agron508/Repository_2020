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

#may need to be changed
#setwd("C:/Users/Richard/Documents/GitHub/Shiny_Richard")

rawhour <- read_csv("../tidy-data/condensed.csv")
#rawhour <- read_csv("condensed.csv")

hourly <- data.frame(rawhour)

#filter(hourly[,4])

hourly_f <- hourly %>% 
  filter(mol_pho<0.002) %>% 
  as_tibble()

c <- 3E8
h <- 6.63E-19
energy <- hourly_f$mol_pho * 6.022E23 * (h * c)/(hourly_f$wavelength / 1E-9)

dat3 <- cbind(hourly_f,energy) %>%  
  mutate(month_lab = month(month, label = T))


#--create the drop down menu choices
dd_month <- dat3 %>% select(month_lab) %>% pull() %>% unique()
dd_var <- colnames(dat3[c(4,5)])



ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # Application title
  navbarPage("Agron508 Richard Project"),
  
  tabsetPanel(tabPanel(
    "Downwelling shortwave radiation near Ellsworth, IA, 2019",
    
    fluidRow(


      #month select
        column(
          2,
          selectInput(
            inputId = "tjmonth",
            label = h4("Pick A Month:"),
            selected = "Jul",
            choices = dd_month
          )
        ),
        #variable select (couldn't get to work so does nothing)
        column(
          3,
          selectInput(
            inputId = "tjvar",
            label = h4("Pick A Variable:"),
            selected = "mol_pho",
            choices = dd_var
          )
        )
        

      ),
      fluidRow(
        #the plot
        column(12,
               plotOutput("tjPlot1"))

      )
      
    )
  )
  )

  
             



# Define server logic required
server <- function(input, output) {
  
  #input: month
  liq_tj1 <- reactive({
    
    dat3 %>%
      filter(month_lab == input$tjmonth)

  })
  
  #makes the plot
  output$tjPlot1 <- renderPlot({
    liq_tj1() %>% 
      select(month, hour, wavelength, mol_pho, month_lab)  %>% 
      filter(hour >= 6, hour <= 20) %>%
      mutate(wl_bins = (wavelength %/% 10)*10,
             hour2 = trunc(hour/2)*2) %>%
      group_by(month,hour2,wl_bins) %>%
      summarise(mol_pho_sum = sum(mol_pho,na.rm = TRUE)) %>%
      ggplot(aes(x = wl_bins,y = mol_pho_sum)) +
      geom_col() +
      facet_wrap(~hour2) + 
      labs(x = "wavelength (nm)",
           y = bquote('photon flux '(number/sm^2)),
           title = "Photon Flux Over Next Two Hours")
  })


  
}

# Run the application 
shinyApp(ui = ui, server = server)
