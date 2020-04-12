#
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

#--read in the data for trouble-shooting

# et <- 
#   read_csv("data/tidy/data_et-sabr-miscan.csv") %>% 
#   mutate(date = mdy(Month),
#          month_id = month(date),
#          doy = DOY) %>% 
#   select(year_id, month_id, doy, ET_daily)
#   
# wx <- read_csv("data/tidy/data_ames-wea-2019.csv") %>% 
#   mutate(year_id = year(date))



et <- 
  read_csv("../data/tidy/data_et-sabr-miscan.csv") %>% 
  mutate(date = mdy(Month),
         month_id = month(date),
         doy = DOY) %>% 
  select(year_id, month_id, doy, ET_daily)

wx <- read_csv("../data/tidy/data_ames-wea-2019.csv") %>% 
  mutate(year_id = year(date))



dat <-
  et %>% 
  left_join(wx) %>%  
  mutate(month_lab = month(month_id, label = T))

# for trouble shooting ----------------------------------------------------

     
dat.fake <- 
  dat %>%
  filter(month_lab == "Jun",
         year_id == 2019) %>% 
  mutate(cc = as.numeric(0.5)) %>% 
  mutate(ET_coef = ET_daily * cc,
         net_mm = -ET_coef + precip_mm,
         cum_mm = cumsum(net_mm),
         paw = as.numeric(15) + cum_mm)

    
dat.fake %>% 
  ggplot(aes(date, paw)) + 
  geom_point() + 
  scale_x_date(date_labels = "%b %d")

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique()
dd_month <- dat %>% select(month_lab) %>% pull() %>% unique()
dd_paw <- seq(1, 15, 1)

ui <- fluidPage(
  # Application title
  navbarPage("Fun Class Projects"),
  
  tabsetPanel(tabPanel(
    "Tyler and Josh",
    
    fluidRow(
      column(6,
             plotOutput("tjPlot1")),
      column(6,
             plotOutput("tjPlot2")),
      
      hr(),
      
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "tjyear",
            label = h4("Pick A Year:"),
            selected = 2019,
            choices = dd_year
          )
        ),
        column(
          2,
          selectInput(
            inputId = "tjmonth",
            label = h4("Pick A Month:"),
            selected = "Jun",
            choices = dd_month
          )
        ),
        
       column(
          3,
          sliderInput(
            "tjcc",
            label = h3("Crop Coefficient"),
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          )
        ),
       
       column(
         2,
         selectInput(
           inputId = "tjpaw",
           label = h4("Starting PAW (mm):"),
           selected = 15,
           choices = dd_paw
         )
       ),
        column(
          3,
          sliderInput(
            "tjpawcut",
            label = h3("Pick a PAW cutoff"),
            min = -15,
            max = 0,
            value = -5,
            step = 1
            
          )
        )
        
      )
    )
  )
  )
)
  
             



# Define server logic required
server <- function(input, output) {
  
  # This is for Tyler/Josh tab-------------------
  liq_tj1 <- reactive({
    
    dat %>%
      filter(month_lab == input$tjmonth,
             year_id == input$tjyear) %>% 
      mutate(cc = as.numeric(input$tjcc)) %>% 
      mutate(ET_coef = ET_daily * cc,
             net_mm = -ET_coef + precip_mm,
             cum_mm = cumsum(net_mm),
             paw = as.numeric(input$tjpaw) + cum_mm,
             paw_color = ifelse(paw < as.numeric(input$tjpawcut), "bad", "good"))
    
  })
  
  
  output$tjPlot1 <- renderPlot({
  
      ggplot(data = liq_tj1(),
           aes(x = date,
               y = ET_coef)) +
      geom_line(color = "red") + 
      geom_line(aes(y = ET_daily),
                color = "gray80") +
      geom_point(aes(y = ET_daily), 
                  color = "gray80",
                  size = 3) +
      geom_point(color = "red", 
                  size = 3) +
      theme_bw() + 
      scale_x_date(date_labels = "%b %d") +
      labs(x = NULL,
           y = "Daily Evapotranspiration (mm)")
  })

  
   output$tjPlot2 <- renderPlot({
    
     
     ggplot(data = liq_tj1(),
            aes(date, paw)) + 
       geom_hline(yintercept = input$tjpawcut,
                  linetype = "dashed",
                  color = "black") + 
       geom_line(color = "gray", size = 1) +
       geom_point(aes(color = paw_color), size = 3) +
       scale_x_date(date_labels = "%b %d") + 
       theme_bw() + 
       guides(color = F) + 
       scale_color_manual(values = c("bad" = "red", 
                                     "good" = "black")) +
       labs(x = NULL,
            y = "Plant Available Water (mm)")
     
     
  })
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
