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
library(shinythemes)

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
  theme = shinytheme("cosmo"),
  # Application title
  navbarPage("Fun Class Projects"),
  
  tabsetPanel(tabPanel(
    "Evapotranspiration and Crop Growth",
    
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
            label = h3("Crop Coefficient (Kc)"),
            min = 0,
            max = 1.2,
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
            label = h3("Plant Wilting Point (mm?)"),
            min = -15,
            max = 0,
            value = -5,
            step = 1
            
          )
        )
        
      ),
      fluidRow(
        column(
          3,
          offset = 4,
          h3("Crop Coefficient Help"),
          tags$img(src = "kc-image.png", height = 200, width = 300, align = "center")
        ),
        column(
          3,
          offset = 2,
          h3("Plant Wilting Point Help"),
          tags$img(src = "pwp-image.gif", height = 200, width = 300, align = "center")
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
      geom_line(color = "purple", size = 2) + 
      geom_line(aes(y = ET_daily),
                color = "gray80") +
      theme_bw() + 
      scale_x_date(date_labels = "%b %d") +
      theme(axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.2))) +
      labs(x = NULL,
           y = "Daily Evapotranspiration (mm)")
  })

  
   output$tjPlot2 <- renderPlot({
    
     
     ggplot(data = liq_tj1(),
            aes(date, paw)) + 
       geom_hline(yintercept = input$tjpawcut,
                  linetype = "dashed",
                  color = "black") + 
       geom_line(color = "gray", size = 1, linetype = "dashed") +
       geom_point(aes(color = paw_color), size = 5) +
       scale_x_date(date_labels = "%b %d") + 
       theme_bw() + 
       guides(color = F) + 
       scale_color_manual(values = c("bad" = "red", 
                                     "good" = "black")) +
       theme(axis.title = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(1.2))) +
       labs(x = NULL,
            y = "Plant Available Water (mm)")
     
     
  })
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
