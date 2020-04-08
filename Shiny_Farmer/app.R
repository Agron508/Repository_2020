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

library(scales)
library(lubridate)


#--read in the data


#dat<- read.csv("C:/Users/myAdmins/Desktop/Rshiny_flux/data_ET-for-shiny.csv")
#dat <- read.csv("~/GitHub/Repository_2020/data/tidy/ET_miscanthus_SABR.csv") %>% 
#  mutate(month = month(Month, label = T))
#ET_monthly=aggregate(dat$ET_daily, by=list(dat$month),mean, na.rm=TRUE)

# use R projects to provide base directory, make sure you have the project open 
# dat <- read_csv("data/tidy/ET_miscanthus_SABR.csv") %>% 
#   mutate(month_id = month(Month, label = T))

# dat <- read_csv("data/tidy/ET_miscanthus_SABR.csv") %>% 
#   mutate(month_id = month(Month, label = T))


dat <- read_csv("../data/tidy/ET_miscanthus_SABR.csv") %>% 
  mutate(month_id = month(Month, label = T)) 


# get the mean ET for each month (?)
datmonth <- 
  dat %>%  
  group_by(month_id) %>% 
  summarise(mean_et = mean(ET_daily))

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique()
dd_month <- dat %>% select(month_id) %>% pull() %>% unique()
dd_paw <- seq(1, 15, 1)

ui <- fluidPage(
  # Application title
  navbarPage("Fun Class Projects"),
  
  tabsetPanel(
    tabPanel("Tyler and Josh",
             
             fluidRow(
               column(6,
                      plotOutput("tjPlot1")),
               column(6,
                      plotOutput("tjPlot2")),
               
               hr(),
               
               fluidRow(
                 
                 column(
                   3,
                   selectInput(
                     inputId = "tjyear",
                     label = "Pick A Year:",
                     selected = 2019,
                     choices = dd_year
                   )
                 ),
                 column(
                 3,
                 selectInput(
                   inputId = "tjmonth",
                   label = "Pick A Month:",
                   selected = "Jul",
                   choices = dd_month
                 )
               ),
               
               column(
                 3,
                 selectInput(
                   inputId = "tjpaw",
                   label = "Pick A Starting PAW (mm):",
                   selected = 15,
                   choices = dd_paw
                 )
               ),
               column(
                 3,
                 sliderInput(
                   "tjcc",
                   label = h3("Pick a Crop Coefficient"),
                   min = 0,
                   max = 1,
                   value = 0.5,
                   step = 0.1
                 )
               )
               )
               
               # tabPanel("Tyler tab", #--start Tyler tab
               #          mainPanel(
               #            # Sidebar with a slider input for number of bins
               #            sidebarPanel(
               #              sliderInput("slider1", label = h3("Pick a Crop Coefficient"),
               #                          min = 0,
               #                          max = 1,
               #                          value = 0.5,
               #                          step = 0.1)
               #            ),
               #
               #              selectInput(inputId = "month2",
               #                          label = "Tyler's Favorite Month:",
               #                          selected = "Jul",
               #                          choices = dd_month)
               #            ),
               #
               #            mainPanel(plotOutput("etPlot2")))
               # ) #--end Tyler tab
               #
             )
    )
  )
)
             



# Define server logic required
server <- function(input, output) {
  
  # This is for Tyler/Josh tab-------------------
  liq_tj1 <- reactive({
    dat %>%
      filter(month_id == input$tjmonth,
             year_id == input$tjyear) %>% 
      mutate(ET_daily_lag = lag(ET_daily),
             ET_daily_lag = ifelse(is.na(ET_daily_lag), 0, ET_daily_lag),
             paw = as.numeric(input$tjpaw),
             cc = as.numeric(input$tjcc)) %>% 
      mutate(cumET = cumsum(ET_daily_lag),
             paw_today = paw - cc*cumET)
    
    
  })
  
  
  output$tjPlot1 <- renderPlot({
  
      ggplot(data = liq_tj1(),
           aes(x = DOY,
               y = ET_daily*cc)) +
      geom_jitter(aes(x = DOY,
                      y = ET_daily), 
                  color = "black",
                  size = 3) +
      geom_jitter(aes(color = color_id), 
                  color = "red", 
                  size = 3) +
      theme_bw() + 
      labs(x = NULL,
           y = "Daily Evapotranspiration (mm)")
  })

  
   output$tjPlot2 <- renderPlot({
    
    ggplot(data = liq_tj1(),
           aes(x = DOY,
               y = paw_today)) +
       geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_point(color = "gray", size = 3) +
       geom_line(color = "gray", size = 2) +
      theme_bw() + 
       labs(x = NULL,
            y = "Plant Available Water (mm)")
     
     
  })
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
