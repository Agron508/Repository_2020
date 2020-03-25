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

ui <- fluidPage(
  # Application title
  navbarPage("ET SABR Miscanthus"),
  
  tabsetPanel(
    
    tabPanel("Rojda tab", #--start Rojda panel
             mainPanel(
               sidebarPanel(
                 selectInput(inputId = "month1", 
                             label = "Month:",
                             selected = "Jun",
                             choices = dd_month)
               ),
               
               mainPanel(plotOutput("etPlot1")))
    ), #--end Rojda panel
    
    
    tabPanel("Tyler tab", #--start Tyler tab
             mainPanel(
               # Sidebar with a slider input for number of bins
               sidebarPanel(
                 sliderInput("slider1", label = h3("Slider"), min = 0, 
                             max = 1, value = 0.5)
               ),
               
                 selectInput(inputId = "month2", 
                             label = "Tyler's Favorite Month:",
                             selected = "Jul",
                             choices = dd_month)
               ),
               
               mainPanel(plotOutput("etPlot2")))
    ), #--end Tyler panel
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("etPlot"),
      plotOutput("et2")
    )
  )



# Define server logic required
server <- function(input, output) {
  
  #--build reactive dataset, changes year and month highlight
  liq_dat <- reactive({
      dat %>%
    mutate(color_id = ifelse(month_id == input$mymonth, "selected month", "no"))})
  
  # this is for Rojda's tab--------------------------
  liq_dat1 <- reactive({
    dat %>%
      mutate(color_id = ifelse(month_id == input$month1, "selected", "no"))})
  
  output$etPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = liq_dat1(),
           aes(x =DOY,
               y = ET_daily)) +
      geom_jitter(aes(color = color_id), size = 3) +
      scale_color_manual(values = c("selected" = "red",
                                    "no" = "gray80")) +
      
      theme_bw()
  })
  
  # This is for Tyler's tab-------------------
  liq_dat2 <- reactive({
    dat %>%
      mutate(color_id = ifelse(month_id == input$month2, "selected", "no"))
  })
  
  
  output$etPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = liq_dat2(),
           aes(x =DOY,
               y = ET_daily*input$slider1)) +
      geom_jitter(aes(color = color_id), size = 3) +
      geom_jitter(aes(x=DOY,y=ET_daily), color="black", size = 3) +
      scale_color_manual(values = c("selected" = "red",
                                    "no" = "gray80")) +
      
      theme_bw()
  })




    ##--build reactive dataset, changes year highlight
  liq_dat <- reactive({
   dat %>%
      mutate(color_id = ifelse(month_id == input$myyear, "selected month", "no"))
  })


  output$et2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = ET_monthly,
           aes(x =Group.1,
               y = x)) +
      #facet_wrap( ~ month,ncol=3)+
      #geom_jitter(aes(color = ""), size = 3) +
      geom_point(color="red", size=3)+
      scale_color_manual(values = c("selected month" = "red",
                                    "no" = "gray80")) +
    ylab("Evapotranspiration (mm/day)")+xlab("")+

      theme_bw()
  })
    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
