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
dat <- read_csv("data/tidy/ET_miscanthus_SABR.csv") %>% 
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
                 selectInput(inputId = "month2", 
                             label = "Tyler's Favorite Month:",
                             selected = "Jul",
                             choices = dd_month)
               ),
               
               mainPanel(plotOutput("etPlot2")))
    ) #--end Tyler panel
    
  )
)



# ui2 <- fluidPage(
#   
#   # Application title
#   titlePanel("ET SABR Miscanthus"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("myyear","Year", choices = dd_year),
#       selectInput("mymonth","Month",choices = dd_month)
#       
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("etPlot")
#     )
#   )
# )


# Define server logic required
server <- function(input, output) {
  
  # this is for Rojda's tab--------------------------
  liq_dat1 <- reactive({
    dat %>%
      mutate(color_id = ifelse(month_id == input$month1, "selected", "no"))
  })
  
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
               y = ET_daily)) +
      geom_jitter(aes(color = color_id), size = 3) +
      scale_color_manual(values = c("selected" = "red",
                                    "no" = "gray80")) +
      
      theme_bw()
  })
  
  
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
