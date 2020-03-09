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
dat <- read.csv("~/GitHub/Repository_2020/data/tidy/ET_miscanthus_SABR.csv") %>% 
  mutate(month = month(Month, label = T))
ET_monthly=aggregate(dat$ET_daily, by=list(dat$month),mean, na.rm=TRUE)
  

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique() 
dd_month <- dat %>% select(month) %>% pull() %>% unique()
dd_DOY<- dat %>% select(DOY) %>% pull() %>% unique()

ui <- fluidPage(
    
    # Application title
    titlePanel("ET SABR Miscanthus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("myyear",
                        "Year oh what fun:",
                        choices = dd_year)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("etPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ##--build reactive dataset, changes year highlight
    liq_dat <- reactive({
        dat %>% 
            #mutate(color_id = ifelse(month_id == input$mymonth, "selected month", "no"))
      mutate(color_id = ifelse(month_id == input$myyear, "selected month", "no"))
    })
    
    
    output$etPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data = liq_dat(),
               aes(x =DOY,
                   y = ET_daily)) + 
      facet_wrap( ~ month,ncol=3)+
            geom_jitter(aes(color = color_id), size = 3) +
            scale_color_manual(values = c("selected month" = "red",
                                          "no" = "gray80")) +
            
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
