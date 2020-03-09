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
<<<<<<< HEAD:Rojda_OurFirstShiny/app.R
library(scales)
library(lubridate)

=======
>>>>>>> 42e37f6fd6ced79236c0d772841776642d1071e8:OurFirstShiny/app.R


#--read in the data

<<<<<<< HEAD:Rojda_OurFirstShiny/app.R
#dat<- read.csv("C:/Users/myAdmins/Desktop/Rshiny_flux/data_ET-for-shiny.csv")
dat <- read.csv("~/GitHub/Repository_2020/data/tidy/ET_miscanthus_SABR.csv") %>% 
  mutate(month = month(Month, label = T))
ET_monthly=aggregate(dat$ET_daily, by=list(dat$month),mean, na.rm=TRUE)
  

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique() 
dd_month <- dat %>% select(month) %>% pull() %>% unique()
dd_DOY<- dat %>% select(DOY) %>% pull() %>% unique()
=======
dat <- read.csv("../data/tidy/data_ET-for-shiny.csv")

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique()
>>>>>>> 42e37f6fd6ced79236c0d772841776642d1071e8:OurFirstShiny/app.R

ui <- fluidPage(
    
    # Application title
    titlePanel("ET SABR Miscanthus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("myyear",
<<<<<<< HEAD:Rojda_OurFirstShiny/app.R
                        "Year oh what fun:",
=======
                        "Year:",
>>>>>>> 42e37f6fd6ced79236c0d772841776642d1071e8:OurFirstShiny/app.R
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
<<<<<<< HEAD:Rojda_OurFirstShiny/app.R

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
=======
  
  ##--build reactive dataset, changes year highlight
  liq_dat <- reactive({
    dat %>% 
      mutate(color_id = ifelse(year_id == input$myyear, "selected year", "no"))
  })
  
  output$etPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = liq_dat(),
           aes(x = Tmax,
               y = ET_monthly)) + 
      geom_point(aes(color = color_id), size = 5) +
      scale_color_manual(values = c("selected year" = "red",
                                    "no" = "gray80")) +
      #labs(x = "Maximum Monthly Temp (degC)",
      #     y = "Montly Total Evapotranspiration (ET; mm)",
      #     color = NULL)
      theme_bw()
  })
>>>>>>> 42e37f6fd6ced79236c0d772841776642d1071e8:OurFirstShiny/app.R
}

# Run the application 
shinyApp(ui = ui, server = server)
