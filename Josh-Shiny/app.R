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


#--read in the data

dat <- read_csv("data/tidy/data_ET-for-shiny.csv")

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique() 

ui <- fluidPage(
    
    # Application title
    titlePanel("Tyler's ET Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("myyear",
                        "Year:",
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
}

# Run the application 
shinyApp(ui = ui, server = server)
