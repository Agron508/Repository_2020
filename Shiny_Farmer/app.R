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
# 
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
# 
#      
# dat.fake <- 
#   dat %>%
#   filter(month_lab == "Jun",
#          year_id == 2019) %>% 
#   mutate(cc = as.numeric(0.5)) %>% 
#   mutate(PET = ET_daily,
#          ET = PET * cc,
#          net_mm = -ET + precip_mm,
#          cum_mm = cumsum(net_mm),
#          smois_pct = as.numeric(50) + cum_mm,
#          smois_pct = ifelse(smois_pct < 0, 0, 
#                             ifelse(smois_pct > 100, 100, smois_pct)))
# 
# dat %>%
#   filter(month_lab == input$tjmonth,
#          year_id == input$tjyear) %>% 
#   mutate(cc = as.numeric(input$tjcc)) %>% 
#   mutate(
#     PET = ET_daily,
#     ET = PET * cc,
#     net_mm = -ET + precip_mm,
#     cum_mm = cumsum(net_mm),
#     smois_pct = as.numeric(input$tjsmois) + cum_mm,
#     smois_pct = ifelse(smois_pct < 0, 0, 
#                        ifelse(smois_pct > 100, 100, smois_pct)),
#     smois_color = ifelse(smois_pct < as.numeric(input$tjpwp), "bad", "good"))
# 
#     
# dat.fake %>% 
#   ggplot(aes(date, smois_pct)) + 
#   geom_point() + 
#   scale_x_date(date_labels = "%b %d")

#--create the drop down menu values
dd_year <- dat %>% select(year_id) %>% pull() %>% unique()
dd_month <- dat %>% select(month_lab) %>% pull() %>% unique()
dd_smois <- seq(0, 100, 5)


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # Application title
  navbarPage("Agron508 Class Projects"),
  
  tabsetPanel(tabPanel(
    "Evapotranspiration Effects on Soil Water",
    
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
           "tjsmois",
           label = h3("Starting Soil Water (vol%):"),
           selected = 50,
           choices = dd_smois)
         ),
        column(
          3,
          sliderInput(
            "tjpwp",
            label = h3("Plant Wilting Point (vol%)"),
            min = 0,
            max = 50,
            value = 15,
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
          2,
          #offset = 1,
          h3("Soil Water Visual"),
          tags$img(src = "pwp-image.gif", height = 200, width = 200, align = "center")
        ),
        column(
          3,
          #offset = 1,
          h3("Plant Wilting Point Help"),
          tags$img(src = "soil water.png", height = 200, width = 300, align = "center")
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
      mutate(
        PET = ET_daily,
        ET = PET * cc,
        net_mm = -ET + precip_mm,
        cum_mm = cumsum(net_mm),
        smois_pct = as.numeric(input$tjsmois) + cum_mm,
        smois_pct = ifelse(smois_pct < 0, 0, 
                           ifelse(smois_pct > 100, 100, smois_pct)),
        smois_color = ifelse(smois_pct < as.numeric(input$tjpwp), "bad", "good"))
    
  })
  
  
  output$tjPlot1 <- renderPlot({
  
    liq_tj1() %>% 
      select(year_id, month_id, doy, date, month_lab, PET, ET) %>% 
      pivot_longer(PET:ET) %>% 
      mutate(name = factor(name, levels = c("PET", "ET"))) %>% 
      ggplot(aes(x = date,
               y = value, 
               color = name)) + 
      geom_point(size = 5) +
      geom_line() +
      scale_color_manual(values = c("PET" = "gray80",
                                    "ET" = "purple")) +
      theme_bw() + 
      scale_x_date(date_labels = "%b %d") +
      theme(axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.2)),
            legend.position = "top",
            legend.direction = "horizontal") +
      labs(x = NULL,
           color = NULL,
           y = "Daily (Potential) Evapotranspiration (mm)")
  })

  
   output$tjPlot2 <- renderPlot({
    
     
     ggplot(data = liq_tj1(),
            aes(date, smois_pct)) + 
       geom_hline(yintercept = input$tjpwp,
                  linetype = "dashed",
                  color = "black") + 
       geom_line(color = "gray", size = 1, linetype = "dashed") +
       geom_point(aes(color = smois_color), size = 5) +
       scale_x_date(date_labels = "%b %d") + 
       theme_bw() + 
       guides(color = F) + 
       scale_color_manual(values = c("bad" = "red", 
                                     "good" = "blue")) +
       theme(axis.title = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(1.2))) +
       labs(x = NULL,
            y = "Plant Available Water (%)")
     
     
  })
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
