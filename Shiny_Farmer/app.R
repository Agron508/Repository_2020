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
#   read_csv("data/tidy/data_ames-pet.csv") %>%
#   mutate(month_id = month(date),
#          doy = yday(date))
# 
# wx <- read_csv("data/tidy/data_ames-wea.csv") %>%
#   mutate(doy = yday(date))


et <-
  read_csv("data_ames-pet.csv") %>%
  mutate(month_id = month(date),
         doy = yday(date))

wx <- read_csv("data_ames-wea.csv") %>%
  mutate(doy = yday(date))



dat <-
  et %>% 
  left_join(wx) %>%  
  mutate(month_lab = month(month_id, label = T))


# for trouble shooting ----------------------------------------------------
# 
#      
#dat.fake <-
  # dat %>%
  # filter(month_lab == "Jul",
  #        year == 2015) %>%
  # mutate(cc = as.numeric(0.9)) %>%
  # mutate(
  #   et_mm = pet_mm * cc,
  #   net_mm = -et_mm + precip_mm,
  #   cum_mm = cumsum(net_mm),
  #   smois_pct_init = as.numeric(50),
  #   #--part to copy-paste
  #   smois_mm_init = smois_pct_init/100 * 200, #--do a 200 mm depth (aka 20 cm)
  #   smois_mm = smois_mm_init  + cum_mm,
  #   smois_pct = smois_mm / 200,
  #   smois_pct = ifelse(smois_pct < 0, 0,
  #                      ifelse(smois_pct > 100, 100, smois_pct))) %>%
  # select(doy, pet_mm, precip_mm, cc, et_mm, net_mm, cum_mm:smois_pct)
  # #mutate(smois_color = ifelse(smois_pct < as.numeric(input$tjpwp), "bad", "good"))

# 
# dat %>%
#   filter(month_lab == input$tjmonth,
#          year == input$tjyear) %>% 
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
dd_year <- dat %>% select(year) %>% pull() %>% unique()
dd_month <- dat %>% select(month_lab) %>% pull() %>% unique()
dd_smois <- seq(0, 100, 5)


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # Application title
  navbarPage("Agron508 Class Projects"),
  
  tabsetPanel(tabPanel(
    "Soil Water In Ames, Iowa",
    
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
           selected = 35,
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
             year == input$tjyear) %>% 
      mutate(cc = as.numeric(input$tjcc),
             smois_pct_init = as.numeric(input$tjsmois)) %>% 
      mutate(
        et_mm = pet_mm * cc,
        net_mm = -et_mm + precip_mm,
        cum_mm = cumsum(net_mm),
    
        smois_mm_init = smois_pct_init/100 * 200, #--do a 200 mm depth (aka 20 cm)
        smois_mm = smois_mm_init  + cum_mm,
        smois_pct = smois_mm / 200 * 100,
        smois_pct = ifelse(smois_pct < 0, 0, 
                           ifelse(smois_pct > 100, 100, smois_pct)),
        smois_color = ifelse(smois_pct < as.numeric(input$tjpwp), "bad", "good"))
    
  })
  
  
  output$tjPlot1 <- renderPlot({
  
    liq_tj1() %>% 
      select(year, month_id, doy, date, month_lab, pet_mm, et_mm) %>% 
      pivot_longer(pet_mm:et_mm) %>% 
      mutate(evapotranspiration = recode(name,
                           "pet_mm" = "Potential",
                           "et_mm" = "Actual"),
               evapotranspiration = factor(evapotranspiration,
                                           levels = c("Potential", "Actual"))) %>% 
      ggplot(aes(x = date,
               y = value, 
               color = evapotranspiration)) + 
      geom_point(size = 5) +
      geom_line() +
      scale_color_manual(values = c("Potential" = "gray80",
                                    "Actual" = "purple")) +
      theme_bw() + 
      scale_x_date(date_labels = "%b %d") +
      theme(axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.2)),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.text = element_text(size = rel(2))) +
      labs(x = NULL,
           color = NULL,
           y = "Daily Evapotranspiration (mm)")
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
             axis.text = element_text(size = rel(1.2)),
             plot.title = element_text(size = rel(2))) +
       labs(x = NULL,
            y = "Soil Water Content (vol%)",
            title = "0-20 cm Soil Profile")
     
     
  })
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
