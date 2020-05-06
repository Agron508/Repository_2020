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
# # 
# et <-
#   read_csv("data/tidy/data_ames-pet.csv") %>%
#   mutate(month_id = month(date),
#          doy = yday(date))
# 
# wx <- read_csv("data/tidy/data_ames-wea.csv") %>%
#   mutate(doy = yday(date))
# 
# dat2 <- read_csv("data/tidy/condensed.csv") %>%
#   filter(mol_pho < 0.002) %>%
#   filter(hour >= 6, hour <= 20) %>%
#   mutate(
#     wl_bins = (wavelength %/% 10) * 10,
#     hour2 = trunc(hour / 2) * 2,
#     energy = mol_pho * 6.022E23 * (6.63E-19 * 3E8) / wavelength / 1E-9,
#     month_lab = month(month, label = T)
#   ) %>%
#   select(month_lab, wl_bins, hour2, mol_pho, energy) %>%
#   pivot_longer(mol_pho:energy) %>%
#   group_by(month_lab, hour2, wl_bins, name) %>%
#   summarise(val_sum = sum(value, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(hour2 = paste0(hour2, "-", hour2+2))


#--tab 1
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

#--tab 2
dat2 <- read_csv("data_richard_wavelengths.csv") %>%
  filter(mol_pho < 0.002) %>%
  filter(hour >= 6, hour <= 20) %>%
  mutate(
    wl_bins = (wavelength %/% 10) * 10,
    hour2 = trunc(hour / 2) * 2,
    energy = mol_pho * 6.022E23 * (6.63E-19 * 3E8) / wavelength / 1E-9,
    month_lab = month(month, label = T)
  ) %>%
  select(month_lab, wl_bins, hour2, mol_pho, energy) %>%
  pivot_longer(mol_pho:energy) %>%
  group_by(month_lab, hour2, wl_bins, name) %>%
  summarise(val_sum = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(hour2 = paste0(hour2, "-", hour2 + 2))

  



# for trouble shooting ----------------------------------------------------
# 
#      
# dat.fake <-
# dat %>%
#   filter(month_lab == "Jul",
#          year == 2015) %>% 
#   mutate(cc = 0.8,
#          smois_pct_init = as.numeric(50)) %>% 
#   mutate(
#     et_mm = pet_mm * cc,
#     net_mm = -et_mm + precip_mm,
#     cum_mm = cumsum(net_mm),
#     
#     smois_mm_init = smois_pct_init/100 * 200, #--do a 200 mm depth (aka 20 cm)
#     smois_mm = smois_mm_init  + cum_mm,
#     smois_pct = smois_mm / 200 * 100,
#     smois_pct = ifelse(smois_pct < 0, 0, 
#                        ifelse(smois_pct > 50, 50, smois_pct)),
#     smois_color = ifelse(smois_pct < as.numeric(15), "bad", "good"))

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
dd_month2 <- dat2 %>% select(month_lab) %>% pull() %>% unique()
dd_var <- c("mol_pho", "energy")

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  # Application title
  navbarPage("Agron508 Class Projects"),
  
  tabsetPanel(
    tabPanel(
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
              choices = dd_smois
            )
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
          column(4,
                 includeMarkdown("readme.md")),
          column(
            3,
            #offset = 4,
            h3("Crop Coefficient Help"),
            tags$img(
              src = "kc-image.png",
              height = 200,
              width = 300,
              align = "center"
            )
          ),
          column(
            2,
            #offset = 1,
            h3("Soil Water Visual"),
            tags$img(
              src = "pwp-image.gif",
              height = 200,
              width = 200,
              align = "center"
            )
          ),
          column(
            3,
            #offset = 1,
            h3("Plant Wilting Point Help"),
            tags$img(
              src = "soil water.png",
              height = 200,
              width = 300,
              align = "center"
            )
          )
          
        )
        
      )
    ),
    #--end tab
    tabPanel(
      "Downwelling shortwave radiation near Ellsworth, IA",
      
      fluidRow(#month select
        column(
          2,
          selectInput(
            inputId = "rmonth",
            label = h4("Pick A Month:"),
            selected = "Jul",
            choices = dd_month2
          )
        ),
        #variable select (couldn't get to work so does nothing)
        column(
          3,
          selectInput(
            inputId = "rvar",
            label = h4("Pick A Variable:"),
            selected = "mol_pho",
            choices = dd_var
          )
        )),
      fluidRow(#the plot
        column(12,
               plotOutput("rPlot1")))
      
    ) #--end tab
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
                           ifelse(smois_pct > 50, 50, smois_pct)))
    
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
               color = evapotranspiration,
               fill = evapotranspiration)) + 
      geom_line() +
      geom_point(pch = 21, stroke = 2, size = 5, alpha = 0.7) +
      scale_color_manual(values = c("Potential" = "gray90",
                                    "Actual" = "black")) +
      scale_fill_manual(values = c("Potential" = "gray80",
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
           fill = NULL,
           y = "Daily Evapotranspiration (mm)")
  })

  
   output$tjPlot2 <- renderPlot({
    
     
     ggplot(data = liq_tj1(),
            aes(date, smois_pct)) + 
       geom_hline(yintercept = input$tjpwp,
                  linetype = "dashed",
                  color = "black") + 
       geom_line(color = "gray", size = 1, linetype = "dashed") +
       geom_point(pch = 21, aes(fill = smois_pct), size = 5, stroke = 2) +
       scale_x_date(date_labels = "%b %d") + 
       theme_bw() + 
       guides(color = F) + 
       scale_fill_gradient2(low = "red", high = "dodgerblue", 
                            na.value = NA, 
                            midpoint = (50 + input$tjpwp)/2) +
       guides(fill = F) +
       theme(axis.title = element_text(size = rel(1.5)),
             axis.text = element_text(size = rel(1.2)),
             plot.title = element_text(size = rel(2))) +
       labs(x = NULL,
            y = "Soil Water Content (vol%)",
            title = "0-20 cm Soil Profile")
     
     
  })
   
   liq_r1 <- reactive({
     
     dat2 %>%
       filter(month_lab == input$rmonth) %>% 
       filter(name == input$rvar)
     
   })
   
   
   #makes the plot
   output$rPlot1 <- renderPlot({
     liq_r1() %>% 
       ggplot(aes(x = wl_bins,y = val_sum)) +
       geom_col() +
       facet_wrap(~hour2) + 
       labs(x = "Wavelength (nm)",
            y = bquote('Photon Flux '(number/sm^2)),
            title = "Photon Flux Or Energy Over Two Hour Period")
   })
   
  



    
  
}

# Run the application 
#shinyApp(ui = ui2, server = server)
shinyApp(ui = ui, server = server)
