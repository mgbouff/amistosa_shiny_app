# Attach packages
library(raster)
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(sf)
library(tmap)
library(fasterize)
library(bslib)
library(shinythemes)
library(rsconnect)
library(s2plot)
library(rgdal)

# tmap fix
tmap_options(check.and.fix = TRUE)

# Turn off spherical geometry
# sf::sf_use_s2(TRUE)

#---------------------------------------
# Inputs
#---------------------------------------

# Map color palettes
time_combo_palette <- c("#c0c0c0","#707170","#f2ea02", "#d9d561", "#569ecd", "#5e8299", "#cadc70", "#8fa428")

crop_combo_palette <- c("#c0c0c0", "#9874a1", "#85c2c0", "#fef287", "#cbb6b2", "#72cf8e", "#21908d")

# Study area shapefile
study_area <- read_sf(here("Maps", "study_area", "studyArea.shp")) %>% 
  st_make_valid()

# st_is_valid(study_area)

# Time combo data
#---------------------------------------
time_cacao_45 <- raster(here("Maps", "Time_combos", "cacao_45.tif"))

time_cacao_85 <- raster(here("Maps", "Time_combos", "cacao_85.tif"))

time_coffee_45 <- raster(here("Maps", "Time_combos", "coffee_45.tif"))

time_coffee_85 <- raster(here("Maps", "Time_combos", "coffee_85.tif"))

time_pineapple_45 <- raster(here("Maps", "Time_combos", "pineapple_45.tif"))

time_pineapple_85 <- raster(here("Maps", "Time_combos", "pineapple_85.tif"))

time_stack <- raster::stack(time_cacao_45,time_coffee_45, time_pineapple_45)


# Crop combo data
#---------------------------------------
crop_current <- raster(here("Maps", "Crop_combos", "tri_all_cur.tif"))

crop_50_45 <- raster(here("Maps", "Crop_combos", "tri_45_50.tif"))

crop_50_85 <- raster(here("Maps", "Crop_combos", "tri_50_85.tif"))

crop_70_45 <- raster(here("Maps", "Crop_combos", "tri_70_45.tif"))

crop_70_85 <- raster(here("Maps", "Crop_combos", "tri_70_45.tif"))

crop_stack_45 <- raster::stack(crop_current, crop_50_45, crop_70_45)


# Suitability Change data
#---------------------------------------
total_hectares45 <- read.csv(here("Data", "total_hectares45.csv")) %>%
  clean_names() %>%
  mutate("Crop" = crop) %>% 
  group_by(Crop)
  

total_hectares85 <- read.csv(here("Data", "total_hectares_85.csv")) %>%
  clean_names() %>%
  mutate("Crop" = crop) %>% 
  group_by(Crop)

#---------------------------------------
# UI
#---------------------------------------

ui <- fluidPage(theme = "osa.css",
  
  # Application title
  titlePanel("Wildlife-Friendly Farming and Crop Resilience 
in Southern Costa Rica"),
  
  navbarPage("Crop Suitability",
             tabPanel("About", 
                      
                      mainPanel(
                        
                        h3("Understanding Climate-driven Shifts in Crop Suitability"),
                        
                        p("Rising temperatures and changing precipitation patterns associated with climate change stand to strongly affect farming and food systems worldwide. As climate change intensifies, suitable ranges for the quality and survival of crops important to the Talamanca-Osa region may shift and necessitate crop switches, farm expansion, or farm relocation."),
                        
                        img(src = "agroforestry.jpg", height = 280, width = 324,
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        h6("Photo credit: National Geographic",
                           align = "center"),
                        
                        br(),
                        
                        p("This app presents analysis on future crop suitability for three crops in the Talamanca-Osa region: coffee, cacao, and pineapple. This analysis was conducted as part of the Wildlife-Friendly Farming and Crop Resilience in Southern Costa Rica master's group project and supports farmers in their long-term planning for climate resilience and implementing sustainable, wildlife friendly farming practices.")
                      ) # Close About mainPanel
             ), # Close About tabPanel

# Time combo maps  
#--------------------------------------- 
tabPanel("Suitability Maps",
         sidebarLayout(
           sidebarPanel(
                        radioButtons(inputId = "pick_crop_suit",
                                     label = ("Select Crop"),
                                     choices = c("Pineapple" = "pineapple_45",
                                                 "Cacao" = "cacao_45",
                                                 "Coffee" = "coffee_45"),
                                     selected = "pineapple_45"),
                        
                        img(src = "time_legend.png", height = "100%", width = "100%",
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        img(src = "Unsuitable.PNG", height = "40%", width = "40%",
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        
                        p("Suitability maps were created to show how suitable area for growing each crop will shift from the current to 2050 and 2070 time periods. This combination map illustrates overlaps and differences between the three time periods for each crop. Knowing when and where these staple crops are likely to shift due to climate change in the coming decades allows farmers to plan to long term shifts or crop switches and helps inform the implementation of sustainable farming techniques.")
                        
           ),
           mainPanel(
             h3("Projected Crop Suitability Maps"),
                     tmapOutput("time_tmap")
           )
         )
),

# Crop combo maps  
#--------------------------------------- 
tabPanel("Crop Overlap Maps",
         sidebarLayout(
           sidebarPanel(
                        radioButtons(inputId = "pick_time_period",
                                     label = ("Select Time Period"),
                                     choices = c("Current" = "tri_all_cur",
                                                 "2050" = "tri_45_50",
                                                 "2070" = "tri_70_45"),
                                     selected = "tri_all_cur"),
                        
                        img(src = "crop_legend.PNG", height = "100%", width = "100%",
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        img(src = "Unsuitable.PNG", height = "40%", width = "40%",
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        
                        p("Crop overlap maps were created to show how suitable area for growing each crop intersect with each other during the current time period and projected future 2050 and 2070 time periods. This is important information for farmers and conservationists to use in their long term planning because of differences in the production of the three crops. Pineapple tends to be grown on large-scale monoculture plantations and is associated with environmental and social harm. Coffee and cacao on the other hand can be grown by small-holder farms can be grown with agroforestry techniques such as including an understory of native trees, that support wildlife connectivity.")
                        
           ),
           mainPanel(
             h3("Projected Crop Suitability Overlap"),
                     tmapOutput("combo_tmap")
                     )
           )
         ),

# Suitable Area Change
#---------------------------------------
tabPanel("Suitability Area Change",
         sidebarLayout(
           sidebarPanel(# RCP 4.5
                        checkboxGroupInput(inputId = "pick_change_45",
                                           label = "Select Crop RCP 4.5",
                                           choices = unique(total_hectares45$crop),
                                           selected = "Pineapple"),
                        # RCP 8.5
                        checkboxGroupInput(inputId = "pick_change_85",
                                           label = "Select Crop RCP 8.5",
                                           choices = unique(total_hectares85$crop),
                                           selected = "Pineapple"),
                        hr(),
                        fluidRow(column(3, verbatimTextOutput("value"))),
                      
                        p("The severity of future climate change depends on how quickly the world is able to reduce our carbon footprint now. Climate scientists have developed models for different future climate scenarios, called Relative Concentration Pathways (RCPs) that are based on how quickly we reduce our greenhouse gas emissions. They range from best case scenario where countries pull together and reduce our global emissions, and worst case scenarios where carbon emissions continue to increase. This app looks at how the change in total suitable area for each crop changes for future time periods depending on which RCP is used. Here, we selected RCP 4.5 which is considered a fairly 'middle of the road' scenario in which future warmins is limited to 3°C, and RCP 8.5 where emissions continue to rise and we see severe warming impacts.")
           ),
           
           mainPanel(
             h3("Suitability Change Plot"),
                    plotOutput("suitability_45_plot"),
                    plotOutput("suitability_85_plot")
                     )
           )
         )

  ) # Close navbarPage
) # CLose Fluidpage

#---------------------------------------
# Server
#---------------------------------------
server <- function(input, output) {

# Time combo maps  
#---------------------------------------
  time_combo_reactive <- reactive({
    time_subset <- subset(time_stack, input$pick_crop_suit)
  })
  
  output$time_tmap <- renderTmap({
    tm_shape(time_combo_reactive())+
      tm_raster(input$pick_crop_suit, style = "cat", palette = time_combo_palette, legend.show = FALSE) +
      tm_shape(study_area) +
      tm_borders("black") +
      tm_basemap("Esri.WorldTopoMap") +
      tm_scale_bar(position = c("left", "bottom")) + 
      tm_layout(legend.show=FALSE)
  })
  
# Crop combo maps  
#---------------------------------------  
  crop_combo_reactive <- reactive({
    crop_subset <- subset(crop_stack_45, input$pick_time_period)
  })
  
  output$combo_tmap <- renderTmap({
    tm_shape(crop_combo_reactive())+
      tm_raster(input$pick_time_period, style = "cat", palette = c("0" = "#c0c0c0", "10" = "#9874a1", "50" = "#85c2c0", "60" = "#6380a6", "200" = "#fef287", "210" = "#cbb6b2", "250" = "#72cf8e", "260" = "#21908d"), legend.show = FALSE) +
    tm_shape(study_area) +
      tm_borders("black") +
      tm_basemap("Esri.WorldTopoMap") +
      tm_scale_bar(position = c("left", "bottom"))
  })
  
# Suitable Area Change
#---------------------------------------
  
  # RCP 4.5
    hectare_reactive_45 <- reactive({
      total_hectares45 %>%
        filter(crop %in% input$pick_change_45)
    })
    
    output$suitability_45_plot <- renderPlot(
      ggplot(data = hectare_reactive_45(), aes(x = time_period,
                                            y = suitable_hectares)) +
        geom_point(aes(color = Crop)) +
        geom_line(aes(color = Crop), size = 2) +
        labs(title = "Total Suitable Hectares RCP 4.5",
             x = "Time Period",
             y = "Number of Suitable Hectares") +
        scale_x_continuous(breaks = c(2020, 2050, 2070),
                           labels = c("Current","2050","2070")) +
        scale_color_manual(values = c("Cacao" = "#85c2c0", "Coffee" = "#9874a1", "Pineapple" = "#fef287")) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0), size = 14),
          legend.title = element_text(size = 14, face = "bold")
        ) +
        guides(fill = guide_legend(title = "Crop"))
    )
    
  # RCP 8.5
    hectare_reactive_85 <- reactive({
      total_hectares85 %>%
        filter(crop %in% input$pick_change_85)
    })
    
    output$suitability_85_plot <- renderPlot(
      ggplot(data = hectare_reactive_85(), aes(x = time_period,
                                            y = suitable_hectares)) +
        geom_point(aes(color = Crop)) +
        geom_line(aes(color = Crop), size = 2) +
        labs(title = "Total Suitable Hectares RCP 8.5",
             x = "Time Period",
             y = "Number of Suitable Hectares") +
        scale_x_continuous(breaks = c(2020, 2050, 2070),
                           labels = c("Current","2050","2070")) +
        scale_color_manual(values = c("Cacao" = "#85c2c0", "Coffee" = "#9874a1", "Pineapple" = "#fef287")) +
        theme_minimal() +
        guides(fill = guide_legend(title = "Crop")) +
        theme(
          text = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 14),
          legend.title = element_text(size = 14, face = "bold")
        ) 
    )
  
} # Close Server

#---------------------------------------
# Run the application 
#---------------------------------------
shinyApp(ui = ui, server = server)
