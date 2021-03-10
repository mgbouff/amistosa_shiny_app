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

#---------------------------------------
# Inputs
#---------------------------------------

osa_theme <- bs_theme(
  bg = "#0DC489",
  fg = "white",
  primary = "black",
  base_font = font_google("Crimson Text"))

# Map color palettes
time_combo_palette <- c("#c0c0c0","#707170","#f2ea02", "#d9d561", "#569ecd", "#5e8299", "#cadc70", "#8fa428")

crop_combo_palette <- c("#c0c0c0", "#9874a1", "#85c2c0", "#6380a6", "#fef287", "#cbb6b2", "#72cf8e", "#21908d")

# Study area shapefile
study_area <- read_sf(here("Maps", "study_area", "studyArea.shp"))

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

crop_stack <- raster::stack(crop_current, crop_50_45, crop_70_45)

# Suitability Change data
#---------------------------------------
total_hectares45 <- read.csv(here("Data", "total_hectares45.csv")) %>%
  clean_names() %>%
  group_by(crop)

#---------------------------------------
# UI
#---------------------------------------

ui <- fluidPage(
  # theme = osa_theme,
  
  # Application title
  titlePanel("Wildlife-Friendly Farming and Crop Resilience 
in Southern Costa Rica"),
  
  navbarPage("Crop Suitability",
             tabPanel("About", 
                      
                      mainPanel(
                        
                        h4("Understanding Climate-driven Shifts in Crop Suitability"),
                        
                        p("Rising temperatures and changing precipitation patterns associated with climate change stand to strongly affect farming and food systems worldwide. As climate change intensifies, suitable ranges for the quality and survival of crops important to the Talamanca-Osa region may shift and necessitate crop switches, farm expansion, or farm relocation."),
                        
                        img(src = "agroforestry.jpg", height = 280, width = 324,
                            style="display: block; margin-left: auto; margin-right: auto;"),
                        
                        br(),
                        
                        p("This app presents analysis on future crop suitability for three crops in the Talamanca-Osa region: coffee, cacao, and pineapple. This analysis was conducted as part of the Wildlife-Friendly Farming and Crop Resilience in Southern Costa Rica master's group project and supports farmers in their long-term planning for climate resilience and implementing sustainable, wildlife friendly farming practices.")
                      ) # Close About mainPanel
             ), # Close About tabPanel

# Time combo maps  
#--------------------------------------- 

               # tabPanel("Suitability Maps",
               #          sidebarLayout(
               #            sidebarPanel("Crop Suitability",
               #                         radioButtons(inputId = "pick_crop_suit", 
               #                                      label = h3("Select Time Period"),
               #                                      choices = list("Pineapple" = "pineapple_45", 
               #                                                     "Cacao" = "cacao_45", 
               #                                                     "Coffee" = "coffee_45"), 
               #                                      selected = "pineapple_45"),
               #                         
               #                         hr(),
               #                         fluidRow(column(3, verbatimTextOutput("value")))
               #                         ), # Close time combo sidebarPanel
               #          mainPanel("Time period suitability rasters",
               #                    tmapOutput("time_tmap")) # Close time combo mainPanel
               #          ) # Close time combo sidebarLayout
               #          ), # Close time combo tabPanel

# Crop combo maps  
#--------------------------------------- 
tabPanel("Crop Overlap Maps",
         sidebarLayout(
           sidebarPanel("Crop Suitability Overlap",
                        radioButtons(inputId = "pick_time_period",
                                     label = h3("Select Time Period"),
                                     choices = c("Current" = "tri_all_cur",
                                                 "2050" = "tri_45_50",
                                                 "2070" = "tri_70_45"),
                                     selected = "tri_all_cur")
           ),
           mainPanel("OUTPUT!",
                     tmapOutput("combo_tmap")
                     )
           )
         ),

# Suitable Area Change
#---------------------------------------
tabPanel("Suitability Area Change",
         sidebarLayout(
           sidebarPanel("Suitability Change",
                        checkboxGroupInput(inputId = "pick_crop_change",
                                           label = "Select Crop",
                                           choices = unique(total_hectares45$crop),
                                           selected = "Pineapple"),
                        hr(),
                        fluidRow(column(3, verbatimTextOutput("value")))
           ),
           mainPanel("Suitability Change Plot",
                     plotOutput("suitability_hectare_plot")
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
  # time_combo_reactive <- reactive({
  #   time_stack_df %>%
  #     dplyr::select(x, y, input$pick_crop_suit)
  #   time_raster <- raster::rasterFromXYZ(time_stack_df, crs = crs(crop_current))
  # })
  # 
  # # output$time_tmap = renderLeaflet({
  # #   leaflet() %>% 
  # #     addRasterImage
  # # })
  # 
  #   output$time_tmap = renderTmap({
  #   tm_shape(time_combo_reactive) +
  #     tm_raster(style = "cat", palette = time_combo_palette) +
  #     tm_shape(study_area) +
  #     tm_borders("black") +
  #     tm_basemap("Esri.WorldTopoMap")
  # })
  
# Crop combo maps  
#---------------------------------------  
  crop_combo_reactive <- reactive({
    crop_subset <- subset(crop_stack, input$pick_time_period)
  })
  
  output$combo_tmap <- renderTmap({
    tm_shape(crop_combo_reactive())+
      tm_raster(input$pick_time_period, style = "cat", palette = crop_combo_palette) +
    tm_shape(study_area) +
      tm_borders("black") +
      tm_basemap("Esri.WorldTopoMap") +
      tm_compass(position = c("left", "bottom")) +
      tm_scale_bar(position = c("left", "bottom"))
  })
  
# Suitable Area Change
#---------------------------------------
    hectare_reactive <- reactive({
      total_hectares45 %>%
        filter(crop %in% input$pick_crop_change)
    })
    
    output$suitability_hectare_plot <- renderPlot(
      ggplot(data = hectare_reactive(), aes(x = time_period,
                                            y = suitable_hectares)) +
        geom_point(aes(color = crop)) +
        geom_line(aes(color = crop)) +
        labs(title = "Total Suitable Hectares",
             x = "Time Period",
             y = "Number of Hectares") +
        scale_x_continuous(breaks = c(2020, 2050, 2070),
                           labels = c("Current","2050","2070")) +
        scale_color_manual(values = c("#85c2c0", "#9874a1", "#fef287")) +
        theme_minimal()
    )
  
} # Close Server

#---------------------------------------
# Run the application 
#---------------------------------------
shinyApp(ui = ui, server = server)
