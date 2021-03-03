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

#---------------------------------------
# Data
#---------------------------------------

# Map color palettes
time_combo_palette <- c("#c0c0c0","#707170","#f2ea02", "#d9d561", "#569ecd", "#5e8299", "#cadc70", "#8fa428")

crop_combo_palette <- c("#c0c0c0", "#9874a1", "#85c2c0", "#6380a6", "#fef287", "#cbb6b2", "#72cf8e", "#21908d")

# Study area shapefile
study_area <- read_sf(here("Maps", "study_area", "studyArea.shp"))

# Time combo data
#---------------------------------------
time_cacao_45 <- raster(here("Maps", "Time_combos", "cacao_45.tif"))

# time_cacao_45_df <- rasterToPoints(time_cacao_45) %>%
#   as.data.frame()
# 
# ggplot(data = time_cacao_45_df, aes(x = x, y = y, fill = cacao_45)) +
#   geom_raster()

time_cacao_85 <- raster(here("Maps", "Time_combos", "cacao_85.tif"))

time_coffee_45 <- raster(here("Maps", "Time_combos", "coffee_45.tif"))

time_coffee_85 <- raster(here("Maps", "Time_combos", "coffee_85.tif"))

time_pineapple_45 <- raster(here("Maps", "Time_combos", "pineapple_45.tif"))

time_pineapple_85 <- raster(here("Maps", "Time_combos", "pineapple_85.tif"))

# Crop combo data
#---------------------------------------
crop_50_45 <- raster(here("Maps", "Crop_combos", "tri_45_50.tif"))

crop_50_85 <- raster(here("Maps", "Crop_combos", "tri_50_85.tif"))

crop_70_45 <- raster(here("Maps", "Crop_combos", "tri_70_45.tif"))

crop_70_85 <- raster(here("Maps", "Crop_combos", "tri_70_45.tif"))

# Suitability Change data
#---------------------------------------
suitability_change <- read.csv(here("Data", "suitability_change_output.csv")) %>% 
  clean_names() %>% 
  filter(rcp == "4.5")

#---------------------------------------
# UI
#---------------------------------------
ui <- fluidPage(
  
    # Application title
    titlePanel("Wildlife-Friendly Farming and Crop Resilience 
in Southern Costa Rica"),

    navbarPage("Crop Suitability",
               tabPanel("Suitability Maps",
                        sidebarLayout(
                          sidebarPanel("Crop Suitability",
                        ),
                        mainPanel("Output!",
                                  tmapOutput("time_tmap"))
                        )),
               tabPanel("Crop Overlap Maps",
                        sidebarLayout(
                          sidebarPanel("Crop Suitability Overlap",
                          ),
                          mainPanel("Output!",
                                    tmapOutput("crop_tmap"))
                        ),
                        ),
               tabPanel("Suitbale Area Change",
                        sidebarLayout(
                          sidebarPanel("Suitability Change",
                                       checkboxGroupInput(inputId = "pick_change_crop",
                                                    label = h3("Select Crop"),
                                                    choices = unique(suitability_change$crop), selected = "Coffee"),
                                       
                                       hr(),
                                       fluidRow(column(3,
                                                       verbatimTextOutput("value")))
                          ),
                          mainPanel("Suitability Change Plot",
                                    plotOutput("suitability_change_plot")
                          )
                        ))
               )
    )

#---------------------------------------
# Server
#---------------------------------------
server <- function(input, output) {

# Time combo maps  
#---------------------------------------
  output$time_tmap = renderTmap({
    tm_shape(time_cacao_45) +
      tm_raster(style = "cat", palette = time_combo_palette) +
      tm_shape(study_area) +
      tm_borders("black") +
      tm_basemap("Esri.WorldTopoMap")
  })

# Crop combo maps  
#---------------------------------------  
  output$crop_tmap = renderTmap({  
  tm_shape(crop_50_45) +
    tm_raster(style = "cat", palette = crop_combo_palette) +
      tm_shape(study_area) +
      tm_borders("black") +
      tm_basemap("Esri.WorldTopoMap")
  })
  
# Suitable Area Change
#---------------------------------------
  change_reactive <- reactive({
    suitability_change %>%
      filter(crop %in% input$pick_change_crop)
  })
  
  output$suitability_change_plot <- renderPlot(
    ggplot(data = change_reactive(), aes(x = i_time_period, y = net)) +
      geom_point(aes(color = crop)) +
      labs(x = "Time Period",
           y = "Net Area Change (Hectares)") +
      theme_minimal()
  )
  
}

#---------------------------------------
# Run the application 
#---------------------------------------
shinyApp(ui = ui, server = server)
