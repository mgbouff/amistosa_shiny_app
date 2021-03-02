# Attach packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(bslib)

#---------------------------------------
# Data
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
               tabPanel("Binary Suitability Maps"),
               tabPanel("Suitability Change Maps"),
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
  
# Suitable Area Change
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
