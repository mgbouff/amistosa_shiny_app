# Attach packages
library(shiny)
library(tidyverse)

# Create User Interface:
ui <- fluidPage(
  titlePanel("Wildlife-Friendly Farming and Crop Resilience 
in Southern Costa Rica
"),
  sidebarLayout(
    sidebarPanel("Widgets"),
    mainPanel("Graph")
  )
  
)

# Create server function:
server <- function(input, output) {}

# Combine into app
shinyApp(ui = ui, server = server)