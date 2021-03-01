# Attach packages
library(shiny)
library(tidyverse)
library(bslib)

# Define UI for application that draws a histogram
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
                                       radioButtons("radio",
                                                    label = h3("Radio buttons"),
                                                    choices = list(
                                                      "Coffee" = 1,
                                                      "Cacao" = 2,
                                                      "Pineapple" = 3),
                                                    selected = 1),
                                       
                                       hr(),
                                       fluidRow(column(3,
                                                       verbatimTextOutput("value")))
                          ),
                          mainPanel("Output!",
                                    plotOutput("sw_plot"))
                        ))
               )
    )

# Server
server <- function(input, output) {

  output$value <- renderPrint({ input$radio })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
