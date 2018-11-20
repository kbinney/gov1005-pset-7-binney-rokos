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
library(ggplot2)
library(readr)

election_data <- read_rds("data")
error_data <- read_rds("error_data")
education_data <- read_rds("education_data")
race_edu_data <- read_rds("race_edu")
race_eth_data <- read_rds("race_eth")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Midterm Election results: Predictions and Actual"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset",
                  label = "Which demographic would you like to consider?",
                  choices = c("education", "race", "race/education"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      tableOutput("tibble")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Pick the correct dataset with compiled demographics for a district
  datasetInput <- reactive({
    switch(input$dataset,
           "race" = race_eth_data,
           "education" = education_data,
           "race/education" = race_edu_data)
  })
  
  
  output$Plot <- renderPlot({
  
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    dataset <- datasetInput()
    election_data <- error_data %>% 
      left_join(dataset, by = c("district"))
    election_data %>% 
      ggplot(aes(x = percent, y = poll_diff, color = factor(demographic))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
