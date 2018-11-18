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

education_data <- read_rds("education_data")
# Get options for education level
educ_levels <- names(education_factor)[2:5]
race_edu_data <- read_rds("race_edu")
# Get options for race/edu level
race_edu_levels <- names(race_edu_data)[2:5]
race_eth_data <- read_rds("race_eth")
# get options for race/eth levels
race_eth_levels <- names(race_eth_data)[2:6]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Midterm Election results: Predictions and Actual"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("educ",
                  "Which education level would you like to compare?",
                  educ_levels),
      selectInput("race",
                  "What race and education level would you like to consider?",
                  race_edu_levels),
      selectInput("race_eth",
                  "Which race would you like to compare?",
                  race_eth_levels)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("educPlot"),
      plotOutput("raceEduPlot"),
      plotOutput("racePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$educPlot <- renderPlot({
    # Let's add a column to our data with predicted win
    election_data <- election_data %>% 
      left_join(education_data)
    election_data %>% 
      ggplot(aes_string(x = input$educ, y = "poll_diff")) +
      geom_point(shape = 21, size = 2) +
      ggtitle("Democrats did slightly better than Upshot polls anticipated",
              subtitle = "Predictions by NYTimes upshot, midterm elections results as of 11/10") +
      xlab("Percent") +
      ylab("Difference between Republican advantage prediction and result")
  })
  
  output$raceEduPlot <- renderPlot({
    # Let's add a column to our data with predicted win
    election_data <- election_data %>% 
      left_join(race_edu_data)
    election_data %>% 
      ggplot(aes_string(x = input$race, y = "poll_diff")) +
      geom_point(shape = 21, size = 2) +
      ggtitle("Democrats did slightly better than Upshot polls anticipated",
              subtitle = "Predictions by NYTimes upshot, midterm elections results as of 11/10") +
      xlab("Percent") +
      ylab("Difference between Republican advantage prediction and result")
  })
  
  output$racePlot <- renderPlot({
    # Let's add a column to our data with predicted win
    election_data <- election_data %>% 
      left_join(race_eth_data)
    election_data %>% 
      ggplot(aes_string(x = input$race_eth, y = "poll_diff")) +
      geom_point(shape = 21, size = 2) +
      ggtitle("Democrats did slightly better than Upshot polls anticipated",
              subtitle = "Predictions by NYTimes upshot, midterm elections results as of 11/10") +
      xlab("Percent") +
      ylab("Difference between Republican advantage prediction and result")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
