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
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Midterm Election results: Predictions and Actual"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("race_type",
                  "Which races would you like to compare?",
                  c("House" = "house",
                    "Senate" = "senate",
                    "Governor" = "governor"),
                  multiple = TRUE,
                  selected = c("house", "senate", "governor"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # Let's add a column to our data with predicted win
    election_data <- election_data %>%
      mutate("Predicted Win" = case_when(rep_advantage_poll < 0 ~ "Dem",
                                         TRUE ~ "Rep"),
             "Actual Win" = case_when(rep_advantage_results < 0 ~ "Dem",
                                      TRUE ~ "Rep"))
    
    election_data %>% 
      filter(race_type %in% input$race_type) %>% 
      ggplot(aes(x = rep_advantage_poll, y = rep_advantage_results, 
                 color = `Predicted Win`,
                 fill = `Actual Win`)) +
      geom_point(shape = 21, size = 2) +
      ggtitle("Democrats did slightly better than Upshot polls anticipated",
              subtitle = "Predictions by NYTimes upshot, midterm elections results as of 11/10") +
      xlab("Predicted Republican Advantage") +
      ylab("Actual Republican Advantage") +
      # Swap default colors so dem is blue and rep is read
      scale_fill_manual(values = c( "#00BFC4", "#F8766D")) +
      scale_color_manual(values = c("#00BFC4", "#F8766D")) +
      # add a line to make clear where predictions and results matched
      geom_abline(aes(slope = 1, intercept = 0)) +
      geom_text(aes(10,10,label = "Prediction matches results", hjust = 1), 
                inherit.aes = FALSE)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
