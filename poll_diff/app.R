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
library(plotly)

election_data <- read_rds("data")
error_data <- read_rds("error_data")
race_eth_data <- read_rds("race_eth")
education_data <- read_rds("education_data")
age_data <- read_rds("age_data")
gender_data <- read_rds("gender_data")

# Define UI for application that draws a histogram
ui <- navbarPage("Midterm Election results: Predictions and Actual", 
  tabPanel("Race/Ethnicity",
           fluidPage(
             # Page title
             titlePanel("Race/Ethnicity and 2018 Midterm Polling Errors"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("race_eth",
                                    label = "Select Race/Ethnicity:",
                                    choices = c("White", "Black", "Hispanic", "Asian"),
                                    selected = "White")
                 ),
               # Show a plot of the generated distribution
               mainPanel(plotOutput("plot_race_eth"),
                         tableOutput("race_eth_tibble"))
               )
             )
           ),
  
  tabPanel("Education",
           fluidPage(
             # Page title
             titlePanel("Education and 2018 Midterm Polling Errors"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("educ",
                                    label = "Select Educational Attainment:",
                                    choices = c("High School or Less", "Some College", "College Graduate", "Post-Graduate"),
                                    selected = "High School or Less")
               ),
               # Show a plot of the generated distribution
               mainPanel(plotOutput("plot_educ"),
                         tableOutput("educ_tibble"))
               )
             )
           ),
  tabPanel("Age",
           fluidPage(
             # Page title
             titlePanel("Age and 2018 Midterm Polling Errors"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("age",
                                    label = "Select Educational Attainment:",
                                    choices = c("18 to 34", "35 to 49", "50 to 64", "65 and older"),
                                    selected = "18 to 34")
               ),
               # Show a plot of the generated distribution
               mainPanel(plotOutput("plot_age"),
                         tableOutput("age_tibble"))
               )
             )
           ),
  tabPanel("Gender",
           fluidPage(
             # Page title
             titlePanel("Gender and 2018 Midterm Polling Errors"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("age",
                                    label = "Select Gender:",
                                    choices = c("Female", "Male"),
                                    selected = "Female")
               ),
               # Show a plot of the generated distribution
               mainPanel(plotOutput("plot_gender"),
                         tableOutput("gender_tibble"))
               )
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
           "age" = age_data,
           "gender" = gender_data)
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
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
      facet_wrap(~demographic)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
