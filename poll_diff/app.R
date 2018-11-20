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
                 radioButtons("race_eth",
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
                 radioButtons("educ",
                                    label = "Select Educational Attainment:",
                                    choices = c("High School or Less", "Some College", 
                                                "4-year College Graduate", "Postgraduate Degree"),
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
                 radioButtons("age",
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
                 radioButtons("age",
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

  output$plot_race_eth <- renderPlot({
  
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(race_eth_data, by = c("district"))
    
    selected_data <- election_data %>% 
      filter(race_eth %in% c(input$race_eth))
    
    ggplot(data = selected_data,
           aes(x = percent, y = poll_diff, text = district)) +
      geom_point() +
      geom_hline(yintercept = 0) +
      ggtitle(paste0("Polling Error vs ", input$race_eth, " Percentage of Those Polled in the District"))
  })
  
  output$plot_educ <- renderPlot({
    
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(education_data, by = c("district"))
    
    selected_data <- switch(input$educ,
                            "High School or Less" = election_data %>% filter(educ4 == "High School Grad. or Less"),
                            "Some College" = election_data %>% filter(educ4 == "Some College Educ."),
                            "4-year College Graduate" = election_data %>% filter(educ4 == "4-year College Grad."),
                            "Postgraduate Degree" = election_data %>% filter(educ4 == "Postgraduate Degree"))
    
    ggplot(data = selected_data,
           aes(x = percent, y = poll_diff, text = district)) +
      geom_point() +
      geom_hline(yintercept = 0) +
      ggtitle(paste0("Polling Error vs the Percentage of Those with a ", input$educ, " Education"))
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
