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
                                    choices = c("High School or Less" = "High School Grad. or Less", 
                                                "Some College" = "Some College Educ.", 
                                                "4-year College Graduate" = "4-year College Grad.", 
                                                "Postgraduate Degree" = "Postgraduate Degree"),
                                    selected = "High School Grad. or Less")
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
                 checkboxGroupInput("gender",
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
    election_data %>% 
      filter(demographic %in% input$race_eth) %>% 
      ggplot(aes(x = percent, y = poll_diff)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
      facet_wrap(~demographic) +
      ggtitle("Racial Minorities Aided Democrats",
              subtitle = "In districts with greater percentages of polled racial minorities, \nthe NYTimes Upshot polls underestimated the Democratic margin") +
      xlab("Percent of Individuals Polled") +
      ylab("Absolute Change in Predicted to Real Democratic Advantage")
  })
  
  output$plot_educ <- renderPlot({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(education_data, by = c("district"))
    election_data %>% 
      filter(demographic %in% input$educ) %>% 
      ggplot(aes(x = percent, y = poll_diff)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
      facet_wrap(~demographic) +
      ggtitle("The Education Divide",
              subtitle = "Districts with greater percentages of college grads (and fewer individuals with a high school diploma or less) \nvoted for Democrats at slighlty higher rates than polls anticipated") +
      xlab("Percent of Individuals Polled") +
      ylab("Absolute Change in Predicted to Real Democratic Advantage")
  })
  
  output$plot_age <- renderPlot({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(age_data, by = c("district"))
    election_data %>% 
      filter(demographic %in% input$age) %>% 
      ggplot(aes(x = percent, y = poll_diff)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
      facet_wrap(~demographic) +
      ggtitle("Age barely mattered",
              subtitle = "Polls may slightly have overweighted the democratic edge in young people
and underplayed it in everyone else, but any relationship between age of 
a district and polling error is very small") +
      xlab("Percent of Individuals Polled") +
      ylab("Absolute Change in Predicted to Real Democratic Advantage")
  })
  
  output$plot_gender <- renderPlot({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(gender_data, by = c("district"))
    election_data %>% 
      filter(demographic %in% input$gender) %>% 
      ggplot(aes(x = percent, y = poll_diff)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
      facet_wrap(~demographic) +
      ggtitle("Female Republicans?",
              subtitle = "Districts with more women voted for Democrats at lower rates than expected") +
      xlab("Percent of Individuals Polled") +
      ylab("Absolute Change in Predicted to Real Democratic Advantage")
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
