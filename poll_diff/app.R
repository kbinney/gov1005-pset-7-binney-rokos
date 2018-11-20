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
               mainPanel(plotlyOutput("plot_race_eth"))
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
               mainPanel(plotlyOutput("plot_educ"))
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
               mainPanel(plotlyOutput("plot_age"))
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
               mainPanel(plotlyOutput("plot_gender"))
               )
             )
           )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot_race_eth <- renderPlotly({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(race_eth_data, by = c("district")) %>% 
      filter(demographic %in% input$race_eth)
    
    ggplotly(tooltip = c("text"),
             ggplot(data = election_data,
                    aes(x = percent, y = poll_diff, text = district, group = 1)) +
               geom_point() +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "Racial Minorities Aided Democrats",
                    subtitle = "In districts with greater percentages of polled racial minorities, 
                    the NYTimes Upshot polls underestimated the Democratic margin")) %>% 
      config(displayModeBar = FALSE)
      
  })
  
  output$plot_educ <- renderPlotly({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(education_data, by = c("district")) %>% 
      filter(demographic %in% input$educ)
    
    ggplotly(tooltip = c("text"),
             ggplot(data = election_data,
                    aes(x = percent, y = poll_diff, text = district, group = 1)) +
               geom_point() +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "The Education Divide",
                    subtitle = "Districts with greater percentages of college grads 
                    (and fewer individuals with a high school diploma or less) voted for Democrats 
                    at slighlty higher rates than polls anticipated")) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$plot_age <- renderPlotly({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(age_data, by = c("district")) %>% 
      filter(demographic %in% input$age)
    
    ggplotly(tooltip = c("text"),
             ggplot(data = election_data,
                    aes(x = percent, y = poll_diff, text = district, group = 1)) +
               geom_point() +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "Age barely mattered",
                    subtitle = "Polls may slightly have overweighted the democratic edge in young people
                    and underplayed it in everyone else, but any relationship between age of 
                    a district and polling error is very small")) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$plot_gender <- renderPlotly({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(gender_data, by = c("district")) %>% 
      filter(demographic %in% input$gender)
    
    ggplotly(tooltip = c("text"),
             ggplot(data = election_data,
                    aes(x = percent, y = poll_diff, text = district, group = 1)) +
               geom_point() +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "Female Republicans?",
                    subtitle = "Districts with more women voted for Democrats at lower rates than expected")) %>% 
      config(displayModeBar = FALSE)
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
