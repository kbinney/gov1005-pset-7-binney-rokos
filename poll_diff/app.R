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
library(reshape2)
library(plotly)


# Our error data and demographic data is stored in different subtables.
# We first load them all, and join them in the relevant piece of code
error_data <- read_rds("error_data")
race_eth_data <- read_rds("race_eth")
education_data <- read_rds("education_data")
age_data <- read_rds("age_data")
gender_data <- read_rds("gender_data")

# Define UI for application. Our application has a navbar with a tab for each
# demographic. Each tab has its own option for selecting input
ui <- navbarPage("Midterm Election results: Predictions and Actual", 
  # Add race page with sidebar and plot               
  tabPanel("Race/Ethnicity",
           fluidPage(
             titlePanel("Race/Ethnicity and 2018 Midterm Polling Errors"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("race_eth",
                                    label = "Select Race/Ethnicity:",
                                    choices = c("White", "Black", "Hispanic", "Asian"),
                                    selected = "White"),
                 # Plotly makes adding subtitles hard, so we add further explanation here
                 p("In districts with greater percentages of polled racial minorities, the NYTimes Upshot polls underestimated the Democratic margin")
                 ),
               mainPanel(plotlyOutput("plot_race_eth"))
               )
             )
           ),
  # Add education page with sidebar and plot
  tabPanel("Education",
           fluidPage(
             titlePanel("Education and 2018 Midterm Polling Errors"),
             sidebarLayout(
               sidebarPanel(
                 # Our plot variables are not super pretty, so we show nice names
                 checkboxGroupInput("educ",
                                    label = "Select Educational Attainment:",
                                    choices = c("High School or Less" = "High School Grad. or Less", 
                                                "Some College" = "Some College Educ.", 
                                                "4-year College Graduate" = "4-year College Grad.", 
                                                "Postgraduate Degree" = "Postgraduate Degree"),
                                    selected = "High School Grad. or Less"),
                 # Subtitles don't look great with plotly, so we add here what we normally
                 # would put as a subtitle
                 p("Districts with greater percentages of college grads 
                    (and fewer individuals with a high school diploma or less) voted for Democrats 
                 at slighlty higher rates than polls anticipated")
               ),
               mainPanel(plotlyOutput("plot_educ"))
               )
             )
           ),
  # Add Age page with sidebar and plot
  tabPanel("Age",
           fluidPage(
             # Page title
             titlePanel("Age and 2018 Midterm Polling Errors"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("age",
                                    label = "Select Age Range:",
                                    choices = c("18 to 34", "35 to 49", "50 to 64", "65 and older"),
                                    selected = "18 to 34"),
                 # Subtitles don't look great with plotly, so we add here what we normally
                 # would put as a subtitle
                 p("Polls may slightly have overweighted the democratic edge in young people
                    and underplayed it in everyone else, but any relationship between age of 
                    a district and polling error is very small")
               ),
               mainPanel(plotlyOutput("plot_age"))
               )
             )
           ),
  # Add gender page with sidebar and plot
  tabPanel("Gender",
           fluidPage(
             titlePanel("Gender and 2018 Midterm Polling Errors"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("gender",
                                    label = "Select Gender:",
                                    choices = c("Female", "Male"),
                                    selected = "Female"),
                 # Subtitles don't look great with plotly, so we add here what we normally
                 # would put as a subtitle
                 p("Districts with more women voted for Democrats at lower rates than expected")
               ),
               mainPanel(plotlyOutput("plot_gender"))
               )
             )
           )
)


# Define server logic required to draw a histogram
# NOTE: using plotly allows for mouseover visibility of the districts. 
# However, faceted wrap axis titles over lap with axis lablels. This
# is a known issues (https://github.com/ropensci/plotly/issues/1224) but
# we decided it the mouse over ability was worth the slighlty worse formatting
server <- function(input, output) {

  output$plot_race_eth <- renderPlotly({
    # This chooses which demographic, which we need to join to the data with 
    # poll error before graphing
    election_data <- error_data %>% 
      left_join(race_eth_data, by = c("district")) %>% 
      filter(demographic %in% input$race_eth)
      ggplotly(tootip = c("text"), 
             ggplot(data = election_data,
                    aes(x = percent, y = poll_diff, text = district, group = 1)) +
               geom_point(size = 1) +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size = 0.5) +
               facet_wrap(~demographic) +
               xlab("Percent of Individuals Polled") +
               labs(y = "Actual Democratic Margin beyond Expected",
                    title = "Racial Minorities Aided Democrats"))  %>% 
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
               geom_point(size = 1) +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size = 0.5) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "The Education Divide") )%>% 
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
               geom_point(size = 1) +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size = 0.5) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "Age barely mattered")) %>% 
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
               geom_point(size = 1) +
               geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, size = 0.5) +
               facet_wrap(~demographic) +
               labs(x = "Percent of Individuals Polled",
                    y = "Actual Democratic Margin beyond Expected",
                    title = "Female Republicans?")) %>% 
      config(displayModeBar = FALSE)
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
