library(tidyverse)
library(readr)

# This file is used to take all interview level data and find out interesting
# things about each district's demographics

# First, we read in preformatted polling data. This data has already been
# compiled and joined with actual election data results.
elections_data <- read_rds("poll_data")

# We want to compare education to polling data. To do this, we first set the
# education levels and set up our NA result. We then figure out the percentage
# of each education level in each district. Finally, we remove the NA results.
education_factor <- elections_data %>% 
  group_by(district)  %>% 
  mutate(education = parse_factor(educ4, ordered = TRUE, include_na = FALSE,
                                  na = c("", "NA", "[DO NOT READ] Don't know/Refused"),
                                  levels = c("High School Grad. or Less", 
                                             "Some College Educ.", 
                                             "4-year College Grad.", 
                                             "Postgraduate Degree")),
         num_interviews = n()) %>% 
  group_by(num_interviews, district) %>% 
  count(education) %>% 
  mutate(percent = 100 * n / num_interviews) %>% 
  ungroup() %>%
  filter(!is.na(education)) %>% 
  # Rename this column so all tables have demographic column 
  mutate(demographic = education)

# We save this data to use in our shiny app as needed
write_rds(education_factor, path = "poll_diff/education_data")
  
race_edu_factor <- elections_data %>% 
  group_by(district)  %>% 
  mutate(race_edu = parse_factor(race_edu, levels = NULL, include_na = FALSE, 
                               na = c("", "na", "[DO NOT READ] Don't know/Refused")),
         race_edu = recode_factor(race_edu,
                                            "Nonwhite" = "Nonwhite",
                                            "White, 4-Year College Grads" = "White, college grad",
                                            "White, No 4-Year College Degree" = "White, not college grad", 
                                            .ordered = TRUE),
       num_interviews = n()) %>% 
  group_by(num_interviews, district) %>% 
  count(race_edu) %>% 
  mutate(percent = 100 * n / num_interviews) %>% 
  ungroup() %>%
  filter(!is.na(race_edu)) %>% 
  select(district, percent, race_edu) %>% 
  # Rename this column so all tables have demographic column 
  mutate(demographic = race_edu)

write_rds(race_edu_factor, path = "poll_diff/race_edu")

race_factor <- elections_data %>% 
  group_by(district)  %>% 
  mutate(race_eth = parse_factor(race_eth, levels = NULL, include_na = FALSE, 
                                 na = c("", "na", "[DO NOT READ] Don't know/Refused")),
         num_interviews = n()) %>% 
  group_by(num_interviews, district) %>% 
  count(race_eth) %>% 
  mutate(percent = 100 * n / num_interviews) %>% 
  ungroup() %>%
  filter(!is.na(race_eth)) %>% 
  select(district, percent, race_eth) %>% 
  # Rename this column so all tables have demographic column 
  mutate(demographic = race_eth)

write_rds(race_factor, path = "poll_diff/race_eth")


