library(tidyverse)
library(readr)

# This file is used to take all interview level data and find out interesting
# things about each district's demographics

# First, we read in preformatted polling data. This data has already been
# compiled and joined with actual election data results.
elections_data <- read_rds("poll_data")

# Education data
education <- elections_data %>% 
  filter(educ4 != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(district)  %>% 
  count(educ4) %>% 
  mutate(percent = 100 * n / sum(n),
         demographic = educ4) %>% 
  ungroup() %>%
  select(district, percent, educ4, demographic)


# Race table
race <- elections_data %>% 
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(district)  %>% 
  count(race_eth) %>% 
  mutate(percent = 100 * n / sum(n),
         demographic = race_eth) %>% 
  ungroup() %>%
  select(district, percent, race_eth, demographic)


# Age table:
age <- elections_data %>% 
  filter(!(ager %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused"))) %>% 
  group_by(district) %>% 
  count(ager) %>% 
  mutate(percent = 100 * n / sum(n),
         demographic = ager) %>% 
  select(district, percent, ager, demographic) %>% 
  ungroup()

# Gender table:
gender <- elections_data %>% 
  filter(gender != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(district) %>% 
  count(gender) %>% 
  mutate(percent = 100 * n / sum(n),
         demographic = gender) %>% 
  select(district, percent, demographic, gender) %>% 
  ungroup()

# Likeliness to vote:
likeliness <- elections_data %>% 
  filter(likely != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(district) %>% 
  count(likely) %>% 
  mutate(percent = 100 * n / sum(n),
         demographic = likely) %>% 
  select(district, percent, demographic, likely) %>% 
  ungroup()

# We keep these as inidividual data frames and write them all for use in our app
write_rds(education, path = "poll_diff/education_data")
write_rds(race, path = "poll_diff/race_eth")
write_rds(age, path = "poll_diff/age_data")
write_rds(gender, path = "poll_diff/gender_data")
write_rds(likeliness, path = "poll_diff/likliness")
