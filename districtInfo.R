library(tidyverse)
library(readr)

elections_data <- read_rds("poll_data") %>% 
  mutate(race_type = as.character(race_type),
         district_abb = case_when(race_type == "senate" ~ "sen",
                                  race_type == "governor" ~ "gov",
                                  TRUE ~ as.character(district)),
         district = paste0(state, "-", district_abb))

# 
education_factor <- elections_data %>% 
  group_by(district)  %>% 
  # Let's nicely refactor eduction
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
  select(district, percent, education) %>% 
  spread(education, percent)
  

mutate(race_edu = parse_factor(race_edu, levels = NULL, include_na = FALSE, 
                               na = c("", "na", "[DO NOT READ] Don't know/Refused")),
       "Race and Education" = recode_factor(race_edu,
                                            "Nonwhite" = "Nonwhite",
                                            "White, 4-Year College Grads" = "White, college grad",
                                            "White, No 4-Year College Degree" = "White, not college grad", 
                                            .ordered = TRUE)) %>% 