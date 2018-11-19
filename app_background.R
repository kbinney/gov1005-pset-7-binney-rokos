# BACKGROUND CODE FOR PROBLEM SET 7 APP

# Loading potentially useful libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(kableExtra)
library(fs)



# LOADING DATA
# Downloading all polling data from the midterm 2 link provided and unzipping it
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")       # .zip file contains folder with data folder which contains polls by congressional district



# CREATING A DATA FRAME WITH ALL POLLS
# Generating a list of the paths & file names for all polls stored in the data folder
file_names <- dir_ls("2018-live-poll-results-master/data")

# Generating a combined table with all of the information from all of the polls using this list of file names and paths
all_polls <- map_dfr(file_names, read_csv, .id = "source")

# Cleaning the source names so that they are easier to work with
all_polls$source <- str_remove(all_polls$source, "2018-live-poll-results-master/data/elections-poll-")
all_polls$source <- str_remove(all_polls$source, ".csv")
all_polls <- separate(all_polls, source, into = c("district", "wave"), sep = "-")
all_polls <- separate(all_polls, district, into = c("state", "district_race"), sep = 2)

# Calculating the predicted democratic margin for each race and the demographic breakdowns of respondents in that race.
dem_advantage <- all_polls %>% 
  select(state, district_race, wave, response, final_weight) %>% 
  group_by(state, district_race, wave, response) %>% 
  summarize(weighted = sum(final_weight)) %>% 
  spread(key = response, value = weighted) %>% 
  ungroup() %>% 
  mutate(Dem_advantage = ((Dem - Rep)/(3 + 4 + 5 + 6 + Dem + Rep + Und))*100,
         district_race = case_when(!district_race %in% c("gov", "sen") ~ as.character(parse_integer(district_race)),
                                   TRUE ~ district_race)) %>% 
  select(state, district_race, wave, Dem_advantage)

# Only using the most recent polls
double_polls <- dem_advantage %>% 
  count(state, district_race) %>% 
  filter(n == 2)

dem_advatage <- dem_advantage %>% 
  left_join(double_polls, by = c("state", "district_race")) %>% 
  filter(is.na(n) | n == 2 & wave == 3) %>% 
  select(-n)

# Age table:
age <- all_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und"), 
         !ager %in% c("[DO NOT READ] Don't know/Refused", "[DO NOT READ] Refused")) %>% 
  group_by(state, district_race) %>% 
  count(ager) %>% 
  mutate(percent_in = n/(sum(n))*100)

# Education table:
education <- all_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und"), 
         educ4 != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(state, district_race) %>% 
  count(educ4) %>% 
  mutate(percent_in = n/(sum(n))*100)

# Gender table:
gender <- all_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und"), 
         gender != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(state, district_race) %>% 
  count(gender) %>% 
  mutate(percent_in = n/(sum(n))*100)

# Race table:
race <- all_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und"), 
         race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(state, district_race) %>% 
  count(race_eth) %>% 
  mutate(percent_in = n/(sum(n))*100)

# Likeliness to vote:
likeliness <- all_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und"), 
         likely != "[DO NOT READ] Don't know/Refused") %>% 
  group_by(state, district_race) %>% 
  count(likely) %>% 
  mutate(percent_in = n/(sum(n))*100)

# CREATING A DATA FRAME THAT INCLUDES THE RESULTS OF EACH RACE AS WELL (STILL AT POLL LEVEL)
# Downloaded results data from link provided in piazza and moved it to this project as "2018_House_Results.csv"
# Now reading in the results data
house_results <- read_csv("2018_House_Results.csv", col_names = TRUE)

# Need to join House results data with polling data. 
# Cleaning the House results data so that it is ideal for joining
house_results <- house_results %>% 
  mutate(`Dem %` = str_replace(`Dem %`, "%", ""),
         `Dem %` = parse_double(`Dem %`),
         `GOP %` = str_replace(`GOP %`, "%", ""),
         `GOP %` = parse_double(`GOP %`),
         `Other %` = str_replace(`Other %`, "%", ""),
         `Other %` = parse_double(`Other %`),
         `Dem Margin` = str_replace(`Dem Margin`, "%", ""),
         `Dem Margin` = parse_double(`Dem Margin`),
         `2016 Clinton Margin` = str_replace(`2016 Clinton Margin`, "%", ""),
         `2016 Clinton Margin` = parse_double(`2016 Clinton Margin`),
         `Swing vs. 2016 Prez` = str_replace(`Swing vs. 2016 Prez`, "%", ""),
         `Swing vs. 2016 Prez` = parse_double(`Swing vs. 2016 Prez`),
         `Raw Votes vs. 2016` = str_replace(`Raw Votes vs. 2016`, "%", ""),
         `Raw Votes vs. 2016` = parse_double(`Raw Votes vs. 2016`)) %>% 
  transmute(state_name = State, district_race = `CD#`, winner = `2018 Winner`,
            party = Party, actual_dem_margin = `Dem Margin`,
            dem_votes = `Dem Votes`, rep_votes = `GOP Votes`, other_votes = `Other Votes`,
            dem_percent = `Dem %`, rep_percent = `GOP %`, other_percent = `Other %`,
            clinton_16 = `2016 Clinton Margin`, swing_16 = `Swing vs. 2016 Prez`,
            total_votes_16 = `2016 Total Votes Cast`, percent_16_turnout = `Raw Votes vs. 2016`)

# Adding column with state abbreviations in lower case to make join with polling data easier
state_names <- bind_cols(state = state.abb, state_name = state.name) %>% 
  mutate(state = str_to_lower(state))

house_results <- house_results %>% 
  left_join(state_names, by = "state_name")

# Binding the two data sets so that we have each poll response and the actual outcome for the district of that response
results_and_polls <- all_polls %>% 
  mutate(district_race = case_when(!district_race %in% c("gov", "sen") ~ as.character(parse_integer(district_race)),
                            TRUE ~ district_race)) %>% 
  left_join(house_results, by = c("state", "district_race")) %>% 
  mutate(district = str_c(state, "-", district_race),
         district = str_to_upper(district)) %>% 
  select(-36:-86)   # dropping unnecessary survey questions

# Manually filling in the results for the gubernatiorial and senate races as well as for not-yet-called house races
results_and_polls %>% 
  filter(is.na(winner)) %>% 
  count(district)

results_and_polls <- results_and_polls %>% 
  mutate(winner = case_when(district == "az-sen" ~ "Kyrsten Sinema", 
                            TRUE ~ winner),
         party = case_when(district == "az-sen" ~ "D",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "az-sen" ~ 1.7,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "fl-gov" ~ "Ron DeSantis", 
                            TRUE ~ winner),
         party = case_when(district == "fl-gov" ~ "R",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "fl-gov" ~ -0.3,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "fl-sen" ~ "Rick Scott", 
                            TRUE ~ winner),
         party = case_when(district == "fl-sen" ~ "R",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "fl-sen" ~ -0.2,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "nv-sen" ~ "Jacky Rosen", 
                            TRUE ~ winner),
         party = case_when(district == "nv-sen" ~ "D",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "nv-sen" ~ 5,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "tn-sen" ~ "Marsha Blackburn",
                            TRUE ~ winner),
         party = case_when(district == "tn-sen" ~ "R",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "tn-sen" ~ -10.8,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "tx-sen" ~ "Ted Cruz",
                            TRUE ~ winner),
         party = case_when(district == "tx-sen" ~ "R",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "tx-sen" ~ -2.6,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "ca-39" ~ "Gil Cisneros",
                            TRUE ~ winner),
         party = case_when(district == "ca-39" ~ "D",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "ca-39" ~ 0.4,
                                       TRUE ~ actual_dem_margin),
         winner = case_when(district == "ut-4" ~ "Ben McAdams",
                            TRUE ~ winner),
         party = case_when(district == "ut-4" ~ "D",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "ut-4" ~ 0.4,
                                       TRUE ~ actual_dem_margin),
         not_called = case_when(district %in% c("ca-39", "ut-4", 
                                                "fl-gov", "fl-sen") ~ 0.5,
                                TRUE ~ 1))

# Calculating the predicted democratic margin for each race
results_and_polls %>% 
  select(state, district_race, wave, response, final_weight) %>% 
  group_by(state, district_race, wave, response) %>% 
  summarize(weighted = sum(final_weight)) %>% 
  spread(key = response, value = weighted) %>% 
  ungroup() %>% 
  mutate(Rep_advantage = ((Rep - Dem)/(3 + 4 + 5 + 6 + Dem + Rep + Und))*100,
         district_race = case_when(!district_race %in% c("gov", "sen") ~ as.character(parse_integer(district_race)),
                                   TRUE ~ district_race)) %>% 
  select(state, district_race, wave, Rep_advantage)


# Generating a table with columns for each demographic element and a column with the percent of respondents in that category
# First category is age:
# Likeliness to vote:
int_to_vote <- results_and_polls %>% 
  filter(response %in% c("Dem", "Rep", "Und")) %>% 
  count(likely, response) %>% 
  group_by(likely) %>% 
  mutate(total_responses = sum(n), percent_vote = n/total_responses) %>%
  select(likely, total_responses, response, percent_vote) %>% 
  spread(key = response, value = percent_vote)                # these numbers don't quite match the NYT table...

# Below NYT table it says, "Percentages are weighted to resemble likely voters; the number of respondents in each subgroup is unweighted. Undecided voters includes those who refused to answer," so I need to recreate my table to account for weights.
weighted_int_to_vote <- maine_02 %>% 
  select(response, likely, final_weight) %>% 
  group_by(likely, response) %>% 
  summarize(weighted_count = sum(final_weight)) %>% 
  filter(response %in% c("Dem", "Rep", "Und")) %>%
  group_by(likely) %>% 
  mutate(percent_vote = weighted_count/sum(weighted_count)*100) %>% 
  select(likely, response, percent_vote) %>% 
  spread(key = response, value = percent_vote) %>% 
  left_join(int_to_vote, by = "likely") %>% 
  select(likely, total_responses, Dem.x, Rep.x, Und.x)


