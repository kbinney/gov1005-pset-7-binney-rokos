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
  transmute(state_name = State, 
            district_race = `CD#`, 
            winner = `2018 Winner`,
            party = Party, 
            actual_dem_margin = `Dem Margin`,
            dem_votes = `Dem Votes`, 
            rep_votes = `GOP Votes`, 
            other_votes = `Other Votes`,
            dem_percent = `Dem %`, 
            rep_percent = `GOP %`, 
            other_percent = `Other %`,
            clinton_16 = `2016 Clinton Margin`, 
            swing_16 = `Swing vs. 2016 Prez`,
            total_votes_16 = `2016 Total Votes Cast`, 
            percent_16_turnout = `Raw Votes vs. 2016`)

# Adding column with state abbreviations in lower case to make join with polling data easier
state_names <- bind_cols(state = state.abb, state_name = state.name) %>% 
  mutate(state = str_to_lower(state))

house_results <- house_results %>% 
  left_join(state_names, by = "state_name")

# Binding the two data sets so that we have each poll response and the actual outcome for the district of that response
results_and_polls <- all_polls %>% 
  mutate(district_race = case_when(district_race == "gov" ~ "gov",
                                   district_race == "sen" ~ "sen",
                                   TRUE ~ as.character(parse_number(district_race)))) %>% 
  left_join(house_results, by = c("state", "district_race")) %>% 
  mutate(district = str_c(state, "-", district_race)) %>% 
  select(-36:-86)   # dropping unnecessary survey questions

results_and_polls <- results_and_polls %>% 
  mutate(winner = case_when(district == "az-sen" ~ "Kyrsten Sinema", 
                            district == "fl-gov" ~ "Ron DeSantis",
                            district == "fl-sen" ~ "Rick Scott",
                            district == "nv-sen" ~ "Jacky Rosen", 
                            district == "tn-sen" ~ "Marsha Blackburn",
                            district == "tx-sen" ~ "Ted Cruz",
                            district == "ca-39" ~ "Gil Cisneros",
                            district == "ut-4" ~ "Ben McAdams",
                            TRUE ~ winner),
         party = case_when(district == "az-sen" ~ "D",
                           district == "fl-gov" ~ "R",
                           district == "fl-sen" ~ "R",
                           district == "nv-sen" ~ "D",
                           district == "tn-sen" ~ "R",
                           district == "tx-sen" ~ "R",
                           district == "ca-39" ~ "D",
                           district == "ut-4" ~ "D",
                           TRUE ~ party),
         actual_dem_margin = case_when(district == "az-sen" ~ 1.7,
                                       district == "fl-gov" ~ -0.3,
                                       district == "fl-sen" ~ -0.2,
                                       district == "nv-sen" ~ 5,
                                       district == "tn-sen" ~ -10.8,
                                       district == "tx-sen" ~ -2.6,
                                       district == "ca-39" ~ 0.4,
                                       district == "ut-4" ~ 0.4,
                                       TRUE ~ actual_dem_margin),
         not_called = case_when(district %in% c("ca-39", "ut-4", 
                                                "fl-gov", "fl-sen") ~ 0.5,
                                TRUE ~ 1))

# We store this data for use in figuring out interesting things about each district
write_rds(results_and_polls, path = "poll_data")


