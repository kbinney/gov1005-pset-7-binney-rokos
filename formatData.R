# In this file, I create the same elections database
# as in my Rmd file. However, I then save the data nicely
# so I can load it directly in my app more easily. I find
# that cleaning my data in advance helps keep my shiny file
# cleaner and easier to work in

library(tidyverse)
library(readr)
library(fs)

download.file("https://goo.gl/ZRCBda",
              "2018-live-poll-results-master.zip",
              quiet = TRUE)
unzip("2018-live-poll-results-master.zip")


poll_results <- dir_ls("2018-live-poll-results-master/data") %>%
  map_dfr(read_csv, .id = "source")  
# delete all data files stored in downloaded folder and delete the xip file 
file_delete(dir_ls("2018-live-poll-results-master/"))
file_delete("2018-live-poll-results-master.zip")


# As Preceptor commented in his midterm 1 solutions, the poll data files
# contain information in their names in a structurally consistent manner. Each
# file is titled "elections-poll-[2 letter state code][race]-[poll
# number].csv" We can use this structure to extract the relevant information
# from the source. Note the race is either a 2 digit congressional district or
# sen, for senate. Sen is 3 characters, so we need to be careful when
# extracting the race and the poll number, as these are not always the same
# number character in the string. I choose to extract the state name by
# character index in the string, then got the rest of the name before the .csv
# (ie character right after the state until character 5 before the end). This
# gave a string of the format [race]-[poll number] which we could separate via
# the separate function
poll_results <- poll_results %>% 
  mutate(state = toupper(str_sub(source, 51, 52)),
         race_poll = str_sub(source, 53, -5)) %>% 
  separate(race_poll, into = c("race_type", "poll_wave"), sep = "-") %>% 
  # I created two columns giving me information about races. The first is
  # a numeric column denoting the district for house races. The second
  # is a factor column denoting house, senate, or governer race.
  # We also want to convert the poll from char to int
  mutate(district = parse_number(race_type, na = c("", "NA", "sen", "gov")),
         race_type = recode_factor(race_type, 
                                   "sen" = "senate",
                                   "gov" = "governor",
                                   .default = "house"),
         poll_wave = parse_number(poll_wave)) 
  
# After joining all data, we save it to read in when finding interesting
# district info
write_rds(poll_results, path = "poll_data")

poll_results <- poll_results %>% 
  # We need to figure out which poll is latest for each election, and
  # then calculate the rep advantage in each race
  group_by(state, race_type, district) %>% 
  # this allows us to find max poll in a race
  mutate(max_wave = max(poll_wave, na.rm = TRUE)) %>%
  # filter for columns in final poll for race
  filter(poll_wave == max_wave) %>% 
  mutate(group_wt = sum(final_weight)) %>% 
  # To calculate responses, we need to group by poll
  # and response. To keep our group_wt number around
  # we also need to include that variable in our grouping
  group_by(state, race_type, district, group_wt, response) %>% 
  tally(wt = final_weight) %>% 
  # Spreading our response into columns makes it easier to 
  # calculate the republican advantage
  spread(response, n) %>% 
  summarize(dem = sum(Dem),
            rep = sum(Rep),
            other = group_wt - dem - rep,
            rep_advantage = 100 * (rep - dem) / group_wt) %>% 
  # I want to join the tables to have state, race_type, district as 
  # [State]-[district num, sen, gov], dem, rep, other, rep_advantage_poll, rep_advantage
  ungroup() %>% 
  mutate(race_type = as.character(race_type),
         district_abb = case_when(race_type == "senate" ~ "sen",
                                  race_type == "governor" ~ "gov",
                                  TRUE ~ as.character(district)),
         district = paste0(state, "-", district_abb)) %>% 
  select(state, race_type, district, dem, rep, other, rep_advantage)


# Next, I read in and reformat my data from the actual results 
# I used the house data linked in piazza, however downloaded as 
# csv (was html by default) using link:
# https://docs.google.com/spreadsheets/d/1WxDaxD5az6kdOjJncmGph37z0BPNhV1fNAH_g7IkpC0/gviz/tq?tqx=out:csv&sheet={Sheet1}
house_results <- read_csv("house_results.csv") %>% 
  # We only need the state, district, and vote numbers
  # For simplicity, we rename as we select columns
  select("district" = `CD#`,
         "state" = "State",
         "dem" = `Dem Votes`,
         "rep" = `GOP Votes`,
         "other" = `Other Votes`) %>% 
  # We want to convert our state names to abbreviations to match election 
  # prediction dataframe. I found help on how to do this here
  # https://stackoverflow.com/questions/5411979/state-name-to-abbreviation-in-r
  # R has built in state.abb and state.name we can use to convert
  mutate(state = state.abb[match(state, state.name)]) %>% 
  # We will convert districts to standard naming (MA-1) and get
  # rid of two rows that hold basic information. Some states have 
  # "At-large" districts, but these are not in the upshot data so we can
  # ignore them. We will keep the state around in case we want it
  # for reactiveness in our shiny app. 
  filter(!is.na(state), !is.na(district)) %>% 
  mutate(rep_advantage = 100 * (rep - dem) / (rep + dem + other),
         state2 = state,
         race_type = "house") %>% 
  unite(district, state2, district, sep = "-")


# I manually found the relevant senate and governor races polled in the
# upshot data and made them csv files. I found the numbers on the NYTimes
# website at 11:45, saturday
senate_results <- read_csv("senate_results.csv") %>% 
  mutate(rep_advantage = 100 * (rep - dem) / (rep + dem + other),
         race_type = "senate",
         district = paste0(state, "-", "sen"))

governor_results <- read_csv("governor_results.csv") %>% 
  mutate(rep_advantage = 100 * (rep - dem) / (rep + dem + other),
         race_type = "governor",
         district = paste0(state, "-", "gov"))

# Let's bind all results into a single data frame
results <- bind_rows(house_results, senate_results, governor_results)

# Finally, let's join the poll data and the actual data
# Left joining to poll results ensures that we only keep 
# information about races in the upshot data
all_election_data <- poll_results %>% 
  left_join(results, by = c("district", "state", "race_type"), 
            suffix = c("_poll", "_results"))

# We now have really nice, clean data we can store to use in our app
write_rds(all_election_data, path = "poll_diff/data")