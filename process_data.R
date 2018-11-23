library(readr)
library(tidyverse)
library(fs)
library(stringr)
library(scales)



mt_2_results <- read_csv("mt_2_results.csv") %>% 
  filter(district != "sen",
         district != "gov",
         district != "AL") %>% 
  mutate(total = dem_votes + rep_votes + other_votes, 
         real_dem = dem_votes / total * 100, real_rep = rep_votes / total * 100, 
         real_other = other_votes / total * 100) %>% 
  filter(total != 0) %>% 
  unite("join", c("state", "district"), sep = "-", remove = FALSE)

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "poll-results.zip",
              quiet = TRUE, 
              mode = "wb")

unzip("poll-results.zip")
file_delete("poll-results.zip")

my_list <- dir_ls("2018-live-poll-results-master/data/")

x <- map_dfr(my_list, read_csv, .id = "name") %>% 
  filter(!str_detect(name, ("gov")), !str_detect(name, ("sen"))) %>% 
  filter(str_sub(name, start = -5, end = -5) == 3) %>% 
  mutate(state = toupper(str_sub(name, -10, -9)), 
         district = str_sub(name, -8, -7)) %>% 
  unite("join", c("state", "district"), sep = "-", remove = FALSE) %>% 
  filter(educ != "[DO NOT READ] Refused", educ != "Grade school", 
           approve != "[DO NOT READ] Don't know/Refused") %>%
  filter(response %in% c("Rep", "Dem", "Und")) %>% 
  group_by(join, educ, response) %>%
  summarize(count = n()) %>% 
  spread(key = response, count, fill = 0) %>% 
  mutate(Total = Dem + Rep + Und,
         predicted_dem = Dem/Total * 100,
         predicted_rep = Rep/Total * 100,
         predicted_other = Und/Total * 100)

all_data <- left_join(x, mt_2_results, by = "join")

write_rds(all_data, path = "ps_7/ps_7_data.rds")



