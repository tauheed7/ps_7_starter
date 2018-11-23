library(readr)
library(tidyverse)
library(fs)
library(stringr)

#Here I downloaded all the necessary libraries.


mt_2_results <- read_csv("mt_2_results.csv") %>% 
  filter(district != "sen",
         district != "gov",
         district != "AL") %>% 
  mutate(total = dem_votes + rep_votes + other_votes, 
         real_dem = dem_votes / total * 100, real_rep = rep_votes / total * 100, 
         real_other = other_votes / total * 100) %>% 
  filter(total != 0) %>% 
  unite("join", c("state", "district"), sep = "-", remove = FALSE)

#Here I read in Mr. Schroeder's data, filtered out non-Congressional races and created some new desired variables. 
#I also used the unite function to help me join two datasets together later. 

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "poll-results.zip",
              quiet = TRUE, 
              mode = "wb")

unzip("poll-results.zip")
file_delete("poll-results.zip")

#Here I downloaded, unzipped, and deleted data to keep my repo clean.

my_list <- dir_ls("2018-live-poll-results-master/data/")

#This function allowed me to create a list of all the file names of races.

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

#Here I mapped all the data from the file names to a new dataframe x and 
# did a lot of data manipulation in order to eventually show the difference in polling results of voter education.
# I created a few variables to get percentages of vote shares, spread by response, grouped by my 3 desired variables, 
# and filtered out undesired information. 

all_data <- left_join(x, mt_2_results, by = "join")

#I joined the two datasets here. 

write_rds(all_data, path = "ps_7/ps_7_data.rds")

#This will allow me to read in the data in my shiny app.



