library(rvest)
library(tidyr)
library(dplyr)
library(tibble)
library(here)
library(ggmap)
library(stringr)

url_base <- "https://www.basketball-reference.com"
url_main <- "https://www.basketball-reference.com/coaches/"

page_main <- read_html(url_main)

# scrape url endings for all listed coaches (suffixes for url_base)
url_coachesVector <- page_main %>%
  html_nodes('#coaches th a') %>%
  html_attr('href')


# scrape 3-column coaches table on main page, producing single character vector
coaches_main_vec <- page_main %>%
  html_nodes('#coaches .left') %>%
  html_text()

# convert single vector of main table data to 3-column dataframe
# https://stackoverflow.com/questions/39655452/convert-a-vector-to-data-frame-with-multiple-columns
coaches_main_df <- cbind.data.frame(
  split(coaches_main_vec[-(1:3)], rep(1:3, times = length(coaches_main_vec[-(1:3)])/3)),
  stringsAsFactors=F
  ) %>%
  rename(
    name = `1`,
    birth_date = `2`,
    college = `3`
  ) %>%
  filter(
    !str_detect(birth_date, 'Birth Date') # remove header rows sprinkled throughout
  ) %>%
  mutate(
    name = gsub("\\*", "", name) # remove asterisks from names
  )


# create vector of column names for tables about to be iteratively scraped
coach_colNames <- c(
  "season",
  "age",
  "league",
  "team",
  "g_reg",
  "w_reg",
  "l_reg",
  "wPct_reg",
  "ov500_reg",
  "place_reg",
  "g_playoff",
  "w_playoff",
  "l_playoff",
  "wPct_playoff",
  "notes"
)

# initialize lists to which I'll iteratively write coaching dataframes
headCoach_list <- list()
asstCoach_list <- list()

for(i in 1:length(url_coachesVector)){
  # read html from specified head coach's full page
  page_coach <- read_html( paste(url_base, url_coachesVector[[i]], sep='') )
  
  # scrape table w/ career coaching stats, as dataframe in list of length 1
  coach_tab <- page_coach %>%
    html_nodes('#coach-stats') %>%
    html_table()
 
  # read df from list, remove 1st row (labels), and rename columns
  coach_tab_edit <- coach_tab[[1]][2:nrow(coach_tab[[1]]), ]
  colnames(coach_tab_edit) <- coach_colNames # vector specified just above
  
  # filter out trailing rows w/ summary data and blank space, add name field
  coach_tab_edit <- coach_tab_edit %>% 
    filter( !str_detect(season, 'season') ) %>%
    filter( !str_detect(season, 'Career') ) %>%
    filter(season != '')
  coach_tab_edit$name <- coaches_main_df$name[i]
  
  # split remaining df in two: one df for head coaching seasons, one for non-head
  headCoach_df <- coach_tab_edit %>%
    filter( !str_detect(g_reg, 'Coach') ) %>%
    mutate_at(
      vars(g_reg:wPct_playoff), as.numeric
    )
  headCoach_df$coach_type <- 'head'
  
  asstCoach_df <- coach_tab_edit %>%
    filter( str_detect(g_reg, 'Coach') ) %>%
    select(
      season,age,league,team,name
    )
  if(nrow(asstCoach_df) > 0){
    asstCoach_df$coach_type <- 'assistant'
  }
  
  headCoach_list[[i]] <- headCoach_df
  asstCoach_list[[i]] <- asstCoach_df
}


save(
  headCoach_list,
  asstCoach_list,
  file = "C:/Users/tdtue/Dropbox/NBA_analytics/data/coachingLists_bbref_20180915.RData"
)



