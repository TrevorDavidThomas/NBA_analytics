library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(stringr)

# define headers for use in GET() call to stats.nba API
request_headers = c(
  "accept-encoding" = "gzip, deflate, sdch",
  "accept-language" = "en-US,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"
)


# load full, historical player roster (generated via dataAssembly_historicalRoster.R)
load(here('data', 'players_fullRoster_df.RData'))

# initialize list in which to write a dataframe of season totals for each player
playerRegSeasonTots_allHist_list <- list()


for(i in 1:nrow(players_fullRoster_df)){ # loop through historical roster;
  # if code stalls (as is its wont),
  # can restart loop index from last succesful iteration
  
  players_fullRoster_profiles_JSON <- GET(
    "http://stats.nba.com/stats/playerprofilev2",
    query = list(
      PlayerID = players_fullRoster_df$person_id[i],
      LeagueID = "00",
      PerMode = "Totals"
    ),
    add_headers(request_headers)
  )
  
  # take json output, convert to dataframe
  players_fullRoster_profiles_list <- fromJSON(content(players_fullRoster_profiles_JSON, as = "text"))
  players_fullRoster_profiles_df <- tbl_df(data.frame(players_fullRoster_profiles_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
  
  if(ncol(players_fullRoster_profiles_df) >= 1){
    names(players_fullRoster_profiles_df) <- tolower(players_fullRoster_profiles_list$resultSets$headers[[1]])
  }
  playerRegSeasonTots_allHist_list[[i]] <- players_fullRoster_profiles_df
  
  # print index to monitor progress
  print(i)
}



# Find IDs for players in full roster who didn't make it into reg season stats list,
#    try again with them (some will be missing because they were on a roster but never played;
#    make sure that's the case)

prelim_results_df <- playerRegSeasonTots_allHist_list %>% bind_rows()
roster_round2_df <- players_fullRoster_df %>%
  filter(!(person_id %in% prelim_results_df$player_id))

playerRegSeasonTots_allHist_list_round2 <- list()

for(i in 1:nrow(players_fullRoster_df)){
  players_fullRoster_profiles_JSON <- GET(
    "http://stats.nba.com/stats/playerprofilev2",
    query = list(
      PlayerID = players_fullRoster_df$person_id[i],
      LeagueID = "00",
      PerMode = "Totals"
    ),
    add_headers(request_headers)
  )
  
  # take json output, convert to dataframe
  players_fullRoster_profiles_list <- fromJSON(content(players_fullRoster_profiles_JSON, as = "text"))
  players_fullRoster_profiles_df <- tbl_df(data.frame(players_fullRoster_profiles_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
  
  if(ncol(players_fullRoster_profiles_df) >= 1){
    names(players_fullRoster_profiles_df) <- tolower(players_fullRoster_profiles_list$resultSets$headers[[1]])
  }
  playerRegSeasonTots_allHist_list_round2[[i]] <- players_fullRoster_profiles_df
  
  # print index to monitor progress
  print(i)
}

# append newly gathered data to initial list of gathered data
#   (this process can be iterated)
playerRegSeasonTots_allHist_list <- append(
  playerRegSeasonTots_allHist_list, playerRegSeasonTots_allHist_list_round2
)

# save final output
save(playerRegSeasonTots_allHist_list,
     file = here("data","player_profiles_all_regSeasTots_list.RData"))

