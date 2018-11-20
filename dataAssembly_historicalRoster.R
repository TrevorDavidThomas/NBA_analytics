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


# call 'commonallplayers' endpoint to get full roster of all historical NBA players, up to current year
# (shouldn't take long to run)
players_fullRoster_JSON <- GET(
  "http://stats.nba.com/stats/commonallplayers",
  query = list(
    Season = '2018-19',
    LeagueID = '00',
    IsOnlyCurrentSeason = 0),
  add_headers(request_headers)
)

# use fromJSON to get list of returns from API, process into dataframe,
#   apply headers as column names
players_fullRoster_list <- fromJSON(content(players_fullRoster_JSON, as = "text"))
players_fullRoster_df <- tbl_df(data.frame(players_fullRoster_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players_fullRoster_df) = tolower(players_fullRoster_list$resultSets$headers[[1]])

# create additional column with just player last name
players_fullRoster_df <- players_fullRoster_df %>%
  mutate(
    lastName = str_extract(display_last_comma_first, "^([^,])+")
  )

# save roster df to relative 'data' directory
save(players_fullRoster_df,
     file = here('data', 'players_fullRoster_df.RData'))

