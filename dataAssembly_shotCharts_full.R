library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(here)
library(stringr)

localPath <- "C:/Users/tdtue/Dropbox/NBA_analytics/data/"

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


# specify vector of (start) years to collect data for, corresponding to range of available
#    short chart data
years <- 1996:2018

# specify vector of year strings (formatted for stats.nba API call)
years_str <- character()
for(i in 1:length(years)){
  if(years[i] >= 1999){
    years_str[i] <- paste(
      (years[i]), "-",
      str_pad(
        as.character(years[i]-2000 + 1), width=2, pad = '0'
      ),
      sep = ''
    )} else {
      years_str[i] <- paste(
        (years[i]), "-",
        str_pad(
          as.character(years[i]-1900 + 1), width=2, pad = '0'
        ),
        sep = ''
      )
    }
}

shotChart_fullYear_list <- list()

for(i in 1:length(years_str)){ #
  t1 <- Sys.time()
  
  # specify request to stats.nba.com API
  request_shotChart <- GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = 0, # set to 0 to get all players
      Season = years_str[i],
      SeasonType = "Regular Season",
      PlayerPosition = "",
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  stop_for_status(request_shotChart) # make sure request didn't fail
  
  # get content of response from json object
  shot_data_list <- fromJSON(content(request_shotChart, as = "text"))
  
  # convert to dataframe and add column names
  shot_data_df <- tbl_df(data.frame(shot_data_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
  names(shot_data_df) <- tolower(shot_data_list$resultSets$headers[[1]])
  
  # mutate variables to convert to numeric values and add shot make and shot value
  #   numeric indicators
  shot_data_df <- mutate(shot_data_df,
                         # loc_x = as.numeric(as.character(loc_x)) / 10,
                         # loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                         shot_distance = as.numeric(as.character(shot_distance)),
                         shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                         shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                         shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                         shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                         game_date = as.Date(game_date, format = "%Y%m%d"),
                         season_id = years_str[i]
  )
  
  # write shot dataframe to list 
  shotChart_fullYear_list[[i]] <- shot_data_df
  
  # update with last year completed and time it took to complete
  t2 <- Sys.time()
  print(paste(years_str[i], "complete in", t2 - t1,
              sep = ' '))
}


save(shotChart_fullYear_list,
     file = paste(localPath, "shotChart_fullYear_list.RData", sep=''))

