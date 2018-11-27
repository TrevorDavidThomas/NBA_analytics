library(httr)
library(jsonlite)
library(tidyverse)

setwd("C:/Users/tdtue/Dropbox/NBA_analytics/Data")

request_headers = c(
  "accept-encoding" = "gzip, deflate, sdch",
  "accept-language" = "en-US,en;q=0.8",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)


players_fullRoster_JSON <- GET(
  "http://stats.nba.com/stats/commonallplayers",
  query = list(
    Season = '2017-18',
    LeagueID = '00',
    IsOnlyCurrentSeason = 0),
  add_headers(request_headers)
)


players_fullRoster_list <- fromJSON(content(players_fullRoster_JSON, as = "text"))
players_fullRoster_df <- tbl_df(data.frame(players_fullRoster_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players_fullRoster_df) = tolower(players_fullRoster_list$resultSets$headers[[1]])

players_fullRoster_df <- players_fullRoster_df %>%
  mutate(
    lastName = str_extract(display_last_comma_first, "^([^,])+")
  )

save(players_fullRoster_df, file = "players_fullRoster_df.RData")
# load("players_fullRoster_df.RData")


player_profiles_all_regSeasTots_list <- list()

system.time(
  for(i in 4208:nrow(players_fullRoster_df)){
    
    players_fullRoster_profiles_JSON <- GET(
      "http://stats.nba.com/stats/playerprofilev2",
      query = list(
        PlayerID = players_fullRoster_df$person_id[i],
        LeagueID = "00",
        PerMode = "Totals"
      ),
      add_headers(request_headers)
    )
    
    players_fullRoster_profiles_list <- fromJSON(content(players_fullRoster_profiles_JSON, as = "text"))
    players_fullRoster_profiles_df <- tbl_df(data.frame(players_fullRoster_profiles_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
    
    if(ncol(players_fullRoster_profiles_df) >= 1){
      names(players_fullRoster_profiles_df) <- tolower(players_fullRoster_profiles_list$resultSets$headers[[1]])
    }
    player_profiles_all_regSeasTots_list[[i]] <- players_fullRoster_profiles_df
    print(i)
  }
)

save(player_profiles_all_regSeasTots_list, file = "player_profiles_all_regSeasTots_list.RData")
# load("player_profiles_all_regSeasTots_list.RData")


player_profiles_all_regSeasTots_rbindDF <- bind_rows(player_profiles_all_regSeasTots_list)

player_profiles_all_regSeasTots_rbindDF$row_ID <- 1:nrow(player_profiles_all_regSeasTots_rbindDF)

dups <- player_profiles_all_regSeasTots_rbindDF %>%
  group_by(player_id,season_id) %>%
  filter(n() > 1)

player_profiles_all_regSeasTots_rbindDF_singSeas <- player_profiles_all_regSeasTots_rbindDF %>%
  filter(
    !(row_ID %in% dups$row_ID) | team_abbreviation == "TOT"
  )

player_profiles_all_regSeasTots_rbindDF_singSeas$year <- as.numeric(
  substr(
    player_profiles_all_regSeasTots_rbindDF_singSeas$season_id,1,4
  )
)

player_profiles_all_regSeasTots_rbindDF_singSeas$TSA <- 
  as.numeric(player_profiles_all_regSeasTots_rbindDF_singSeas$fga) +
  0.44*as.numeric(player_profiles_all_regSeasTots_rbindDF_singSeas$fta)

player_profiles_all_regSeasTots_rbindDF_singSeas$TS_pct <-
  as.numeric(player_profiles_all_regSeasTots_rbindDF_singSeas$pts) /
  (2 * player_profiles_all_regSeasTots_rbindDF_singSeas$TSA)

player_profiles_all_regSeasTots_rbindDF_singSeas$ppg <-
  as.numeric(player_profiles_all_regSeasTots_rbindDF_singSeas$pts) /
  as.numeric(player_profiles_all_regSeasTots_rbindDF_singSeas$gp)

save(player_profiles_all_regSeasTots_rbindDF_singSeas, file = "player_profiles_all_regSeasTots_rbindDF_singSeas.RData")
# load("player_profiles_all_regSeasTots_list.RData")



player_profiles_all_regSeasTots_rbindDF_singSeas <- player_profiles_all_regSeasTots_rbindDF_singSeas %>%
  filter(year >= 1996 & as.numeric(gp) >= 58)

player_profiles_all_regSeasTots_rbindDF_singSeas <- 
  player_profiles_all_regSeasTots_rbindDF_singSeas %>%
  mutate(ppg_rank = min_rank(-ppg))


player_profiles_all_regSeasTots_rbindDF_singSeas <- players_fullRoster_df %>%
  select(
    player_id = person_id,
    display_first_last, lastName
  ) %>%
  right_join(player_profiles_all_regSeasTots_rbindDF_singSeas)


player_profiles_all_regSeasTots_rbindDF_singSeas$labs <- paste(
  player_profiles_all_regSeasTots_rbindDF_singSeas$lastName,
  " '",
  substr(player_profiles_all_regSeasTots_rbindDF_singSeas$season_id,3,4),
  sep=''
)


top100scorers_since96 <- player_profiles_all_regSeasTots_rbindDF_singSeas %>%
  filter(ppg_rank <= 100)

save(player_profiles_all_regSeasTots_rbindDF_singSeas, file = "allScorers_since96.RData")
save(top100scorers_since96, file = "top100scorers_since96.RData")
# load("allScorers_since96.RData")
# load("top100scorers_since96.RData")


shotCharts_full_top100_list <- list()
shotCharts_oneLine_top100_list <- list()
shotDashboard_top100_list <- list()

for(i in 1:100){
  request_shotChart = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = top100scorers_since96$player_id[i],
      Season = top100scorers_since96$season_id[i],
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
  
  stop_for_status(request_shotChart)
  shots_data <- content(request_shotChart)
  
  raw_shots_data <- shots_data$resultSets[[1]]$rowSet
  col_names <- tolower(as.character(shots_data$resultSets[[1]]$headers))
  
  if (length(raw_shots_data) == 0) {
    shots <- data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots <- data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }
  
  shots <- tbl_df(shots)
  names(shots) <- col_names
  
  shots <- mutate(shots,
                  # loc_x = as.numeric(as.character(loc_x)) / 10,
                  # loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
                  shot_distance = as.numeric(as.character(shot_distance)),
                  shot_made_numeric = as.numeric(as.character(shot_made_flag)),
                  shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
                  shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
                  shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2),
                  game_date = as.Date(game_date, format = "%Y%m%d"),
                  season_id = top100scorers_since96$season_id[i]
  )

  shotCharts_full_top100_list[[i]] <- shots
  
  ###########################################
  
  shotZone_pcts <- shots %>%
    summarise(
      Backcourt = mean(shot_zone_basic=='Backcourt'),
      AboveTheBreak3 = mean(shot_zone_basic=='Above the Break 3'),
      Corner3 = mean(shot_zone_basic=='Left Corner 3') + mean(shot_zone_basic=='Right Corner 3'),
      MidRange = mean(shot_zone_basic=='Mid-Range'),
      Paint_NonRA = mean(shot_zone_basic=='In The Paint (Non-RA)'),
      RestrictedArea = mean(shot_zone_basic=='Restricted Area')
    ) %>%
    mutate(
      player_id = top100scorers_since96$player_id[i],
      season_id = top100scorers_since96$season_id[i],
      labs = top100scorers_since96$labs[i]
    )
  
  shotCharts_oneLine_top100_list[[i]] <- shotZone_pcts
  
  print(paste('Finished record', i))
}

save(shotCharts_full_top100_list, file = "shotCharts_full_top100_list.RData")
save(shotCharts_oneLine_top100_list, file = "shotCharts_oneLine_top100_list.RData")
# load("shotCharts_full_top100_list.RData")
# load("shotCharts_oneLine_top100_list.RData")



for(i in 1:100){
  
  request_playerdashboardbyshootingsplits = GET(
    "http://stats.nba.com/stats/playerdashboardbyshootingsplits",
    query = list(
      PlayerID = top100scorers_since96$player_id[i],
      Season = top100scorers_since96$season_id[i],
      SeasonType = "Regular Season",
      PlayerPosition = "",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      MeasureType = "Base",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PaceAdjust = "N",
      Period = 0,
      PerMode = "Totals",
      PlusMinus = "N",
      Position = "",
      Rank = "N",
      RookieYear = "",
      SeasonSegment = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    ),
    add_headers(request_headers)
  )
  
  players_data_playerdashboardbyshootingsplits <- fromJSON(content(request_playerdashboardbyshootingsplits, as = "text"))
  players_playerdashboardbyshootingsplits <- tbl_df(data.frame(players_data_playerdashboardbyshootingsplits$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
  names(players_playerdashboardbyshootingsplits) <- tolower(players_data_playerdashboardbyshootingsplits$resultSets$headers[[1]])
  
  players_playerdashboardbyshootingsplits <- players_playerdashboardbyshootingsplits %>%
    select(fgm:pct_uast_fgm) %>%
    mutate(
      player_id = top100scorers_since96$player_id[i],
      season_id = top100scorers_since96$season_id[i],
      labs = top100scorers_since96$labs[i]
      
    )
  
  shotDashboard_top100_list[[i]] <- players_playerdashboardbyshootingsplits
  
  print(paste('Finished record', i))
}


save(shotDashboard_top100_list, file = "shotDashboard_top100_list.RData")
# load("shotDashboard_top100_list.RData")

#####################################################

shotCharts_full_top100_zoneSums_df <- shotCharts_full_top100_list %>%
  bind_rows() %>%
  group_by(player_id,season_id) %>%
  summarise(
    Backcourt_att = sum(shot_zone_basic=='Backcourt'),
    AboveTheBreak3_att = sum(shot_zone_basic=='Above the Break 3'),
    Corner3_att = sum(shot_zone_basic=='Left Corner 3') + sum(shot_zone_basic=='Right Corner 3'),
    MidRange_Long_att = sum(shot_zone_basic=='Mid-Range' & shot_distance >= 17),
    MidRange_Short_att = sum(shot_zone_basic=='Mid-Range' & shot_distance < 17),
    Paint_NonRA_att = sum(shot_zone_basic=='In The Paint (Non-RA)'),
    RestrictedArea_att = sum(shot_zone_basic=='Restricted Area'),
    
    Backcourt_makes = sum(as.numeric(shot_zone_basic=='Backcourt') * shot_made_numeric),
    AboveTheBreak3_makes = sum(as.numeric(shot_zone_basic=='Above the Break 3') * shot_made_numeric),
    Corner3_makes = sum(as.numeric(shot_zone_basic=='Left Corner 3') * shot_made_numeric) + sum(as.numeric(shot_zone_basic=='Right Corner 3') * shot_made_numeric),
    MidRange_Long_makes = sum(as.numeric(shot_zone_basic=='Mid-Range' & shot_distance >= 17) * shot_made_numeric),
    MidRange_Short_makes = sum(as.numeric(shot_zone_basic=='Mid-Range' & shot_distance < 17) * shot_made_numeric),
    Paint_NonRA_makes = sum(as.numeric(shot_zone_basic=='In The Paint (Non-RA)') * shot_made_numeric),
    RestrictedArea_makes = sum(as.numeric(shot_zone_basic=='Restricted Area') * shot_made_numeric),
    
    Backcourt_pts = sum(as.numeric(shot_zone_basic=='Backcourt') * shot_made_numeric * shot_value),
    AboveTheBreak3_pts = sum(as.numeric(shot_zone_basic=='Above the Break 3') * shot_made_numeric * shot_value),
    Corner3_pts = sum(as.numeric(shot_zone_basic=='Left Corner 3') * shot_made_numeric * shot_value) + sum(as.numeric(shot_zone_basic=='Right Corner 3') * shot_made_numeric * shot_value),
    MidRange_Long_pts = sum(as.numeric(shot_zone_basic=='Mid-Range' & shot_distance >= 17) * shot_made_numeric * shot_value),
    MidRange_Short_pts = sum(as.numeric(shot_zone_basic=='Mid-Range' & shot_distance < 17) * shot_made_numeric * shot_value),
    Paint_NonRA_pts = sum(as.numeric(shot_zone_basic=='In The Paint (Non-RA)') * shot_made_numeric * shot_value),
    RestrictedArea_pts = sum(as.numeric(shot_zone_basic=='Restricted Area') * shot_made_numeric * shot_value)
  )

save(shotCharts_full_top100_zoneSums_df, file = "shotCharts_full_top100_zoneSums_df.RData")




