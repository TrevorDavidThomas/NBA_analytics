library(httr)
library(jsonlite)
library(ballr)
library(dplyr)
library(stringr)
library(here)

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
#    combine years
years <- 2001:2019

# specify vector of year strings (formatted for stats.nba API call)
years_str <- paste(
  (years - 1), "-",
  str_pad(
    as.character(years-2000), width=2, pad = '0'
  ),
  sep = ''
)


# write function to call stats.nba API, get combine data for specified year,
#   process into dataframe
getCombineStats <- function(year){
  
  players_combineStats_JSON <- GET(
    "http://stats.nba.com/stats/draftcombinestats",
    query = list(
      SeasonYear = year,
      LeagueID = '00'),
    add_headers(request_headers)
  )
  
  players_combineStats_list <- fromJSON(content(players_combineStats_JSON, as = "text"))
  
  players_combineStats_df <- tbl_df(data.frame(players_combineStats_list$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
  names(players_combineStats_df) = tolower(players_combineStats_list$resultSets$headers[[1]])

  return(players_combineStats_df)
}

# get list of dataframes by mapping combine API function to vector of target years
#   (shouldn't take long; as of 11/20/18, the 2016 combine measures are missing)
combineStats_allYrList <- years_str %>%
  purrr::map(getCombineStats)

# save list of combine year dataframes to relative path
save(combineStats_allYrList,
     file = here('data','combineStats_allYrList.RData'))


# get list of (bbref) advanced stats dataframes by mapping scraping function from
#   ballr package to vector of years.
#   note that inputing '2001' to function will return df for 
#   all players accruing stats in 2000-01 season.
advanceStats_allYrList <- years %>%
  purrr::map(NBAPerGameAdvStatistics)

# save list of advanced stats dataframes to relative path
save(advanceStats_allYrList,
     file = here('data', 'advanceStats_2000toPresent_allYrList.RData'))



#################################################
# merge the combine data into single df, taking the most recent results for any player tested twice

#bind_rows
combine_df <- combineStats_allYrList %>% bind_rows()

# find 2nd measures for players who got measured twice
combine_dups <- combine_df[duplicated(combine_df$player_name),]

# remove duplicates from original dataframe & add 2nd measures back in
combine_noDups_df <- combine_df %>%
  filter( !(player_name %in% combine_dups$player_name) )

combine_fin_df <- bind_rows(
  combine_noDups_df, combine_dups
)


# select relevant columns for joining to adv stats
combine_fin_df <- combine_fin_df %>%
  select(
    combine_year = season,
    player = player_name,
    
    position,
    height_wo_shoes,
    weight,
    wingspan,
    standing_reach,
    body_fat_pct:bench_press
  ) %>%
  mutate( # add flag to later tell easily if a player had any combine data joined
    player = str_replace_all(player, '[\\.\\*\\,]', ''),
    combine_flag = 1
  ) %>%
  mutate_at(vars(height_wo_shoes:bench_press), as.numeric)


###############
# Clean data

# reassign 0-valued lane agility time to NA
combine_fin_df$modified_lane_agility_time[
  combine_fin_df$modified_lane_agility_time == 0
  ] <- NA

# remove Svi Mykhailiuk's 2017 combine records (re-tested in 2018 under full name)
combine_fin_df <- combine_fin_df %>% filter(player != 'Svi Mykhailiuk')

# save finalized combine dataframe
save(combine_fin_df,
     file = here("data","combine_fin_df.RData"))

#################################################
# add year to adv stats dfs, bind them, select relevant vars

add_year <- function(df,years_str){
  df$season <- years_str
  return(df)
}

advanceStats_allYrList <- purrr::map2(
  advanceStats_allYrList,years_str,
  add_year
)

advanceStats_df <-advanceStats_allYrList %>%
  bind_rows() %>%
  select(
    player, season,
    pos, age,
    team = tm,
    g,mp,per,orbpercent,drbpercent,trbpercent,
    stlpercent,blkpercent,dws,dbpm,
    link
  ) %>%
  mutate(
    player = str_replace_all(player, '[\\.\\*\\,]', ''),
    playerYr = paste(player,season)
  )

# identify player-seasons in which player played for multiple teams;
#   keep the TOT year stats and remove the team-specific stats
advanceStats_combSeas <- advanceStats_df %>%
  filter(team == 'TOT')
advanceStats_fin_df <- advanceStats_df %>%
  filter(!(playerYr %in% advanceStats_combSeas$playerYr) |
           team == 'TOT')

# save final advanced stats dataframe
save(advanceStats_fin_df,
     file = here('data', 'advanceStats_fin_df_2000_01toPresent.RData'))

#################################################
# join combine data to adv stats data

join_df <- advanceStats_fin_df %>%
  left_join(combine_fin_df)


# check for erroneous joins -- combine year later than season year
join_df$season_num <- substr(join_df$season, start = 1, stop = 4) %>%
  as.numeric()

# ID records as 'bad' if combine year comes after season
join_df$temporal_flag <- NA_character_
join_df$temporal_flag[join_df$season_num < join_df$combine_year] <- 'Bad'
join_df$temporal_flag[join_df$season_num >= join_df$combine_year] <- 'Good'

# remove all records for players who've been flagged as chronologically sketchy
#   (this screens for guys who share names and are attributed the wrong set of combine measures)
join_df_bad <- join_df %>% filter(temporal_flag == 'Bad')
join_df$temporal_flag[
  join_df$link %in% join_df_bad$link
  ] <- 'Bad'


combine_advDef_final_df <- join_df %>%
  filter(temporal_flag == 'Good')


save(combine_advDef_final_df,
     file = here('data', 'combine_advDef_final_df.RData'))


