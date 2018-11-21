library(rvest)
library(tidyr)
library(dplyr)
library(tibble)
library(here)
library(ggmap)
library(stringr)

# set URLs to bbref home page, as well as player birthplace directory
url_base <- "https://www.basketball-reference.com"
url_main <- "https://www.basketball-reference.com/friv/birthplaces.fcgi"

page_main <- read_html(url_main)


# get vector of US state-page URLs
url_statesVector <- page_main %>%
  html_nodes('#birthplace_1 a') %>%
  html_attr('href')

# get vector of non-US country-page URLs
url_countriesVector <- page_main %>%
  html_nodes('#birthplace_2 a') %>%
  html_attr('href')

# Get character vectors of names for US states and non-US countries
names_statesVector <- page_main %>%
  html_nodes('#birthplace_1 p') %>%
  html_text()
names_statesVector <- gsub("\\s*\\([^\\)]+\\)\\s*$", "", names_statesVector)

names_contriesVector <- page_main %>%
  html_nodes('#birthplace_2 p') %>%
  html_text()
names_contriesVector <- gsub("\\s*\\([^\\)]+\\)\\s*$", "", names_contriesVector)


# initialize lists to store state- and country-specific player dataframes
player_full_domestic_list <- list()
player_full_foreign_list <- list()

# loop through state pages, scraping all relevant player information in each
for(i in 1:length(names_statesVector)){
  page_state <- read_html(paste(url_base,url_statesVector[[i]],sep=''))
  
  # scrape and rename player name, player birth date, and player birth city fields
  player_names_births <- page_state %>%
    html_nodes('#stats .left') %>%
    html_text()
  
  player_names_births <- cbind.data.frame(
    split(
      player_names_births,
      rep(1:3,times=length(player_names_births)/3)
    ),
    stringsAsFactors = FALSE
  ) %>% rename(
    Name = `1`, Birth_Date = `2`, Birth_City = `3`
  )
  
  player_names_births$Birth_Year <- player_names_births$Birth_Date %>%
    str_sub(-4,-1)
  player_names_births$Birth_State <- names_statesVector[[i]]
  player_names_births$Nationality <- 'Domestic'
  player_names_births$Name <- player_names_births$Name %>%
    str_remove("[*]") # remove extraneous characters from player names
  
  player_names_births$Profile_URL <- page_state %>%
    html_nodes('#stats a') %>%
    html_attr('href') # save player profile URLs for potential further deep-dives
  
  
  # scrape, process, and rename relevant, basic player stats
  player_stats <- page_state %>%
    html_nodes('#stats .right') %>%
    html_text() 
  
  player_stats <- cbind.data.frame(
    split(player_stats, rep(1:27, times=length(player_stats)/27)),
    stringsAsFactors = FALSE
  ) %>%
    select(
      Years_Played = `2`,
      Year_Start = `3`,
      Year_End = `4`,
      Games_Played = `5`,
      Minutes_Played = `6`,
      Points = `20`
    )
  
  # column-bind player name/birth info and player stats
  player_full <- cbind.data.frame(
    player_names_births,
    player_stats
  )
  
  # store state-specific dataframe to list
  player_full_domestic_list[[i]] <- player_full
}


# repeat the above code for states, but for countries
for(i in 1:length(names_contriesVector)){
  page_state <- read_html(paste(url_base,url_countriesVector[[i]],sep=''))
  
  player_names_births <- page_state %>%
    html_nodes('#stats .left') %>%
    html_text()
  
  player_names_births <- cbind.data.frame(
    split(
      player_names_births,
      rep(1:3,times=length(player_names_births)/3)
    ),
    stringsAsFactors = FALSE
  ) %>% rename(
    Name = `1`, Birth_Date = `2`, Birth_City = `3`
  )
  
  player_names_births$Birth_Year <- player_names_births$Birth_Date %>%
    str_sub(-4,-1)
  player_names_births$Birth_State <- names_contriesVector[[i]]
  player_names_births$Nationality <- 'Foreign'
  player_names_births$Name <- player_names_births$Name %>%
    str_remove("[*]")
  
  player_names_births$Profile_URL <- page_state %>%
    html_nodes('#stats a') %>%
    html_attr('href')
  
  
  player_stats <- page_state %>%
    html_nodes('#stats .right') %>%
    html_text()
  
  player_stats <- cbind.data.frame(
    split(player_stats, rep(1:27, times=length(player_stats)/27)),
    stringsAsFactors = FALSE
  ) %>%
    select(
      Years_Played = `2`,
      Year_Start = `3`,
      Year_End = `4`,
      Games_Played = `5`,
      Minutes_Played = `6`,
      Points = `20`
    )
  
  player_full <- cbind.data.frame(
    player_names_births,
    player_stats
  )
  
  
  player_full_foreign_list[[i]] <- player_full
}

# save lists of domestic and foreign player origin dfs
save(player_full_domestic_list,
     file = here("data", "player_full_domestic_list.RData"))

save(player_full_foreign_list,
     file = here("data", "player_full_foreign_list.RData"))


# initialize lists to store geocoded place results
#   as well as "second-pass" results for places that
#   weren't successfully placed on first geocoding attempt
domestic_locations_list <- list()
domestic_locations_2ndPass_list <- list()
foreign_locations_list <- list()
foreign_locations_2ndPass_list <- list()


# manually enter a bunch of state abbreviations for distinct states,
#   in order to facilitate geocoding process
state_abrevKey <- player_full_domestic_list %>%
  bind_rows() %>%
  select(Birth_State) %>%
  distinct()
state_abrevKey$Birth_State_Abrev <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
  "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
  "RI","SC","SD","TN","TX","UT","VA","WA","WV","WI","WY"
)

# loop through each state-specific data frame in domestic list
#   identify distinct birth places, and geocode them using the
#   DataScienceToolkit ('dsk') option in ggmap package
for(i in 1:length(player_full_domestic_list)){
  df <- player_full_domestic_list[[i]] %>%
    left_join(state_abrevKey) %>%
    mutate(
      Birth_Place = paste(Birth_City, ', ', Birth_State_Abrev, sep = '')
    )

  df_geocoded <- df  %>%
    select(Birth_Place) %>%
    distinct() %>% # only geocode distinct places (no need to duplicate geocoding processes)
    mutate_geocode(Birth_Place, source = "dsk") # use mutate_geocode() to add coordinates directly to existing df
  
  domestic_locations <- df %>%
    left_join(df_geocoded) # join geocodes of distinct places back to full player/place df
  
  domestic_locations_list[[i]] <- domestic_locations # store to list
}
# save list
save(domestic_locations_list,
     file = here("data", "domestic_locations_list.RData"))

# loop through above list of saved geocodes
#   ID failed geocodes (missing lat/lon coords) and retry geocode
for(i in 1:length(player_full_domestic_list)){
  df <- domestic_locations_list[[i]] %>%
    filter(
      is.na(lon) # create dataframe of failed geocodes
    )
  
  df2 <- df  %>%
    select(Birth_Place) %>% # create 2nd dataframe of unique places among failed geocodes
    distinct()
  
  if(nrow(df) > 0){
    df_geocoded <- mutate_geocode(df2,
                                  Birth_Place, source = "dsk")
    
    domestic_locations <- df %>%
      select(-lat, -lon) %>%
      left_join(df_geocoded)
    
    domestic_locations_2ndPass_list[[i]] <- domestic_locations
  }
}

# save 2nd pass list
save(domestic_locations_2ndPass_list,
     file = here("data", "domestic_locations_2ndPass_list.RData"))



# repeat 2-stage geocoding process for foreign-born players
for(i in 1:length(player_full_foreign_list)){
  df <- player_full_foreign_list[[i]] %>%
    left_join(state_abrevKey) %>%
    mutate(
      Birth_Place = paste(Birth_City, ', ', Birth_State, sep = '')
    )
  
  df_geocoded <- df  %>%
    select(Birth_Place) %>%
    distinct() %>%
    mutate_geocode(Birth_Place, source = "dsk")
  
  foreign_locations <- df %>%
    left_join(df_geocoded)
  
  foreign_locations_list[[i]] <- foreign_locations
}

save(foreign_locations_list,
     file = here("data", "foreign_locations_list.RData"))


for(i in 1:length(player_full_foreign_list)){
  df <- foreign_locations_list[[i]] %>%
    filter(
      is.na(lon)
    )
  
  df2 <- df  %>%
    select(Birth_Place) %>%
    distinct()
  
  if(nrow(df) > 0){
    df_geocoded <- mutate_geocode(df2,
                                  Birth_Place, source = "dsk")
    
    foreign_locations <- df %>%
      select(-lat, -lon) %>%
      left_join(df_geocoded)
    
    foreign_locations_2ndPass_list[[i]] <- foreign_locations
  }
}

save(foreign_locations_2ndPass_list,
     file = here("data", "foreign_locations_2ndPass_list.RData"))


# row-bind all successful geocodes, foreign and domestic,
#   into single dataframe
player_geocode_final_df <- bind_rows(
  domestic_locations_list,
  foreign_locations_list
) %>%
  filter(!is.na(lat)) %>%
  bind_rows(
    domestic_locations_2ndPass_list,
    foreign_locations_2ndPass_list
  )


# Go through and do some manual corrections

# Error correction #1: Carrick Felix in Las Vegas, *NV*
player_geocode_final_df$Birth_State[
  player_geocode_final_df$Name == 'Carrick Felix'
  ] <- 'Nevada'
player_geocode_final_df$Birth_State_Abrev[
  player_geocode_final_df$Name == 'Carrick Felix'
  ] <- 'NV'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Carrick Felix'
  ] <- 'Las Vegas, NV'
player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Carrick Felix'
  ] <- player_geocode_final_df$lon[
    player_geocode_final_df$Birth_Place == 'Las Vegas, NV'
    ][2]
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Carrick Felix'
  ] <- player_geocode_final_df$lat[
    player_geocode_final_df$Birth_Place == 'Las Vegas, NV'
    ][2]


# Error correction #2: Kirk Haston is from Lobelville, not Loblesville, TN
player_geocode_final_df$Birth_City[
  player_geocode_final_df$Name == 'Kirk Haston'
  ] <- 'Lobelville'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Kirk Haston'
  ] <- 'Lobelville, TN'

geocode_Haston <- geocode('Lobelville, TN', source = 'dsk')

player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Kirk Haston'
  ] <- geocode_Haston$lon
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Kirk Haston'
  ] <- geocode_Haston$lat



# Error correction #3: Gaylon Nickerson is from Osceola, AR, not Osecola
player_geocode_final_df$Birth_City[
  player_geocode_final_df$Name == 'Gaylon Nickerson'
  ] <- 'Osceola'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Gaylon Nickerson'
  ] <- 'Osceola, AR'

geocode_Nickerson <- geocode('Osceola, AR', source = 'dsk')

player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Gaylon Nickerson'
  ] <- geocode_Nickerson$lon
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Gaylon Nickerson'
  ] <- geocode_Nickerson$lat



# Error correction #4: Major Jones is from McGehee, AR, not McGhee
player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Major Jones'
  ] <- player_geocode_final_df$lon[
    player_geocode_final_df$Birth_Place == 'McGehee, AR'
    ][1]
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Major Jones'
  ] <- player_geocode_final_df$lat[
    player_geocode_final_df$Birth_Place == 'McGehee, AR'
    ][1]

player_geocode_final_df$Birth_City[
  player_geocode_final_df$Name == 'Major Jones'
  ] <- 'McGehee'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Major Jones'
  ] <- 'McGehee, AR'

# Error correction #5: Lowes Moore is from Plymouth, NC, not SC
player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Lowes Moore'
  ] <- player_geocode_final_df$lon[
    player_geocode_final_df$Birth_Place == 'Plymouth, NC'
    ][1]
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Lowes Moore'
  ] <- player_geocode_final_df$lat[
    player_geocode_final_df$Birth_Place == 'Plymouth, NC'
    ][1]

player_geocode_final_df$Birth_State[
  player_geocode_final_df$Name == 'Lowes Moore'
  ] <- 'North Caroline'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Lowes Moore'
  ] <- 'Plymouth, NC'


# Error correction #6: Queens, NY is placed in western NY; reassign lat/long to Astoria's
player_geocode_final_df$lon[
  player_geocode_final_df$Birth_Place == "Queens, NY"
  ] <- player_geocode_final_df$lon[
    player_geocode_final_df$Birth_Place == 'Astoria, NY'
    ][1]
player_geocode_final_df$lat[
  player_geocode_final_df$Birth_Place == "Queens, NY"
  ] <- player_geocode_final_df$lat[
    player_geocode_final_df$Birth_Place == 'Astoria, NY'
    ][1]


# Error correction #7: Mt Vernon, NY is placed in western NY; reassign lat/long to the Bronx's
player_geocode_final_df$lon[
  player_geocode_final_df$Birth_Place == "Mount Vernon, NY"
  ] <- player_geocode_final_df$lon[
    player_geocode_final_df$Birth_Place == 'Bronx, NY'
    ][1]
player_geocode_final_df$lat[
  player_geocode_final_df$Birth_Place == "Mount Vernon, NY"
  ] <- player_geocode_final_df$lat[
    player_geocode_final_df$Birth_Place == 'Bronx, NY'
    ][1]

# Place Orbie Bowling in Sandy Hook, KY (this information is missing from his Basketbally Reference page
#   but is given on his Wiki page)
player_geocode_final_df$Birth_City[
  player_geocode_final_df$Name == 'Orbie Bowling'
  ] <- 'Sandy Hook'
player_geocode_final_df$Birth_Place[
  player_geocode_final_df$Name == 'Orbie Bowling'
  ] <- 'Sandy Hook, KY'

geocode_Haston <- geocode('Sandy Hook, KY', source = 'dsk')

player_geocode_final_df$lon[
  player_geocode_final_df$Name == 'Orbie Bowling'
  ] <- geocode_Haston$lon
player_geocode_final_df$lat[
  player_geocode_final_df$Name == 'Orbie Bowling'
  ] <- geocode_Haston$lat


# save final, cleaned dataframe containing all foreign and domestic geocodes
save(player_geocode_final_df,
     file = here("data", "player_geocode_final_df.RData"))






