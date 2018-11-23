library(tidyverse)
library(here)
library(network)
library(sna)
library(ggnetwork)
library(gridExtra)
library(cowplot)

# load coaching data, consisting of two lists: one of (mostly) head coaches,
#   one of assistant coaches, scraped from Basketball Reference;
#   only assistants that also have head coaching record are included;
#   see dataAssembly_coachingStaffs_bballRef_20180915.R
load(here('data','coachingLists_bbref.RData'))

# row-bind head coach dfs, group by coaching name (assuming no two coaches
#   share same name), and calculate career coaching stats (to be used as node attributes)
headCoach_aggStats <- headCoach_list %>%
  bind_rows() %>%
  mutate(
    year = as.numeric(
      substr(season, 1, 4)
    )
  ) %>%
  group_by(name) %>%
  summarise(
    games = sum(g_reg, na.rm = TRUE),
    wins = sum(w_reg, na.rm = TRUE),
    chips = sum(notes == 'NBA Champions'),
    first_year_as_head = min(year)
  ) %>%
  mutate(
    win_pct = wins / games
  )


# row-bind head coach dfs for one half of pair of dfs from which to construct
#    df giving full edge list (all head coach/assistant coach pairs, with number of full seasons,
#    that they were paired)
headCoach_fullSeas_joinDf <- headCoach_list %>%
  bind_rows() %>%
  filter(!is.na(g_reg)) %>% # filter out few assistant head coaches, who all appear to have NAs for game stats
  mutate(
    year = as.numeric(
      substr(season, 1, 4)
    ),
    team_seas = paste(team, season, sep = ' ')
  )

# find duplicate records for single franchise year (indicating mid-year coaching change)
headCoach_coachChgSeas <- headCoach_fullSeas_joinDf[duplicated(headCoach_fullSeas_joinDf$team_seas),]

# creat head coaching key, removing mid-year changes and keeping only name and franchise-year
headCoach_fullSeas_joinDf <- headCoach_fullSeas_joinDf %>%
  filter(!(team_seas %in% headCoach_coachChgSeas$team_seas)) %>%
  select(
    head_coach = name,
    team_seas,
    year
  )


# row-bind assistant coach dfs, create franchise-year field to join on
asstCoach_joinDf <- asstCoach_list %>%
  bind_rows() %>%
  left_join(headCoach_aggStats) %>%
  mutate(
    team_seas = paste(team, season, sep = ' ')
  ) %>%
  select(
    asst_coach = name,
    first_year_as_head,
    team_seas
  )

# get df of assistant coaches placed within the head coach master list
asstCoach_extras <- headCoach_list %>%
  bind_rows() %>%
  filter(is.na(g_reg)) %>%
  left_join(headCoach_aggStats) %>%
  mutate(
    team_seas = paste(team, season, sep = ' ')
  ) %>%
  filter(!(team_seas %in% headCoach_coachChgSeas$team_seas)) %>%
  select(
    asst_coach = name,
    first_year_as_head,
    team_seas
  )

# combine main asst coach key with add-ons from erroneously placed head coaches
asstCoach_joinDf <- asstCoach_joinDf %>%
  bind_rows(asstCoach_extras)

seasonPairs_df <- asstCoach_joinDf %>%
  left_join(headCoach_fullSeas_joinDf, by = "team_seas")

# write function that takes vertex attributes, edge list, chronological filter
#   (assistant coach can't yet have been head for link to count),
#   and duration filter (number of years needed together for link to count)
networkCreation_func <- function(headCoach_aggStats,
                                 seasonPairs_df,
                                 chronFilt = 'no',
                                 durFilt = 1){
  
  coach_edgeList_df <- seasonPairs_df %>%
    drop_na()
  
  if(chronFilt=='yes'){                          # add conditional filter including only asst-head pairs if
    coach_edgeList_df <- coach_edgeList_df %>%   #   asst coach hasn't been head yet
      filter(year < first_year_as_head)
  }
  
  coach_edgeList_df <- coach_edgeList_df %>%
    group_by(asst_coach, head_coach) %>%                 
    summarise(
      full_seasons = n()
    ) %>%
    filter(
      full_seasons >= durFilt # include pairs that have been together longer than durFilt years
    )
  
  net <- network.initialize(nrow(headCoach_aggStats))
  network.vertex.names(net) <- headCoach_aggStats$name
  
  net[as.matrix(coach_edgeList_df[,c(2,1)])] <- 1
  
  # use vertex/edge assignment syntax from 'network' package
  net %v% "games" <- headCoach_aggStats$games
  net %v% "win_pct" <- headCoach_aggStats$win_pct
  net %v% "chips" <- headCoach_aggStats$chips
  net %v% "degree" <- degree(net)
  net %v% "degree_in" <- degree(net, cmode = 'indegree')
  net %v% "degree_out" <- degree(net, cmode = 'outdegree')
  net %v% "betweenness" <- betweenness(net)
  net %v% "closeness" <- closeness(net, cmode="gil-schmidt")
  
  net %e% "full_seas_tgthr" <- coach_edgeList_df$full_seasons
  
  return(net)
}

# write function that takes a network object (output from previous function)
#   and creates a network graph, minus isolates, and with appropriate
#   vertex/edge mappings and with optional light labeling
#   Note: need to define title_text variable for this to run
networkGraph_func <- function(net,
                              remove_isos = 'yes',
                              lab_cat = 'betweenness', #choose b/n betweenness & degree
                              lab_length = 10){
  
  if(remove_isos == 'yes'){ # remove nodes from graph that don't have any ties
    net <- net %s% which(net %v% 'degree' > 0)
  }
  
  table_df <- data_frame(
    Name = net %v% 'vertex.names',
    `Total\nConnections` = net %v% 'degree',
    `Connections\nAs Asst` = net %v% 'degree_in',
    `Connections\nAs Head` = net %v% 'degree_out',
    Betweenness = net %v% 'betweenness',
    `Games\nAs Head` = net %v% 'games',
    `Win Pct\nAs Head` = net %v% 'win_pct'
  ) %>%
    mutate(Betweenness = Betweenness / max(Betweenness))
  
  # sort table of coaches based on chosen label category
  if(lab_cat == 'betweenness'){
    table_df <- table_df %>%
      arrange(-Betweenness, -`Total\nConnections`) %>%
      add_column(.before = 'Name',
                 `Graph\nLabel` = c(1: nrow(table_df)))
    
    subtitle_text <- 'Coaches Ranked by Betweenness'
  } else{
    table_df <- table_df %>%
      arrange(-`Total\nConnections`, -Betweenness) %>%
      add_column(.before = 'Name',
                 `Graph\nLabel` = c(1: nrow(table_df)))
    
    subtitle_text <- 'Coaches Ranked by Total Connections (Degree Centrality)'
  }
  
  # generate dataframe of (numeric) labels for chosen nodes
  label_df <- data_frame(
    Name = net %v% 'vertex.names'
  ) %>%
    left_join(
      table_df
    )
  label_df$`Graph\nLabel`[label_df$`Graph\nLabel` > lab_length] <- ''
  
  net %v% 'labels' <- label_df$`Graph\nLabel`
  
  # generate network plot with force-directed node placement assigned via ggnetwork() function
  p <- ggplot(
    data = ggnetwork(net, arrow.gap = 0.02,
                     layout = 'fruchtermanreingold',
                     weights = 'full_seas_tgthr'),
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_edges(
      aes(alpha = full_seas_tgthr),
      color = "grey50",
      arrow = arrow(length = unit(5, "pt"))
    ) +
    geom_nodes(aes(color = win_pct, size = games)) +
    scale_color_distiller(
      type = "div",
      palette = "PiYG",
      limits=c(0.25, 0.75),
      oob=scales::squish,
      breaks = c(0.25,0.4,0.5,0.60,0.75),
      labels = c('0.25 -','0.40','0.50','0.60','0.75 +'),
      name = "Career\nWin Pct"
    ) +
    scale_size_area(
      name = "Games\nAs Head\nCoach",
      breaks = c(100,250,500,1000,2000)
    ) +
    scale_alpha_continuous(
      range = c(0.1,0.75),
      breaks = c(1,4,8,16)
    ) +
    guides(alpha = 'none') +
    geom_nodetext(aes(label = labels), fontface = "plain", size = rel(3)) +
    labs(
      title = title_text,
      subtitle = subtitle_text
    ) +
    theme_blank()
  
  # assign theme for accompanying table
  myTheme <- gridExtra::ttheme_minimal(
    core = list(fg_params=list(cex = 0.75)),
    colhead = list(fg_params=list(cex = 0.75))
  )
  
  # generate table corresponding to labeled coach nodes
  t <- tableGrob(
    table_df[1:lab_length,] %>%
      mutate(Betweenness = format(Betweenness, digits = 3, nsmall = 3),
             `Win Pct\nAs Head` = format(`Win Pct\nAs Head`, digits = 3, nsmall = 3)
      ),
    rows = NULL, theme = myTheme
  )
  
  # draw network and table together
  ggdraw() +
    draw_plot( plot = p, x = 0, y = .33, width = 1, height = 0.67 ) +  # z
    draw_plot( plot = t, x = 0, y = 0, width = 1, height = .33 )  # x
  
}


# write function that takes a full network & a focus node and graphs
#   the immediate neighborhood
focusNodeGraph_func <- function(net,
                                node,
                                generations_up = 2,
                                generations_down = 2){
  set.seed(0) # make placement repeatable
  
  net_dists <- geodist(
    net, count.paths = FALSE
  )[[1]] # create matrix of directed network distances
  
  row.names(net_dists) <- net %v% "vertex.names"
  colnames(net_dists) <- net %v% "vertex.names"
  
  
  # generate dataframe giving names of all vertices within generations_down links downstream from target node
  neighb_name_vec_out <- net_dists %>%
    t() %>%
    as_tibble() %>%
    data.table::setnames(
      net %v% "vertex.names"
    ) %>%
    mutate(
      name = net %v% "vertex.names"
    ) %>%
    select(
      dist = !!node,
      name
    ) %>%
    filter(
      dist > 0 & dist <= generations_down
    )
  
  # generate dataframe giving names of all vertices within generations_up links upstream from target node
  neighb_name_vec_in <- net_dists %>%
    as_tibble() %>%
    data.table::setnames(
      net %v% "vertex.names"
    ) %>%
    mutate(
      name = net %v% "vertex.names"
    ) %>%
    select(
      dist = !!node,
      name
    ) %>%
    filter(
      dist > 0 & dist <= generations_up
    )
  
  # generate subgraph from full network, containing target vertex, along with all vertices within specified,
  #    directed neighborhood distances
  subgraph <- net %s% which(net %v% 'vertex.names' %in%
                              c(neighb_name_vec_out$name,
                                neighb_name_vec_in$name,
                                node)
  )

  # plot directed subgraph, with arrows.
  ggplot(
    data = ggnetwork(subgraph, arrow.gap = 0.04,
                     layout = 'fruchtermanreingold',
                     weights = 'full_seas_tgthr'),
    aes(x = x, y = y, xend = xend, yend = yend)
  ) +
    geom_edges(
      # aes(alpha = full_seas_tgthr),
      color = "blue",
      alpha = 0.5,
      arrow = arrow(length = unit(7, "pt"))
    ) +
    geom_nodes(aes(color = win_pct, size = games)) +
    geom_nodetext(aes(label = vertex.names), fontface = "plain", size = rel(3), nudge_y = .025) +
    scale_color_distiller(
      type = "div",
      palette = "PiYG",
      limits=c(0.25, 0.75),
      oob=scales::squish,
      breaks = c(0.25,0.4,0.5,0.60,0.75),
      labels = c('0.25 -','0.40','0.50','0.60','0.75 +'),
      name = "Career\nWin Pct"
    ) +
    scale_size_area(
      name = "Games\nAs Head\nCoach",
      breaks = c(100,250,500,1000,2000),
      max_size = 10,
      limits = c(0,2500)
    ) +
    scale_x_continuous(expand = c(0.15,0)) +
    labs(
      title = paste('Coaching Subgraph: ', node, sep = ''),
      subtitle = paste('Coaches within ', generations_down, ' generations, based on sequential, multiyear links', sep = ''),
      caption = 'Source: basketball-reference.com/coaches'
    ) +
    theme_blank()
  
}


###################################################

# create network where:
#   1) head -> assistant links are only defined if assistant hasn't yet been head,
#   2) assistant spent at least two years under head
net <- networkCreation_func(headCoach_aggStats,
                            seasonPairs_df,
                            chronFilt = 'yes',
                            durFilt = 2)

title_text <- 'Coaching Graph: Multiyear, Sequential Connections'

# graph above network, with labels and accompnying table giving top 10 coaches by
#    degree connectivity
networkGraph_func(net, lab_cat = 'degree')

# graph above network, with labels and accompnying table giving top 10 coaches by
#    betweenness centrality
networkGraph_func(net, lab_cat = 'betweenness')


# create subgraphs for coaches within 3 generates of some prominent coaches
focusNodeGraph_func(net, 'Gregg Popovich',
                    generations_down = 3,
                    generations_up = 3)


focusNodeGraph_func(net, 'Larry Brown',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Don Nelson',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Mike Brown',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Pat Riley',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Phil Jackson',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Rick Carlisle',
                    generations_down = 3,
                    generations_up = 3)

focusNodeGraph_func(net, 'Rick Adelman',
                    generations_down = 3,
                    generations_up = 3)
