library(ggplot2)
library(dplyr)
library(GGally)
library(rlang)
library(ggrepel)
library(viridis)
library(ggpmisc)
library(cowplot)

# load combine & defensive advanced stats data files
#   generated in dataAssembly_draftCombine_defAdvStats.R
load(here("data","combine_fin_df.RData"))
load(here("data","combine_advDef_final_df.RData"))

# Create function to write correlations w/o extra label,
#   and with size scaling
#   see https://github.com/ggobi/ggally/issues/139
my_custom_cor <- function(data, mapping, color = I("grey50"), sizeRange = c(1, 5), ...) {
  
  # get the x and y data to use the other code
  x <- eval_tidy(mapping$x, data)
  y <- eval_tidy(mapping$y, data)
  
  ct <- cor.test(x,y)

  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  
  # since we can't print it to get the strsize, just use the max size range
  cex <- max(sizeRange)
  
  # helper function to calculate a useable size
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)
  }
  
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...
  )
}

# plot custom correlation matrix for full set of combine measures
combine_fin_df %>%
  select( # select and rename variables to include
    Height = height_wo_shoes,
    `Wing-\nspan` = wingspan,
    Reach = standing_reach,
    `Hand\nLength` = hand_length,
    `Hand\nWidth` = hand_width,
    Weight = weight,
    `Body\nFat %` = body_fat_pct,
    `Vert,\nStand` = standing_vertical_leap,
    `Vert,\nMax` = max_vertical_leap,
    `Lane\nAgility` = lane_agility_time,
    `Lang\nAgility, 2` = modified_lane_agility_time,
    `3/4 Sprint` = three_quarter_sprint,
    `Bench\nReps` = bench_press
    ) %>%
  ggpairs(
    upper = list(continuous = wrap("smooth_loess", alpha = 0.01, color = 'blue')),
    lower = list(continuous = wrap(my_custom_cor, sizeRange = c(1,3))), #use custom correlation function for lower triangle
    diag = list(continuous = 'density'),
    axisLabels = 'none',
    showStrips = TRUE
  ) +
  theme_void() +
  theme(
    strip.text = element_text(face = 'bold')
  )


###########################################
# Close analysis of height - wingspan

# identify players with especially long & short wingspans to highlight.
combine_fin_df_wingspanCallOuts <- combine_fin_df %>%
  filter(player %in% c(
    "Donovan Mitchell",
    "Doug Wrenn",
    "Hassan Whiteside",
    "Jason Maxiell",
    "Kawhi Leonard",
    "Mohamed Bamba",
    "Will Solomon",
    
    "Jimmy Butler",
    "JJ Redick",
    "Kelly Olynyk",
    "Martynas Andriuskevicius",
    "Sviatoslav Mykhailiuk",
    "TJ Ford"
  ))

# specify linear models predicting wingspan as function of height
#   with first, second, and third order polynomials;
#   use to assess linearity of fit
lm_ht_wng_O1 <- lm(wingspan ~ poly(height_wo_shoes, 1, raw = TRUE),
            na.action = "na.exclude",
            data = combine_fin_df)
lm_ht_wng_O2 <- lm(wingspan ~ poly(height_wo_shoes, 2, raw = TRUE),
            na.action = na.exclude,
            data = combine_fin_df)
lm_ht_wng_O3 <- lm(wingspan ~ poly(height_wo_shoes, 3, raw = TRUE),
            na.action = na.exclude,
            data = combine_fin_df)

# fit is linear; use 1st order polynomial curve to display on scatter
formula_ht_wng <- y ~ poly(x, 1, raw = TRUE)

# generate scatterplot with labelled/highlighted outliers
ggplot(
  data = combine_fin_df,
  aes(x = height_wo_shoes, y = wingspan)
) +
  geom_abline(slope = 1, intercept = 0, color = 'black', linetype = 2) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm',
              formula = formula_ht_wng, color = 'black') +
  ggpmisc::stat_poly_eq( aes( label = paste(stat(eq.label), stat(rr.label), sep = "~~~~~~~~") ), 
               formula = formula_ht_wng, parse = TRUE,
               eq.x.rhs = '` `~italic(Height)',
               eq.with.lhs = "italic(Wingspan)~`=`~",
               rr.digits = 3) +
  geom_point(
    data = combine_fin_df_wingspanCallOuts,
    color = 'red'
    ) +
  geom_text_repel(
    data = combine_fin_df_wingspanCallOuts,
    aes(x = height_wo_shoes, y = wingspan, label = player),
    nudge_y = ifelse(combine_fin_df_wingspanCallOuts$lm_ht_wng_O1_resid > 0, 3, -3)
  ) +
  scale_y_continuous(
    limits = c(5.5*12, 8.5*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12, 8*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"', '8\'0"')
  ) +
  scale_x_continuous(
    limits = c(5.5*12, 7.5*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = "Height (no shoes)",
    y = "Wingspan"
  ) +
  theme_minimal()



##########################################
# Close analysis of height - standing reach

# repeat above procedure for height & standing reach
lm_ht_stndRch_O1 <- lm(standing_reach ~ poly(height_wo_shoes, 1, raw = TRUE),
                   na.action = "na.exclude",
                   data = combine_fin_df)

lm_ht_wng_stndRch_O1 <- lm(standing_reach ~ height_wo_shoes + wingspan,
                      na.action = "na.exclude",
                      data = combine_fin_df)

lm_ht_wng_stndRch_O1_noWrenn <- lm(standing_reach ~ height_wo_shoes + wingspan,
                           na.action = "na.exclude",
                           data = combine_fin_df %>% filter(
                             player != "Doug Wrenn"
                           ))

formula_ht_stndRch <- y ~ poly(x, 1, raw = TRUE)


combine_fin_df$lm_ht_wng_stndRch_O1_resid <- NA
combine_fin_df$lm_ht_wng_stndRch_O1_resid[
  !is.na(combine_fin_df$height_wo_shoes) & !is.na(combine_fin_df$wingspan) & !is.na(combine_fin_df$standing_reach)
  ] <- lm_ht_wng_stndRch_O1$residuals




ggplot(
  data = combine_fin_df,
  aes(x = height_wo_shoes, y = standing_reach)
) +
  # geom_abline(slope = 1, intercept = 0, color = 'blue', linetype = 2) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'lm',
              formula = formula_ht_wng) +
  ggpmisc::stat_poly_eq( aes( label = paste(stat(eq.label), stat(rr.label), sep = "~~~~~~~~") ),
                         formula = formula_ht_stndRch, parse = TRUE,
                         eq.x.rhs = '` `~italic(Height)',
                         eq.with.lhs = "italic(Standing_Reach)~`=`~",
                         rr.digits = 3) +
  geom_point(
    data = combine_fin_df_wingspanCallOuts,
    color = 'red'
  ) +
  geom_text_repel(
    data = combine_fin_df_wingspanCallOuts,
    aes(x = height_wo_shoes, y = standing_reach, label = player),
    nudge_y = ifelse(combine_fin_df_wingspanCallOuts$lm_ht_wng_O1_resid > 0, 3, -3)
  ) +
  scale_y_continuous(
    limits = c(7.25*12, 10*12),
    breaks = seq(7.5*12, 10*12, by = 6),
    labels = c('7\'6"', '8\'0"', '8\'6"', '9\'0"', '9\'6"', '10\'0"')
  ) +
  scale_x_continuous(
    limits = c(5.5*12, 7.5*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = "Height (no shoes)",
    y = "Standing Reach"
  ) +
  theme_minimal()



##########################################
##########################################
# Close analysis of height, wingspan, standing reach w/ defensive stats

# identify player position groups
combine_advDef_final_df$position_gen <- NA_character_

combine_advDef_final_df$position_gen[
  combine_advDef_final_df$position %in% c('PG','PG-SG', 'SG-PG')
] <- 'Lead Guard'
combine_advDef_final_df$position_gen[
  combine_advDef_final_df$position %in% c('SG','SG-SF', 'SF-SG', 'SF', 'SF-PF')
  ] <- 'Wing'
combine_advDef_final_df$position_gen[
  combine_advDef_final_df$position %in% c('PF-SF','PF', 'PF-C', 'C-PF', 'C')
  ] <- 'Big'
combine_advDef_final_df$position_gen <- factor(combine_advDef_final_df$position_gen,
                                               levels = c(
                                                 'Lead Guard','Wing','Big'
                                               ))

# specify general formulas for 1st/2nd/3rd order polynomial relations
formula_O1 <- y ~ poly(x, 1, raw = TRUE)
formula_O2 <- y ~ poly(x, 2, raw = TRUE)
formula_O3 <- y ~ poly(x, 3, raw = TRUE)


# for each of defensive rebounding, off rebounding, blocked shots,
#   and steals, generate graphs that show the relationships between both
#   height and wingspan with the rebounding/defensive stat, overall and
#   broken out by position group. Label with R^2 value for 3rd order polynomial fit
#   ***make more parsimonious with graphing function***
########################################
########Def Rebounding Graphs###########
########################################

# generate curve splits for each of 3 position groups
p_ht_drbRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = height_wo_shoes, y = drbpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE, # fit curve
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen), # label curve with R^2
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Defensive Rebound Rate',
    title = 'By Position'
  ) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5, face = 'italic', size = rel(1.1))
  )

# same as above but for all observations regardless of position
p_ht_drbRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = height_wo_shoes, y = drbpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Defensive Rebound Rate',
    title = 'Total'
  ) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5, face = 'italic', size = rel(1.1))
    )

# use cowplot::plot_grid() to place the the full & position-breakdown graphs side-by-side
g_ht_drbRt <- plot_grid(
  p_ht_drbRt_allPos, p_ht_drbRt_posBrk,
  align = 'h', axis = 'bt'
)

# create title
title_ht_drbRt <- ggdraw() + 
  draw_label("Defensive Rebound Rate vs. Height",
             fontface = 'bold')

# add title
plot_grid(
  title_ht_drbRt, g_ht_drbRt, ncol = 1, rel_heights = c(0.1, 1)
)


########################################
########################################

# repreat the above process by for wingspan instead of height
p_wing_drbRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = wingspan, y = drbpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Defensive Rebound Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_wing_drbRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = wingspan, y = drbpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Defensive Rebound Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_wing_drbRt <- plot_grid(
  p_wing_drbRt_allPos, p_wing_drbRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########Off Rebounding Graphs###########
########################################

p_ht_drbRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = height_wo_shoes, y = drbpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Defensive Rebound Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_ht_drbRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = height_wo_shoes, y = drbpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Defensive Rebound Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_ht_drbRt <- plot_grid(
  p_ht_drbRt_allPos, p_ht_drbRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########################################

p_wing_drbRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = wingspan, y = drbpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Defensive Rebound Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_wing_drbRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = wingspan, y = drbpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,40)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Defensive Rebound Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_wing_drbRt <- plot_grid(
  p_wing_drbRt_allPos, p_wing_drbRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########Block Graphs####################
########################################

p_ht_blkRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = height_wo_shoes, y = blkpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,10)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Block Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_ht_blkRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = height_wo_shoes, y = blkpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,10)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Block Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_ht_blkRt <- plot_grid(
  p_ht_blkRt_allPos, p_ht_blkRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########################################

p_wing_blkRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = wingspan, y = blkpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,10)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Block Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_wing_blkRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = wingspan, y = blkpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,10)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Block Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_wing_blkRt <- plot_grid(
  p_wing_blkRt_allPos, p_wing_blkRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########Steal Graphs####################
########################################

p_ht_stlRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = height_wo_shoes, y = stlpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,5)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Steal Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_ht_stlRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = height_wo_shoes, y = stlpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,5)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.1*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"')
  ) +
  labs(
    x = 'Height (no shoes)',
    y = 'Steal Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_ht_stlRt <- plot_grid(
  p_ht_stlRt_allPos, p_ht_stlRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################
########################################

p_wing_stlRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = wingspan, y = stlpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3, parse = TRUE,
                rr.digits = 3) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,5)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Steal Rate',
    title = 'Broken by Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_wing_stlRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = wingspan, y = stlpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,5)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Steal Rate',
    title = 'Total'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

g_wing_stlRt <- plot_grid(
  p_wing_stlRt_allPos, p_wing_stlRt_posBrk,
  align = 'h', axis = 'bt'
)


########################################

p_wing_orbRt_posBrk <- combine_advDef_final_df %>%
  filter(mp >= 1000 & !is.na(position_gen)) %>%
  ggplot(aes(x = wingspan, y = orbpercent, group = position_gen)) +
  geom_point(alpha = 0.1, aes(color = position_gen)) +
  geom_smooth(method = 'lm', formula = formula_O3, se = FALSE,
              aes(color = position_gen)) +
  stat_poly_eq( aes(color = position_gen),
                formula = formula_O3,
                rr.digits = 3, parse = TRUE) +
  scale_color_viridis(discrete=TRUE,
                      name = '') +
  coord_cartesian(ylim = c(0,20)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Offensive Rebound Rate',
    title = 'By Position'
  ) +
  theme_minimal() + theme(legend.position = 'bottom')

p_wing_orbRt_allPos <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  ggplot(aes(x = wingspan, y = orbpercent)) +
  geom_point(alpha = 0.1, color = 'blue') +
  geom_smooth(method = 'lm', formula = formula_O3,
              se = TRUE, color = 'black') +
  stat_poly_eq(formula = formula_O3, parse = TRUE,
               rr.digits = 3) +
  coord_cartesian(ylim = c(0,20)) +
  scale_x_continuous(
    limits = c(5.5*12, 7.75*12),
    breaks = c(5.5*12, 6*12, 6.5*12, 7*12, 7.5*12),
    labels = c('5\'6"', '6\'0"', '6\'6"', '7\'0"', '7\'6"')
  ) +
  labs(
    x = 'Wingspan',
    y = 'Offensive Rebound Rate',
    title = 'Total'
  ) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    title.text
    )


g_wing_orbRt <- plot_grid(
  p_wing_orbRt_allPos, p_wing_orbRt_posBrk,
  align = 'h', axis = 'bt'
)

title_wing_orgRt <- ggdraw() + 
  draw_label("Offensive Rebound Rate vs. Wingspan",
             fontface = 'bold')

g_wing_orbRt_title <- plot_grid(
  title_wing_orgRt, g_wing_orbRt, ncol = 1, rel_heights = c(0.1, 1)
)


########################################
########################################















