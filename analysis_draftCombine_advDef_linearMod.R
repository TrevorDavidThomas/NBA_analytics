library(here)
library(ggplot2)
library(naniar)
library(viridis)
library(dplyr)
library(tidyr)
library(forcats)
library(rlang)
library(lme4)
library(broom.mixed)

# load combine & defensive advanced stats data files
#   generated in dataAssembly_draftCombine_defAdvStats.R;
#   each record in the loaded dataframe corresponds to a single season
#   for a player with accompanying combine stats
lad(here("data","combine_advDef_final_df.RData"))


# define position group column
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


#########################################################
# view patterns of missingness in data using naniar package
combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  select(
    height_wo_shoes:bench_press
  ) %>%
  gg_miss_upset(
    nsets = 13,
    nintersects = NA
  )


#########################################################
# edit primary dataframe to keep only include predictors of interest
#   (with few missing values) and listwise delete incomplete observations;
#   also keep only player-seasons with 1000+ minutes; finally,
#   apply scale() function to each numeric predictor, giving mean 0 and sd 1
df_dropNA_scale <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  select(
    player,
    age,
    orbpercent:dbpm,
    height_wo_shoes:bench_press,
    position_gen,
    -modified_lane_agility_time, -hand_length, -hand_width
  ) %>%
  tidyr::drop_na() %>%
  mutate_at(
    vars(age:bench_press),
    scale
  )

# examine principle components of dataframe  
df_dropNA_scale_prComp <- prcomp(df_dropNA_scale[,10:19],
                                 center = FALSE,
                                 scale. = FALSE)

#########################################################
# Over following ~400 lines of code, specify hierarchical linear models
#   predicting each of 5 defensive stats outcomes. One model consists of
#   the full set of predictors, the next 3 only include one each of ht/ws/sr,
#   the next two consist of one of standing and max vert, and the final two
#   contain one of 3/4 court sprint and lane agility.
#   Use tidy() function to extract model coefficients, and use glance()
#   to extract model fit statistics.
#   ****TO DO: write function that takes dependent variable as input
#              and outputs full set of models/model statistics, to make
#              this section vastly more parsimonious****

mod_full_hlm_orb <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_orb_htOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_orb_wngOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + wingspan +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_orb_stnReachOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + standing_reach +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_orb_stndVertOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_orb_maxVertOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_orb_laneAgilOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_orb_sprintOnly <- lmer(
  orbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + three_quarter_sprint +
    bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_orb_tidy <- mod_full_hlm_orb %>% tidy(effects = 'fixed')
mod_full_hlm_orb_htOnly_tidy <- mod_full_hlm_orb_htOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_wngOnly_tidy <- mod_full_hlm_orb_wngOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_stnReachOnly_tidy <- mod_full_hlm_orb_stnReachOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_stndVertOnly_tidy <- mod_full_hlm_orb_stndVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_maxVertOnly_tidy <- mod_full_hlm_orb_maxVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_laneAgilOnly_tidy <- mod_full_hlm_orb_laneAgilOnly %>% tidy(effects = 'fixed')
mod_full_hlm_orb_sprintOnly_tidy <- mod_full_hlm_orb_sprintOnly %>% tidy(effects = 'fixed')


mod_full_hlm_orb_glance <- mod_full_hlm_orb %>% glance()
mod_full_hlm_orb_htOnly_glance <- mod_full_hlm_orb_htOnly %>% glance()
mod_full_hlm_orb_wngOnly_glance <- mod_full_hlm_orb_wngOnly %>% glance()
mod_full_hlm_orb_stnReachOnly_glance <- mod_full_hlm_orb_stnReachOnly %>% glance()
mod_full_hlm_orb_stndVertOnly_glance <- mod_full_hlm_orb_stndVertOnly %>% glance()
mod_full_hlm_orb_maxVertOnly_glance <- mod_full_hlm_orb_maxVertOnly %>% glance()
mod_full_hlm_orb_laneAgilOnly_glance <- mod_full_hlm_orb_laneAgilOnly %>% glance()
mod_full_hlm_orb_sprintOnly_glance <- mod_full_hlm_orb_sprintOnly %>% glance()

######################################################

mod_full_hlm_drb <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_drb_htOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_drb_wngOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + wingspan +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_drb_stnReachOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + standing_reach +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_drb_stndVertOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_drb_maxVertOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_drb_laneAgilOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_drb_sprintOnly <- lmer(
  drbpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + three_quarter_sprint +
    bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_drb_tidy <- mod_full_hlm_drb %>% tidy(effects = 'fixed')
mod_full_hlm_drb_htOnly_tidy <- mod_full_hlm_drb_htOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_wngOnly_tidy <- mod_full_hlm_drb_wngOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_stnReachOnly_tidy <- mod_full_hlm_drb_stnReachOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_stndVertOnly_tidy <- mod_full_hlm_drb_stndVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_maxVertOnly_tidy <- mod_full_hlm_drb_maxVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_laneAgilOnly_tidy <- mod_full_hlm_drb_laneAgilOnly %>% tidy(effects = 'fixed')
mod_full_hlm_drb_sprintOnly_tidy <- mod_full_hlm_drb_sprintOnly %>% tidy(effects = 'fixed')


mod_full_hlm_drb_glance <- mod_full_hlm_drb %>% glance()
mod_full_hlm_drb_htOnly_glance <- mod_full_hlm_drb_htOnly %>% glance()
mod_full_hlm_drb_wngOnly_glance <- mod_full_hlm_drb_wngOnly %>% glance()
mod_full_hlm_drb_stnReachOnly_glance <- mod_full_hlm_drb_stnReachOnly %>% glance()
mod_full_hlm_drb_stndVertOnly_glance <- mod_full_hlm_drb_stndVertOnly %>% glance()
mod_full_hlm_drb_maxVertOnly_glance <- mod_full_hlm_drb_maxVertOnly %>% glance()
mod_full_hlm_drb_laneAgilOnly_glance <- mod_full_hlm_drb_laneAgilOnly %>% glance()
mod_full_hlm_drb_sprintOnly_glance <- mod_full_hlm_drb_sprintOnly %>% glance()

######################################################

mod_full_hlm_blk <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_blk_htOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_blk_wngOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + wingspan +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_blk_stnReachOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + standing_reach +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_blk_stndVertOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_blk_maxVertOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_blk_laneAgilOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_blk_sprintOnly <- lmer(
  blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + three_quarter_sprint +
    bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_blk_tidy <- mod_full_hlm_blk %>% tidy(effects = 'fixed')
mod_full_hlm_blk_htOnly_tidy <- mod_full_hlm_blk_htOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_wngOnly_tidy <- mod_full_hlm_blk_wngOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_stnReachOnly_tidy <- mod_full_hlm_blk_stnReachOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_stndVertOnly_tidy <- mod_full_hlm_blk_stndVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_maxVertOnly_tidy <- mod_full_hlm_blk_maxVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_laneAgilOnly_tidy <- mod_full_hlm_blk_laneAgilOnly %>% tidy(effects = 'fixed')
mod_full_hlm_blk_sprintOnly_tidy <- mod_full_hlm_blk_sprintOnly %>% tidy(effects = 'fixed')


mod_full_hlm_blk_glance <- mod_full_hlm_blk %>% glance()
mod_full_hlm_blk_htOnly_glance <- mod_full_hlm_blk_htOnly %>% glance()
mod_full_hlm_blk_wngOnly_glance <- mod_full_hlm_blk_wngOnly %>% glance()
mod_full_hlm_blk_stnReachOnly_glance <- mod_full_hlm_blk_stnReachOnly %>% glance()
mod_full_hlm_blk_stndVertOnly_glance <- mod_full_hlm_blk_stndVertOnly %>% glance()
mod_full_hlm_blk_maxVertOnly_glance <- mod_full_hlm_blk_maxVertOnly %>% glance()
mod_full_hlm_blk_laneAgilOnly_glance <- mod_full_hlm_blk_laneAgilOnly %>% glance()
mod_full_hlm_blk_sprintOnly_glance <- mod_full_hlm_blk_sprintOnly %>% glance()

######################################################

mod_full_hlm_stl <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_stl_htOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_stl_wngOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + wingspan +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_stl_stnReachOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + standing_reach +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_stl_stndVertOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_stl_maxVertOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_stl_laneAgilOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_stl_sprintOnly <- lmer(
  stlpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + three_quarter_sprint +
    bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_stl_tidy <- mod_full_hlm_stl %>% tidy(effects = 'fixed')
mod_full_hlm_stl_htOnly_tidy <- mod_full_hlm_stl_htOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_wngOnly_tidy <- mod_full_hlm_stl_wngOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_stnReachOnly_tidy <- mod_full_hlm_stl_stnReachOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_stndVertOnly_tidy <- mod_full_hlm_stl_stndVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_maxVertOnly_tidy <- mod_full_hlm_stl_maxVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_laneAgilOnly_tidy <- mod_full_hlm_stl_laneAgilOnly %>% tidy(effects = 'fixed')
mod_full_hlm_stl_sprintOnly_tidy <- mod_full_hlm_stl_sprintOnly %>% tidy(effects = 'fixed')


mod_full_hlm_stl_glance <- mod_full_hlm_stl %>% glance()
mod_full_hlm_stl_htOnly_glance <- mod_full_hlm_stl_htOnly %>% glance()
mod_full_hlm_stl_wngOnly_glance <- mod_full_hlm_stl_wngOnly %>% glance()
mod_full_hlm_stl_stnReachOnly_glance <- mod_full_hlm_stl_stnReachOnly %>% glance()
mod_full_hlm_stl_stndVertOnly_glance <- mod_full_hlm_stl_stndVertOnly %>% glance()
mod_full_hlm_stl_maxVertOnly_glance <- mod_full_hlm_stl_maxVertOnly %>% glance()
mod_full_hlm_stl_laneAgilOnly_glance <- mod_full_hlm_stl_laneAgilOnly %>% glance()
mod_full_hlm_stl_sprintOnly_glance <- mod_full_hlm_stl_sprintOnly %>% glance()

######################################################

mod_full_hlm_dbpm <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_dbpm_htOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_dbpm_wngOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + wingspan +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_dbpm_stnReachOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + standing_reach +
    weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_dbpm_stndVertOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_dbpm_maxVertOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    max_vertical_leap + lane_agility_time +
    three_quarter_sprint + bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_dbpm_laneAgilOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + lane_agility_time +
    bench_press + (1 | player),
  data = df_dropNA_scale
)

mod_full_hlm_dbpm_sprintOnly <- lmer(
  dbpm ~ poly(age, 2) + position_gen + height_wo_shoes +
    wingspan + standing_reach + weight + body_fat_pct +
    standing_vertical_leap + max_vertical_leap + three_quarter_sprint +
    bench_press + (1 | player),
  data = df_dropNA_scale
)


mod_full_hlm_dbpm_tidy <- mod_full_hlm_dbpm %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_htOnly_tidy <- mod_full_hlm_dbpm_htOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_wngOnly_tidy <- mod_full_hlm_dbpm_wngOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_stnReachOnly_tidy <- mod_full_hlm_dbpm_stnReachOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_stndVertOnly_tidy <- mod_full_hlm_dbpm_stndVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_maxVertOnly_tidy <- mod_full_hlm_dbpm_maxVertOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_laneAgilOnly_tidy <- mod_full_hlm_dbpm_laneAgilOnly %>% tidy(effects = 'fixed')
mod_full_hlm_dbpm_sprintOnly_tidy <- mod_full_hlm_dbpm_sprintOnly %>% tidy(effects = 'fixed')


mod_full_hlm_dbpm_glance <- mod_full_hlm_dbpm %>% glance()
mod_full_hlm_dbpm_htOnly_glance <- mod_full_hlm_dbpm_htOnly %>% glance()
mod_full_hlm_dbpm_wngOnly_glance <- mod_full_hlm_dbpm_wngOnly %>% glance()
mod_full_hlm_dbpm_stnReachOnly_glance <- mod_full_hlm_dbpm_stnReachOnly %>% glance()
mod_full_hlm_dbpm_stndVertOnly_glance <- mod_full_hlm_dbpm_stndVertOnly %>% glance()
mod_full_hlm_dbpm_maxVertOnly_glance <- mod_full_hlm_dbpm_maxVertOnly %>% glance()
mod_full_hlm_dbpm_laneAgilOnly_glance <- mod_full_hlm_dbpm_laneAgilOnly %>% glance()
mod_full_hlm_dbpm_sprintOnly_glance <- mod_full_hlm_dbpm_sprintOnly %>% glance()


######################################################
# Creat plots of predictor fixed effect point extimates,
#   with standard error bars


p_blk <- mod_full_hlm_blk_tidy[6:15, ] %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 2) +
  geom_segment(
    aes(y = estimate - std.error, yend = estimate + std.error,
        xend = term),
    color = 'blue'
  ) +
  geom_point() +
  coord_flip() +
  labs(
    title = 'Combine Predictors: Blocked Shot Percentage',
    x = '', y = '',
    caption = 'Expected standard deviation difference in shot blocking based on one standard deviation difference
    in predictor; estimates based on hierarchical model of 1484 seasons of at least 1000 minutes played,
    grouped among 273 players, with additional player age and position controls.
    Blue lines represent point estimate +/- one standard error.
    Source: stats.nba API & basketball-reference.com
    
    VisualizingTheLeague.com | @VisualizingTheL'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = 'grey80'),
    axis.ticks.y = element_blank()
  )


p_stl <- mod_full_hlm_stl_tidy[6:15, ] %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 2) +
  geom_segment(
    aes(y = estimate - std.error, yend = estimate + std.error,
        xend = term),
    color = 'blue'
  ) +
  geom_point() +
  coord_flip() +
  labs(
    title = 'Combine Predictors: Steal Percentage',
    x = '', y = '',
    caption = 'Expected standard deviation difference in steals based on one standard deviation difference
    in predictor; estimates based on hierarchical model of 1484 seasons of at least 1000 minutes played,
    grouped among 273 players, with additional player age and position controls.
    Blue lines represent point estimate +/- one standard error.
    Source: stats.nba API & basketball-reference.com
    
    VisualizingTheLeague.com | @VisualizingTheL'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = 'grey80'),
    axis.ticks.y = element_blank()
  )


p_orb <- mod_full_hlm_orb_tidy[6:15, ] %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 2) +
  geom_segment(
    aes(y = estimate - std.error, yend = estimate + std.error,
        xend = term),
    color = 'blue'
  ) +
  geom_point() +
  coord_flip() +
  labs(
    title = 'Combine Predictors: Offensive Rebound Percentage',
    x = '', y = '',
    caption = 'Expected standard deviation difference in shot blocking based on one standard deviation difference
    in predictor; estimates based on hierarchical model of 1484 seasons of at least 1000 minutes played,
    grouped among 273 players, with additional player age and position controls.
    Blue lines represent point estimate +/- one standard error.
    Source: stats.nba API & basketball-reference.com
    
    VisualizingTheLeague.com | @VisualizingTheL'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = 'grey80'),
    axis.ticks.y = element_blank()
  )


p_drb <- mod_full_hlm_drb_tidy[6:15, ] %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 2) +
  geom_segment(
    aes(y = estimate - std.error, yend = estimate + std.error,
        xend = term),
    color = 'blue'
  ) +
  geom_point() +
  coord_flip() +
  labs(
    title = 'Combine Predictors: Defensive Rebound Percentage',
    x = '', y = '',
    caption = 'Expected standard deviation difference in shot blocking based on one standard deviation difference
    in predictor; estimates based on hierarchical model of 1484 seasons of at least 1000 minutes played,
    grouped among 273 players, with additional player age and position controls.
    Blue lines represent point estimate +/- one standard error.
    Source: stats.nba API & basketball-reference.com
    
    VisualizingTheLeague.com | @VisualizingTheL'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = 'grey80'),
    axis.ticks.y = element_blank()
  )


p_dbpm <- mod_full_hlm_dbpm_tidy[6:15, ] %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = 'grey50', linetype = 2) +
  geom_segment(
    aes(y = estimate - std.error, yend = estimate + std.error,
        xend = term),
    color = 'blue'
  ) +
  geom_point() +
  coord_flip() +
  labs(
    title = 'Combine Predictors: Defensive Boxscore Plus/Minus',
    x = '', y = '',
    caption = 'Expected standard deviation difference in shot blocking based on one standard deviation difference
    in predictor; estimates based on hierarchical model of 1484 seasons of at least 1000 minutes played,
    grouped among 273 players, with additional player age and position controls.
    Blue lines represent point estimate +/- one standard error.
    Source: stats.nba API & basketball-reference.com
    
    VisualizingTheLeague.com | @VisualizingTheL'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_line(color = 'grey80'),
    axis.ticks.y = element_blank()
  )


