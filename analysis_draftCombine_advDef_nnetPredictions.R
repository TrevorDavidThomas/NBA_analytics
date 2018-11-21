library(ggplot2)
library(dplyr)
library(rlang)
library(here)
library(naniar)
library(forcats)
library(viridis)
library(ggthemes)
library(ggrepel)
library(lme4)
library(nnet)
library(NeuralNetTools)
library(tidyr)
library(glue)

load(here("combine_fin_df.RData"))
load(here("advanceStats_fin_df.RData"))
load(here("combine_advDef_final_df.RData"))


# Look at patterns of missingness across combine/performance observations
combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  select(
    height_wo_shoes:bench_press
  ) %>%
  gg_miss_upset(
    nsets = 13,
    nintersects = NA
  )

# Look at patterns of missingness across rookie prediction dataset
combine_fin_df %>%
  filter(combine_year == '2018') %>%
  select(
    height_wo_shoes:bench_press
  ) %>%
  gg_miss_upset(
    nsets = 13,
    nintersects = NA
  )


#########################################################

# Specify position-group variable for observed data
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

#specify RMSE function for evaluating subsequent model fits,
#   taking two numeric vectors of equal length as input

rmse <- function(x,y){
  z <- ( sum((x - y)^2) / length(x) ) ^ 0.5
  return(z)
}


# generate dataset with only full data for predictors
#   set cutoff for season inclusion to 1000 minutes played
obs_full_df <- combine_advDef_final_df %>%
  filter(mp >= 1000) %>%
  select(
    player,
    age,
    orbpercent:dbpm,
    height_wo_shoes:bench_press,
    position_gen,
    -modified_lane_agility_time, -hand_length, -hand_width
  ) %>%
  tidyr::drop_na()

# generate means and std devs for numeric predictors (to be used for normalizing & retrieving init. values)
obs_full_scaleVars_df <- obs_full_df %>%
  summarise_at(
    vars(age:bench_press),
    funs(mean,sd)
  )

# use above means and std devs to convert initial predictor vars into scaled versions, centered on 0 w/ sd of 1 
for(i in names(obs_full_df)[c(-1,-20)]){
  obs_full_df[i] <- ( obs_full_df[i] -
                        as.numeric(obs_full_scaleVars_df[paste(i,'_mean',sep='')]) ) /
    as.numeric(obs_full_scaleVars_df[paste(i,'_sd',sep='')])
}


# get vector of unique player names to sample from, shuffle the order;
#   use set.seed() for repeatability
set.seed(0)
names_uniq_vec <- sample( unique(obs_full_df$player) )


# determine the number of folds to use for training/testing
fold_n_trainTest <- 10
fold_n_trainVal <- 10

fold_size_trainTest <- round(length(names_uniq_vec) / fold_n_trainTest)

# use partition() in the Hmisc package to generate lists of players for training/test splits
sep_trainTest <- c(
  rep(
    fold_size_trainTest, fold_n_trainTest - 1
  ),
  length(names_uniq_vec) - fold_size_trainTest*(fold_n_trainTest - 1)
)
test_names_list <- Hmisc::partition.vector(names_uniq_vec, sep_trainTest)

# initialize subsequent list for training name splits (the obserse of the above test splits)
train_names_list <- list()

# initialize further lists of validation and training splits:
#   within the above training splits, split again into training and validation splits
validate_names_list <- list()
trainVal_names_list <- list()

for(i in 1:fold_n_trainTest){ # initialize sublists within the list containing training/validation splits for each test split
  validate_names_list[[i]] <- list()
  trainVal_names_list[[i]] <- list()
}

# create list of training player vectors based on obverse of test players
for(i in 1:fold_n_trainTest){
  train_names_list[[i]] <- names_uniq_vec[
    !(names_uniq_vec %in% test_names_list[[i]])
    ]
}

# create nested lists of training/validating player names for each training fold
for(i in 1:fold_n_trainTest){
  fold_size_trainVal <- round(length(train_names_list[[i]]) / fold_n_trainVal)
  
  sep_trainVal <- c(
    rep(
      fold_size_trainVal, fold_n_trainVal - 1
    ),
    length(train_names_list[[i]]) - fold_size_trainVal*(fold_n_trainVal - 1)
  )
  
  validate_names_list[[i]] <- Hmisc::partition.vector(train_names_list[[i]], sep_trainVal)
  
  for(j in 1:fold_n_trainVal){
    trainVal_names_list[[i]][[j]] <- train_names_list[[i]][
      !(train_names_list[[i]] %in% validate_names_list[[i]][[j]])
      ]
    }
  }



#####################################################################
# test performance of nnet procedure in predicting novel data

# set nnet model parameters to scan through (tuning)
nnet_sizes <- seq(4,7,1)
nnet_decays <- seq(0,8,0.25)

# initialize list to store nnet model fit summary dataframes
nnet_trainTestFits <- list()

#initialize index counter
index <- 1

# loop through train/test splits
for(i in 1:fold_n_trainTest){
  
  # loop through train/validate splits
  for(j in 1:fold_n_trainVal){
    
    # loop through nnet parameters
    for(k in 1:length(nnet_sizes)){
      for(l in 1:length(nnet_decays)){
        
        # run neural net model for particular trainging data and parameter values
        mod <-  nnet(blkpercent ~ age + height_wo_shoes +
                       weight + wingspan + standing_reach + body_fat_pct +
                       standing_vertical_leap + max_vertical_leap +
                       lane_agility_time + three_quarter_sprint + bench_press +
                       position_gen,
                     data = obs_full_df %>%
                       filter(player %in% trainVal_names_list[[i]][[j]]),
                     linout = TRUE,
                     size = nnet_sizes[k],
                     decay = nnet_decays[l],
                     maxit = 1000
        )
        
        preds_train <- mod$fitted.values
        preds_validate <- predict(mod, obs_full_df %>%
                                    filter(player %in% validate_names_list[[i]][[j]]))
        preds_test <- predict(mod, obs_full_df %>%
                                filter(player %in% test_names_list[[i]]))
        
        obs_train <- subset(obs_full_df, player %in% trainVal_names_list[[i]][[j]])$blkpercent
        obs_validate <- subset(obs_full_df, player %in% validate_names_list[[i]][[j]])$blkpercent
        obs_test <- subset(obs_full_df, player %in% test_names_list[[i]])$blkpercent
        
        df <- data_frame(
          testing_fold = i,
          validation_fold = j,
          hiddenLayer_size = nnet_sizes[k],
          decay = nnet_decays[l],
          
          train_rmse = rmse(preds_train, obs_train),
          validate_rmse = rmse(preds_validate, obs_validate),
          test_rmse = rmse(preds_test, obs_test)
        )
        
        nnet_trainTestFits[[index]] <- df
        index <- index + 1
      }
    }
    
  }
  
}

# save list of model summary stats to scrap folder
save(nnet_trainTestFits,
     file = paste(scrapLocation,'nnet_trainTestFits_20181107.RData'))


# calculate mean RMSE across validation folds within each train/test split
meanRMSE_validationFolds <- nnet_trainTestFits %>%
  bind_rows() %>%
  gather(
    "rmse_measure", "value", 5:7
  ) %>%
  group_by(
    testing_fold, hiddenLayer_size, decay, rmse_measure
  ) %>%
  summarise(
    mean = mean(value),
    sd = sd(value)
  )

# find the parameter combos that yield RMSE means for each set of validation folds
meanRMSE_valFoldMins <- meanRMSE_validationFolds %>%
  filter(rmse_measure=='validate_rmse') %>%
  group_by(testing_fold) %>%
  summarise(
    min_mean_rmse = min(mean)
  )

meanRMSE_valFoldMinRecords <- meanRMSE_validationFolds %>%
  left_join(meanRMSE_valFoldMins) %>%
  filter(mean == min_mean_rmse)

# fit model to full training data splits (10), using best-performing parameters ID'd above
test_rmses_vec <- numeric(fold_n_trainTest)
for(i in 1:fold_n_trainTest){
  mod <-  nnet(blkpercent ~ age + height_wo_shoes +
                 weight + wingspan + standing_reach + body_fat_pct +
                 standing_vertical_leap + max_vertical_leap +
                 lane_agility_time + three_quarter_sprint + bench_press +
                 position_gen,
               data = obs_full_df %>%
                 filter(player %in% train_names_list[[i]]),
               linout = TRUE,
               size = meanRMSE_valFoldMinRecords$hiddenLayer_size[i],
               decay = meanRMSE_valFoldMinRecords$decay[i],
               maxit = 1000
  )
  
  test_preds <- predict(mod, obs_full_df %>%
                          filter(player %in% test_names_list[[i]]))
  test_obs <-  subset(obs_full_df, player %in% test_names_list[[i]])$blkpercent
  
  test_rmse <- rmse(test_preds,test_obs)
  test_rmses_vec[i] <- test_rmse
}
test_rmses_mean <- mean(test_rmses_vec)


#####################################################################
# test performance of hlm on same splits
test_rmses_vec_hlmComp <- numeric(fold_n_trainTest)

for(i in 1:fold_n_trainTest){
  
  mod <- lmer(
    blkpercent ~ poly(age, 2) + position_gen + height_wo_shoes +
      wingspan + standing_reach + weight + body_fat_pct +
      standing_vertical_leap + max_vertical_leap + lane_agility_time +
      three_quarter_sprint + bench_press + (1 | player),
    data = obs_full_df %>%
      filter(player %in% train_names_list[[i]])
  )
  
  test_preds <- predict(mod, obs_full_df %>%
                          filter(player %in% test_names_list[[i]]),
                        allow.new.levels = TRUE)
  test_obs <-  subset(obs_full_df, player %in% test_names_list[[i]])$blkpercent
  
  test_rmse <- rmse(test_preds,test_obs)
  test_rmses_vec_hlmComp[i] <- test_rmse
  
}
test_rmses_hlmComp_mean <- mean(test_rmses_vec_hlmComp)


#####################################################################
# test performance of baseline hlm on same splits
test_rmses_vec_hlmBaseComp <- numeric(fold_n_trainTest)

for(i in 1:fold_n_trainTest){
  
  mod <- lmer(
    blkpercent ~ poly(age, 2) + position_gen + (1 | player),
    data = obs_full_df %>%
      filter(player %in% train_names_list[[i]])
  )
  
  test_preds <- predict(mod, obs_full_df %>%
                          filter(player %in% test_names_list[[i]]),
                        allow.new.levels = TRUE)
  test_obs <-  subset(obs_full_df, player %in% test_names_list[[i]])$blkpercent
  
  test_rmse <- rmse(test_preds,test_obs)
  test_rmses_vec_hlmBaseComp[i] <- test_rmse
  
}
test_rmses_hlmBaseComp_mean <- mean(test_rmses_vec_hlmBaseComp)

#####################################################################


# graph the fold-specific errors for nnet and hlm preds
foldErrorComp_df <- data_frame(
  fold = rep(c(1:fold_n_trainTest),3),
  rmse = c(test_rmses_vec,test_rmses_vec_hlmComp,test_rmses_vec_hlmBaseComp),
  model = c(
    rep('nnet',fold_n_trainTest), rep('hlm_full',fold_n_trainTest), rep('hlm_base',fold_n_trainTest)
  )
)
foldErrorComp_mean_df <- data_frame(
  fold = rep('mean',3),
  rmse = c(test_rmses_mean,test_rmses_hlmComp_mean,test_rmses_hlmBaseComp_mean),
  model = c('nnet', 'hlm_full', 'hlm_base')
  )

# generate slope plot
ggplot(foldErrorComp_df, aes(x=model,y=rmse, group=as.factor(fold))) +
  geom_line(data = foldErrorComp_mean_df,
            color = 'grey50', size = 2) +
  geom_point(data = foldErrorComp_mean_df,
             shape = 21,
             aes(fill = model),
             color = 'white',
             size = 5) +
  geom_line(color = 'grey30', linetype = 3) +
  geom_point(aes(fill = model),
             shape = 21,
             color = 'white',
             size = 2.5,
             alpha = 0.5) +
  scale_fill_viridis_d() +
  theme_tufte() +
  theme(
    legend.position = 'none'
  )

#####################################################################
# fit cross-validated nnet model using full set of observations

# set nnet model parameters to scan through (tuning)
nnet_sizes <- seq(4,7,1)
nnet_decays <- seq(0,8,0.25)

# initialize list to store nnet model fit summary dataframes
nnet_trainTestFits_2ndRnd <- list()

#initialize index counter
index <- 1

# loop through train/test splits
for(i in 1:fold_n_trainTest){
  
  # loop through nnet parameters
  for(k in 1:length(nnet_sizes)){
    for(l in 1:length(nnet_decays)){
      
      # run neural net model for particular trainging data and parameter values
      mod <-  nnet(blkpercent ~ age + height_wo_shoes +
                     weight + wingspan + standing_reach + body_fat_pct +
                     standing_vertical_leap + max_vertical_leap +
                     lane_agility_time + three_quarter_sprint + bench_press +
                     position_gen,
                   data = obs_full_df %>%
                     filter(player %in% train_names_list[[i]]),
                   linout = TRUE,
                   size = nnet_sizes[k],
                   decay = nnet_decays[l],
                   maxit = 1000
      )
      
      preds_train <- mod$fitted.values
      preds_test <- predict(mod, obs_full_df %>%
                              filter(player %in% test_names_list[[i]]))
      
      obs_train <- subset(obs_full_df, player %in% train_names_list[[i]])$blkpercent
      obs_test <- subset(obs_full_df, player %in% test_names_list[[i]])$blkpercent
      
      df <- data_frame(
        testing_fold = i,
        hiddenLayer_size = nnet_sizes[k],
        decay = nnet_decays[l],
        
        train_rmse = rmse(preds_train, obs_train),
        test_rmse = rmse(preds_test, obs_test)
      )
      
      nnet_trainTestFits_2ndRnd[[index]] <- df
      index <- index + 1
    }
  }

}

# save list of model summary stats to scrap folder
save(nnet_trainTestFits_2ndRnd,
     file = paste(scrapLocation,'nnet_trainTestFits_2ndRnd_20181107.RData'))
# load(here('data','scrap','nnet_trainTestFits_2ndRnd_20181107.RData'))

# calculate mean RMSEs for across training/test folds for each parameter combo
meanRMSE_paramSettings <- nnet_trainTestFits_2ndRnd %>%
  bind_rows() %>%
  gather(
    "rmse_measure", "value", 4:5
  ) %>%
  group_by(
    hiddenLayer_size, decay, rmse_measure
  ) %>%
  summarise(
    mean = mean(value),
    sd = sd(value)
  )

# graph training and test RMSEs for different parameter settings
ggplot(
  meanRMSE_paramSettings, aes(x = decay, y = mean, color = rmse_measure)
) +
  geom_line() +
  coord_cartesian(ylim = c(0,1)) +
  facet_grid(as.factor(hiddenLayer_size) ~ .) +
  theme_minimal()

# fit a full, final nnet model to full dataset using best-performing parameters from above
RMSE_test_best <- min(
  subset(meanRMSE_paramSettings, rmse_measure=='test_rmse')$mean
)


nnet_mod_final <- nnet(blkpercent ~ age + height_wo_shoes +
                         weight + wingspan + standing_reach + body_fat_pct +
                         standing_vertical_leap + max_vertical_leap +
                         lane_agility_time + three_quarter_sprint + bench_press +
                         position_gen,
                       data = obs_full_df,
                       linout = TRUE,
                       size = meanRMSE_paramSettings$hiddenLayer_size[
                         meanRMSE_paramSettings$rmse_measure=='test_rmse' & meanRMSE_paramSettings$mean==RMSE_test_best
                         ],
                       decay = meanRMSE_paramSettings$decay[
                         meanRMSE_paramSettings$rmse_measure=='test_rmse' & meanRMSE_paramSettings$mean==RMSE_test_best
                         ],
                       maxit = 1000
)


# use final model to predict block rates as function of predictors,
#   separating by position and holding other predictors at their positional means

# create vector of positions
pos_vec <- unique(obs_full_df$position_gen)

# create list of dataframes that give the predictor means for each position group
obs_full_df_preds <- obs_full_df %>%
  select(
    -player,
    -(orbpercent:dbpm)
  )

pos_meansDF_list <- list()
for(i in 1:length(pos_vec)){
  pos_meansDF_list[[i]] <- obs_full_df_preds %>%
    filter(position_gen == pos_vec[i]) %>%
    mutate_at(
      vars(age:bench_press),
      mean
    ) %>%
    slice(1)
}

predictor_num <- ncol(obs_full_df_preds) - 1
prediction_num <- 500


# cycle through positions and target predictor variables to generate
#   nnet model predictions for position means, and target predictor values
#   ranging over the spread of values for the position


nnet_positionBasedPreds_list <- list()
counter <- 1

for(i in 1:length(pos_vec)){
  
  input_df <- pos_meansDF_list[[i]]
  
  for(j in 1:predictor_num){
    
    target_predictor_range <- c(
      min(
        subset(obs_full_df_preds, position_gen==pos_vec[i])[,j]
      ),
      max(
        subset(obs_full_df_preds, position_gen==pos_vec[i])[,j]
      )
    )
    
    target_predictor_vec <- seq(
      target_predictor_range[1],
      target_predictor_range[2],
      length.out = prediction_num
    )
    
    for(k in 1:prediction_num){
      
      pred_input <- input_df
      pred_input[,j] <- target_predictor_vec[k]
      
      pred <- predict(
        nnet_mod_final, pred_input
      ) 
      
      output_df <- data.frame(
        position = pos_vec[i],
        predictor_var = names(obs_full_df_preds)[j],
        predictor_val = target_predictor_vec[k],
        prediction = pred
      )
      
      nnet_positionBasedPreds_list[[counter]] <- output_df
      counter <- counter + 1
    }
  }
}

# bind individual predictions into single data frame and process for graphing
nnet_positionBasedPreds_df <- nnet_positionBasedPreds_list %>%
  bind_rows()

mean_wings <- subset(obs_full_df, position_gen==pos_vec[1])$blkpercent %>%
  mean()
mean_bigs <- subset(obs_full_df, position_gen==pos_vec[2])$blkpercent %>%
  mean()
mean_LGs <- subset(obs_full_df, position_gen==pos_vec[3])$blkpercent %>%
  mean()

# order the predictive factors for purposes of graphing legibility
nnet_positionBasedPreds_df$predictor_var <- nnet_positionBasedPreds_df$predictor_var %>%
  as_factor() %>%
  fct_relevel(
    "age", "height_wo_shoes", "wingspan", "standing_reach", "weight", "body_fat_pct",
    "standing_vertical_leap", "max_vertical_leap", "three_quarter_sprint", "lane_agility_time", "bench_press"
  )

# generate faceted marginal effects curves 
ggplot(
  nnet_positionBasedPreds_df,
  aes(x = predictor_val, y = prediction, color = position)
) +
  geom_line() +
  geom_hline(yintercept = mean_bigs,
             color = viridis(3)[3],
             linetype = 3) +
  geom_hline(yintercept = mean_wings,
             color = viridis(3)[2],
             linetype = 3) +
  geom_hline(yintercept = mean_LGs,
             color = viridis(3)[1],
             linetype = 3) +
  geom_hline(yintercept = 0, linetype = 1, color = 'grey20') +
  scale_color_viridis_d() +
  facet_wrap(~ predictor_var,
             ncol = 6,
             scales = 'free_x') +
  labs(
    x = 'Predictor values (standardized)',
    y = 'Prediction values (standardized)'
  ) +
  theme_tufte() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'bottom'
  )


# use NeuralNetTools package to visualize final graph & variable importance
plotnet(nnet_mod_final)
garson(nnet_mod_final)
olden(nnet_mod_final)


# use same, full model to examine over-/under-production among observed players
residGraph_df <- obs_full_df
residGraph_df$blkpercent_pred <- nnet_mod_final$fitted.values[,1]
residGraph_df$blkpercent_resid <- nnet_mod_final$residuals[,1]

# take the 6 most over- and underperforming seasons, 1 max per player
residGraph_df_topResids <- residGraph_df %>%
  group_by(player) %>%
  top_n(1, blkpercent_resid) %>%
  group_by(position_gen) %>%
  top_n(6, blkpercent_resid)

residGraph_df_botResids <- residGraph_df %>%
  group_by(player) %>%
  top_n(-1, blkpercent_resid) %>%
  group_by(position_gen) %>%
  top_n(-6, blkpercent_resid)


# plot scatterplots of predictions vs observations
#   faceted by position group with over- and under-performers highlighted and labeled
ggplot(residGraph_df,
       aes(x = blkpercent_pred, y = blkpercent)) +
  geom_point(alpha = 0.05) +
  geom_point(data = residGraph_df_topResids,
             color = 'red') +
  geom_point(data = residGraph_df_botResids,
             color = 'blue') +
  geom_abline(intercept = 0, slope = 1) +
  geom_text_repel(data = residGraph_df_topResids,
                  aes(label = player), size = rel(3),
                  nudge_y = 0.6, nudge_x = -0.3) +
  geom_text_repel(data = residGraph_df_botResids,
                  aes(label = player), size = rel(3),
                  nudge_y = -0.6, nudge_x = 0.3) +
  scale_x_continuous(limits = c(
    min(residGraph_df$blkpercent_pred) - 0.5,
    max(residGraph_df$blkpercent_pred) + 0.1
  )) +
  scale_y_continuous(limits = c(
    min(residGraph_df$blkpercent) - 1,
    max(residGraph_df$blkpercent) + 1
  )) +
  facet_grid(position_gen ~ ., scales = 'fixed') +
  labs(
    x = 'Block rate prediction (scaled)',
    y = 'Block rate observation (scaled)'
  ) +
  theme_tufte()


########################################################
# plot predictions for new rookie class

# load advanced stats for 2018-19 season (through 11/17/2018)
load(paste(scrapLocation,'advanceStats_fin_df_Nov18Update.RData'))

# fixe Svi's name so his data can be joined
combine_fin_df$player[combine_fin_df$player == 'Sviatoslav Mykhailiuk'] <- 'Svi Mykhailiuk'

# filter for rookies and filter out players who've gotten 0 minutes thus far
rookie_predObsComp_df <- combine_fin_df %>%
  filter(combine_year==2018) %>% View()
  left_join(advanceStats_fin_df) %>%
  filter(mp > 0)

# scale combine predictors according to same scale factors used for deriving final nnet model
rookie_predObsComp_df <- rookie_predObsComp_df %>%
  mutate(
    age = (age - obs_full_scaleVars_df$age_mean) / obs_full_scaleVars_df$age_sd,
    height_wo_shoes = (height_wo_shoes - obs_full_scaleVars_df$height_wo_shoes_mean) / obs_full_scaleVars_df$height_wo_shoes_sd,
    weight = (weight - obs_full_scaleVars_df$weight_mean) / obs_full_scaleVars_df$weight_sd,
    wingspan = (wingspan - obs_full_scaleVars_df$wingspan_mean) / obs_full_scaleVars_df$wingspan_sd,
    standing_reach = (standing_reach - obs_full_scaleVars_df$standing_reach_mean) / obs_full_scaleVars_df$standing_reach_sd,
    body_fat_pct = (body_fat_pct - obs_full_scaleVars_df$body_fat_pct_mean) / obs_full_scaleVars_df$body_fat_pct_sd,
    standing_vertical_leap = (standing_vertical_leap - obs_full_scaleVars_df$standing_vertical_leap_mean) / obs_full_scaleVars_df$standing_vertical_leap_sd,
    max_vertical_leap = (max_vertical_leap - obs_full_scaleVars_df$max_vertical_leap_mean) / obs_full_scaleVars_df$max_vertical_leap_sd,
    lane_agility_time = (lane_agility_time - obs_full_scaleVars_df$lane_agility_time_mean) / obs_full_scaleVars_df$lane_agility_time_sd,
    three_quarter_sprint = (three_quarter_sprint - obs_full_scaleVars_df$three_quarter_sprint_mean) / obs_full_scaleVars_df$three_quarter_sprint_sd,
    bench_press = (bench_press - obs_full_scaleVars_df$bench_press_mean) / obs_full_scaleVars_df$bench_press_sd
   )

# assign position group
rookie_predObsComp_df$position_gen <- NA_character_

rookie_predObsComp_df$position_gen[
  rookie_predObsComp_df$position %in% c('PG','PG-SG', 'SG-PG')
  ] <- 'Lead Guard'
rookie_predObsComp_df$position_gen[
  rookie_predObsComp_df$position %in% c('SG','SG-SF', 'SF-SG', 'SF', 'SF-PF')
  ] <- 'Wing'
rookie_predObsComp_df$position_gen[
  rookie_predObsComp_df$position %in% c('PF-SF','PF', 'PF-C', 'C-PF', 'C')
  ] <- 'Big'
rookie_predObsComp_df$position_gen <- factor(rookie_predObsComp_df$position_gen,
                                             levels = c(
                                               'Lead Guard','Wing','Big'
                                             ))

# generate block shot rate prediction, back-transform to raw value
rookie_predObsComp_df$blkpercent_pred_norm <- predict(nnet_mod_final,
                                                      rookie_predObsComp_df)

rookie_predObsComp_df$blkpercent_pred_raw <- (
  rookie_predObsComp_df$blkpercent_pred_norm * obs_full_scaleVars_df$blkpercent_sd
) + obs_full_scaleVars_df$blkpercent_mean

rookie_predObsComp_df$impute = 'no'


# ID rookies who are missing some combine predictors
rookie_predObsComp_df_missingPreds <- rookie_predObsComp_df %>%
  filter(is.na(blkpercent_pred_raw))

### generate models to impute missing combine data from measure data
###   that's complete for all players
standing_vertical_leap_lm <- lm(standing_vertical_leap ~ height_wo_shoes + weight +
                wingspan + standing_reach + body_fat_pct,
              data = obs_full_df)
max_vertical_leap_lm <- lm(max_vertical_leap ~ height_wo_shoes + weight +
                                  wingspan + standing_reach + body_fat_pct,
                                data = obs_full_df)
lane_agility_time_lm <- lm(lane_agility_time ~ height_wo_shoes + weight +
                                  wingspan + standing_reach + body_fat_pct,
                                data = obs_full_df)
three_quarter_sprint_lm <- lm(three_quarter_sprint ~ height_wo_shoes + weight +
                                  wingspan + standing_reach + body_fat_pct,
                                data = obs_full_df)
bench_press_lm <- lm(bench_press ~ height_wo_shoes + weight +
                                  wingspan + standing_reach + body_fat_pct,
                                data = obs_full_df)

# for all missing variables, fill in with lm-based imputations
rookie_predObsComp_df_missingPreds$standing_vertical_leap[
  is.na(rookie_predObsComp_df_missingPreds$standing_vertical_leap)
] <- predict(
  standing_vertical_leap_lm,
  rookie_predObsComp_df_missingPreds[
    is.na(rookie_predObsComp_df_missingPreds$standing_vertical_leap),
    ]
)

rookie_predObsComp_df_missingPreds$max_vertical_leap[
  is.na(rookie_predObsComp_df_missingPreds$max_vertical_leap)
  ] <- predict(
    max_vertical_leap_lm,
    rookie_predObsComp_df_missingPreds[
      is.na(rookie_predObsComp_df_missingPreds$max_vertical_leap),
      ]
  )

rookie_predObsComp_df_missingPreds$lane_agility_time[
  is.na(rookie_predObsComp_df_missingPreds$lane_agility_time)
  ] <- predict(
    lane_agility_time_lm,
    rookie_predObsComp_df_missingPreds[
      is.na(rookie_predObsComp_df_missingPreds$lane_agility_time),
      ]
  )

rookie_predObsComp_df_missingPreds$three_quarter_sprint[
  is.na(rookie_predObsComp_df_missingPreds$three_quarter_sprint)
  ] <- predict(
    three_quarter_sprint_lm,
    rookie_predObsComp_df_missingPreds[
      is.na(rookie_predObsComp_df_missingPreds$three_quarter_sprint),
      ]
  )

rookie_predObsComp_df_missingPreds$bench_press[
  is.na(rookie_predObsComp_df_missingPreds$bench_press)
  ] <- predict(
    bench_press_lm,
    rookie_predObsComp_df_missingPreds[
      is.na(rookie_predObsComp_df_missingPreds$bench_press),
      ]
  )


# make new prediction for block rate using imputed combine data
rookie_predObsComp_df_missingPreds$blkpercent_pred_norm <- predict(nnet_mod_final,
                                                                   rookie_predObsComp_df_missingPreds)

rookie_predObsComp_df_missingPreds$blkpercent_pred_raw <- (
  rookie_predObsComp_df_missingPreds$blkpercent_pred_norm * obs_full_scaleVars_df$blkpercent_sd
) + obs_full_scaleVars_df$blkpercent_mean

rookie_predObsComp_df_missingPreds$impute = 'yes'


# combine original predictions with predictions using imputations
rookie_predObsComp_df_combined <- rookie_predObsComp_df %>%
  drop_na(blkpercent_pred_raw) %>%
  bind_rows(rookie_predObsComp_df_missingPreds)


rookie_predObsComp_df_combined$player <- rookie_predObsComp_df_combined$player %>%
  as_factor() %>%
  fct_reorder(rookie_predObsComp_df_combined$blkpercent_pred_raw)


# Plot overlayed bar charts and bubble plots, faceted by position group
ggplot(rookie_predObsComp_df_combined,
       aes(x = player, y = blkpercent_pred_raw, fill = position_gen)) +
  geom_bar(stat = 'identity', alpha = 0.5,
           aes(color = impute)) +
  geom_point(aes(y = blkpercent, size = mp),
             shape = 21, color = 'black', alpha = 0.5) +
  scale_color_manual(values = c('white','red'),
                     name = 'Imputed\nCombine\nPredictors') +
  scale_size_area(name = 'Minutes Played\n(as of 11/17/18)') +
  scale_fill_viridis_d(name = 'Position Group') +
  facet_grid(position_gen ~ ., scales = 'free', space = 'free_y') +
  labs(
    x = '',
    y = 'Block Rate, Predicted (bar) and Observed (dot)'
  ) +
  coord_flip() +
  theme_tufte() +
  theme(
    legend.position = 'right',
    strip.text = element_blank()
  )


