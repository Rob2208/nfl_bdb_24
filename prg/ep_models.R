######################################################################################
######################################################################################
#### EP model (mostly from Baldwin)
######################################################################################
######################################################################################

library(tidyverse)
library(nflverse)
library(xgboost)

###################################
#### load and prep data 
###################################

make_model_mutations <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::mutate(
      #for EP, CP, and WP model, xgb needs 0/1 for eras
      era0 = dplyr::if_else(.data$season <= 2001, 1, 0),
      era1 = dplyr::if_else(.data$season > 2001 & .data$season <= 2005, 1, 0),
      era2 = dplyr::if_else(.data$season > 2005 & .data$season <= 2013, 1, 0),
      era3 = dplyr::if_else(.data$season > 2013 & .data$season <= 2017, 1, 0),
      era4 = dplyr::if_else(.data$season > 2017, 1, 0),
      #for fg model, an era factor
      era = dplyr::case_when(
        .data$era0 == 1 ~ 0,
        .data$era1 == 1 ~ 1,
        .data$era2 == 1 ~ 2,
        .data$era3 == 1 | era4 == 1 ~ 3
      ),
      era = as.factor(.data$era),
      down1 = dplyr::if_else(.data$down == 1, 1, 0),
      down2 = dplyr::if_else(.data$down == 2, 1, 0),
      down3 = dplyr::if_else(.data$down == 3, 1, 0),
      down4 = dplyr::if_else(.data$down == 4, 1, 0),
      home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
      model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
      model_roof = as.factor(.data$model_roof),
      retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)
    )
  
  return(pbp)
}

pbp_data <- readRDS("data/pbp_data_prep_2010-2022.rds") %>%
  # in 'R/helper_add_nflscrapr_mutations.R'
  make_model_mutations() %>%
  filter(!is.na(Drive_Score_Half)) %>%
  mutate(
    label = case_when(
      Next_Score_Half == "Touchdown" ~ 0,
      Next_Score_Half == "Opp_Touchdown" ~ 1,
      Next_Score_Half == "Field_Goal" ~ 2,
      Next_Score_Half == "Opp_Field_Goal" ~ 3,
      Next_Score_Half == "Safety" ~ 4,
      Next_Score_Half == "Opp_Safety" ~ 5,
      Next_Score_Half == "No_Score" ~ 6
    ),
    label = as.factor(label),
    # use nflscrapR weights
    Drive_Score_Dist = Drive_Score_Half - drive,
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    ScoreDiff_W = (max(abs(score_differential), na.rm = T) - abs(score_differential)) /
      (max(abs(score_differential), na.rm = T) - min(abs(score_differential), na.rm = T)),
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W, na.rm = T)) /
      (max(Total_W, na.rm = T) - min(Total_W, na.rm = T))
  ) %>%
  filter(
    !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
    !is.na(yardline_100)
  ) %>%
  select(
    label,
    season,
    half_seconds_remaining,
    yardline_100,
    home,
    retractable,
    dome,
    outdoors,
    ydstogo,
    era0, era1, era2, era3, era4,
    down1, down2, down3, down4,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    Total_W_Scaled,
    score_differential,
    qtr
  )

pbp_data <- pbp_data %>%
  mutate(
    label = as.numeric(label),
    label = label - 1
  )

###################################
#### tune models
###################################

tuning_data <- pbp_data %>% filter(season != 2022) %>%
  rowid_to_column("ind")

folds <- tuning_data %>% group_split(season) %>%
  map(~pull(.,ind))

### perform random grid search
### use LOSO CV procedure
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

full_tuning <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = tuning_data %>% select(-ind,-season,-label, -Total_W_Scaled)),
                                   label = tuning_data$label, weight = tuning_data$Total_W_Scaled
)

for (iter in 1:50) {
  cat(iter," ")
  param <- list(booster = "gbtree",
                objective = "multi:softprob",
                eval_metric = c("mlogloss"),
                num_class = 7,
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=full_tuning, params = param, nthread=6, 
                 folds = folds, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=8, maximize=FALSE)
  
  
  min_logloss_index = mdcv$best_iteration
  min_logloss = mdcv$evaluation_log[min_logloss_index]$test_mlogloss_mean
  
  #min_logloss_index = mdcv$best_iteration
  #min_logloss = mdcv$evaluation_log[min_logloss_index]$test_aucpr_mean
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}

### tuned parameters
saveRDS(list(best_param = best_param,
             best_logloss_index = best_logloss_index,
             best_logloss = best_logloss,
             best_seednumber = best_seednumber),
        "models/best_param_full_xgb_mod.rds")

############################################
### tune simpler model

simple_tuning <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = tuning_data %>% select(-retractable,
                                                                                          -dome,
                                                                                          -outdoors,
                                                                                          -era0, -era1, -era2, -era3, -era4,
                                                                                          -ind,-season,-label, -Total_W_Scaled,
                                                                                          -half_seconds_remaining)),
                                    label = tuning_data$label, weight = tuning_data$Total_W_Scaled
)

### perform random grid search
### use LOSO CV procedure
best_param_s = list()
best_seednumber_s = 1234
best_logloss_s = Inf
best_logloss_index_s = 0

for (iter in 1:50) {
  cat(iter," ")
  param <- list(booster = "gbtree",
                objective = "multi:softprob",
                eval_metric = c("mlogloss"),
                num_class = 7,
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=simple_tuning, params = param, nthread=6, 
                 folds = folds, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds=8, maximize=FALSE)
  
  
  min_logloss_index = mdcv$best_iteration
  min_logloss = mdcv$evaluation_log[min_logloss_index]$test_mlogloss_mean
  
  #min_logloss_index = mdcv$best_iteration
  #min_logloss = mdcv$evaluation_log[min_logloss_index]$test_aucpr_mean
  
  if (min_logloss < best_logloss_s) {
    best_logloss_s = min_logloss
    best_logloss_index_s = min_logloss_index
    best_seednumber_s = seed.number
    best_param_s = param
  }
}

### tuned parameters
saveRDS(list(best_param = best_param_s,
             best_logloss_index = best_logloss_index_s,
             best_logloss = best_logloss_s,
             best_seednumber = best_seednumber_s),
        "models/best_param_simple_xgb_mod.rds")

###################################
#### train models
###################################

nround = best_logloss_index
set.seed(best_seednumber)
full_mod <- xgb.train(data=full_tuning, params=best_param, nrounds=nround, nthread=6)

saveRDS(full_mod,"models/ep_model_full.rds")

nround_s = best_logloss_index_s
set.seed(best_seednumber_s)
simple_mod <- xgb.train(data=simple_tuning, params=best_param_s, nrounds=nround_s, nthread=6)

saveRDS(simple_mod,"models/ep_model_simple.rds")

### evaluate model on 2022 data

#full_mod <- readRDS("models/ep_model_full.rds")
#simple_mod <- readRDS("models/ep_model_simple.rds")

pbp_test_data_full <- pbp_data %>% 
  filter(season == 2022) %>%
  select(-label,-season,-Total_W_Scaled)

pbp_test_data_simple <- pbp_data %>% 
  filter(season == 2022) %>%
  select(-label,-season,-Total_W_Scaled,
         -half_seconds_remaining,
         -era0,-era1,-era2,-era3,-era4,
         -retractable,-dome,-outdoors)

calculate_ep <- function(pbp_data,ep_model){
  preds_ep <- as.data.frame(matrix(predict(ep_model,newdata = as.matrix(pbp_data)),ncol =7,byrow = TRUE))
  colnames(preds_ep) <- c("td_prob", "opp_td_prob", "fg_prob", 
                               "opp_fg_prob", "safety_prob", "opp_safety_prob", "no_score_prob")
  preds_ep <- preds_ep %>% dplyr::mutate(ep = (-3 * .data$opp_fg_prob) + 
                                                     (-2 * .data$opp_safety_prob) + (-7 * .data$opp_td_prob) + 
                                                     (3 * .data$fg_prob) + (2 * .data$safety_prob) + (7 *.data$td_prob)) 
  return(preds_ep)
}

eps_test_full <- calculate_ep(pbp_test_data_full,full_mod)
eps_test_simple <- calculate_ep(pbp_test_data_simple,simple_mod)
pbp_data_check <- readRDS("data/pbp_data_prep_2010-2022.rds") %>%
  filter(season == 2022) %>%
  mutate(uId = paste(old_game_id,play_id,sep = "_"))
comp_data <- pbp_data_check %>% 
  select(uId,Next_Score_Half,Drive_Score_Half,ep) %>%
  rename(ep_baldwin = ep) %>%
  bind_cols(eps_test_full) %>%
  rename(ep_full = ep) %>%
  bind_cols(eps_test_simple) %>%
  rename(ep_simple = ep) %>%
  mutate(points = case_when(
    Next_Score_Half == "Touchdown" ~ 7,
    Next_Score_Half == "Opp_Touchdown" ~ -7,
    Next_Score_Half == "Field_Goal" ~ 3,
    Next_Score_Half == "Opp_Field_Goal" ~ -3,
    Next_Score_Half == "Safety" ~ 2,
    Next_Score_Half == "Opp_Safety" ~ -2,
    Next_Score_Half == "No_Score" ~ 0
  ),
  abs_err_epb = abs(ep_baldwin-points),
  abs_err_full = abs(ep_full-points),
  abs_err_simple = abs(ep_simple-points)
  )

mean(comp_data$abs_err_epb) ## ep from nflfastR data (baldwin)
mean(comp_data$abs_err_full) ## full model better on test set (wrt MAE)
mean(comp_data$abs_err_simple) ## simple model a bit better on test set (wrt MAE)

t.test(comp_data$abs_err_epb,comp_data$abs_err_full,paired = TRUE,alternative = "greater") ## full significantly better on test set     
t.test(comp_data$abs_err_epb,comp_data$abs_err_simple,paired = TRUE,alternative = "greater") ## simple not, but also not worse!  

### visualize some results

s <- seq(-10,100,by = 1)
yd_dta_simple <- pbp_test_data_simple %>% dplyr::slice(rep(6,each = length(s))) %>%
  mutate(yardline_100 = s)
eps_yd_simple <- calculate_ep(yd_dta_simple,simple_mod)

