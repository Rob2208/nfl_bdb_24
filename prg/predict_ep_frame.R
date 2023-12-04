######################################################################################
######################################################################################
#### Calculate EP for predicted yards
######################################################################################
######################################################################################

library(tidyverse)
library(nflverse)
library(randomForest)
library(xgboost)

###################################
#### load data
###################################

### pbp data 

pbp_bdb <- read.csv("data/nfl-big-data-bowl-2024/plays.csv") %>% ## not necessary
  mutate(uId = paste(gameId,playId,sep = "_"))

pbp_data2 <- readRDS("data/pbp_data_prep_2010-2022.rds") %>%
  mutate(uId = paste(old_game_id,play_id,sep = "_"))

pbp_with_play_length <- readRDS("data/pbp_2022_with_play_length.rds")

mean_pl <- pbp_with_play_length %>% ### in theory one should distinguish between passing and rushing plays...
  group_by(playResult) %>%
  summarise(avg = mean(frameId)/10)

### tracking data for in game ep values

trackingsub <- readRDS("data/sub_trackingWide_reordered.rds")

x_test = trackingsub %>% 
  select(-c(x_player_off1:def_dir_bc_diff11, nflId_or_off1:yards_gained)) %>% # deselect columns not ordered by distance to bc
  select(-(gameplayId:ballCarrierId)) %>% # and deselect first meta columns
  select(-dist_bc_or_off1, -xdist_bc_or_off1,- ydist_bc_or_off1) # exclude dist(bc,bc) (could have done this earlier)
#colnames(x_test)

x_test_scaled = as.data.frame(scale(x_test))


### load models

mod_missed <- readRDS("models/mod_missed_v1.rds")

ind <- which(trackingsub$gameplayId %in% unique(trackingsub$gameplayId)[1])
test_pred <- predict(mod_missed, newdata = x_test_scaled[ind,], predict.all = T)$individual

#ep_model2 <- readRDS("models/ep_model_baldwin.rds")
ep_model <- readRDS("models/ep_model_full.rds")


###################################
#### estimate ep for each frame and full cond density 
###################################

eop_yardline_to_pbp <- function(yards_gained,playId,pbp,mod_type = c("simple","full")){
  
  mod_type = match.arg(mod_type)
  play_dets <- pbp %>% filter(uId == playId) %>% select(half_seconds_remaining,
                                 yardline_100,
                                 roof,
                                 season,qtr,down,
                                 ydstogo,
                                 score_differential,
                                 posteam_timeouts_remaining,defteam_timeouts_remaining,
                                 posteam,defteam,home_team
  )
  
  if(mod_type == "simple"){
    yardline_100 = pmax(play_dets$yardline_100-yards_gained,0)
    qtr = play_dets$qtr
    down = case_when(yards_gained >= play_dets$ydstogo ~ 1,
                     play_dets$down < 4 ~ play_dets$down+1,
                     .default = 1) ## 1 in case of possession changes
    ydstogo = case_when(yards_gained >= play_dets$ydstogo & play_dets$yardline_100-yards_gained >= 10 ~ 10,
                        yards_gained >= play_dets$ydstogo & play_dets$yardline_100-yards_gained < 10 ~ play_dets$yardline_100-yards_gained,
                        play_dets$down < 4 ~ play_dets$ydstogo-yards_gained,
                        .default = 10) ## 10 in case of possession changes
    score_differential = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,-play_dets$score_differential, ### possession changes
                                play_dets$score_differential)
    posteam_timeouts_remaining = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$defteam_timeouts_remaining, ### possession changes
                                        play_dets$posteam_timeouts_remaining)
    defteam_timeouts_remaining = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$posteam_timeouts_remaining, ### possession changes
                                        play_dets$defteam_timeouts_remaining)
    posteam = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$defteam, ### possession changes
                     play_dets$posteam)
    home_team = play_dets$home_team
    
    out <- data.frame(yardline_100,ydstogo,
              score_differential,
              qtr,down,
              posteam_timeouts_remaining,defteam_timeouts_remaining,
              posteam,home_team)
  }else{
    half_seconds_remaining = play_dets$half_seconds_remaining - mean_pl$avg[which(mean_pl$playResult %in% round(yards_gained))] ## very crude approximation atm
    yardline_100 = pmax(play_dets$yardline_100-yards_gained,0)
    roof = play_dets$roof
    season = play_dets$season
    qtr = play_dets$qtr
    down = case_when(yards_gained >= play_dets$ydstogo ~ 1,
                     play_dets$down < 4 ~ play_dets$down+1,
                     .default = 1) ## 1 in case of possession changes
    ydstogo = case_when(yards_gained >= play_dets$ydstogo & play_dets$yardline_100-yards_gained >= 10 ~ 10,
                        yards_gained >= play_dets$ydstogo & play_dets$yardline_100-yards_gained < 10 ~ play_dets$yardline_100-yards_gained,
                        play_dets$down < 4 ~ play_dets$ydstogo-yards_gained,
                        .default = 10) ## 10 in case of possession changes
    score_differential = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,-play_dets$score_differential, ### possession changes
                                play_dets$score_differential)
    posteam_timeouts_remaining = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$defteam_timeouts_remaining, ### possession changes
                                        play_dets$posteam_timeouts_remaining)
    defteam_timeouts_remaining = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$posteam_timeouts_remaining, ### possession changes
                                        play_dets$defteam_timeouts_remaining)
    posteam = ifelse(play_dets$down == 4 & play_dets$ydstogo-yards_gained > 0,play_dets$defteam, ### possession changes
                     play_dets$posteam)
    home_team = play_dets$home_team
    
    out <- data.frame(yardline_100,ydstogo,
                      half_seconds_remaining,
                      score_differential,
                      season,qtr,down,roof,
                      posteam_timeouts_remaining,defteam_timeouts_remaining,
                      posteam,home_team)
  }
  return(out)
}


calculate_ep <- function(pbp_data,ep_model){
  preds_ep <- as.data.frame(matrix(predict(ep_model,newdata = as.matrix(pbp_data)),ncol =7,byrow = TRUE))
  colnames(preds_ep) <- c("td_prob", "opp_td_prob", "fg_prob", 
                          "opp_fg_prob", "safety_prob", "opp_safety_prob", "no_score_prob")
  preds_ep <- preds_ep %>% dplyr::mutate(ep = (-3 * .data$opp_fg_prob) + 
                                           (-2 * .data$opp_safety_prob) + (-7 * .data$opp_td_prob) + 
                                           (3 * .data$fg_prob) + (2 * .data$safety_prob) + (7 *.data$td_prob)) 
  return(preds_ep)
}

predict_frame_eps <- function(frame,playId,ep_model,sel_cols = NULL){
  if(is.null(sel_cols)){
    sel_cols <- c("half_seconds_remaining",
                  "yardline_100",
                  "home",
                  "retractable",
                  "dome",
                  "outdoors",
                  "ydstogo",
                  "era0", "era1", "era2", "era3", "era4",
                  "down1", "down2", "down3", "down4",
                  "posteam_timeouts_remaining",
                  "defteam_timeouts_remaining",
                  "score_differential",
                  "qtr")
  }
  frame_df <- eop_yardline_to_pbp(frame,playId = playId,pbp = pbp_data2,mod_type = "full") %>%
    make_model_mutations() %>% ### from ep_model file
    select(all_of(sel_cols)) ## 
  
  calculate_ep(frame_df,ep_model)
}

simple_cols <- c("yardline_100",
                 "home",
                 "ydstogo",
                 "down1", "down2", "down3", "down4",
                 "posteam_timeouts_remaining",
                 "defteam_timeouts_remaining",
                 "score_differential",
                 "qtr")

full_cols <- c("half_seconds_remaining",
               "yardline_100",
               "home",
               "retractable",
               "dome",
               "outdoors",
               "ydstogo",
               "era0", "era1", "era2", "era3", "era4",
               "down1", "down2", "down3", "down4",
               "posteam_timeouts_remaining",
               "defteam_timeouts_remaining",
               "score_differential",
               "qtr")


### example

pid <- paste(trackingsub$gameId[1],trackingsub$playId[1],sep = "_")
ep_frame_ex <- predict_frame_eps(test_pred[1,],playId = pid,ep_model = ep_model, sel_cols = full_cols)
#ep_frame_ex <- predict_frame_eps(test_pred[1,],playId = pid,ep_model = ep_model, sel_cols = full_cols)
