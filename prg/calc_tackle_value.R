######################################################################################
######################################################################################
#### calc tackle value clean
######################################################################################
######################################################################################

library(tidyverse)
library(randomForest)
library(RCurl)
source("prg/ep_functions.R")

###################################
#### load model and data
###################################

# load a trained RF model for EOP prediction
mod_used <- readRDS("models/mod_week1-3_new.rds")

# load pbp data (note: all of them are needed)
pbp_data <- readRDS("data/pbp_data_prep_2010-2022.rds") %>%
  mutate(uId = paste(old_game_id,play_id,sep = "_"))
pbp_with_play_length <- readRDS("data/pbp_2022_with_play_length.rds")
pbp_data_bdb <- read.csv("data/nfl-big-data-bowl-2024/plays.csv") %>% 
  mutate(uId = paste(gameId,playId,sep = "_"))

players_bdb <- read.csv("data/nfl-big-data-bowl-2024/players.csv")

# load tackles data, in the best case: load test data only!
# ## Note: name "tackles_train" is misleading; this should be test data (ich wollte nicht alles im Code ändern...)
tackles_pred_list <- readRDS("data/tackles_data_for_preds.RDS")
tackles <- tackles_pred_list[[1]]
tackles_train <- tackles_pred_list[[2]]
tackles_el_def <- tackles_pred_list[[3]]

tackle_to_pred <- tackles_el_def %>% 
  select(-ends_with("def1_old")) ### eliminate nearest defender

# this is not really necessary but needed for the code to run atm
mean_pl <- pbp_with_play_length %>% ### in theory one should distinguish between passing and rushing plays...
  group_by(playResult) %>%
  summarise(avg = mean(frameId)/10)

# load an ep model
ep_model <- readRDS("models/ep_model_simple.rds")


###################################
#### calculate tackle value hypothetical
###################################

pred_data_scaled_s <- tackle_to_pred %>%
  mutate(tackle_dummy = 0,
         missed_tackle_dummy = 1) 
tackle_pred_s <- predict(mod_used, newdata = pred_data_scaled_s, predict.all = T)$individual
tackle_info_simple <- as.data.frame(matrix(0,nrow = nrow(tackle_to_pred),ncol = 4))
for(i in 1:nrow(tackle_to_pred)){
  if(i %% 500 == 0){
    cat("iter ",i,"\n")
  }
  playInfos <- tackles[i,] %>% select(gameplayId:ballCarrierId) 
  pid <- paste(playInfos$gameId,playInfos$playId,sep = "_")
  ep_df <- predict_frame_eps(as.numeric(tackles[i,"x_bc"])-tackle_pred_s[i,],playId = pid,pbp = pbp_data,ep_model = ep_model, sel_cols = simple_cols) ### adjust predict_frame_eps function!
  ep_avg <- mean(ep_df$ep)
  tackle_info_simple[i,] <- c(unlist(playInfos %>% select(gameplayId,gameId,playId)),ep_avg) 
}
names(tackle_info_simple) <- c("gameplayId","gameId","playId","ep_tackle_def2-11")
tackle_info_simple2 <- tackle_info_simple %>%
  mutate(avg_yg = rowMeans(tackle_pred_s),
         uId = paste(gameId,playId,sep = "_"),
         tackle_yardline = tackles[,"x_bc"],
         yardline_pred = tackle_yardline-avg_yg) %>%
  left_join(pbp_data %>% select(uId,yardline_100)) %>%
  left_join(pbp_data_bdb %>% select(uId,playResult)) %>%
  mutate(true_yardline = yardline_100-playResult,
         yardline_diff = tackle_yardline-true_yardline)

###################################
#### calculate tackle values "real" frame
###################################

pred_data_scaled_real <- tackles_train %>%
  select(-ends_with("def11")) 
tackle_pred_real <- predict(mod_used, newdata = pred_data_scaled_real, predict.all = T)$individual
tackle_info_real <- as.data.frame(matrix(0,nrow = nrow(tackle_to_pred),ncol = 4))
for(i in 1:nrow(tackle_to_pred)){
  if(i %% 500 == 0){
    cat("iter ",i,"\n")
  }
  playInfos <- tackles[i,] %>% select(gameplayId:ballCarrierId) 
  pid <- paste(playInfos$gameId,playInfos$playId,sep = "_")
  ep_df <- predict_frame_eps(as.numeric(tackles[i,"x_bc"])-tackle_pred_real[i,],playId = pid,pbp = pbp_data,ep_model = ep_model, sel_cols = simple_cols) ### adjust predict_frame_eps function!
  ep_avg <- mean(ep_df$ep)
  tackle_info_real[i,] <- c(unlist(playInfos %>% select(gameplayId,gameId,playId)),ep_avg) 
}
names(tackle_info_real) <- c("gameplayId","gameId","playId","ep_tackle_def1-10")
tackle_info_real2 <- tackle_info_real %>%
  mutate(avg_yg = rowMeans(tackle_pred_real,na.rm = TRUE),
         uId = paste(gameId,playId,sep = "_"),
         tackle_yardline = tackles[,"x_bc"],
         yardline_pred = tackle_yardline-avg_yg) %>%
  left_join(pbp_data %>% select(uId,yardline_100)) %>%
  left_join(pbp_data_bdb %>% select(uId,playResult)) %>%
  mutate(true_yardline = yardline_100-playResult,
         yardline_diff = tackle_yardline-true_yardline)

###################################
#### calculate tackle values true EOP (from pbp)
###################################

playInfos_full <- tackles %>% select(gameplayId:ballCarrierId) 
pids <- paste(playInfos_full$gameId,playInfos_full$playId,sep = "_")
play_res_df <- as.data.frame(matrix(0,nrow=length(pids),ncol = 9))
for(i in 1:length(pids)){
  if(i %% 500 == 0){
    cat("iter ",i,"\n")
  }
  pid = pids[i]
  yards_gained <- pbp_data_bdb %>% filter(uId == pid) %>% pull(playResult)
  play_res_df[i,] <- predict_frame_eps(yards_gained,playId = pid,pbp = pbp_data,ep_model = ep_model, sel_cols = simple_cols,is_yards_gained = TRUE)
}

###################################
#### combine data
###################################

tackle_vals <- cbind(tackle_info_simple,tackle_info_real$`ep_tackle_def1-10`) 
names(tackle_vals)[5] = "ep_tackle_def1-10"
tackle_vals$info <- as.numeric(tackle_vals$`ep_tackle_def2-11` > tackle_vals$`ep_tackle_def1-10`)
tackle_vals$gain <- tackle_vals$`ep_tackle_def2-11` - tackle_vals$`ep_tackle_def1-10`

tackle_vals$ep_tackle_real_eop <- play_res_df$V8
tackle_vals$info2 <- as.numeric(tackle_vals$`ep_tackle_def2-11` > tackle_vals$ep_tackle_real_eop)
tackle_vals$gain2 <- tackle_vals$`ep_tackle_def2-11` - tackle_vals$ep_tackle_real_eop


###################################
#### player evaluations
###################################

id_team <- tackles %>% select(club_or_def1,nflId_or_def1) %>% distinct(nflId_or_def1,.keep_all = TRUE)
tackle_vals$tackler_id <- tackles$nflId_or_def1
best_tacklers <- tackle_vals %>% select(tackler_id,gain,gain2) %>%
  group_by(tackler_id) %>%
  summarise(sum_tackle_gains = sum(gain),sum_tackle_gains2 = sum(gain2),n_tackles = n()) %>%
  mutate(avg_per_tackle = sum_tackle_gains/n_tackles,
         avg_per_tackle2 = sum_tackle_gains2/n_tackles) %>%
  arrange(desc(sum_tackle_gains)) %>%
  left_join(players_bdb, by = c("tackler_id" = "nflId")) %>%
  left_join(id_team,by = c("tackler_id" = "nflId_or_def1")) %>%
  rename(club = club_or_def1)

url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)

best_tacklers <- best_tacklers %>%
  left_join(df.logos , by = c("club" = "team_code"))

saveRDS(list(tackles,
             tackle_info_real2,tackle_info_simple2,
             tackle_pred_real,tackle_pred_s,
             tackle_vals,best_tacklers),"data/tackles_info_data_trw1-3.rds") ## trw1-3 = training data from week 1-3
