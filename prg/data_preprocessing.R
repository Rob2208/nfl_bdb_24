######################################################################################
######################################################################################
#### Basic data preprocessing
######################################################################################
######################################################################################

library(tidyverse)
library(nflverse)

###################################
#### load data
###################################


files <- list.files("data/nfl-big-data-bowl-2024")
dta_list <- list()
i <- 1
for(file in files){
  dta_list[[i]] <- read.csv(paste0("data/nfl-big-data-bowl-2024/",file))
  names(dta_list)[i] <- gsub(".csv","",file)
  i <- i+1
}


full_tracking <- bind_rows(dta_list[grep("week",names(dta_list))]) %>%
  mutate(uId = paste(gameId,playId,sep = "_"), ## Standardize plays such that each play from left to right
         x_std = ifelse(playDirection == "left", 120-x, x), ## Standardizes X
         y_std = ifelse(playDirection == "left", 160/3-y, y),  ## Standardized Y
         dir_std = ifelse(playDirection == "left", dir - 180, dir), ## Standardized dir
         x_sc_std = s*cos((90-dir_std)*pi/180) + x_std, 
         y_sc_std = s*sin((90-dir_std)*pi/180) + y_std)

tackles <- dta_list$tackles %>%
  mutate(uId = paste(gameId,playId,sep = "_"),
         uId_player = paste(uId,nflId,sep = "_"))

## these should maybe be taken care of?
both_data <- tackles %>% filter(tackle == pff_missedTackle) %>% filter(tackle == 1)

pbp <- dta_list$plays %>%
  mutate(uId = paste(gameId,playId,sep = "_"))


###################################
#### Extract tackle frames
###################################

full_tracking <- full_tracking %>%
  left_join(pbp %>% select(uId,ballCarrierId),by = "uId") %>%
  mutate(is_ball_carrier = ifelse(nflId == ballCarrierId,1,0),
         uId_player = paste(uId,nflId,sep = "_"),
         tackle_attempt = ifelse(uId_player %in% tackles$uId_player,1,0))

tackle_tracking <- full_tracking %>% 
  filter(tackle_attempt == 1) %>%
  mutate(uId_frame = paste(uId,frameId,sep = "_"))
ball_carrier_tracking <- full_tracking %>% 
  filter(is_ball_carrier == 1) %>%
  mutate(uId_frame = paste(uId,frameId,sep = "_"))

tracking_pairs <- tackle_tracking %>%
  left_join(ball_carrier_tracking %>% select(uId,nflId,ends_with("std"),uId_player,uId_frame), by = "uId_frame", suffix = c("_tackler","_carrier")) %>%
  mutate(dist_x = x_std_tackler-x_std_carrier,
         dist_y = y_std_tackler-y_std_carrier,
         dist = sqrt(dist_x^2+dist_y^2))

rel_frames <- tracking_pairs %>%
  group_by(uId_player_tackler) %>%
  summarize(min_dist = min(dist), uId_frame = uId_frame[which.min(dist)])

ft_needed <- full_tracking %>%
  mutate(uId_frame = paste(uId,frameId,sep = "_")) %>%
  filter(uId_frame %in% rel_ids$uId_frame)

saveRDS(ft_needed,"data/tackle_frames_tracking.rds")

## still to do: merge with pbp data to get full game state