######################################################################################
######################################################################################
#### create tackles data for prediction of tackle vals
######################################################################################
######################################################################################

library(tidyverse)

###################################
#### load data
###################################

## load data; in the best case: load test data only!
tracking_used <- readRDS("data/trackingWide_week1-3.rds")

## create relevant data for prediction
## Note: name "x_train" is misleading; this should be test data
x_train = tracking_used %>% 
  select(-c(x_player_off1:def_dir_bc_diff11, nflId_or_off1:yards_gained)) %>% # deselect columns not ordered by distance to bc
  select(-(gameplayId:ballCarrierId)) %>% # and deselect first meta columns
  select(-dist_bc_or_off1, -xdist_bc_or_off1,- ydist_bc_or_off1) %>% # exclude dist(bc,bc) (could have done this earlier)
  #select(-ends_with("def11"))%>% ### eliminate 11th defender
  select(-treatment) ### eliminate treatment
colnames(x_train)

x_train_scaled = x_train %>% 
  mutate_at(-c(8,9),~scale(.))

### create data in for hypothetical frame (eliminate tackler, only 10 defenders)

tackles <- tracking_used %>% filter(tackle_dummy == 1)
tackles_train <- x_train_scaled %>% filter(tackle_dummy == 1)

oldcn <-  tackles_train %>% select(ends_with("or_def1")) %>% names()  
colns <-  str_sub(oldcn,end = -2)
tackles_el_def <- tackles_train %>%
  rename_at(vars(ends_with("or_def1")),~ paste0(oldcn,"_old")) 

for(i in 1:10){
  tackles_el_def <- tackles_el_def %>%
    rename_at(vars(ends_with(paste0("or_def",i+1))),~ paste0(colns,i))
}

saveRDS(list(tackles,tackles_train,tackles_el_def),"data/tackles_data_for_preds.rds")