######################################################################################
######################################################################################
#### new model, only 10 defenders rel
######################################################################################
######################################################################################

library(tidyverse)
library(randomForest)

###################################
#### load data
###################################

# load tracking data as obtained from file "preprocessing_new.R"
tracking_used <- readRDS("data/trackingWide_week1-3.rds")

x_train = tracking_used %>% 
  select(-c(x_player_off1:def_dir_bc_diff11, nflId_or_off1:yards_gained)) %>% # deselect columns not ordered by distance to bc
  select(-(gameplayId:ballCarrierId)) %>% # and deselect first meta columns
  select(-dist_bc_or_off1, -xdist_bc_or_off1,- ydist_bc_or_off1) %>% # exclude dist(bc,bc) (could have done this earlier)
  select(-ends_with("def11"))%>% ### eliminate 11th defender
  select(-treatment) ### eliminate treatment
colnames(x_train)

#x_train_scaled = as.data.frame(scale(x_train))
x_train_scaled = x_train %>% 
  mutate_at(-c(8,9),~scale(.))

#n_train = 50000 # my RAM would not allow more
n_train = nrow(x_train)
na.inds = apply(is.na(x_train_scaled[1:n_train,]), 1, sum)
na.inds = which(na.inds > 0)
na.inds = as.numeric(na.inds)
train.ind = setdiff(1:n_train, na.inds)

#a <- Sys.time()
mod = randomForest(tracking_used$yards_gained[train.ind] ~., data = x_train_scaled[train.ind,], ntree = 1000,
                   do.trace = TRUE) ## argument do.trace not necessary, but provides info about progress
#Sys.time()-a

saveRDS(mod, "models/mod_week1-3_new.rds")
