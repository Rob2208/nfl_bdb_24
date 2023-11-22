
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(vroom)
library(randomForest)

# Load data ---------------------------------------------------------------

games = read.csv("data/games.csv")
plays = read.csv("data/plays.csv")
tackles = read.csv("data/tackles.csv")
tracking = vroom("data/tracking_week_1.csv")
for(i in 2:9){
  print(i)
  tracking = rbind(tracking, vroom(paste0("data/tracking_week_",i,".csv")))
}


# you can ignore this part and just load the data further down

# Preprocessing -----------------------------------------------------------

## here I'm essentially following Yurko et al.

# creating adjusted x value
leftInd = which(tracking$playDirection == "left")
tracking$x[leftInd] = tracking$x[leftInd] - 10 # x dist to endzone
tracking$x[-leftInd] = (110) - tracking$x[-leftInd]  # x dist to endzone
# creating adjusted y value
tracking$y = tracking$y - (53.3/2)

# creating adjusted direction
tracking$dir[leftInd] = (tracking$dir[leftInd] + 90) %% 360
tracking$dir[-leftInd] = (tracking$dir[-leftInd] + (360-90)) %% 360

plays$gameplayId = as.numeric(paste0(plays$gameId, plays$playId))
tracking$gameplayId = as.numeric(paste0(tracking$gameId, tracking$playId))

plays_sub = plays %>% select(gameplayId, ballCarrierId)
tracking = tracking %>% merge(plays_sub, by = c("gameplayId"))

trackingList = split(tracking, tracking$gameplayId)
playernames = c(paste0("player_off", 1:11), paste0("player_def", 1:11), "football")

nObs = as.numeric(lapply(trackingList, nrow))
out = which(nObs < 253)

# reorder within play by frame id, still long format
tracking_reorder = trackingList[-out] %>% 
  map(~mutate(., isballCarrier = ifelse(nflId == ballCarrierId, 1, 0))) %>% 
  map(~mutate(., inballCarrierTeam = ifelse(club == club[which(isballCarrier==1)][1],1,0))) %>% 
  map(~mutate(., x_bc = rep(x[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., y_bc = rep(y[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., s_bc = rep(s[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., a_bc = rep(a[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., dis_bc = rep(dis[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., o_bc = rep(o[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., dir_bc = rep(dir[which(isballCarrier == T)], 23))) %>% 
  map(~arrange(., frameId, -inballCarrierTeam)) %>% 
  map(~mutate(., isFootball = ifelse(displayName=="football",1,0))) %>% 
  map(~mutate(., player = rep(playernames, max(unique(frameId))))) %>% 
  bind_rows()

# pivot to wide format
trackingWide = tracking_reorder %>% 
  select(-isballCarrier, -inballCarrierTeam, -isFootball) %>% 
  pivot_wider(names_from = player, values_from = c(x, y, s, a, dis, o, dir, nflId, displayName, jerseyNumber, club))

# L2 distance to ballcarrier
for(i in 1:11){ # offensive players
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = sqrt((trackingWide$x_bc - trackingWide[,15+i])^2 + (trackingWide$y_bc - trackingWide[,38+i])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_bc_off",i))
}
for(i in 1:11){ # defensive players
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = sqrt((trackingWide$x_bc - trackingWide[,26+i])^2 + (trackingWide$y_bc - trackingWide[,49+i])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_bc_def",i))
}
# x distance to ballcarrier
for(i in 1:11){# offensive players
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = trackingWide$x_bc - trackingWide[,15+i]
  colnames(trackingWide) = c(oldcolnames, paste0("xdist_bc_off",i))
}
for(i in 1:11){ # defensive players
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = trackingWide$x_bc - trackingWide[,26+i]
  colnames(trackingWide) = c(oldcolnames, paste0("xdist_bc_def",i))
}
# y distance to ballcarrier
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = trackingWide$y_bc - trackingWide[,38+i]
  colnames(trackingWide) = c(oldcolnames, paste0("ydist_bc_off",i))
}
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = trackingWide$y_bc - trackingWide[,49+i]
  colnames(trackingWide) = c(oldcolnames, paste0("ydist_bc_def",i))
}
# angle to shortest segment defenders as described in the paper
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  shortest_segments = cbind(trackingWide$x_bc, trackingWide$y_bc) - cbind(trackingWide[,26+i],trackingWide[,49+i])
  angles = atan(shortest_segments[,2]/shortest_segments[,1])
  angles = angles*180/pi
  angles = (360 - angles + 540) %% 360
  trackingWide$newangle = abs(angles - trackingWide[,164+i])
  colnames(trackingWide) = c(oldcolnames, paste0("def_dir_bc_diff",i))
}

# reordering: I now create new colums for each player and each variable where player1 is closest, then player 2, etc.

x_ordered = y_ordered = s_ordered = a_ordered = dis_ordered = o_ordered = dir_ordered = nflId_ordered = name_ordered = jersey_ordered = club_ordered = dist_bc_ordered = xdist_ordered = ydist_ordered = data.frame(matrix(nrow = nrow(trackingWide), ncol = 22))
def_dir_bc_diff_ordered = data.frame(matrix(nrow = nrow(trackingWide), ncol = 11))

distance_cols_off = trackingWide %>% select(dist_bc_off1:dist_bc_off11)
distance_cols_def = trackingWide %>% select(dist_bc_def1:dist_bc_def11)

reorder_tracking = function(trackingWide_slice){
  distance_cols_off = trackingWide_slice[268+1:11]
  distance_cols_def = trackingWide_slice[268+11+1:11]
  
  ordering_off = distance_cols_off %>% unlist() %>% as.numeric() %>% order()
  ordering_def = distance_cols_def %>% unlist() %>% as.numeric() %>% order()
  
  x_ordered = c(trackingWide_slice[15+ordering_off], trackingWide_slice[15+11+ordering_def])
  y_ordered = c(trackingWide_slice[38+ordering_off], trackingWide_slice[38+11+ordering_def])
  s_ordered = c(trackingWide_slice[61+ordering_off], trackingWide_slice[61+11+ordering_def])
  a_ordered = c(trackingWide_slice[84+ordering_off], trackingWide_slice[84+11+ordering_def])
  dis_ordered = c(trackingWide_slice[107+ordering_off], trackingWide_slice[107+11+ordering_def])
  o_ordered = c(trackingWide_slice[130+ordering_off], trackingWide_slice[130+11+ordering_def])
  dir_ordered = c(trackingWide_slice[153+ordering_off], trackingWide_slice[153+11+ordering_def])
  nflId_ordered = c(trackingWide_slice[176+ordering_off], trackingWide_slice[176+11+ordering_def])
  # name_ordered = c(trackingWide_slice[199+ordering_off], trackingWide_slice[199+11+ordering_def])
  jersey_ordered = c(trackingWide_slice[222+ordering_off], trackingWide_slice[222+11+ordering_def])
  # club_ordered = c(trackingWide_slice[245+ordering_off], trackingWide_slice[245+11+ordering_def])
  dist_bc_ordered = c(as.matrix(trackingWide_slice[268+ordering_off]), as.matrix(trackingWide_slice[268+11+ordering_def]))
  xdist_bc_ordered = c(as.matrix(trackingWide_slice[290+ordering_off]), as.matrix(trackingWide_slice[290+11+ordering_def]))
  ydist_bc_ordered = c(as.matrix(trackingWide_slice[312+ordering_off]), as.matrix(trackingWide_slice[312+11+ordering_def]))
  def_dir_bc_diff_ordered = c(as.matrix(trackingWide_slice[334+ordering_def]))
  
  return(
    c(
      as.numeric(c(x_ordered,
                   y_ordered,
                   s_ordered,
                   a_ordered,
                   dis_ordered,
                   o_ordered,
                   dir_ordered,
                   dist_bc_ordered,
                   xdist_bc_ordered,
                   ydist_bc_ordered,
                   def_dir_bc_diff_ordered)),
      
      as.integer(nflId_ordered),
      as.integer(jersey_ordered)
    )
  )
}
reorder_tracking_char = function(trackingWide_slice){
  distance_cols_off = trackingWide_slice[268+1:11]
  distance_cols_def = trackingWide_slice[268+11+1:11]
  
  ordering_off = distance_cols_off %>% unlist() %>% as.numeric() %>% order()
  ordering_def = distance_cols_def %>% unlist() %>% as.numeric() %>% order()
  
  name_ordered = c(trackingWide_slice[199+ordering_off], trackingWide_slice[199+11+ordering_def])
  club_ordered = c(trackingWide_slice[245+ordering_off], trackingWide_slice[245+11+ordering_def])
  
  return(
    c(as.character(name_ordered),
      as.character(club_ordered))
  )
}

newcols = t(apply(trackingWide, 1, reorder_tracking))
newcols_char = t(apply(trackingWide, 1, reorder_tracking_char))
newcols = as.data.frame(newcols)
colnames(newcols) = c(paste0("x_or_off", 1:11), paste0("x_or_def", 1:11),
                      paste0("y_or_off", 1:11), paste0("y_or_def", 1:11),
                      paste0("s_or_off", 1:11), paste0("s_or_def", 1:11),
                      paste0("a_or_off", 1:11), paste0("a_or_def", 1:11),
                      paste0("dis_or_off", 1:11), paste0("dis_or_def", 1:11),
                      paste0("o_or_off", 1:11), paste0("o_or_def", 1:11),
                      paste0("dir_or_off", 1:11), paste0("dir_or_def", 1:11),
                      paste0("dist_bc_or_off", 1:11), paste0("dist_bc_or_def", 1:11),
                      paste0("xdist_bc_or_off", 1:11), paste0("xdist_bc_or_def", 1:11),
                      paste0("ydist_bc_or_off", 1:11), paste0("ydist_bc_or_def", 1:11),
                      paste0("dir_bc_diff_or_def", 1:11),
                      
                      paste0("nflId_or_off", 1:11), paste0("nflId_or_def", 1:11),
                      paste0("jerseyN_or_off", 1:11), paste0("jerseyN_or_def", 1:11)
)

newcols_char = as.data.frame(newcols_char)
colnames(newcols_char) = c(paste0("name_or_off", 1:11), paste0("name_or_def", 1:11),
                           paste0("club_or_off", 1:11), paste0("club_or_def", 1:11))

allnewcols = cbind(newcols, newcols_char)

trackingWidenew = cbind(trackingWide, allnewcols)


# creating yard line at the end of play and yards_gained

trackingWideList = split(trackingWidenew, trackingWidenew$gameplayId)

# I don't know if this makes sense, yardline at eop just x location of ball carrier at the end
trackingWidenew = trackingWideList %>% 
  map(~mutate(., eop_yardline = x_bc[length(x_bc)])) %>% 
  bind_rows()

trackingWidenew$yards_gained = trackingWidenew$x_bc - trackingWidenew$eop_yardline


## Instead of this stuff, you can just do 

trackingWidenew = readRDS("data/trackingWide_ordered2.rds")



# Model training ----------------------------------------------------------

# Train a model on a sample of the data set -------------------------------

x_train = trackingWidenew %>% 
  select(-c(x_player_off1:def_dir_bc_diff11, nflId_or_off1:yards_gained)) %>% # deselect columns not ordered by distance to bc
  select(-(gameplayId:ballCarrierId)) %>% # and deselect first meta columns
  select(-dist_bc_or_off1, -xdist_bc_or_off1,- ydist_bc_or_off1) # exclude dist(bc,bc) (could have done this earlier)
colnames(x_train)

x_train_scaled = as.data.frame(scale(x_train))

n_train = 70000
na.inds = apply(is.na(x_train_scaled[1:n_train,]), 1, sum)
na.inds = which(na.inds > 0)
na.inds = as.numeric(na.inds)
train.ind = setdiff(1:n_train, na.inds)

# mod = randomForest(trackingWidenew$yards_gained[train.ind] ~., data = x_train_scaled[train.ind,], ntree = 1000)
# saveRDS(mod, "mod70K.rds")

mod = readRDS("mod70K.rds")

# RMSE out of sample (this takes time!)
errors = trackingWidenew$yards_gained[70001:nrow(x_train_scaled)]-
  predict(mod, x_train_scaled[70001:nrow(x_train_scaled),])
sqrt(mean(errors^2, na.rm = T))
# RMSE of 6.1 yards out of sample


## Visualizing the global model

n_train = 70000
gameplayIds = unique(trackingWidenew$gameplayId)
which(gameplayIds == trackingWidenew$gameplayId[n_train])
# we can start at play 1654 and be out of sample
ind = which(trackingWidenew$gameplayId==gameplayIds[1657])
playlength = length(ind) # length of play 
trackingsub = trackingWidenew[ind,]

pred = predict(mod, newdata = x_train_scaled[ind,], predict.all = T)$individual
# RMSE of this particular play
sqrt(mean((trackingsub$x_bc[playlength] - (trackingsub$x_bc-pred))^2))

### plotting

par(mfrow = c(2,1), mar = c(4, 4, 0.5, 2) + 0.1)
for(i in 1:playlength){
  plot(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 1, lwd = 6, col = "blue", xlim = c(0,min(round(max(trackingsub$x_bc),-1)+30,100)), ylim = c(-53.3/2, 53.3/2), xlab = "x", ylab = "y")
  abline(v = 1:20 *5, col = "lightgray", lwd = 0.5)
  abline(v = trackingsub$x_football[1], lwd = 1) # line of scrimmage
  abline(v = 0, lwd = 1.5) # endzone
  abline(v = trackingsub$x_bc[playlength]) # true eop yardline
  # plot predicted yard end yard line
  for(j in 1:1000){
    abline(v = trackingsub$x_bc[i]-pred[i,j], lwd = 2, col = alpha("chartreuse1", 0.01))
  }
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = 1, lwd = 2) # expected eop yardline
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = "chartreuse1", lwd = 1) # expected eop yardline
  points(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 1, lwd = 6, col = "blue")
  points(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 16, col = "blue")
  for(j in 2:11){
    points(trackingsub[i,paste0("x_or_off", j)], trackingsub[i, paste0("y_or_off", j)], pch = 20, col = "blue")
  }
  for(j in 1:11){
    points(trackingsub[i,paste0("x_or_def", j)], trackingsub[i, paste0("y_or_def", j)], pch = 20, col = "red")
  }
  points(trackingsub$x_football[i], trackingsub$y_football[i], pch = 9, col = "coral1", lwd = 2)
  plot(density(trackingsub$x_bc[i]-pred[i,], bw = 1), col = "chartreuse1", lwd = 2, xlim = c(0,min(round(max(trackingsub$x_bc),-1)+30,100)), bty = "n", main = "", xlab = "eop yardline")
  abline(v = 1:20 *5, col = "lightgray", lwd = 0.5)
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = 1, lwd = 2)
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = "chartreuse1", lwd = 1)
  abline(v = trackingsub$x_bc[playlength])
  Sys.sleep(0.4)
}



# Train a model only on plays with missed tackles -------------------------

missed = tackles %>% filter(pff_missedTackle==1)
missed$gameplayId = paste0(missed$gameId, missed$playId)
missed_gameplayIds = unique(missed$gameplayId) # all gameplayIds where a tackle was missed

x_train_missed = trackingWidenew %>% 
  filter(gameplayId %in% missed_gameplayIds) %>% 
  select(-c(x_player_off1:def_dir_bc_diff11, nflId_or_off1:yards_gained)) %>% 
  select(-(gameplayId:ballCarrierId)) %>% 
  select(-dist_bc_or_off1, -xdist_bc_or_off1,- ydist_bc_or_off1) # exclude dist(bc,bc)
colnames(x_train_missed)
nrow(x_train_missed)

x_train_missed_scaled = as.data.frame(scale(x_train_missed))

n_train = 60000 # my RAM does not allow for more
# this means I have about 40.000 frames for model evaluation on missed tackles
na.inds = apply(is.na(x_train_missed_scaled[1:n_train,]), 1, sum)
na.inds = which(na.inds > 0)
na.inds = as.numeric(na.inds)
train.ind = setdiff(1:n_train, na.inds) # exclude NAs

trackingWideMissed = trackingWidenew %>% 
  filter(gameplayId %in% missed_gameplayIds) 

# mod_missed = randomForest(trackingWideMissed$yards_gained[train.ind] ~., data = x_train_missed_scaled[train.ind,], ntree = 1000)
# saveRDS(mod_missed, "mod_missed.rds")

mod_missed = readRDS("mod_missed.rds")

pred_missed = predict(mod_missed, x_train_missed_scaled[60001:nrow(x_train_missed_scaled),]) # point predictions

# RMSE out of sample
errors = trackingWideMissed$yards_gained[60001:nrow(x_train_missed)]-pred_missed
sqrt(mean(errors^2, na.rm = T))
# RMSE of ~8.9 yards out of sample is okay as plays with missed tackles are quite complex and 
# conditional density prediction often shows multimodality (RMSE does not tell the whole story)

# RMSE in sample
errors = trackingWideMissed$yards_gained[1:60000]-predict(mod_missed, x_train_missed_scaled[1:60000,])
sqrt(mean(errors^2, na.rm = T))
# really on point


### plotting the missed model

n_train = 60000
gameplayIds = unique(trackingWideMissed$gameplayId)
oos = which(gameplayIds == trackingWideMissed$gameplayId[n_train]) # predict out of sample but for missed situation
gameplayIds_tackle = setdiff(unique(trackingWidenew$gameplayId), gameplayIds) # gameplayIds with tackle

# predict for a missed situation out of sample
ind = which(trackingWideMissed$gameplayId==gameplayIds[oos+1])
trackingsub = trackingWideMissed[ind,]
prediction = predict(mod_missed, newdata = x_train_missed_scaled[ind,], predict.all = T)

# predict for a tackle situation out of sample
ind = which(trackingWidenew$gameplayId==gameplayIds_tackle[15])
trackingsub = trackingWidenew[ind,]
prediction = predict(mod_missed, newdata = x_train_scaled[ind,], predict.all = T)

pred = prediction$individual
(playlength = length(ind))

# RMSE
sqrt(mean((trackingsub$x_bc[playlength] - (trackingsub$x_bc-pred))^2))

tackle_ind = which(trackingsub$event=="tackle")
i = tackle_ind-10

par(mfrow = c(2,1), mar = c(4, 4, 0.5, 2) + 0.1)
for(i in 1:playlength){
  plot(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 1, lwd = 6, col = "blue", xlim = c(0,min(round(max(trackingsub$x_bc),-1)+30,100)), ylim = c(-53.3/2, 53.3/2), xlab = "x", ylab = "y")
  abline(v = 1:20 *5, col = "lightgray", lwd = 0.5)
  
  abline(v = trackingsub$x_football[1], lwd = 1) # line of scrimmage
  abline(v = 0, lwd = 1.5) # endzone
  abline(v = trackingsub$x_bc[playlength]) # true eop yardline
  
  # plot predicted yard end yard line
  for(j in 1:1000){
    abline(v = trackingsub$x_bc[i]-pred[i,j], lwd = 2, col = alpha("chartreuse1", 0.01))
  }
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = 1, lwd = 2) # expected eop yardline
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = "chartreuse1", lwd = 1) # expected eop yardline
  
  points(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 1, lwd = 6, col = "blue")
  points(trackingsub$x_bc[i], trackingsub$y_bc[i], pch = 16, col = "blue")
  
  for(j in 2:11){
    points(trackingsub[i,paste0("x_or_off", j)], trackingsub[i, paste0("y_or_off", j)], pch = 20, col = "blue")
  }
  for(j in 1:11){
    points(trackingsub[i,paste0("x_or_def", j)], trackingsub[i, paste0("y_or_def", j)], pch = 20, col = "red")
  }
  points(trackingsub$x_football[i], trackingsub$y_football[i], pch = 9, col = "coral1", lwd = 2)
  
  plot(density(trackingsub$x_bc[i]-pred[i,], bw = 2.5), col = "chartreuse1", lwd = 2, xlim = c(0,min(round(max(trackingsub$x_bc),-1)+30,100)), bty = "n", main = "", xlab = "eop yardline")
  abline(v = 1:20 *5, col = "lightgray", lwd = 0.5)
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = 1, lwd = 2)
  abline(v = trackingsub$x_bc[i] - mean(pred[i,]), col = "chartreuse1", lwd = 1)
  abline(v = trackingsub$x_bc[playlength])
  
  Sys.sleep(0.4)
}


# when predicting for a play without a missed tackle, we see that the model, as it is used to the first
# tackle(s) being missed is much more optimistic regarding the predicted eop yardline, once the ballcarrier
# crosses the line of scrimmage. We can calculate the within play EPs from this model and average in the interval:
# line of [scrimmage crossed, 1 sec before tackle] where the model mostly assumes any potential tackle attempt wil be missed.
# Therefore we get an estimate of the expected points if the tackle was missed that we can compare to the true tackle to obtain a tackle value on EP scale.
# We calculate expected points from the entire density estimate.










