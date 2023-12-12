
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(vroom)


# Load data ---------------------------------------------------------------

games = read.csv("data/games.csv")
plays = read.csv("data/plays.csv")
tackles = read.csv("data/tackles.csv")
tracking = vroom("data/tracking_week_1.csv")
nweeks = 3 # only 3 weeks for data size
for(i in 2:nweeks){
  print(i)
  tracking = rbind(tracking, vroom(paste0("data/tracking_week_",i,".csv")))
}

# creating adjusted x value
leftInd = which(tracking$playDirection == "left")
tracking$x[leftInd] = tracking$x[leftInd] - 10
tracking$x[-leftInd] = (110) - tracking$x[-leftInd]
# creating adjusted y value
tracking$y = tracking$y - (53.3/2)

# creating adjusted direction
tracking$dir[leftInd] = (tracking$dir[leftInd] + 90) %% 360
tracking$dir[-leftInd] = (tracking$dir[-leftInd] + (360-90)) %% 360

plays$gameplayId = as.numeric(paste0(plays$gameId, plays$playId))
tracking$gameplayId = as.numeric(paste0(tracking$gameId, tracking$playId))
tackles$Id = tackles$gameplayId = as.numeric(paste0(tackles$gameId, tackles$playId))

plays_sub = plays %>% select(gameplayId, ballCarrierId)
tracking = tracking %>% merge(plays_sub, by = c("gameplayId"))

trackingList = split(tracking, tracking$gameplayId)

playernames = c(paste0("player_off", 1:11), paste0("player_def", 1:11), "football")

# trackingList = trackingList[1:1000]

nObs = as.numeric(lapply(trackingList, nrow))
out = which(nObs < 253)

build_tackledummy = function(trackingPlay){
  tackles_sub = tackles %>% filter(gameplayId == unique(trackingPlay$gameplayId))
  tackles_sub = tackles_sub %>% filter((tackle == 1 & pff_missedTackle == 0) | (assist == 1 & pff_missedTackle == 0))
  
  tackledummy = rep(0, nrow(trackingPlay) / 23)
  if(nrow(tackles_sub > 0)){
    for(i in 1:nrow(tackles_sub))
      trackingPlayTackler = trackingPlay %>% filter(nflId == tackles_sub$nflId[i])
    dist = sqrt((trackingPlayTackler$x_bc-trackingPlayTackler$x)^2 + (trackingPlayTackler$y_bc-trackingPlayTackler$y)^2)
    
    newdummy = rep(0, nrow(trackingPlayTackler))
    newdummy[which.min(dist)] = 1
    
    tackledummy = tackledummy + newdummy
  }
  return(tackledummy)
}

build_missed_tackledummy = function(trackingPlay){
  tackles_sub = tackles %>% filter(gameplayId == unique(trackingPlay$gameplayId))
  tackles_sub = tackles_sub %>% filter(tackle == 0, pff_missedTackle == 1)
  
  tackledummy = rep(0, nrow(trackingPlay) / 23)
  if(nrow(tackles_sub > 0)){
    for(i in 1:nrow(tackles_sub))
      trackingPlayTackler = trackingPlay %>% filter(nflId == tackles_sub$nflId[i])
    dist = sqrt((trackingPlayTackler$x_bc-trackingPlayTackler$x)^2 + (trackingPlayTackler$y_bc-trackingPlayTackler$y)^2)
    
    newdummy = rep(0, nrow(trackingPlayTackler))
    newdummy[which.min(dist)] = 1
    
    tackledummy = tackledummy + newdummy
  }
  return(tackledummy)
}


tracking_reorder = trackingList[-out] %>% 
  # tracking_reorder = trackingList[1:30] %>% 
  map(~mutate(., isballCarrier = ifelse(nflId == ballCarrierId, 1, 0))) %>% 
  map(~mutate(., inballCarrierTeam = ifelse(club == club[which(isballCarrier==1)][1],1,0))) %>% 
  map(~mutate(., x_bc = rep(x[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., y_bc = rep(y[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., s_bc = rep(s[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., a_bc = rep(a[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., dis_bc = rep(dis[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., o_bc = rep(o[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., dir_bc = rep(dir[which(isballCarrier == T)], 23))) %>% 
  map(~mutate(., tackle_dummy = rep(build_tackledummy(.), 23))) %>% 
  map(~mutate(., missed_tackle_dummy = rep(build_missed_tackledummy(.), 23))) %>% 
  map(~arrange(., frameId, -inballCarrierTeam)) %>% 
  map(~mutate(., isFootball = ifelse(displayName=="football", 1, 0))) %>% 
  map(~mutate(., player = rep(playernames, max(unique(frameId))))) %>% 
  bind_rows()

trackingWide = tracking_reorder %>% 
  select(-isballCarrier, -inballCarrierTeam, -isFootball) %>% 
  pivot_wider(names_from = player, values_from = c(x, y, s, a, dis, o, dir, nflId, displayName, jerseyNumber, club))

## reorder columns: dummy columns to the end
trackingWide = trackingWide %>% 
  select(gameplayId:dir_bc, x_player_off1:club_football, tackle_dummy, missed_tackle_dummy)

# creating treatment variable
trackingWide$treatment = "none"
trackingWide$treatment[which(trackingWide$tackle_dummy==1)] = "tackle"
trackingWide$treatment[which(trackingWide$missed_tackle_dummy==1)] = "missed_tackle"
trackingWide$treatment = as.factor(trackingWide$treatment)

# L2 distance to ballcarrier
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = sqrt((trackingWide$x_bc - trackingWide[,15+i])^2 + (trackingWide$y_bc - trackingWide[,38+i])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_bc_off",i))
}
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = sqrt((trackingWide$x_bc - trackingWide[,26+i])^2 + (trackingWide$y_bc - trackingWide[,49+i])^2)
  colnames(trackingWide) = c(oldcolnames, paste0("dist_bc_def",i))
}
# x distance to ballcarrier
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  trackingWide$distance = trackingWide$x_bc - trackingWide[,15+i]
  colnames(trackingWide) = c(oldcolnames, paste0("xdist_bc_off",i))
}
for(i in 1:11){
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
# angle to shortest segment defenders
for(i in 1:11){
  oldcolnames = colnames(trackingWide)
  shortest_segments = cbind(trackingWide$x_bc, trackingWide$y_bc) - cbind(trackingWide[,26+i],trackingWide[,49+i])
  angles = atan(shortest_segments[,2]/shortest_segments[,1])
  angles = angles*180/pi
  angles = (360 - angles + 540) %% 360
  trackingWide$newangle = abs(angles - trackingWide[,164+i])
  colnames(trackingWide) = c(oldcolnames, paste0("def_dir_bc_diff",i))
}

# reorder columns again

trackingWide = trackingWide %>% 
  select(gameplayId:club_football, dist_bc_off1:def_dir_bc_diff11, tackle_dummy, missed_tackle_dummy, treatment)

# reordering --> closest to ballcarrier

x_ordered = y_ordered = s_ordered = a_ordered = dis_ordered = o_ordered = dir_ordered = nflId_ordered = name_ordered = jersey_ordered = club_ordered = dist_bc_ordered = xdist_ordered = ydist_ordered = data.frame(matrix(nrow = nrow(trackingWide), ncol = 22))
def_dir_bc_diff_ordered = data.frame(matrix(nrow = nrow(trackingWide), ncol = 11))

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

trackingWidenew = cbind(trackingWide, newcols, newcols_char)

# creating yard line at the end of play
library(tidyverse)
trackingWideList = split(trackingWidenew, trackingWidenew$gameplayId)

# I don't know if this makes sense, yardline at eop just x location of ball carrier at the end
trackingWidenew = trackingWideList %>% 
  map(~mutate(., eop_yardline = x_bc[length(x_bc)])) %>% 
  bind_rows()

trackingWidenew$yards_gained = trackingWidenew$x_bc - trackingWidenew$eop_yardline

saveRDS(trackingWidenew, "data/trackingWide_week1-3.rds")

