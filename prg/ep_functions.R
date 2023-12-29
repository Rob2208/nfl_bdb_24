######################################################################################
######################################################################################
#### EP calculation functions
######################################################################################
######################################################################################



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


eop_yardline_to_pbp <- function(yardline_pred,playId,pbp,is_yards_gained = FALSE){
  
  play_dets <- pbp %>% filter(uId == playId) %>% select(half_seconds_remaining,
                                                        yardline_100,
                                                        roof,
                                                        season,qtr,down,
                                                        ydstogo,
                                                        score_differential,
                                                        posteam_timeouts_remaining,defteam_timeouts_remaining,
                                                        posteam,defteam,home_team
  )
  
  if(is_yards_gained){
    yards_gained <- yardline_pred
  }else{
    yards_gained <- (play_dets$yardline_100 - yardline_pred)
  }
  
  
  half_seconds_remaining = play_dets$half_seconds_remaining - mean_pl$avg[match(round(yards_gained),mean_pl$playResult,nomatch = 22)] ## very crude approximation atm
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


predict_frame_eps <- function(frame,playId,ep_model,pbp,sel_cols = NULL, is_yards_gained = FALSE){
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
  frame_df <- eop_yardline_to_pbp(frame,playId = playId,pbp = pbp,is_yards_gained = is_yards_gained) %>%
    make_model_mutations() %>% ### from ep_model file
    select(all_of(sel_cols),posteam) ## 
  
  calculate_ep(frame_df %>% select(-posteam),ep_model) %>%
    mutate(pos_team = frame_df %>% pull(posteam))
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