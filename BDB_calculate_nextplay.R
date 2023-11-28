library(dplyr)
exp_yline <- 13
prev_yline <- 12
prev_down <- 4
prev_ytg <- 4
prev_goal_to_go <- FALSE

# Alles unter der Annahme: kein TD
# NA heißt, dass der Ballbesitz wechselt aufgrund von Loss of Downs
calculate_next <- function(exp_yline, prev_yline, prev_down, prev_ytg, prev_goal_to_go){
next_down <- case_when(
  prev_yline - exp_yline >= prev_ytg  ~ 1,
  prev_down < 4 & prev_yline - exp_yline < prev_ytg  ~ prev_down + 1,
  prev_down == 4 & prev_yline - exp_yline < prev_ytg  ~ NA,
)
next_ytg <- case_when(
  prev_yline - exp_yline >= prev_ytg & exp_yline >= 10 ~ 10,
  prev_yline - exp_yline >= prev_ytg & exp_yline < 10 ~ exp_yline,
  prev_down < 4 & prev_yline - exp_yline < prev_ytg  ~ prev_ytg - (prev_yline - exp_yline),
  prev_down == 4 & prev_yline - exp_yline < prev_ytg  ~ NA,
)
next_goal_to_go <- case_when(
  prev_goal_to_go == FALSE & prev_down < 4 & prev_yline - exp_yline >= prev_ytg & 
    exp_yline <= 10 ~ TRUE,
  prev_goal_to_go == FALSE & prev_down < 4 & prev_yline - exp_yline < prev_ytg & 
    exp_yline <= 10 ~ FALSE,
  prev_goal_to_go == FALSE & prev_down < 4 & prev_yline - exp_yline >= prev_ytg & 
    exp_yline > 10 ~ FALSE,
  prev_goal_to_go == FALSE & prev_down < 4 & prev_yline - exp_yline < prev_ytg & 
    exp_yline > 10 ~ FALSE,
  prev_goal_to_go == TRUE & prev_down < 4 & prev_yline - exp_yline < prev_ytg ~ TRUE,
  prev_goal_to_go == TRUE & prev_down == 4 & prev_yline - exp_yline < prev_ytg ~ NA,
)
return(list(next_down = next_down, next_ytg = next_ytg, next_goal_to_go = next_goal_to_go))
}

calculate_next(exp_yline, prev_yline, prev_down, prev_ytg, prev_goal_to_go)
