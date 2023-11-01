######################################################################################
######################################################################################
#### visualisations
######################################################################################
######################################################################################

library(tidyverse)
library(crayon)
library(plotly)
library(gganimate)


###################################
#### visualize field
###################################

plot_field <- function(field_color="#00b140", line_color = "#ffffff",
                       start_x = 0,start_y = 0,
                       add_los = FALSE,
                       los_val = NULL,
                       aspect_rat = NULL) {
  field_height <- 160/3
  field_width <- 120
  
  if(is.null(aspect_rat)){
    aspect_rat <- field_height/field_width
  }
  
  field <- ggplot() +
    #theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      # legend.title = element_text(color = "#212529", size = 12, vjust = 1),
      legend.title.align = 1,
      # legend.text = element_text(color = "#343a40", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      #axis.text = element_blank(),
      #axis.line = element_blank(),
      # panel.background = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = aspect_rat
    ) +
    # major lines
    annotate(
      "segment",
      x = c(-start_x, -start_x, -start_x,field_width-start_x, seq(10, 110, by=5)-start_x),
      xend = c(field_width-start_x,field_width-start_x, 0-start_x, field_width-start_x, seq(10, 110, by=5)-start_x),
      y = c(0-start_y, field_height-start_y, 0-start_y, 0-start_y, rep(0, 21)-start_y),
      yend = c(0-start_y, field_height-start_y, field_height-start_y, field_height-start_y, rep(field_height, 21)-start_y),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4)-start_x,
      xend = rep(seq(10, 110, by=1), 4)-start_x,
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101))-start_y,
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101))-start_y,
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10)-start_x,
      y = rep(12, 9)-start_y,
      label = paste0(c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10)))),
      size = 5,
      #family = "mono",
      colour = line_color, # "#495057",
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10)-start_x,
      y = rep(field_height-12, 9)-start_y,
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 5,
      #family = "mono",
      colour = line_color, 
    )
  
  if(add_los){
    if(is.null(los_val)){
      field <- field + geom_vline(xintercept = start_x,linetype = "dashed",color = "red")
    }else{
      field <- field + geom_vline(xintercept = los_val,linetype = "dashed",color = "red")
    }
  }
  
  return(field)
}


plot_field()

### example usage: plot tackle frame

tackle_frames <- readRDS("data/tackle_frames_tracking.rds")

play1 <- tackle_frames %>% filter(uId == "2022090800_56")

plot_field(field_color="white", line_color = "black")+
  geom_point(data = play1,
            aes(x_std, y_std,color = club)) 

### example usage: plot full play

td1 <- read.csv("data/nfl-big-data-bowl-2024/tracking_week_1.csv") %>%
  mutate(uId = paste(gameId,playId,sep = "_"),
         x_std = ifelse(playDirection == "left", 120-x, x), ## Standardizes X
         y_std = ifelse(playDirection == "left", 160/3-y, y),  ## Standardized Y
         dir_std = ifelse(playDirection == "left", dir - 180, dir), ## Standardized dir
         x_sc_std = s*cos((90-dir_std)*pi/180) + x_std, 
         y_sc_std = s*sin((90-dir_std)*pi/180) + y_std) 

pbp <- read.csv("data/nfl-big-data-bowl-2024/plays.csv") %>%
  mutate(uId = paste(gameId,playId,sep = "_"))

play1_f <- td1 %>% filter(uId == "2022090800_56")
los <- 120 - pbp %>% filter(uId == "2022090800_56") %>% pull(absoluteYardlineNumber)

plot_field(field_color="white", line_color = "black",add_los = TRUE,los_val = los)+
  geom_path(data = play1_f,
             aes(x_std, y_std,color = club,group = nflId)) ### football tracking not accurate?? 


### play animation

play1_f <- td1 %>% filter(uId == "2022090800_80")
los <- 120 - pbp %>% filter(uId == "2022090800_80") %>% pull(absoluteYardlineNumber)

p_anim <- plot_field(field_color="white", line_color = "black",add_los = TRUE,los_val = los)+
  geom_path(data = play1_f,
            aes(x_std, y_std,group = nflId,color = club)) +
  scale_color_manual(values = c("blue","darkgreen","darkred")) +
  transition_reveal(frameId)+
  ease_aes('linear')

p_anim

#anim <- animate(p_anim,
#                width = 850,
#                height = 500,
#                end_pause = 10)

#anim_save("visualisations/play1.gif", anim)
