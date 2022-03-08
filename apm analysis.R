library(tidyverse)
library(rlist)

rm(list = ls())

indiv_box_socre <- read_csv("data_raw/boxscore_indiv.csv") %>% filter(period == "Total")

players <- indiv_box_socre %>% 
  mutate(name = ifelse(name == "ADMU","ATENEO",name)) %>% 
              group_by(player_id,team_id,name,display_name,pos) %>% summarise(n=n()) %>% arrange(n)

pbp <-  read_csv("data_raw/league_conso_pbp.csv",
         col_types = cols(
           .default = col_character(),
           game_clock = col_double(),
           period = col_double(),
           success = col_logical(),
           value = col_double(),
           x = col_double(),
           y = col_double(),
           zone = col_double(),
           home_score = col_double(),
           away_score = col_double(),
           shirt_no = col_double(),
           starter = col_logical(),
           game_id = col_double(),
           game_no = col_double()
         ))

game <- pbp %>% filter(game_no == 1)



#initialize starters
game <- cbind(game, home_players = NA, 
              away_players = NA,
              h1 = NA,
              h2 = NA,
              h3 = NA,
              h4 = NA,
              h5 = NA,
              a1 = NA,
              a2 = NA,
              a3 = NA,
              a4 = NA,
              a5 = NA
              ) %>% as_tibble()


home_players <- 
  game %>% 
  filter(home_away == "home" & type== "Actions::Start") %>% .$display_name

away_players <- 
  game %>% 
  filter(home_away == "away" & type== "Actions::Start") %>% .$display_name


game$home_players[1:13] = list(home_players)
game$away_players[1:13] = list(away_players)

game$h1[1:13] = home_players[[1]]
game$h2[1:13] = home_players[[2]]
game$h3[1:13] = home_players[[3]]
game$h4[1:13] = home_players[[4]]
game$h5[1:13] = home_players[[5]]

game$a1[1:13] = away_players[[1]]
game$a2[1:13] = away_players[[2]]
game$a3[1:13] = away_players[[3]]
game$a4[1:13] = away_players[[4]]
game$a5[1:13] = away_players[[5]]



for (i in 14:nrow(game)){
  
  
  if ((game$type[[i]] != "Actions::SubOut") |  (game$type[[i]] != "Actions::SubIn")) {
    game$home_players[[i]] = home_players
    game$away_players[[i]] = away_players
  }
  
  if ((game$type[[i]] == "Actions::SubOut") & (game$home_away[[i]] == "home")){
    home_players <- 
      home_players %>% unlist() %>% 
      .[. != game$display_name[i] ]
    game$home_players[[i]] = home_players
  }
  
  i

  
  if ((game$type[[i]] == "Actions::SubOut") & (game$home_away[[i]] == "away")){
    away_players <- 
      away_players %>% unlist() %>% 
      .[. != game$display_name[i] ] 
    game$away_players[[i]] = away_players
  }
  
  
  if ((game$type[[i]] == "Actions::SubIn") & (game$home_away[[i]] == "home")){
    home_players <- 
        append(home_players, game$display_name[i] )
    game$home_players[[i]] = home_players
  }
  
  if ((game$type[[i]] == "Actions::SubIn") & (game$home_away[[i]] == "away")){
    away_players <- 
      append(away_players, game$display_name[i] )
    game$away_players[[i]] = away_players
  }
  
  
  game$h1[[i]] = home_players[[1]]
  game$h2[[i]] = home_players[[2]]
  game$h3[[i]] = home_players[[3]]
  game$h4[[i]] = home_players[[4]]
  game$h5[[i]] = home_players[[5]]
  
  game$a1[[i]] = away_players[[1]]
  game$a2[[i]] = away_players[[2]]
  game$a3[[i]] = away_players[[3]]
  game$a4[[i]] = away_players[[4]]
  game$a5[[i]] = away_players[[5]]
}

         
game

gameplayers <- unique(unlist(game[,34:43]))
players <- unique()

         
shots <- game[which(game$type == "Actions::ShotAttempt" |
             ((game$type == "Actions::Rebound") & (game$sub_type == "defensive"))   |
             game$type == "Actions::Turnover" |
             game$type == "Actions::FreeThrowAttempt"
             ),]





