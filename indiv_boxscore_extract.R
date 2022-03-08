library(stringr)
library(jsonlite)
library(tidyverse)
library(httr)
library(rlist)

rm(list = ls())

if (exists("league_player_box_score_conso")){
  rm(league_player_box_score_conso)
}





for (g in 6:61)
{
  
  #Get Game API
  resp <- GET(paste0("https://stats.livestats.ph/api/v1/games/",g,"/statistics"))
  http_type(resp)
  jsonRespText <- content(resp,as="text")
  jsonRespParsed <- content(resp,as="parsed")
  game <-  fromJSON(jsonRespText)
  
  
  #game level data
  game_header <- tibble(game_no  = game$data$game_no,
                        schedule = game$data$schedule,
                        venue = game$data$venue
  )
  
  #clear variables   
  if (exists("home")){
    rm(home)  
  }
  
  
  if (exists("away")){
    rm(away)  
  }
  
  
  if (exists("teams")){
    rm(teams)  
  }
  
  if (exists("home_players")){
    rm(home_players)  
  }
  
  if (exists("away_players")){
    rm(away_players)  
  }
  
  
  #Team Information
  home <- unlist(game$data$game_statistics$home,recursive = TRUE)[1:5] %>% as_tibble(rownames = "attr") %>% 
    spread(attr,value) %>% mutate(home_away = "home")
  away <- unlist(game$data$game_statistics$away,recursive = TRUE)[1:5] %>% as_tibble(rownames = "attr") %>% 
    spread(attr,value) %>% mutate(home_away = "away")
  teams <- union(home,away)
  

  #Player data per team
  home_players <- 
    as.data.frame(unlist(game$data$game_statistics$home$players) ) %>% 
    rownames_to_column(var = "rownames") %>%
    separate(rownames, into = c("id","attr"), sep = "\\." ,extra = "drop", fill = "right") %>%
    rename( values = `unlist(game$data$game_statistics$home$players)` ) %>% 
    spread(attr,values) %>% as_tibble()
  
  away_players <- 
    as.data.frame(unlist(game$data$game_statistics$away$players) ) %>% 
    rownames_to_column(var = "rownames") %>%
    separate(rownames, into = c("id","attr"), sep = "\\." ,extra = "drop", fill = "right") %>%
    rename( values = `unlist(game$data$game_statistics$away$players)` ) %>% 
    spread(attr,values) %>% as_tibble()

#get boxscores per home player    
  if (exists("home_player_box_score")){
    rm(home_player_box_score)  
  }
  
  if (exists("home_player_box_score_conso")){
    rm(home_player_box_score_conso)  
  }
  
  
  
  
  for (ph in 1:length(game$data$game_statistics$boxscore$home$players$player_id)) {
    
    
    
    home_player_box_score <-   
      as_tibble(game$data$game_statistics$boxscore$home$players$stats[[ph]]) %>% 
      cbind(period = c("Total", 1:(nrow(.) -1) )) %>% 
      mutate(player_id = game$data$game_statistics$boxscore$home$players$player_id[[ph]],
      ) 
    
    if (exists("home_player_box_score_conso")){
      home_player_box_score_conso <-   union(home_player_box_score_conso,home_player_box_score)
    } else {
      home_player_box_score_conso <- bind_rows(home_player_box_score)
    }
    
  }
  

#get boxscores per away player    
  if (exists("away_player_box_score")){
    rm(away_player_box_score)  
  }
  
  if (exists("home_player_box_score_conso")){
    rm(away_player_box_score_conso)  
  }
  
  
  for (pa in 1:length(game$data$game_statistics$boxscore$away$players$player_id)) {
    away_player_box_score <-   
      as_tibble(game$data$game_statistics$boxscore$away$players$stats[[pa]]) %>% 
      cbind(period = c("Total", 1:(nrow(.) -1) )) %>% 
      mutate(player_id = game$data$game_statistics$boxscore$away$players$player_id[[pa]]
      ) 
    
    if (exists("away_player_box_score_conso")){
      away_player_box_score_conso <-   union(away_player_box_score_conso,away_player_box_score)
    } else {
      away_player_box_score_conso <- bind_rows(away_player_box_score)
    }
    
  }
  


player_box_score_conso <- 
  union(home_player_box_score_conso,away_player_box_score_conso) %>% 
  left_join( union(home_players,away_players), by = c("player_id" = "id")) %>%
  left_join(teams,by= c("team_id" = "id")) %>% 
  mutate(
    game_no  = game$data$game_no,
    schedule = game$data$schedule,
    venue = game$data$venue
  )





  
if (exists("league_player_box_score_conso")){
  league_player_box_score_conso <-   plyr::rbind.fill(league_player_box_score_conso,player_box_score_conso)
} else {
  league_player_box_score_conso <- bind_rows(player_box_score_conso)
}
  
  
}



write_csv(league_player_box_score_conso,"./data_raw/boxscore_indiv.csv",na = "",append = FALSE)
















