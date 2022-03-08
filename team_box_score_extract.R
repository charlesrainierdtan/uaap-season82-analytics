library(stringr)
library(jsonlite)
library(tidyverse)
library(httr)
library(rlist)

rm(list = ls())

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
  
  team_minutes <- (game$data$period - 4 ) * 5 + 40
  
  #Team Information
  home <- unlist(game$data$game_statistics$home,recursive = TRUE)[1:5] %>% as_tibble(rownames = "attr") %>% 
    spread(attr,value) %>% mutate(home_away = "home")
  
  away <- unlist(game$data$game_statistics$away,recursive = TRUE)[1:5] %>% as_tibble(rownames = "attr") %>% 
    spread(attr,value) %>% mutate(home_away = "away")
  
  teams <- union(home,away)
  
  
  
  home_box_score <- 
    as_tibble(game$data$game_statistics$boxscore$home$stats) %>% 
      cbind(period = c("Total", 1:(nrow(.) -1) )) %>% 
      mutate(team_id = game$data$game_statistics$boxscore$home$team_id,
             team_minutes = if_else(period == "Total",(game$data$period - 4 ) * 5 + 40,10)
             )
  
  away_box_score <- 
    as_tibble(game$data$game_statistics$boxscore$away$stats) %>% 
    cbind(period = c("Total", 1:(nrow(.) -1) )) %>% 
    mutate(team_id = game$data$game_statistics$boxscore$away$team_id,
           team_minutes = if_else(period == "Total",(game$data$period - 4 ) * 5 + 40,10))
  
  game_box_score <- union(home_box_score,away_box_score) %>% 
    left_join(teams,by = c("team_id" = "id")) %>% 
    mutate(game_no  = game$data$game_no,
           schedule = game$data$schedule,
           venue = game$data$venue
    )
  

  
  if (exists("leauge_box_score_conso")){
    leauge_box_score_conso <-  plyr::rbind.fill(game_box_score,leauge_box_score_conso)
  } else {
    leauge_box_score_conso <- bind_rows(game_box_score)
  }
  
}

leauge_box_score_conso %>% filter(period == 'Total') %>% distinct(game_no) %>% nrow() == g - 6 + 1 


write_csv(leauge_box_score_conso, path = "./data_raw/team_box_score.csv")





