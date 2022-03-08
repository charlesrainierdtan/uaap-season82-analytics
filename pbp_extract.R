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

#Raw Play by Play data
play_by_play <- as_tibble(game$data$game_statistics$play_by_play)


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


#consolidate play-by-play, player, and team data
game_data <-  play_by_play %>% 
  left_join( union(home_players,away_players),by = c("player_id" = "id")) %>% 
  left_join(teams,by=c("team_id.x" = "id")) %>% 
  mutate(game_id = g,
         game_no  = game$data$game_no,
         schedule = game$data$schedule,
         venue = game$data$venue
         ) 


if (exists("leauge_conso_pbp")){
leauge_conso_pbp <-  union(leauge_conso_pbp,game_data)
} else {
  leauge_conso_pbp <- bind_rows(game_data)
}

}





write_csv(leauge_conso_pbp,path = "./data_raw/league_conso_pbp.csv")









  
