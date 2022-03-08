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
                        venue = game$data$venue,
                        period = game$data$period,
                        home_team_id = game$data$game_statistics$home$id,
                        home_team_school = game$data$game_statistics$home$name,
                        home_team_nickname = game$data$game_statistics$home$nickname,
                        away_team_id = game$data$game_statistics$away$id,
                        away_team_school = game$data$game_statistics$away$name,
                        away_team_nickname = game$data$game_statistics$away$nickname,
  )
  
  if (exists("season_game_header")){
    season_game_header <-  plyr::rbind.fill(game_header,season_game_header)
  } else {
    season_game_header <- bind_rows(game_header)
  }
  
  
}

write_csv(season_game_header,"./data_raw/season_game_header.csv", append = FALSE)