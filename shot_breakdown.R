library(tidyverse)

pbp <-  read_csv("data_raw/league_conso_pbp.csv",
                 col_types = cols(
                   .default = col_character(),
                   game_clock = col_double(),
                   period = col_double(),
                   success = col_character(),
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

zone_mapping <- tibble(
  zone = 1:14 , zone_name = c(
    "corner_three",
    "wing_three",
    "top_key_three",
    "wing_three",
    "corner_three",
    "short_corner_two",
    "elbow_two",
    "top_key_two",
    "elbow_two",
    "short_corner_two",
    "high_paint",
    "high_paint",
    "low_paint",
    "low_paint"
  )
)



player_shot_dist <- 
pbp %>% 
  filter(type == "Actions::ShotAttempt") %>% 
  mutate(zone = ifelse(zone == 0,14,zone)) %>% 
  left_join(zone_mapping, by = "zone") %>% 
  mutate(shot_zone = paste0(zone_name,"_",ifelse(success == "true","made","missed") ),
         name = ifelse(name == "ADMU","ATENEO",name)) %>% 
  group_by(player_id,shot_zone) %>% summarise(count=n()) %>% ungroup() %>% 
  spread(shot_zone,count) %>% 
  replace(is.na(.),0) %>% 
  mutate(corner_three_attempts = corner_three_made + corner_three_missed,
         wing_three_attempts = wing_three_made + wing_three_missed,
         top_key_three_attempts = top_key_three_made + top_key_three_missed,
         short_corner_two_attempts = short_corner_two_made + short_corner_two_missed,
         elbow_two_attempts = elbow_two_made + elbow_two_missed,
         top_key_two_attempts = top_key_two_made + top_key_two_missed,
         high_paint_attempts = high_paint_made + high_paint_missed,
         low_paint_attempts = low_paint_made + low_paint_missed,
         perimeter_two_made = short_corner_two_made + top_key_two_made + elbow_two_made,
         perimeter_two_missed = short_corner_two_missed + top_key_two_missed + elbow_two_missed,
         perimeter_two_attempts = perimeter_two_made + perimeter_two_missed,
         paint_made = high_paint_made + low_paint_made,
         paint_attempts = high_paint_attempts + low_paint_attempts,
         perimeter_made = perimeter_two_made + corner_three_made + wing_three_made + top_key_three_made,
         perimeter_attempts = perimeter_two_attempts + corner_three_attempts + wing_three_attempts + top_key_three_attempts
         )

team_shot_dist_1 <- 
  pbp %>% 
  filter(type == "Actions::ShotAttempt") %>% 
  mutate(zone = ifelse(zone == 0,14,zone)) %>% 
  left_join(zone_mapping, by = "zone") %>% 
  mutate(shot_zone = paste0(zone_name,"_",ifelse(success == "true","made","missed") ),
         name = ifelse(name == "ADMU","ATENEO",name),
         opp_team = ifelse(home_away =="home","away","home")) %>% 
  group_by(game_no,team_id.x,home_away,opp_team,shot_zone) %>% summarise(count=n()) %>% ungroup() %>% 
  spread(shot_zone,count) %>% 
  replace(is.na(.),0) %>% 
  mutate(corner_three_attempts = corner_three_made + corner_three_missed,
         wing_three_attempts = wing_three_made + wing_three_missed,
         top_key_three_attempts = top_key_three_made + top_key_three_missed,
         short_corner_two_attempts = short_corner_two_made + short_corner_two_missed,
         elbow_two_attempts = elbow_two_made + elbow_two_missed,
         top_key_two_attempts = top_key_two_made + top_key_two_missed,
         high_paint_attempts = high_paint_made + high_paint_missed,
         low_paint_attempts = low_paint_made + low_paint_missed,
         perimeter_two_made = short_corner_two_made + top_key_two_made + elbow_two_made,
         perimeter_two_missed = short_corner_two_missed + top_key_two_missed + elbow_two_missed,
         perimeter_two_attempts = perimeter_two_made + perimeter_two_missed,
         paint_made = high_paint_made + low_paint_made,
         paint_attempts = high_paint_attempts + low_paint_attempts,
         perimeter_made = perimeter_two_made + corner_three_made + wing_three_made + top_key_three_made,
         perimeter_attempts = perimeter_two_attempts + corner_three_attempts + wing_three_attempts + top_key_three_attempts
  )

team_shot_dist <- 
  team_shot_dist_1 %>% 
  left_join(.,., by = c("game_no" = "game_no", "opp_team" = "home_away"  ),
            suffix = c(".team",".opp"))

write_csv(player_shot_dist,"data_raw/player_shot_dist.csv")
write_csv(team_shot_dist,"data_raw/team_shot_dist.csv")
