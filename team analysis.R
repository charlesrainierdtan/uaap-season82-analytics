library(tidyverse)


rm(list = ls())

team_shot_dist <- read_csv("data_raw/team_shot_dist.csv")

team_box_score <-  read_csv("data_raw/team_box_score.csv")  %>% 
  #Get only end of game stats
  filter(period == "Total") %>%  
  #Rearrange columns
  select(game_no,schedule,venue,home_away,team_id,name,abbreviation,nickname,photo_url,everything()) %>%
  #Fix anomaly in team name and add helper column for joining to opponent data 
  mutate(name = ifelse(name == "ADMU","ATENEO",name),
         opp_team = ifelse(home_away =="home","away","home")
           ) %>% 
  #join opposing team's box score data 
  left_join(.,., by = c("game_no" = "game_no", "opp_team" = "home_away"  ),
            suffix = c(".team",".opp")) %>% 
  left_join(team_shot_dist,by = c("game_no" = "game_no", "team_id.team" = "team_id.x.team")) %>% 
  #add possession and pace metrics
  mutate(team_possessions = .96 * (field_goal_attempts.team + turnovers.team + .475 * free_throw_attempts.team - offensive_rebounds.team),
         opp_possessions  = .96 * (field_goal_attempts.opp + turnovers.opp + .475 * free_throw_attempts.opp - offensive_rebounds.opp),
         pace = 40 * ((team_possessions + opp_possessions) / (2*(team_minutes.team))),
         point_differential = points.team - points.opp,
         win = ifelse(point_differential > 0,1,0),
         lose = ifelse(point_differential < 0,1,0)
         )

team_metrics <- 
team_box_score %>% 
  group_by(team_id.team,name.team,nickname.team) %>% 
  summarise(win = sum(win,na.rm = TRUE),
            lose = sum(lose,na.rm = TRUE),
            total_possessions = sum(team_possessions,na.rm = TRUE),
            ortg = sum(points.team,na.rm = TRUE)/total_possessions * 100, #Offensive Rating
            drtg = sum(points.opp,na.rm=TRUE)/sum(opp_possessions,na.rm = TRUE) * 100, #Defensive Rating
            net_rtg = ortg - drtg,
            pace = mean(pace,na.rm = TRUE),
            avg_point_differential = mean(point_differential,na.rm = TRUE),
            #Offensive Four Factors
            efg = sum(field_goals_made.team + .5* three_points_made.team)/sum(field_goal_attempts.team),
            tov_rate = sum(turnovers.team) / sum(field_goal_attempts.team + .44 * free_throw_attempts.team + turnovers.team),
            oreb_rate = sum(offensive_rebounds.team)/(sum(offensive_rebounds.team) + sum(defensive_rebounds.opp)),
            ft_rate = sum(free_throws_made.team)/sum(field_goal_attempts.team),
            #Defensive Four Factos  
            efg_def = sum(field_goals_made.opp + .5* three_points_made.opp)/sum(field_goal_attempts.opp),
            tov_rate_def = sum(turnovers.opp) / sum(field_goal_attempts.opp + .44 * free_throw_attempts.opp + turnovers.opp),
            dreb_rate = sum(defensive_rebounds.team)/(sum(offensive_rebounds.opp) + sum(defensive_rebounds.team)),
            ft_rate_def = sum(free_throws_made.opp)/sum(field_goal_attempts.opp),
            #Other Metrics
            ast_rate = sum(assists.team) /(sum(field_goal_attempts.team) + sum(free_throw_attempts.team)*.44 + sum(assists.team) +sum(turnovers.team)),
            contested_fg_pct =  sum(contested_field_goals_made.team)/sum(contested_field_goal_attempts.team),
            uncontested_fg_pct = sum(uncontested_field_goals_made.team)/sum(uncontested_field_goal_attempts.team),
            contested_rate = sum(contested_field_goal_attempts.team) / sum(field_goal_attempts.team),
            contested_fg_pct_def = sum(contested_field_goals_made.opp) / sum(contested_field_goal_attempts.opp),
            contested_rate_def = sum(contested_field_goal_attempts.opp) / sum(field_goal_attempts.opp),
            uncontested_rate = sum(uncontested_field_goal_attempts.team) / sum(field_goal_attempts.team),
            #FG % per shot zone
            corner_three_fg_pct = sum(corner_three_made.team) / sum(corner_three_attempts.team),
            wing_three_fg_pct = sum(wing_three_made.team) / sum(wing_three_attempts.team),
            top_key_three_fg_pct = sum(top_key_three_made.team) / sum(top_key_three_attempts.team),
            short_corner_two_fg_pct = sum(short_corner_two_made.team) / sum(short_corner_two_attempts.team),
            elbow_two_fg_pct = sum(elbow_two_made.team) / sum(elbow_two_attempts.team),
            top_key_two_fg_pct = sum(top_key_two_made.team) / sum(top_key_two_attempts.team),
            high_paint_fg_pct = sum(high_paint_made.team) / sum(high_paint_attempts.team),
            low_paint_fg_pct = sum(low_paint_made.team) / sum(low_paint_attempts.team),
            #Perimeter/Paint FG%
            perimeter_fg_pct = sum(perimeter_made.team) / sum(perimeter_attempts.team),
            perimeter_two_fg_pct = sum(perimeter_two_made.team) / sum(perimeter_two_attempts.team),
            paint_fg_pct = sum(paint_made.team) / sum(paint_attempts.team),
            #FG Defense per shot zone
            corner_three_def = sum(corner_three_made.opp) / sum(corner_three_attempts.opp),
            wing_three_def = sum(wing_three_made.opp) / sum(wing_three_attempts.opp),
            top_key_three_def = sum(top_key_three_made.opp) / sum(top_key_three_attempts.opp),
            short_corner_two_def = sum(short_corner_two_made.opp) / sum(short_corner_two_attempts.opp),
            elbow_two_def = sum(elbow_two_made.opp) / sum(elbow_two_attempts.opp),
            top_key_two_def = sum(top_key_two_made.opp) / sum(top_key_two_attempts.opp),
            high_paint_def = sum(high_paint_made.opp) / sum(high_paint_attempts.opp),
            low_paint_def = sum(low_paint_made.opp) / sum(low_paint_attempts.opp),            
            #Perimeter/Paint Defense
            perimeter_def = sum(perimeter_made.opp) / sum(perimeter_attempts.opp),
            perimeter_two_def = sum(perimeter_two_made.opp) / sum(perimeter_two_attempts.opp),
            paint_def = sum(paint_made.opp) / sum(paint_attempts.opp),
            fastbreak_ppa = sum(fastbreak_points.team) / sum(fastbreak_attempts.team),
            fastbreak_attempts = sum(fastbreak_attempts.team),
            fastbreak_points = sum(fastbreak_points.team),
            fastbreak_papa = sum(fastbreak_points.opp) / sum(fastbreak_attempts.opp),
            #Points Distribution
            perimeter_share = sum(perimeter_points.team) / sum(points.team),
            paint_share = sum(points_in_the_paint.team) / sum(points.team),
            ft_share = sum(free_throws_made.team) / sum(points.team),
            second_ppa = sum(second_chance_points.team) / sum(offensive_rebounds.team),
            second_chance_def = sum(second_chance_points.opp) / sum(offensive_rebounds.opp)
            ) %>% ungroup() %>% 
  mutate(
    #Offensive FF League Avgs
    league_avg_efg = mean(efg),
    league_avg_oreb_rate = mean(oreb_rate),
    league_avg_tov_rate = mean(tov_rate),
    league_avg_ft_rate = mean(ft_rate),
    #Defensive FF League Avgs
    league_avg_efg_def = mean(efg_def),
    league_avg_dreb_rate = mean(dreb_rate),
    league_avg_tov_rate_def = mean(tov_rate_def),
    league_avg_ft_rate_def = mean(ft_rate_def),
    #Offensive FF differential from league average
    efg_diff = efg - league_avg_efg,
    oreb_rate_diff = oreb_rate - league_avg_oreb_rate,
    tov_rate_diff = tov_rate - league_avg_tov_rate,
    ft_rate_diff = ft_rate - league_avg_ft_rate,
    #Defensive FF differential from league average
    efg_def_diff = efg_def - league_avg_efg_def,
    dreb_rate_diff = dreb_rate - league_avg_dreb_rate,
    tov_rate_diff_def = tov_rate_def - league_avg_tov_rate_def,
    ft_rate_def_diff = ft_rate_def - league_avg_ft_rate_def

  ) %>% 
  arrange(desc(win))

write_csv(team_metrics,"data/team_metrics.csv")

team_metrics %>%
  select(team_id.team,team,oreb_rate,second_chance_success) %>% 
  arrange(desc(oreb_rate)) %>% 
  ggplot(aes(x=oreb_rate,y=second_chance_success)) +
  geom_point(aes(color = team))


team_metrics %>%
  select(team_id.team,team,perimeter_share,paint_share,ft_share) %>% 
  arrange(desc(perimeter_share))

team_metrics %>% 
  select(team_id.team,team,fastbreak_success_rate,fastbreak_points,fastbreak_attempts,fastbreak_def_rate) %>% 
  arrange(desc(fastbreak_success_rate))

team_metrics %>% 
  select(team,contested_fg_pct,uncontested_fg_pct,contested_rate,uncontested_rate,contested_fg_pct_def,contested_rate_def) %>% 
  arrange(desc(uncontested_rate))
