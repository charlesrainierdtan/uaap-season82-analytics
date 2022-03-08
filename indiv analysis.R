library(tidyverse)
library(stringr)

rm(list = ls())


team_box_score <-  read_csv("data_raw/team_box_score.csv")  %>% 
  #Get only end of game stats
  filter(period == "Total") %>%  
  #Rearrange columns
  select(game_no,schedule,venue,home_away,team_id,name,abbreviation,nickname,photo_url,everything()) %>%
  #Fix anomaly in team name and add helper column for joining to opponent data 
  mutate(name = ifelse(name == "ADMU","ATENEO",name),
         opp_team = ifelse(home_away =="home","away","home")
  ) %>% 
  #filter(name %in% c("UP","ATENEO") ) %>% 
  #join opposing team's box score data 
  left_join(.,., by = c("game_no" = "game_no", "opp_team" = "home_away"  ),
            suffix = c(".team",".opp")) %>% 
  #add possession and pace metrics
  mutate(team_possessions = .96 * (field_goal_attempts.team + turnovers.team + .475 * free_throw_attempts.team - offensive_rebounds.team),
         opp_possessions  = .96 * (field_goal_attempts.opp + turnovers.opp + .475 * free_throw_attempts.opp - offensive_rebounds.opp),
         pace = 40 * ((team_possessions + opp_possessions) / (2*(team_minutes.team))),
         point_differential = points.team - points.opp,
         win = ifelse(point_differential > 0,1,0),
         lose = ifelse(point_differential < 0,1,0)
  )

individual_box_score <- read_csv("data_raw/boxscore_indiv.csv",
                                 col_types = cols(
                                   .default = col_double(),
                                   started = col_logical(),
                                   period = col_character(),
                                   player_id = col_character(),
                                   display_name = col_character(),
                                   photo_url.x = col_character(),
                                   pos = col_character(),
                                   short_name = col_character(),
                                   starter = col_logical(),
                                   team_id = col_character(),
                                   abbreviation = col_character(),
                                   name = col_character(),
                                   nickname = col_character(),
                                   photo_url.y = col_character(),
                                   home_away = col_character(),
                                   schedule = col_character(),
                                   venue = col_character()
                                 )) %>% 
  filter(period == "Total") %>% 
  mutate(name = ifelse(name == "ADMU","ATENEO",name),
         opp_team = ifelse(home_away =="home","away","home"),
         minutes_played = minutes_played/60,
         played = ifelse(minutes_played > 0,1,0),
         pos_gfc = str_sub(pos,-1,-1)
         ) %>% 
  #join to team box score data
  left_join(team_box_score, by = c("game_no" = "game_no","home_away"="home_away"  ), suffix = c(".player",".team")) %>% 
  #add possession and pace metrics
  mutate(team_possessions = .96 * (field_goal_attempts.team + turnovers.team + .475 * free_throw_attempts.team - offensive_rebounds.team),
         opp_possessions  = .96 * (field_goal_attempts.opp + turnovers.opp + .475 * free_throw_attempts.opp - offensive_rebounds.opp),
         pace = 40 * ((team_possessions + opp_possessions) / (2*(team_minutes.team))),
         point_differential = points.team - points.opp,
         win = ifelse(point_differential > 0,1,0),
         lose = ifelse(point_differential < 0,1,0)
  )



individual_adv_metrics <- 
individual_box_score %>% 
  group_by(player_id,display_name,team_id,name.team,nickname.team,shirt_no,pos_gfc) %>% 
  summarise(
      #Games Played and Total Minutes
      games_played = sum(played),
      total_minutes = sum(minutes_played),
      mpg = total_minutes / games_played,
      pct_of_team_minutes = total_minutes / sum(team_minutes.team * 5),
      
      #Basic Stats Totals
      total_points = sum(points),
      total_assists = sum(assists),
      total_rebounds = sum(rebounds),
      total_offensive_rebounds = sum(offensive_rebounds),
      total_defensive_rebounds = sum(defensive_rebounds),
      total_field_goals_made = sum(field_goals_made),
      total_field_goal_attempts = sum(field_goal_attempts),
      total_three_points_made = sum(three_points_made),
      total_three_point_attempts = sum(three_point_attempts),
      total_free_throws_made = sum(free_throws_made),
      total_free_throw_attempts = sum(free_throw_attempts),
      total_steals = sum(steals),
      total_blocks = sum(blocks),
      total_turnovers = sum(turnovers),
      total_contested_field_goals_made = sum(contested_field_goals_made),
      total_contested_field_goal_attempts = sum(contested_field_goal_attempts),
      total_uncontested_field_goals_made = sum(uncontested_field_goals_made),
      total_uncontested_field_goal_attempts = sum(uncontested_field_goal_attempts),
      
      
      #Per Game Stats
      ppg = total_points / games_played,
      apg = total_assists / games_played,
      trpg = total_rebounds / games_played,
      orpg = total_offensive_rebounds / games_played,
      drpg = total_defensive_rebounds / games_played,
      fgmpg = total_field_goals_made / games_played,
      fgapg = total_field_goal_attempts / games_played,
      threempg = total_three_points_made / games_played,
      threeapg = total_three_point_attempts / games_played,
      ftmpg = total_free_throws_made / games_played,
      ftapg = total_free_throw_attempts / games_played,
      stlpg = total_steals / games_played,
      blkpg = total_blocks / games_played,
      topg = total_turnovers / games_played,
      
      #Per 30 Minute Stats 
      pp30m = total_points / total_minutes * 30,
      ap30m = total_assists / total_minutes * 30,
      trp30m = total_rebounds / total_minutes * 30,
      orp30m = total_offensive_rebounds / total_minutes * 30,
      drp30m = total_defensive_rebounds / total_minutes * 30,
      fgm30m = total_field_goals_made / total_minutes * 30,
      fgap30m = total_field_goal_attempts / total_minutes * 30,
      threemp30m = total_three_points_made / total_minutes * 30,
      threeap30m = total_three_point_attempts / total_minutes * 30,
      ftmp30m = total_free_throws_made / total_minutes * 30,
      ftap30m = total_free_throw_attempts / total_minutes * 30,
      stlp30m = total_steals / total_minutes * 30,
      blkp30m = total_blocks / total_minutes * 30,
      top30m = total_turnovers / total_minutes * 30,
      
      
      #Ratios
      fg_pct = sum(field_goals_made) / sum(field_goal_attempts),
      threefg_pct = sum(three_points_made) / sum(three_point_attempts),
      ft_pct = sum(free_throws_made) / sum(free_throw_attempts),
      contested_fg_pct = sum(contested_field_goals_made) / sum(contested_field_goal_attempts),
      uncontested_fg_pct = sum(uncontested_field_goals_made) / sum(uncontested_field_goal_attempts),
      contested_rate = sum(contested_field_goal_attempts) / sum(field_goal_attempts),
    
      ast_pct = sum(assists) /  (((sum(minutes_played)/sum(team_minutes.team)) * sum(field_goals_made.team)) -sum(field_goals_made)),
      orb_pct = (sum(offensive_rebounds) * (sum(team_minutes.team))) / (sum(minutes_played) * (sum(offensive_rebounds.team) + sum(defensive_rebounds.opp))),
      drb_pct = (sum(defensive_rebounds) * (sum(team_minutes.team))) / (sum(minutes_played) * (sum(defensive_rebounds.team) + sum(offensive_rebounds.opp))),
      trb_pct = (sum(rebounds) * (sum(team_minutes.team))) / (sum(minutes_played) * (sum(rebounds.team) + sum(rebounds.opp))),
    
      #Advanced Stats
      eFG = (sum(field_goals_made) + sum(three_points_made * .5))/sum(field_goal_attempts),
      TS_pct = sum(points)/(2 * (sum(field_goal_attempts + .475 * free_throw_attempts))),
      USG_Rate = (sum(field_goal_attempts + 0.475 * free_throw_attempts +turnovers)) /
        (sum(minutes_played)/sum(team_minutes.team) * sum(field_goal_attempts.team + .475 * free_throw_attempts.team + turnovers.team)),
     

      
      #Get Players total possessions
      #Assist Adjustment Factor
      qAST = (( sum(minutes_played) / sum(team_minutes.team)) * ( 1.14 *((sum(assists.team) - sum(assists))/sum(field_goals_made.team)))) +
              ( (( sum(minutes_played) / sum(team_minutes.team))*sum(assists.team) - sum(assists) ) 
                /
                (( sum(minutes_played) / sum(team_minutes.team))*sum(field_goals_made.team) - sum(field_goals_made) )   
                  )
        
             #  ((((sum(assists.team) / sum(team_minutes.team)) * sum(minutes_played * 5) -sum(assists)) / ((sum(field_goals_made.team) / sum(team_minutes.team)) *
             # sum (minutes_played * 5) - sum(field_goals_made))) * (1 - (sum(minutes_played) / sum(team_minutes.team*5))))
      ,
      FG_Part = sum(field_goals_made) * (1 - ((0.5 * sum(points - free_throws_made)) / (sum(2*field_goal_attempts))) * qAST ),
      AST_Part = 0.5  * (((sum(points.team) - sum(free_throws_made.team)) -(sum(points) - sum(free_throws_made))) / (2 *(sum(field_goal_attempts.team) - sum(field_goal_attempts)))) * sum(assists),
      FT_Part = (1-(1-(sum(free_throws_made)/sum(free_throw_attempts)))^2) * .4 * sum(free_throw_attempts),
      Team_Scoring_Poss = sum(field_goals_made.team) + (1-(1-(sum(free_throws_made.team)/sum(free_throw_attempts.team)))^2) * sum(free_throw_attempts.team) * .4,
      Team_ORB_rate = sum(offensive_rebounds.team) / (sum(offensive_rebounds.team + defensive_rebounds.opp  )),
      Team_play_rate = Team_Scoring_Poss / (sum(field_goal_attempts.team) + sum(free_throw_attempts.team *.44) + sum(turnovers.team) ),
      Team_ORB_Weight = ((1 - Team_ORB_rate ) * Team_play_rate )/ ((1- Team_ORB_rate ) * Team_play_rate + Team_ORB_rate * (1- Team_play_rate )),
      ORB_Part = sum(offensive_rebounds) * Team_ORB_Weight * Team_play_rate,
      ScPoss = (FG_Part + AST_Part + FT_Part) * (1 - (sum(offensive_rebounds.team)/Team_Scoring_Poss) * Team_ORB_Weight * Team_play_rate) + ORB_Part,
      FGxPoss = (sum(field_goal_attempts) - sum(field_goals_made)) * (1-1.07*Team_ORB_rate),
      FTxPoss = ((1-(sum(free_throws_made)/sum(free_throw_attempts)))^2) * .4 * sum(free_throw_attempts),
      TotPoss = ScPoss + FGxPoss  + FTxPoss + sum(turnovers),
      
      #Get Individual Points Produced
      PProd_FG_Part = 2 * (sum(field_goals_made + .5 * three_points_made)) * (1- 0.5 *  ( sum(points - free_throws_made) / sum(2*field_goal_attempts)) * qAST ),
      PProd_AST_Part = 
        #Teammates points per shot
        2 * ( 
            
          (sum(field_goals_made.team - field_goals_made) + 0.5 * sum(three_points_made.team - three_points_made)) 
          /sum(field_goals_made.team - field_goals_made)
          ) 
        * 0.5  * ((sum(points.team - free_throws_made.team) - sum(points - free_throws_made)) / 
                  (2 * sum(field_goal_attempts.team - field_goal_attempts))) * sum(assists),
      PProd_ORB_Part = sum(offensive_rebounds) * Team_ORB_Weight * Team_play_rate * (
          sum(points.team) / 
          (sum(field_goals_made.team) + (1-(1-(sum(free_throws_made.team)/sum(free_throw_attempts.team)))^2) * .4 * sum(free_throw_attempts.team))
          ),
      PProd = (PProd_FG_Part + PProd_AST_Part + sum(free_throws_made) ) * (1 - (sum(offensive_rebounds.team)/Team_Scoring_Poss) * Team_ORB_Weight * Team_play_rate) + PProd_ORB_Part,
      ORtg = 100 * (PProd / TotPoss),
      Floor_pct = ScPoss / TotPoss,

      
      #Compute Defensive Rating
      DFG_pct = sum(field_goals_made.opp) / sum(field_goal_attempts.opp),
      DOR_pct = sum(offensive_rebounds.opp) / sum(offensive_rebounds.opp + defensive_rebounds.team),
      FMwt = (DFG_pct * (1- DOR_pct)) / (DFG_pct * (1 - DOR_pct) + (1 -  DFG_pct) * DOR_pct ),
      Stops1 = sum(steals) + sum(blocks) * FMwt * (1- 1.07 * DOR_pct) + sum(defensive_rebounds) * (1- FMwt),
      Stops2 = ((sum(field_goal_attempts.opp - field_goals_made.opp - blocks.team) / sum(team_minutes.team)) * FMwt *  
        (1 - 1.07 * DOR_pct) + (sum(turnovers.opp - steals.team)/sum(team_minutes.team))) * sum(minutes_played) + (sum(personal_fouls)/sum(personal_fouls.team)) * 0.4 * sum(free_throw_attempts.opp) * (1- (sum(free_throws_made.opp)/sum(free_throw_attempts.opp) ))^2,
      Stops = Stops1 + Stops2,
      Stop_pct = (Stops * sum(team_minutes.opp)) / (sum(team_possessions) * sum(minutes_played) ),
      team_drtg = sum(points.opp,na.rm=TRUE)/sum(team_possessions,na.rm = TRUE) * 100,
      D_Pts_per_ScPoss = sum(points.opp) / (sum(field_goals_made.opp) + (1-(1-(sum(free_throws_made.opp)/sum(free_throw_attempts.opp)))^2  ) * sum(free_throw_attempts.opp) * 0.4 ),
      DRtg = team_drtg + 0.2 * (100 * D_Pts_per_ScPoss * (1- Stop_pct) -team_drtg ),
      Net_RTg = ORtg - DRtg
  )


individual_adv_metrics[is.na(individual_adv_metrics)] <- ''

write_csv(individual_adv_metrics,"data/player_metrics.csv", na = "" )



individual_adv_metrics %>% ungroup() %>% 
  filter(name.team == "UP") %>% 
  select(display_name,USG_Rate,total_turnovers,total_field_goal_attempts,total_free_throw_attempts,total_minutes) %>% arrange(desc(USG_Rate))
