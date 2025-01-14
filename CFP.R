library(cfbfastR)
library(ggplot2)

load("~/Downloads/team_logos.rda")


team_sum <- as.data.frame(NULL)

for(i in 1:nrow(team_logos)){
  print(team_logos[i,2])
  team_sum <- rbind(team_sum, data.frame(cfbd_stats_season_team(2024, team = team_logos[i,2])))
}

team_adv <- as.data.frame(NULL)

for(i in 1:nrow(team_sum)){
  team_adv <- rbind(team_adv, data.frame(cfbd_stats_season_advanced(2024, team = team_sum[i,2])))
}

top_25 <- c("Oregon", "Ohio State",	"Texas", "Penn State", "Indiana", "BYU","Tennessee","Notre Dame","Miami","Alabama","Ole Miss","Georgia","Boise State","SMU","Texas A&M","Arizona State","Colorado","Iowa State","Illinois","Clemson","South Carolina",				
"UNLV" , "Missouri","Army","Tulane")

for(i in 1:nrow(team_sum)){
  print(i)
  if(team_sum[i,2] %in% top_25){
    team_sum$Ranked[i]<- "Yes"
  } else {
    team_sum$Ranked[i] <- "No"
  }
}


top_25_adv <- as.data.frame(NULL)

for(i in top_25){
  top_25_adv <- rbind(top_25_adv, data.frame(team_adv[team_adv$team==i,]))
}

top_25_sum <- as.data.frame(NULL)

for(i in top_25){
  top_25_sum <- rbind(top_25_sum, data.frame(team_sum[team_adv$team==i,]))
}

for(i in 1:nrow(team_adv)){
  print(i)
  if(team_adv[i,2] %in% top_25){
    team_adv$Ranked[i]<- "Yes"
  } else {
    team_adv$Ranked[i] <- "No"
  }
}


team_sum$off_plays <- team_sum$pass_atts + team_sum$rush_atts

team_sum$total_tds <- (team_sum$pass_TDs + team_sum$rush_TDs + team_sum$kick_return_TDs + team_sum$punt_return_TDs + team_sum$passes_intercepted_TDs) 

team_sum$td_ratio <- team_sum$total_tds / team_sum$turnovers

team_sum$off_plays_per_td <- team_sum$off_plays / (team_sum$rush_TDs + team_sum$pass_TDs)

team_sum$ypg <- team_sum$total_yds / team_sum$games

team_sum$passes_per_game <- team_sum$pass_atts / team_sum$games

team_sum$rushes_per_game <- team_sum$rush_atts / team_sum$games




ggplot(data = team_sum, aes(rushes_per_game, passes_per_game, color = Ranked)) + geom_point() +theme_minimal() + geom_smooth()

ggplot(data = team_adv, aes(off_success_rate, def_success_rate, color = Ranked)) + geom_point() +theme_minimal() + geom_smooth()

ggplot(data = team_adv, aes(off_passing_plays_success_rate, off_rushing_plays_success_rate, color = Ranked)) + geom_point() +theme_minimal() + geom_smooth()


ggplot(data = team_sum, aes(td_ratio,total_yds, color = Ranked)) + geom_point() +theme_minimal()
ggplot(data = team_sum[team_sum$Ranked == 'Yes',], aes(td_ratio, total_yds, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_sum, aes(off_plays_per_td,td_ratio, color = Ranked)) + geom_point() +theme_minimal()
ggplot(data = team_sum[team_sum$Ranked == 'Yes',], aes(off_plays_per_td,td_ratio, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_sum, aes(off_plays_per_td,ypg, color = Ranked)) + geom_point() +theme_minimal() + geom_smooth(method = "lm")
ggplot(data = team_sum[team_sum$Ranked == 'Yes',], aes(off_plays_per_td,ypg, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_adv, aes(off_field_pos_avg_predicted_points,off_pts_per_opp, color = Ranked)) + geom_point() +theme_minimal()
ggplot(data = team_adv[team_sum$Ranked == 'Yes',], aes(off_field_pos_avg_predicted_points,off_pts_per_opp, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_adv, aes(off_rushing_plays_ppa,off_passing_plays_ppa, color = Ranked)) + geom_point() +theme_minimal()
ggplot(data = team_adv[team_sum$Ranked == 'Yes',], aes(off_rushing_plays_ppa,off_passing_plays_ppa, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_adv, aes(off_ppa,def_ppa, color = Ranked)) + geom_point() +theme_minimal()
ggplot(data = team_adv[team_sum$Ranked == 'Yes',], aes(off_ppa,def_ppa, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = top_25_adv, aes(off_success_rate, def_success_rate, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = team_adv, aes(off_passing_plays_success_rate, off_rushing_plays_success_rate, color = Ranked)) + geom_point() +theme_minimal() 
ggplot(data = top_25_adv, aes(off_passing_plays_success_rate, off_rushing_plays_success_rate, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)

ggplot(data = top_25_sum, aes(pass_TDs, interceptions, color = conference, label = team)) + geom_point() +theme_minimal() + geom_text(hjust=0, vjust=0)


