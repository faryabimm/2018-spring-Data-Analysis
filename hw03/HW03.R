library(engsoccerdata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(highcharter)
library(magrittr)
fdb <- as.tbl(spain) # as.tbl is a function defined in dplyr package!
# fdb$Date <- as.character(fdb$Date)
# fdb %>% mutate(year = strsplit(Date, "-")[[1]][1],
#                day = strsplit(Date, "-")[[1]][2],
#                month = strsplit(Date, "-")[[1]][3]) -> fdb
# fdb$Date <- NULL


# what about season 1986 phase2? we igonre it!
fdb %>% filter(round != 'phase2') -> fdb


################################################################################################
# Q1

rbind(
  fdb %>% select(Date, Season, team = home, opp = visitor, GS = hgoal, GR = vgoal),
  fdb %>% select(Date, Season, team = visitor, opp = home, GS = vgoal, GR = hgoal)
) -> team_results
team_results %>% group_by(team, Season) %>% 
  summarise(PTS = sum(GS > GR) * 3 + sum(GS == GR), GS = sum(GS), GR = sum(GR)) -> team_points
# sounds like the order of summarise is important
team_points %>% group_by(Season) %>% top_n(1, PTS) %>% arrange(Season) %>%  
  mutate(GB = GS - GR) %>% group_by(Season) %>% top_n(1, GB) -> season_champions
# some Seasons, top teams have the same score
# using goal balance to determine champion
season_champions %>% group_by(team) %>% summarise(championships = n()) %>% arrange(desc(championships)) -> team_champs
team_champs %>% hchart(type = 'column', hcaes(x = team, y = championships))
# OMG! only 8 teams have ever won the laliga!
# hcaes := HighCharterAES hcaes

################################################################################################
# Q2 _ the most boring leagues

# find team count for each league
team_results %>% group_by(Season) %>% distinct(team) %>% summarise(team_count = n()) -> season_teams

fdb %>% mutate(goals = hgoal + vgoal) %>% group_by(Season) %>% summarise(goals = sum(goals)) %>% 
  arrange(Season) -> season_goals

season_data <- full_join(season_teams, season_goals)  
season_data %>% mutate(GPT = goals/team_count) %>% arrange(GPT) %>% slice(1:10) -> data_2_1 # Goals Per Team

data_2_1$Season <- factor(data_2_1$Season, levels = data_2_1$Season)
data_2_1 %>% ggplot(aes(x = Season, y = GPT)) + geom_col() + coord_cartesian(ylim = c(33.5, 38.5)) -> plot_2_1

################################################################################################
# Q2 _ the most boring teams

team_results %>% group_by(team) %>% summarise(team_goals = sum(GS), team_games = n()) %>% 
  mutate(GPG = team_goals/team_games) %>% arrange(GPG) %>% slice(1:10) -> data_2_2 # Goals Per Game
data_2_2$team <- factor(data_2_2$team, levels = data_2_2$team)

data_2_2 %>% ggplot(aes(x = team, y = GPG)) + geom_col() + coord_cartesian(ylim = c(0.75, 1)) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8)) -> plot_2_2

################################################################################################
# Q2 _ result

plot_grid(plot_2_1, plot_2_2)


################################################################################################
# Q3

team_results %>% group_by(Season) %>% arrange(Date) %>% slice(1:(n() %/% 2)) -> half_season_resluts
half_season_resluts %>% group_by(team, Season) %>%
  summarise(PTS = sum(GS > GR)*3 + sum(GS==GR), GS = sum(GS), GR = sum(GR)) %>%
  group_by(Season) %>% arrange(desc(PTS)) %>% top_n(1, PTS) %>% mutate(GB=GS-GR) %>% arrange(desc(GB)) %>%
  top_n(1, GB) %>% arrange(desc(GS)) %>% top_n(1, GS) -> mid_season_champions

season_champions %>% View()
mid_season_champions %>% View()

full_join(season_champions, mid_season_champions, by = "Season") %>%
  mutate(champion_stay_status = team.x == team.y) %$% mean(.$champion_stay_status) %>% View()

################################################################################################
# Q4

team_results %>% filter(Season >= 2001 & Season <= 2010) %>% group_by(team) %>% filter(GS <= GR) %>% 
  group_by(team, opp) %>% summarise(bad_lucks = n()) -> bad_lucks

team_results %>% filter(Season >= 2001 & Season <= 2010) %>% group_by(team) %>%
  summarise(mean_gb = (sum(GS) - sum(GR))/n(), mean_pts = (sum(GS > GR)*3 + sum(GS==GR))/n()) -> mean_pts_gb

LEAGUE_SIZE <- 20

team_results %>% filter(Season >= 2001 & Season <= 2010) %>% group_by(Season, team) %>% 
  summarise(PTS = sum(GS > GR)*3 + sum(GS==GR), GS = sum(GS), GR = sum(GR)) %>% arrange(desc(PTS)) %>%
  mutate(rank = 1:n()) %>% group_by(team) %>% summarise(mean_rank = LEAGUE_SIZE - mean(rank)) -> mean_rank

full_join(mean_pts_gb, mean_rank, by = "team") %>%
  mutate(power_rank = mean_pts/max(mean_pts) + (mean_gb - min(mean_gb))/(max(mean_gb) - min(mean_gb)) + mean_rank/max(mean_rank)) %>%
  arrange(desc(power_rank)) %>% select(team, power_rank) -> power_ranks

full_join(bad_lucks, power_ranks, by = "team") %>% full_join(power_ranks, by = c("opp" = "team")) %>% 
  mutate(gorbeh_siah_factor = power_rank.x/power_rank.y*bad_lucks) %>% arrange(desc(gorbeh_siah_factor)) %>%
  ungroup() %>% slice(1:20) %>% print()


################################################################################################
# Q5_a

full_join(team_results, season_teams, by = "Season") %>% arrange(Date) %>% group_by(Season) %>%
  mutate(week_index = (0:(n()-1) %/% team_count) + 1) %>% group_by(week_index) -> week_data

week_data %>% mutate(rem_weeks = (team_count-1)*2-week_index) -> week_data

week_data %>% View()

week_data %>% mutate(PTS = (GS > GR) * 2 + (GS >= GR)) %>%
  group_by(Season, team) %>% arrange(Date) %>% mutate(tot_PTS = cumsum(PTS)) -> week_data

week_data %>% ungroup() %>% group_by(Season, week_index) %>% arrange(desc(tot_PTS)) %>% slice(2) %>%
  select(Season, week_index, second_pts = tot_PTS) -> second_pts_by_week

full_join(week_data, second_pts_by_week) -> week_data_with_second_places

week_data_with_second_places %>% ungroup() %>%  group_by(Season, week_index) %>% arrange(desc(tot_PTS)) %>%
  mutate(champion = (rem_weeks * 3) < (tot_PTS - second_pts) ) -> champion_status

champion_status %>% filter(champion == TRUE) %>% group_by(Season) %>% arrange(desc(rem_weeks)) %>%
  top_n(1, rem_weeks) %>% select(Season, team, rem_weeks) %>% ungroup() %>% slice(1:10) %>% View()

################################################################################################
# Q5_b
# the most perfect championship is interpreted as the most difference gap between champion and its first chaser

team_points %>% group_by(Season) %>% arrange(desc(PTS)) %>% slice(2) %>% arrange(Season) %>%  
  mutate(GB = GS - GR) -> season_second_champions

full_join(
  season_champions %>% select(Season, team_1 = team, PTS_1 = PTS, GB_1 = GB),
  season_second_champions %>% select(Season, team_2 = team, PTS_2 = PTS, GB_2 = GB),
  by = 'Season'
) %>% mutate(pts_gap = PTS_1 - PTS_2, gb_gap = GB_1 - GB_2) %>% arrange(desc(pts_gap)) %>% ungroup() %>% 
  slice(1:10) -> champion_pts_gap

champion_pts_gap %>% View()

champion_pts_gap %>% hchart(type = 'column', hcaes(x = team_1, y = pts_gap))


################################################################################################
# Q6
sequence_finder <- function(seq) {
  result <- integer(length(seq))
  for (i in 1:length(seq)) {
    if (seq[i] == TRUE) {
      if (i == 1) {
        result[i] = 1
      } else {
        result[i] = result[i-1] + 1
      }
    } else {
      result[i] = 0
    }
  }
  return(result)
}
team_results %>% group_by(team) %>% arrange(Date) %>% mutate(wins_row = sequence_finder(GS>GR)) %>%
  summarise(max_wins_row = max(wins_row)) %>% arrange(desc(max_wins_row)) %>%
  hchart(type = 'column', hcaes(x = team, y = max_wins_row))

team_results %>% group_by(team) %>% arrange(Date) %>% mutate(draws_row = sequence_finder(GS==GR)) %>%
  summarise(max_draws_row = max(draws_row)) %>% arrange(desc(max_draws_row)) %>%
  hchart(type = 'column', hcaes(x = team, y = max_draws_row))

team_results %>% group_by(team) %>% arrange(Date) %>% mutate(loses_row = sequence_finder(GS<GR)) %>%
  summarise(max_loses_row = max(loses_row)) %>% arrange(desc(max_loses_row)) %>%
  hchart(type = 'column', hcaes(x = team, y = max_loses_row))

################################################################################################
# Q7

week_data %>% ungroup() %>% group_by(Season, week_index) %>% arrange(desc(tot_PTS)) %>% slice(17) %>%
  select(Season, week_index, last_stayer_pts = tot_PTS) -> last_stayer_pts_by_week

full_join(week_data, last_stayer_pts_by_week) -> week_data_with_last_stayers

week_data_with_last_stayers %>% ungroup() %>%  group_by(Season, week_index) %>% arrange(desc(tot_PTS)) %>%
  mutate(fall = (rem_weeks * 3) < (last_stayer_pts - tot_PTS) ) -> fallouts_status

fallouts_status %>% filter(fall == TRUE) %>% group_by(Season) %>% arrange(desc(rem_weeks)) %>%
  top_n(1, rem_weeks) %>% select(Season, team, rem_weeks) %>% ungroup() %>% slice(1:10) %>% View()

################################################################################################
# Q8

week_data %>% filter(Season == 1998) %>% group_by(week_index) %>% arrange(desc(tot_PTS)) %>% mutate(rank = 1:n()) %>%
  select(team, week_index, rank) %>% group_by(team) %>% arrange(rank) -> week_ranks

week_ranks$rank = factor(week_ranks$rank, levels = 20:1)
week_ranks$rank = factor(week_ranks$rank, levels = rev(levels(week_ranks$rank)))
week_ranks$week_index = factor(week_ranks$week_index, 1:38)

last_week_team_names <- week_ranks %>% filter(week_index == 38) %>% arrange(desc(rank)) %>% select(team, rank)

ggplot(data = week_ranks, aes(x = week_index, y = rank, group = team, colour = team)) + geom_line() +
  coord_fixed() + theme_bw() + labs(y = 'Position') + theme(legend.position="none") +
  geom_text(data = last_week_team_names, aes(x = 37, y = rank, label = team)) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

################################################################################################
# Q9

fdb %>% filter(Season == 2012) %>% select(home, visitor, FT) -> team_results_2012

library(xtable)
team_results_2012 %>% tidyr::spread(visitor, FT) %>% xtable() %>%  print(type = 'html')

################################################################################################
# Q10_a

# barcelona plays a more beautiful game but real madrid plays with a better strategy.
# barcelona has better offenders but real madrid has better defenders
# barca's defense lines has become better and reals defense line has become worse these days
# also barce's offense has an small advantage over real's. 
# barca is seriously gaining on real!
# barca's games are more pleasing to watch in comparison to real's games!

team_results %>% group_by(Season, team) %>% mutate(GS = sum(GS), GR = sum(GR)) -> gs_and_gr

gs_and_gr %>% filter(team == 'FC Barcelona' | team == 'Real Madrid') %>% group_by(team) %>%
  hchart(type = 'line', hcaes(x = Season, y = GS, group = team))

gs_and_gr %>% filter(team == 'FC Barcelona' | team == 'Real Madrid') %>% group_by(team) %>%
  hchart(type = 'line', hcaes(x = Season, y = GR, group = team))



c_gs_barca <- coef(lm(GS~Season, data = gs_and_gr %>% filter(team == 'FC Barcelona')))
c_gs_realm <- coef(lm(GS~Season, data = gs_and_gr %>% filter(team == 'Real Madrid')))
gs_and_gr %>% filter(team == 'FC Barcelona' | team == 'Real Madrid') %>% group_by(team) %>%
  ggplot(aes(x = Season, y = GS, group = team, colour = team)) + geom_line() +
  geom_abline(intercept = c_gs_barca[1], slope = c_gs_barca[2], color = 'red') +
  geom_abline(intercept = c_gs_realm[1], slope = c_gs_realm[2], color = 'cyan')


c_gr_barca <- coef(lm(GR~Season, data = gs_and_gr %>% filter(team == 'FC Barcelona')))
c_gr_realm <- coef(lm(GR~Season, data = gs_and_gr %>% filter(team == 'Real Madrid')))
gs_and_gr %>% filter(team == 'FC Barcelona' | team == 'Real Madrid') %>% group_by(team) %>%
  ggplot(aes(x = Season, y = GR, group = team, colour = team)) + geom_line() +
  geom_abline(intercept = c_gr_barca[1], slope = c_gr_barca[2], color = 'red') +
  geom_abline(intercept = c_gr_realm[1], slope = c_gr_realm[2], color = 'cyan')

################################################################################################
# Q10_b

# in recent 10 years no teams other than barca and real were able to take the champion cup home!
# barca is overally better than real in both defense and offense but better tecknique has come to real's aid!
# real has taken the cup home 5 times whereas barca has won only two leages!

team_points %>% group_by(Season) %>% arrange(desc(PTS)) %>% slice(2) %>% arrange(Season) %>%  
  mutate(GB = GS - GR) -> season_second_champions

full_join(
  season_champions %>% select(Season, team_1 = team, PTS_1 = PTS, GB_1 = GB),
  season_second_champions %>% select(Season, team_2 = team, PTS_2 = PTS, GB_2 = GB),
  by = 'Season'
) %>% mutate(pts_gap = PTS_1 - PTS_2, gb_gap = GB_1 - GB_2) %>% arrange(desc(pts_gap)) %>% ungroup() %>% 
  slice(1:10) -> champion_pts_gap

champion_pts_gap %>% View()

champion_pts_gap %>% hchart(type = 'column', hcaes(x = team_1, y = pts_gap))

################################################################################################
# Q10_c

# other than FC Barcelona and Real Madrid, Athletic Bilbao is the only team which has never fell from La Liga
# to Liga Adelante

fdb %>% select(home, Season) %>% unique() %>% group_by(home) %>%  summarise(participations = n()) %>%
  arrange(desc(participations)) %>% hchart(type = 'column', hcaes(x = home, y = participations))

################################################################################################
# Q10_d

# only 8 teams have ever won the La Liga! (based on resluts from Q1)
