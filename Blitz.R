# Syracuse Blitz Competition Code

library(tidyverse)
library(ggplot2)
library(ggimage)
library(teamcolors)
library(ggrepel)
library(caret)
library(nflfastR)
library(gt)
options(scipen = 9999)

pbp <- read.csv("./Play By Play.csv")
grades <- read.csv("./Facet Grades.csv")

pbp_rp <- pbp %>%
  filter(rps == "R" | rps == "P", down != 0) %>%
  mutate(success = ifelse(EPA > 0, 1, 0),
         length = case_when(distance <= 3 ~ 'Short',
                            distance < 7 ~ 'Medium',
                            distance >= 7 ~ 'Long'),
         field_pos = case_when(yards_to_go > 75 ~ "Deep",
                                yards_to_go > 40 ~ "Midfield",
                                yards_to_go > 20 ~ "FG Range",
                                yards_to_go > 10 ~ "Red Zone",
                                TRUE ~ "Goal Line"),
         rush = ifelse(rps == "R", 1, 0), 
         pass = ifelse(rps == "P", 1, 0))

seasons <- 2014:2020
fastR_pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, !is.na(epa), !is.na(posteam), posteam != "") 
})

# Columns from fastR_pbp: posteam, defteam, yardline_100, quarter_seconds_remaining, season, week, qtr,
# down, ydstogo, yards_gained, desc, air_yards, run_gap, wp, wpa
fastR_pbp <- fastR_pbp %>%
  select(season, week, posteam, defteam, down, ydstogo, yardline_100, qtr, quarter_seconds_remaining,
         half_seconds_remaining, yards_gained, air_yards, run_gap, wp, wpa, desc)

# Seeing team names in both to correct before joining files
pffTeams <- pbp_rp %>%
  group_by(offense) %>%
  summarize(n = n())

fastRTeams <- fastR_pbp %>%
  group_by(posteam) %>%
  summarize(n = n())

# Names to switch in fastR: ARI to ARZ, BAL to BLT, CLE to CLV, HOU to HST
# Names to switch in PFF: OAK to LV, SD to LAC, SL to LA
# Switching names in both pbp
fastR_pbp <- fastR_pbp %>%
  mutate(posteam = case_when(posteam == "ARI" ~ "ARZ",
                             posteam == "BAL" ~ "BLT",
                             posteam == "CLE" ~ "CLV",
                             posteam == "HOU" ~ "HST",
                             TRUE ~ posteam),
         defteam = case_when(defteam == "ARI" ~ "ARZ",
                             defteam == "BAL" ~ "BLT",
                             defteam == "CLE" ~ "CLV",
                             defteam == "HOU" ~ "HST",
                             TRUE ~ defteam))

pbp_rp <- pbp_rp %>%
  mutate(offense = as.character(offense), defense = as.character(defense)) %>%
  mutate(offense = case_when(offense == "OAK" ~ "LV",
                             offense == "SD" ~ "LAC", 
                             offense == "SL" ~ "LA", 
                             TRUE ~ offense),
         defense = case_when(defense == "OAK" ~ "LV",
                             defense == "SD" ~ "LAC",
                             defense == "SL" ~ "LA",
                             TRUE ~ defense))


pbp_combined <- pbp_rp %>%
  left_join(fastR_pbp, by = c("season" = "season", "week" = "week", "offense" = "posteam", "defense" = "defteam",
                              "down" = "down", "distance" = "ydstogo", "yards_to_go" = "yardline_100",
                              "quarter" = "qtr", "seconds_left_in_quarter" = "quarter_seconds_remaining")) %>%
  filter(!is.na(desc))

pbp_combined <- pbp_combined %>%
  mutate(success = case_when(down == 1 ~ ifelse(yards_gained >= 0.45*distance, 1, 0), # Change back to 0.45*distance
                             down == 2 ~ ifelse(yards_gained >= 0.60*distance, 1, 0), # Change back to 0.60*distance
                             down == 3 ~ ifelse(yards_gained >= distance, 1, 0),
                             down == 4 ~ ifelse(yards_gained >= distance, 1,0)))
                             

dfByLengthAndPos <- pbp_combined %>%
  group_by(down,length,field_pos) %>%
  summarize(pass_rate = sum(rps=="P")/sum(rps=="P" | rps == "R"),
            rush_rate = sum(rps=="R")/sum(rps=="P" | rps == "R"),
            total_plays = n())

rush_stats <- pbp_combined %>%
  filter(rps == "R") %>%
  group_by(down, length, field_pos) %>%
  summarize(rush_success = mean(success), avg_rush_epa = mean(EPA), rush_plays = n())

pass_stats <- pbp_combined %>%
  filter(rps == "P") %>%
  group_by(down, length, field_pos) %>%
  summarize(pass_success = mean(success), avg_pass_epa = mean(EPA), pass_plays = n()) 

dfByLengthAndPos <- dfByLengthAndPos %>%
  left_join(pass_stats, by = c("down" = "down", "length" = "length", "field_pos" = "field_pos")) %>%
  left_join(rush_stats, by = c("down" = "down", "length" = "length", "field_pos" = "field_pos"))

team_stats <- pbp_combined %>%
  group_by(offense, season) %>% #, down, length, field_pos) %>%
  summarize(n_dropbacks = sum(pass), n_rushes = sum(rush), pass_rate = n_dropbacks/(n_dropbacks + n_rushes),
            rush_rate = n_rushes/(n_dropbacks+n_rushes), pa_rate = sum(play_action)/sum(pass),
            avg_pass_epa = sum(pass*EPA)/n_dropbacks, avg_rush_epa = sum(rush*EPA)/n_rushes,
            avg_pass_success = sum(pass*success)/n_dropbacks, avg_rush_success = sum(rush*success)/n_rushes
            )

# Prep to merge grades and team_stats. Update team names in grades to match
gradesTeams <- grades %>%
  group_by(team) %>%
  summarize(n = n())
grades <- grades %>%
  mutate(abbr = case_when(team == "Arizona Cardinals" ~ "ARZ",
                          team == "Atlanta Falcons" ~ "ATL",
                          team == "Baltimore Ravens" ~ "BLT",
                          team == "Buffalo Bills" ~ "BUF",
                          team == "Carolina Panthers" ~ "CAR",
                          team == "Chicago Bears" ~ "CHI",
                          team == "Cincinnati Bengals" ~ "CIN",
                          team == "Cleveland Browns" ~ "CLV",
                          team == "Dallas Cowboys" ~ "DAL",
                          team == "Denver Broncos" ~ "DEN",
                          team == "Detroit Lions" ~ "DET",
                          team == "Green Bay Packers" ~ "GB",
                          team == "Houston Texans" ~ "HST",
                          team == "Indianapolis Colts" ~ "IND",
                          team == "Jacksonville Jaguars" ~ "JAX", 
                          team == "Kansas City Chiefs" ~ "KC",
                          team == "Miami Dolphins" ~ "MIA",
                          team == "Minnesota Vikings" ~ "MIN",
                          team == "New England Patriots" ~ "NE",
                          team == "New Orleans Saints" ~ "NO",
                          team == "New York Giants" ~ "NYG", 
                          team == "New York Jets" ~ "NYJ",
                          team == "Oakland Raiders" ~ "LV",
                          team == "Las Vegas Raiders" ~ "LV",
                          team == "Philadelphia Eagles" ~ "PHI",
                          team == "Pittsburgh Steelers" ~ "PIT",
                          team == "San Diego Chargers" ~ "LAC",
                          team == "Los Angeles Chargers" ~ "LAC",
                          team == "San Francisco 49ers" ~ "SF",
                          team == "St. Louis Rams" ~ "LA",
                          team == "Los Angeles Rams" ~ "LA",
                          team == "Seattle Seahawks" ~ "SEA",
                          team == "Tampa Bay Buccaneers" ~ "TB",
                          team == "Tennessee Titans" ~ "TEN",
                          team == "Washington Redskins" ~ "WAS",
                          team == "Washington Football Team" ~ "WAS"))

dfStatsGrades <- team_stats %>%
  left_join(grades, by = c("offense" = "abbr", "season" = "Year"))

pbp_combined <- pbp_combined %>%
  left_join(grades, by = c("offense" = "abbr", "season" = "Year"))

pbp_combined <- pbp_combined %>%
  rename(passing = "pass.y", pass = "pass.x")

pbp_combined <- pbp_combined %>%
  mutate(field_pos = factor(field_pos, levels = c("Deep", "Midfield", "FG Range", "Red Zone", "Goal Line")),
         length = factor(length, levels = c("Long", "Medium", "Short")))

pbp_combined <- pbp_combined %>%
  mutate(wp2 = cut(wp*100, breaks = c(0,25,75,100)))

dfByTeamWP <- pbp_combined %>%
  group_by(offense, season, wp2, down,length,field_pos) %>%
  summarize(n_dropbacks = sum(pass), n_rushes = sum(rush), pass_rate = n_dropbacks/(n_dropbacks+n_rushes),
            avg_pass_epa = sum(pass*EPA)/n_dropbacks, avg_rush_epa = sum(rush*EPA)/n_rushes,
            avg_pass_success = sum(pass*success)/n_dropbacks, avg_rush_success = sum(rush*success)/n_rushes,
            pa_rate = sum(play_action)/sum(pass))

dfData <- dfByTeamWP %>%
  filter(n_dropbacks >= 1, n_rushes >= 1) %>%
  left_join(grades, by = c("offense" = "abbr", "season" = "Year"))

# Get only the stuff we need for the dataset 
dfData <- dfData %>%
  select(-c("n_dropbacks","n_rushes", "team", "record", "pa", "def", 
            "rdef", "tack", "prsh", "cov", "spec"))

# Create testing dataset for overall averages
testing <- pbp_combined %>%
  group_by(wp2, down, length, field_pos) %>%
  summarize(n_dropbacks = sum(pass), n_rushes = sum(rush), pass_rate = n_dropbacks/(n_dropbacks+n_rushes),
            avg_pass_epa = sum(pass*EPA)/n_dropbacks, avg_rush_epa = sum(rush*EPA)/n_rushes,
            avg_pass_success = sum(pass*success)/n_dropbacks, avg_rush_success = sum(rush*success)/n_rushes,
            pa_rate = sum(play_action)/sum(pass))

# Find average overall grades
avgGrades <- grades %>%
  summarize(pf = mean(pf), over = mean(over), off = mean(off), pass = mean(pass), pblk = mean(pblk),
            recv = mean(recv), run = mean(run), rblk = mean(rblk))

testing$pf <- avgGrades$pf
testing$over <- avgGrades$over
testing$off <- avgGrades$off
testing$pass <- avgGrades$pass
testing$pblk <- avgGrades$pblk
testing$recv <- avgGrades$recv
testing$run <- avgGrades$run
testing$rblk <- avgGrades$rblk

testing <- testing %>%
  filter(n_dropbacks > 0, n_rushes > 0)

model <- lm(pass_rate ~ wp2 + down + length + field_pos + avg_pass_epa + avg_rush_epa + avg_pass_success
            + avg_rush_success + pa_rate + pf + over + off + pass + pblk + recv + run + rblk,
            data = dfData)

testing$pred <- predict(model, newdata = testing)
testing <- testing[c(1:7,21,8:20)]

# Fit model to Titans and Vikings data
# Pull Out Titans and Vikings data

titansAndVikingsStats <- dfData %>%
  filter(offense == "MIN" | offense == "TEN", season == 2020)

titansAndVikingsStats$pred <- predict(model, newdata = titansAndVikingsStats)
titansAndVikingsStats <- titansAndVikingsStats[c(1:7,21,8:20)]

pa_rates <- pbp_combined %>%
  group_by(field_pos) %>%
  summarize(pa_rate = sum(play_action)/sum(pass))

ten_min_pa_rates <- pbp_combined %>%
  group_by(season, offense, field_pos) %>%
  summarize(pa_rate = sum(play_action)/sum(pass)) %>%
  filter(season == 2020, offense == "TEN" | offense == "MIN")

ten_min_pa_rates <- ten_min_pa_rates %>%
  left_join(pa_rates, by = c("field_pos" = "field_pos"), suffix = c("_team", "_avg"))

dfTeamEPAs <- pbp_combined %>%
  filter(season == 2020) %>%
  group_by(offense) %>%
  summarize(n_dropbacks = sum(pass), n_rushes = sum(rush), pass_rate = n_dropbacks/(n_dropbacks+n_rushes),
            avg_pass_epa = sum(pass*EPA)/n_dropbacks, avg_rush_epa = sum(rush*EPA)/n_rushes,
            avg_pass_success = sum(pass*success)/n_dropbacks, avg_rush_success = sum(rush*success)/n_rushes,
            pa_rate = sum(play_action)/sum(pass)) %>%
  arrange(desc(avg_pass_epa)) %>%
  select(-c("n_dropbacks","n_rushes"))
  

# Write our data to csvs to send to team
write.csv(testing, file = "Blitz Competition Results.csv")
write.csv(titansAndVikingsStats, file = "Titans and Vikings Predictions.csv")  
write.csv(ten_min_pa_rates, file = "Titans and Vikings Play Action.csv")

# Create tables to put in presentation
tables <- testing %>%
  select(wp2, down, length, field_pos, pass_rate, pred) %>%
  mutate(pass_rate = round(pass_rate*100,2), pred = round(pred*100,2)) %>%
  rename(`Actual Pass Rate` = "pass_rate", `Predicted Pass Rate` = "pred", `Win Probability` = "wp2",
         Down = "down", Distance = "length", `Field Position` = "field_pos")

firstLong025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 1, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
    tab_header(title = "1st and Long", subtitle = "Win Probability Between 0 and 25")

firstMed025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 1, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "1st and Medium", subtitle = "Win Probability Between 0 and 25")

secondLong025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 2, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Long", subtitle = "Win Probability Between 0 and 25")

secondMed025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 2, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Medium", subtitle = "Win Probability Between 0 and 25")

secondShort025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 2, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Short", subtitle = "Win Probability Between 0 and 25")

thirdLong025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 3, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Long", subtitle = "Win Probability Between 0 and 25")

thirdMed025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 3, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Medium", subtitle = "Win Probability Between 0 and 25")

thirdShort025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 3, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Short", subtitle = "Win Probability Between 0 and 25")

fourth025 <- tables %>%
  filter(`Win Probability` == "(0,25]", Down == 4) %>%
  ungroup() %>%
  select(-c("Win Probability", "Down")) %>%
  gt() %>%
  tab_header(title = "4th Downs", subtitle = "Win Probability Between 0 and 25")

firstLong2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 1, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "1st and Long", subtitle = "Win Probability Between 25 and 75")

firstMed2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 1, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "1st and Medium", subtitle = "Win Probability Between 25 and 75")

secondLong2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 2, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Long", subtitle = "Win Probability Between 25 and 75")

secondMed2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 2, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Medium", subtitle = "Win Probability Between 25 and 75")

secondShort2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 2, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Short", subtitle = "Win Probability Between 25 and 75")

thirdLong2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 3, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Long", subtitle = "Win Probability Between 25 and 75")

thirdMed2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 3, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Medium", subtitle = "Win Probability Between 25 and 75")

thirdShort2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 3, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Short", subtitle = "Win Probability Between 25 and 75")

fourth2575 <- tables %>%
  filter(`Win Probability` == "(25,75]", Down == 4) %>%
  ungroup() %>%
  select(-c("Win Probability", "Down")) %>%
  gt() %>%
  tab_header(title = "4th Downs", subtitle = "Win Probability Between 25 and 75")

firstLong75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 1, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "1st and Long", subtitle = "Win Probability Between 75 and 100")

firstMed75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 1, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "1st and Medium", subtitle = "Win Probability Between 75 and 100")

secondLong75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 2, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Long", subtitle = "Win Probability Between 75 and 100")

secondMed75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 2, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Medium", subtitle = "Win Probability Between 75 and 100")

secondShort75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 2, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "2nd and Short", subtitle = "Win Probability Between 75 and 100")

thirdLong75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 3, Distance == "Long") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Long", subtitle = "Win Probability Between 75 and 100")

thirdMed75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 3, Distance == "Medium") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Medium", subtitle = "Win Probability Between 75 and 100")

thirdShort75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 3, Distance == "Short") %>%
  ungroup() %>%
  select(-c("Win Probability","Down","Distance")) %>%
  gt() %>%
  tab_header(title = "3rd and Short", subtitle = "Win Probability Between 75 and 100")

fourth75100 <- tables %>%
  filter(`Win Probability` == "(75,100]", Down == 4) %>%
  ungroup() %>%
  select(-c("Win Probability", "Down")) %>%
  gt() %>%
  tab_header(title = "4th Downs", subtitle = "Win Probability Between 75 and 100")

titans <- titansAndVikingsStats %>%
  filter(offense == "TEN") %>%
  select(wp2, down, length, field_pos, pass_rate, pred) %>%
  mutate(pass_rate = round(pass_rate*100,2), pred = round(pred*100,2)) %>%
  rename(`Actual Pass Rate` = "pass_rate", `Predicted Pass Rate` = "pred", `Win Probability` = "wp2",
         Down = "down", Distance = "length", `Field Position` = "field_pos")

vikings <- titansAndVikingsStats %>%
  filter(offense == "MIN") %>%
  select(wp2, down, length, field_pos, pass_rate, pred) %>%
  mutate(pass_rate = round(pass_rate*100,2), pred = round(pred*100,2)) %>%
  rename(`Actual Pass Rate` = "pass_rate", `Predicted Pass Rate` = "pred", `Win Probability` = "wp2",
         Down = "down", Distance = "length", `Field Position` = "field_pos")

titans %>%
  filter(`Win Probability` == "(0,25]") %>%
  ungroup() %>%
  select(-c("Win Probability","offense", "season")) %>%
  gt() %>%
  tab_header(title = "Titans", subtitle = "Win Probability Between 0 and 25")
