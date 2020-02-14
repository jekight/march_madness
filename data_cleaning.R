library(tidyverse)


TourneyResults <- read_csv("data/Stage2DataFiles/NCAATourneyCompactResults.csv")
Teams <- read_csv("data/DataFiles/Teams.csv")

TourneyResults <- TourneyResults %>%
  left_join(Teams, by = c("WTeamID" = "TeamID")) %>%
  left_join(Teams, by = c("LTeamID" = "TeamID"))

TourneyResults <- TourneyResults %>%
  rename(Winner = "TeamName.x",
         Loser = "TeamName.y")

TourneyResults$season_day <- paste(TourneyResults$Season, 
                                   TourneyResults$DayNum, sep = "_")
