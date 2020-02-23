library(tidyverse)

#Load in data
TourneyResults <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
Teams <- read_csv("data2020/MDataFiles_Stage1/MTeams.csv")

#Create new feature that will be used in pivot_longer
TourneyResults$WTeamID_Score <- paste(TourneyResults$WTeamID,
                                      TourneyResults$WScore, sep = "_")

#Create new feature that will be used in pivot_longer
TourneyResults$LTeamID_Score <- paste(TourneyResults$LTeamID,
                                      TourneyResults$LScore, sep = "_")

#Use pivot_longer to stack games
TourneyResults <- TourneyResults %>%
  pivot_longer(
    cols = c("WTeamID_Score","LTeamID_Score")) %>%
  separate(value, into = c("TeamID", "Score"), convert = TRUE)

#Create column to show whether a game was won or not
TourneyResults$Win <- ifelse(TourneyResults$name == "WTeamID_Score", "Yes","No")

#Join dataframes by TeamID
TourneyResults <- TourneyResults %>%
  left_join(Teams, by = "TeamID")

#Delete unnecessary columns
TourneyResults <- select(TourneyResults, Season:DayNum, TeamID:TeamName)

#Create feature to show what round the game is in
TourneyResults <- TourneyResults %>%
  mutate(TourneyRound = ifelse(DayNum %in% c(134, 135), "Play in",
                          ifelse(DayNum %in% c(136, 137), "First Round",
                            ifelse(DayNum %in% c(138, 139), "Second Round",
                              ifelse(DayNum %in% c(143, 144), "Sweet 16",
                                ifelse(DayNum %in% c(145, 146), "Elite 8",
                                  ifelse(DayNum == 152, "Final Four", "Championship Game")))))))

#Convert desired columns to Factors
TourneyResults$Season <- as.factor(TourneyResults$Season)
TourneyResults$DayNum <- as.factor(TourneyResults$DayNum)
TourneyResults$TeamID <- as.factor(TourneyResults$TeamID)
TourneyResults$Win <- as.factor(TourneyResults$Win)
TourneyResults$TeamName <- as.factor(TourneyResults$TeamName)
TourneyResults$TourneyRound <- as.factor(TourneyResults$TourneyRound)


TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  coord_flip()

TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  coord_flip()


