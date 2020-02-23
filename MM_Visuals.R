library(tidyverse)
library(scales)
library(gghighlight)
library(ggplot2)
library(knitr)
library(gridExtra)

#Load in data
CompactResults <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
Teams <- read_csv("data2020/MDataFiles_Stage1/MTeams.csv")
TourneySeeds <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneySeeds.csv")
Conferences <- read_csv("data2020/MDataFiles_Stage1/MTeamConferences.csv")
ConferenceNames <- read_csv("data2020/MDataFiles_Stage1/Conferences.csv")
MMasseyOrdinals <- read_csv("data2020/MDataFiles_Stage1/MMasseyOrdinals.csv")
TeamStats <- read_csv("data2020/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")

#Create dataframe to host the tourney results
TourneyResults <- CompactResults

#Create new feature that will be used in pivot_longer
TourneyResults$WTeamID_Score = paste(TourneyResults$WTeamID,
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

#Join TourneyResults and Teams by TeamID
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
TourneyResults$DayNum <- as.factor(TourneyResults$DayNum)
TourneyResults$Win <- as.factor(TourneyResults$Win)
TourneyResults$TeamName <- as.factor(TourneyResults$TeamName)
TourneyResults$TourneyRound <- as.factor(TourneyResults$TourneyRound)

#Create table of past champions
TourneyResults %>%
  filter(Season >= 2005,TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(TeamName) %>%
  select(Season,TeamName) %>%
  kable(align = "cc")

#Create plot to show the schools that won the most championship games since 1985
plot1 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Wins", title = "Championship Game Wins Since 1985") +
  coord_flip()

#Create plot to show the schools that lost the most championship games since 1985
plot2 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Losses", title = "Championship Game Loses Since 1985") +
  coord_flip()

grid.arrange(plot1, plot2, ncol=2)

#Create plot to show the schools with the most Tournament wins since 1985
plot3 <- TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Wins", title = "Tournament Wins Since 1985") +
  coord_flip()

#Create plot to show the schools with the most Tournament losses since 1985
plot4 <- TourneyResults %>%
  filter(Win == "No") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Losses", title = "Tournament Losses Since 1985") +
  coord_flip()

grid.arrange(plot3, plot4, ncol=2)

#Join TourneyResults and TourneySeeds by TeamID
TourneyResults <- TourneyResults %>%
  left_join(TourneySeeds, by = c("TeamID","Season"))

#Split region and seed  
TourneyResults <- TourneyResults %>%
  separate(Seed, into = c("Region", "Seed"), sep = 1)

#Convert new columns to factors 
TourneyResults$Region <- as.factor(TourneyResults$Region)
TourneyResults$Seed <- as.factor(TourneyResults$Seed) 

#Plot to show which seeds have won the most since 1985
plot5 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Wins", title = "Championship Game Wins Since 1985") +
  coord_flip()

#Plot to show which seeds have loss the most since 1985
plot6 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Losses", title = "Championship Game Losses Since 1985") +
  coord_flip()
grid.arrange(plot5, plot6, ncol=2)

#Plot to show which seeds have won the most since 1985
plot7 <- TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(18) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Wins", title = "Tournament Wins Since 1985") +
  coord_flip()

#Plot to show which seeds have loss the most since 1985
plot8 <- TourneyResults %>%
  filter(Win == "No") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(18) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Losses", title = "Tournament Losses Since 1985") +
  coord_flip()
grid.arrange(plot7, plot8, ncol=2)

#Plot to show which region has won the most since 1985
plot9 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Region,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Region", y = "Number of Wins", title = "Championship Game Wins Since 1985") +
  coord_flip()
plot9

#Join TourneyResults and TourneySeeds by TeamID
TourneyResults <- TourneyResults %>%
  left_join(Conferences, by = c("TeamID","Season"))

#Join TourneyResults and ConferenceNames by ConfAbbrev
TourneyResults <- TourneyResults %>%
  left_join(ConferenceNames, by = "ConfAbbrev")

#Delete ConfAbbrev
TourneyResults <- select(TourneyResults, -ConfAbbrev)

#Rename Conference Column
TourneyResults <- rename(TourneyResults, Conference = "Description")

#Convert new columns to factors 
TourneyResults$Conference <- as.factor(TourneyResults$Conference)

#Plot to show which conferences have won the most since 1985
plot10 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Wins", title = "Championship Wins Since 1985") +
  coord_flip()
plot10

#Plot to show which conferences have loss the most since 1985
plot11 <- TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Losses", title = "Championship Loses Since 1985") +
  coord_flip()
plot11

#Plot to show which conferences have won the most since 1985
plot12 <- TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Wins", title = "Tournament Wins Since 1985") +
  coord_flip()
plot12

#Plot to show which conferences have loss the most since 1985
plot13 <- TourneyResults %>%
  filter(Win == "No") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Losses", title = "Tournament Losses Since 1985") +
  coord_flip()
plot13

#Create new dataframe for winning percentage
schoolwin <- TourneyResults %>%
  select(Win:TeamName) %>%
  group_by(Win, TeamName) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = Win, values_from = Amount) %>%
  mutate('Win Percentage' = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(desc(`Win Percentage`))

#Plot top ten winning percentages
plot14 <- schoolwin[0:10,] %>%
  ggplot(aes(x = reorder(TeamName,`Win Percentage`), y = `Win Percentage`)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Winning Percentage", title = "Winning Percentage in Tournament Since 1985") +
  coord_flip()
plot14

#Dataframe for win Percentage from last 10 years
schoolwin10 <- TourneyResults %>%
  filter(Season >= 2009) %>%
  select(Win:TeamName) %>%
  group_by(Win, TeamName) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = Win, values_from = Amount) %>%
  mutate('Win Percentage' = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(desc(`Win Percentage`))

#Plot top ten winning percentage last 10 years
plot15 <- schoolwin10[0:10,] %>%
  ggplot(aes(x = reorder(TeamName,`Win Percentage`), y = `Win Percentage`)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Winning Percentage", title = "Winning Percentage in Tournament Since 2009") +
  coord_flip()
plot15

#Team Name Map
plot16 <- schoolwin10 %>%
  ggplot(aes(No, Yes))+
  geom_text(aes(label = TeamName), size = 3)+
  labs(title = "Wins vs Losses Since 2009",
       x = "Number of Losses", y = "Number of Wins")+
  theme_minimal()
plot16

#Only want to look at last 10 years
TeamStats09 <- TeamStats %>%
  filter(Season >= 2009)

#Average score per game last 10 years
plot17 <- TeamStats09 %>%
  select(Season, WScore, LScore) %>%
  rename(Winner = "WScore", Loser = "LScore") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Score = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Score, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Score") +
  labs(x = "Year", y = "Score", title = "Average Score") +
  coord_flip()

#Average field goals made per game last 10 years
plot18 <- TeamStats09 %>%
  select(Season, WFGM, LFGM) %>%
  rename(Winner = "WFGM", Loser = "LFGM") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FGM = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FGM, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FGM") +
  labs(x = "Year", y = "FGM", title = "Field Goals Made") +
  coord_flip()

#Average 3 pointers made per game last 10 years
plot19 <- TeamStats09 %>%
  select(Season, WFGM3, LFGM3) %>%
  rename(Winner = "WFGM3", Loser = "LFGM3") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FGM3 = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FGM3, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FGM3") +
  labs(x = "Year", y = "FGM3", title = "3 Pointers Made") +
  coord_flip()

#Average free throws made per game last 10 years
plot20 <- TeamStats09 %>%
  select(Season, WFTM, LFTM) %>%
  rename(Winner = "WFTM", Loser = "LFTM") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FTM = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FTM, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FTM") +
  labs(x = "Year", y = "FTM", title = "Free Throws Made") +
  coord_flip()

#Average offensive rebounds per game last 10 years
plot21 <- TeamStats09 %>%
  select(Season, WOR, LOR) %>%
  rename(Winner = "WOR", Loser = "LOR") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(OR = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = OR, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "OR") +
  labs(x = "Year", y = "OR", title = "Offensive Rebounds") +
  coord_flip()

#Average assists per game last 10 years
plot22 <- TeamStats09 %>%
  select(Season, WAst, LAst) %>%
  rename(Winner = "WAst", Loser = "LAst") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Assists = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Assists, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Assists") +
  labs(x = "Year", y = "Assists", title = "Average Assists") +
  coord_flip()

grid.arrange(plot17, plot18, plot19, plot20,plot21,plot22, ncol=2)

#Average defensive rebounds per game last 10 years
plot23 <- TeamStats09 %>%
  select(Season, WDR, LDR) %>%
  rename(Winner = "WDR", Loser = "LDR") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(DR = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = DR, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "DR") +
  labs(x = "Year", y = "DR", title = "Defensive Rebounds") +
  coord_flip()


#Average amount of turnovers per game last 10 years
plot24 <- TeamStats09 %>%
  select(Season, WTO, LTO) %>%
  rename(Winner = "WTO", Loser = "LTO") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(TO = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = TO, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "TO") +
  labs(x = "Year", y = "TO", title = "Turnovers Per Game") +
  coord_flip()

#Average number of steals per game last 10 years
plot25 <- TeamStats09 %>%
  select(Season, WStl, LStl) %>%
  rename(Winner = "WStl", Loser = "LStl") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Stl = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Stl, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Stl") +
  labs(x = "Year", y = "Stl", title = "Steals Per Game") +
  coord_flip()

#Average number of blocks per game last 10 years
plot26 <- TeamStats09 %>%
  select(Season, WBlk, LBlk) %>%
  rename(Winner = "WBlk", Loser = "LBlk") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Blk = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Blk, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Blk") +
  labs(x = "Year", y = "Blk", title = "Blocks Per Game") +
  coord_flip()

#Average number of personal fouls per game last 10 years
plot27 <- TeamStats09 %>%
  select(Season, WPF, LPF) %>%
  rename(Winner = "WPF", Loser = "LPF") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(PF = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = PF, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "PF") +
  labs(x = "Year", y = "PF", title = "Personal Fouls") +
  coord_flip()
grid.arrange(plot23, plot24, plot25, plot26,plot27, ncol=2)

BeforeTourney <- TeamStats 

#Create new feature that will be used in pivot_longer
BeforeTourney$WTeamID_Score = paste(BeforeTourney$WTeamID,
                                    BeforeTourney$WScore, sep = "_")

#Create new feature that will be used in pivot_longer
BeforeTourney$LTeamID_Score <- paste(BeforeTourney$LTeamID,
                                     BeforeTourney$LScore, sep = "_")


#Use pivot_longer to stack games
BeforeTourney <- BeforeTourney %>%
  pivot_longer(
    cols = c("WTeamID_Score","LTeamID_Score")) %>%
  separate(value, into = c("TeamID", "Score"), convert = TRUE)

#Create column to show whether a game was won or not
BeforeTourney$Win <- ifelse(BeforeTourney$name == "WTeamID_Score", "Yes","No")

#Create column to show whether a game was won or not
BeforeTourney$Winner <- ifelse(BeforeTourney$Win == "Yes", 1, 0)

#Create column to show whether a game was won or not
BeforeTourney$Loser <- ifelse(BeforeTourney$Win == "No", 1, 0)

#Create column to show whether a game was won or not
BeforeTourney$Opp_Win <- ifelse(BeforeTourney$Winner == 0, 1, 0)

#Create column to show whether a game was won or not
BeforeTourney$Opp_Lose <- ifelse(BeforeTourney$Winner == 1, 1, 0)

BeforeTourney <- BeforeTourney %>%
  left_join(Teams, by = "TeamID") %>%
  select(Season:DayNum, WFGM:LPF,TeamID:Score, Winner:TeamName) %>%
  filter(DayNum >= 75)

#Average score month before tournament
plot28 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,Score:TeamName) %>%
  group_by(TeamName) %>%
  summarise(Score = round(mean(Score),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Score)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2009")+
  theme(text = element_text(size = 7))

#Average score month before tournament
plot29 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,Score:TeamName) %>%
  group_by(TeamName) %>%
  summarise(Score = round(mean(Score),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Score)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName == "Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2013")+
  theme(text = element_text(size = 7))

#Average score month before tournament
plot30 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,Score:TeamName) %>%
  group_by(TeamName) %>%
  summarise(Score = round(mean(Score),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Score)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName == "Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2017")+
  theme(text = element_text(size = 7))

#Average score month before tournament
plot31 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,Score:TeamName) %>%
  group_by(TeamName) %>%
  summarise(Score = round(mean(Score),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Score)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName == "Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2019")+
  theme(text = element_text(size = 7))
grid.arrange(plot28,plot29,plot30,plot31,ncol = 2)

#Three pointers made per game month before tournament
plot32 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,WFGM3,WFGA3,LFGM3,LFGA3,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser), 
         avg3pt = (FGM3/FGA3)*100) %>%
  summarise(avg = round(mean(avg3pt),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = avg)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2009") +
  theme(text = element_text(size = 7))

#Three pointers made per game month before tournament
plot33 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,WFGM3,WFGA3,LFGM3,LFGA3,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser), 
         avg3pt = (FGM3/FGA3)*100) %>%
  summarise(avg = round(mean(avg3pt),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = avg)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2013") +
  theme(text = element_text(size = 7))

#Three pointers made per game month before tournament
plot34 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,WFGM3,WFGA3,LFGM3,LFGA3,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser), 
         avg3pt = (FGM3/FGA3)*100) %>%
  summarise(avg = round(mean(avg3pt),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = avg)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2017") +
  theme(text = element_text(size = 7))

#Three pointers made per game month before tournament
plot35 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,WFGM3,WFGA3,LFGM3,LFGA3,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser), 
         avg3pt = (FGM3/FGA3)*100) %>%
  summarise(avg = round(mean(avg3pt),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = avg)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot32,plot33,plot34,plot35)

#EFG% average per game month before tournament
plot36 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,WFGM,LFGM,WFGM3,LFGM3,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA = (WFGA*Winner)+(LFGA*Loser),
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100) %>%
  summarise(EFG = round(mean(EFG),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = EFG)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2009") + 
  theme(text = element_text(size = 7))

#EFG% average per game month before tournament
plot37 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,WFGM,LFGM,WFGM3,LFGM3,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA = (WFGA*Winner)+(LFGA*Loser),
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100) %>%
  summarise(EFG = round(mean(EFG),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = EFG)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2013") + 
  theme(text = element_text(size = 7))

#EFG% average per game month before tournament
plot38 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,WFGM,LFGM,WFGM3,LFGM3,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA = (WFGA*Winner)+(LFGA*Loser),
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100) %>%
  summarise(EFG = round(mean(EFG),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = EFG)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2017") + 
  theme(text = element_text(size = 7))

#EFG% average per game month before tournament
plot39 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,WFGM,LFGM,WFGM3,LFGM3,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA = (WFGA*Winner)+(LFGA*Loser),
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100) %>%
  summarise(EFG = round(mean(EFG),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = EFG)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2019") + 
  theme(text = element_text(size = 7))
grid.arrange(plot36,plot37,plot38,plot39, ncol = 2)

#Field goals attempted per game month before tournament
plot40 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser)) %>%
  summarise(FGA = round(mean(FGA),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = FGA)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2009") +
  theme(text = element_text(size = 7))

#Field goals attempted per game month before tournament
plot41 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser)) %>%
  summarise(FGA = round(mean(FGA),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = FGA)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2013") +
  theme(text = element_text(size = 7))

#Field goals attempted per game month before tournament
plot42 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser)) %>%
  summarise(FGA = round(mean(FGA),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = FGA)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2017") +
  theme(text = element_text(size = 7))

#Field goals attempted per game month before tournament
plot43 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser)) %>%
  summarise(FGA = round(mean(FGA),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = FGA)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot40,plot41,plot42,plot43, ncol = 2)

#Offense efficiency average per game month before tournament
plot44 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Poss/Score)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2009") +
  theme(text = element_text(size = 7))

#Offense efficiency average per game month before tournament
plot45 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Poss/Score)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2013") +
  theme(text = element_text(size = 7))

#Offense efficiency average per game month before tournament
plot46 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Poss/Score)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2017") +
  theme(text = element_text(size = 7))

#Offense efficiency average per game month before tournament
plot47 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Poss/Score)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot44,plot45,plot46,plot47,ncol = 2)

#Offensive rating average per game month before tournament
plot48 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2009") +
  theme(text = element_text(size = 7))

#Offensive rating average per game month before tournament
plot49 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2013") +
  theme(text = element_text(size = 7))

#Offensive rating average per game month before tournament
plot50 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2017") +
  theme(text = element_text(size = 7))

#Offensive rating average per game month before tournament
plot51 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot48,plot49,plot50,plot51, ncol=2)

#Defense efficiency average per game month before tournament
plot52 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Poss/Opp_Score)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2009") +
  theme(text = element_text(size = 7))

#Defense efficiency average per game month before tournament
plot53 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Poss/Opp_Score)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2013") +
  theme(text = element_text(size = 7))

#Defense efficiency average per game month before tournament
plot54 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Poss/Opp_Score)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2017") +
  theme(text = element_text(size = 7))

#Defense efficiency average per game month before tournament
plot55 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Poss/Opp_Score)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot52,plot53,plot54,plot55, ncol = 2)

#Defensive Ratings average per game month before tournament
plot56 <- BeforeTourney %>%
  filter(Season == 2009) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Opp_Score/Poss)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2009") +
  theme(text = element_text(size = 7))

#Defensive Ratings average per game month before tournament
plot57 <- BeforeTourney %>%
  filter(Season == 2013) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Opp_Score/Poss)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2013") +
  theme(text = element_text(size = 7))

#Defensive Ratings average per game month before tournament
plot58 <- BeforeTourney %>%
  filter(Season == 2017) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Opp_Score/Poss)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2017") +
  theme(text = element_text(size = 7))

#Defensive Ratings average per game month before tournament
plot59 <- BeforeTourney %>%
  filter(Season == 2019) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Opp_Score/Poss)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot56,plot57,plot58,plot59,ncol = 2)

#Average score month before tournament
plot60 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,Score:TeamName) %>%
  group_by(TeamName) %>%
  summarise(Score = round(mean(Score),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Score)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName == "Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#Three pointers made per game month before tournament
plot61 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,WFGM3,WFGA3,LFGM3,LFGA3,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser), 
         avg3pt = (FGM3/FGA3)*100) %>%
  summarise(avg = round(mean(avg3pt),1), 
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = avg)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#EFG% average per game month before tournament
plot62 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,WFGM,LFGM,WFGM3,LFGM3,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA = (WFGA*Winner)+(LFGA*Loser),
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100) %>%
  summarise(EFG = round(mean(EFG),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = EFG)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#Field goals attempted per game month before tournament
plot63 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,WFGA,LFGA,Winner:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser)) %>%
  summarise(FGA = round(mean(FGA),3),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = FGA)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#Offense efficiency average per game month before tournament
plot64 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Poss/Score)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#Offensive rating average per game month before tournament
plot65 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  select(Season,WFGA,LFGA,WOR,LOR,WTO,LTO,WFTA,LFTA,Score:TeamName) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100 ) %>%
  summarise(Off = mean(Off),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Off)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2009 Through 2019")+
  theme(text = element_text(size = 7))

#Defense efficiency average per game month before tournament
plot66 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Poss/Opp_Score)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2009 Through 2019") +
  theme(text = element_text(size = 7))

#Defensive Ratings average per game month before tournament
plot67 <- BeforeTourney %>%
  filter(Season >= 2009) %>%
  group_by(TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser), 
         FTA = (WFTA*Winner)+(LFTA*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def = (Opp_Score/Poss)*100 ) %>%
  summarise(Def = mean(Def),
            Games = sum(Winner + Loser),
            Wins = sum(Winner)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Wins, y = Def)) +
  gghighlight(TeamName == "Connecticut" |
                TeamName == "North Carolina" |
                TeamName ==  "Kentucky"|
                TeamName =="Duke"|
                TeamName =="Villanova"|
                TeamName =="Louisville"|
                TeamName =="Kansas"|
                TeamName =="Michigan St"|
                TeamName =="Florida"|
                TeamName =="Michigan",label_key = TeamName) +
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2009 Through 2019") +
  theme(text = element_text(size = 7))
grid.arrange(plot60,plot61,plot62,plot63,plot64,plot65,plot66,plot67,ncol=2)


# Create dataframe for key statistics
StatsImpact <- BeforeTourney %>%
  filter(Season >= 2003) %>%
  group_by(Season,TeamName) %>%
  mutate(FGA = (WFGA*Winner)+(LFGA*Loser),
         FGM2 = ((WFGM*Winner)-(WFGM3*Winner)+(LFGM*Loser)-(LFGM3*Loser)),
         FGM3 = (WFGM3*Winner)+(LFGM3*Loser),
         FGA3 = (WFGA3*Winner)+(LFGA3*Loser),
         avg3pt = (FGM3/FGA3)*100,
         EFG = ((FGM2+(1.5*FGM3))/FGA)*100,
         FTA = (WFTA*Winner)+(LFTA*Loser),
         OR = (WOR*Winner)+(LOR*Loser),
         TO = (WTO*Winner)+(LTO*Loser),
         Poss = ((FGA-OR)+TO+(0.44*FTA)), 
         Off = (Score/Poss)*100,
         Opp_Score = ((((WFGM-WFGM3)*2)+(3*WFGM3)+WFTM)*Opp_Win) +
           ((((LFGM-LFGM3)*2)+(3*LFGM3)+LFTM)*Opp_Lose),
         Def_eff = (Poss/Opp_Score)*100,
         Def_rating = (Opp_Score/Poss)*100,
         Blk = (WBlk*Winner)+(LBlk*Loser),
         DR = (WDR*Winner)+(LDR*Loser),
         Ast = (WAst*Winner)+(LAst*Loser),
         Stl = (WStl*Winner)+(LStl*Loser),
         PF = (WPF*Winner)+(LPF*Loser)) %>%
  summarise(Score = round(mean(Score),1),
            FGA = round(mean(FGA),1),
            FGM2 = round(mean(FGM2),1),
            FGM3 = round(mean(FGM3),1),
            avg3pt = round(mean(avg3pt),1),
            EFG = round(mean(EFG),1),
            FTA = round(mean(FTA),1),
            OR = round(mean(OR),1),
            TO = round(mean(TO),1),
            Poss = round(mean(Poss),1),
            Off = round(mean(Off),1),
            Opp_Score = round(mean(Opp_Score),1),
            Def_eff = round(mean(Def_eff),1),
            Def_rating = round(mean(Def_rating),1),
            Blk = round(mean(Blk),1),
            DR = round(mean(DR),1),
            Ast = round(mean(Ast),1),
            Stl = round(mean(Stl),1),
            PF = round(mean(PF),1)) 


TourneyResults <- TourneyResults %>%
  filter(Season >= 2003)

StatsImpact <- StatsImpact %>%
  left_join(TourneyResults, by = c("Season","TeamName")) %>%
  rename(Score_Season = "Score.x", Score_Tournament = "Score.y")


#EFG Impact 1st Round
plot68 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Impact 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))


#3pt Percent Impact 1st Round
plot69 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "3pt Percent Impact 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Offensive Efficiency 1st Round
plot70 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Defensive Efficiency 1st Round
plot71 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Blocks Impact 1st Round
plot72 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Blocks Impact 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Assists Impact 1st Round
plot73 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Assists Impact 1st Round") +
  coord_flip() +
  theme(text = element_text(size = 5))
grid.arrange(plot68,plot69,plot70,plot71,plot72,plot73,ncol=3)


#EFG Percent Impact 2nd Round
plot74 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#3pt Percent Impact 2nd Round
plot75 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "3pt Percent Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5)) 

#Offensive Efficiency Impact 2nd Round
plot76 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Defensive Efficiency Impact 2nd Round
plot77 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Blocks Impact 2nd Round
plot78 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Assists Impact 2nd Round
plot79 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact 2nd Round") +
  coord_flip() +
  theme(text = element_text(size = 5))
grid.arrange(plot74,plot75,plot76,plot77,plot78,plot79,ncol=3)

#EFG Percent Impact Sweet 16
plot80 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average 3pt Percent Impact Sweet 16
plot81 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Offensive Efficiency Impact Sweet 16
plot82 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Defensive Efficiency Impact Sweet 16
plot83 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Blocks Impact Sweet 16
plot84 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Assists Impact  Sweet 16
plot85 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact  Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 5))
grid.arrange(plot80,plot81,plot82,plot83,plot84,plot85,ncol=3)

#EFG Percent Impact Elite 8
plot86 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))

#3pt Percent Impact Elite 8
plot87 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "3pt Percent Impact Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Offensive Efficiency Impact Elite 8
plot88 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Defensive Efficiency Impact Elite 8
plot89 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Blocks Impact Elite 8
plot90 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Assists Impact  Elite 8
plot91 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact  Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 5))
grid.arrange(plot86,plot87,plot88,plot89,plot90,plot91,ncol=3)

#EFG Percent Impact Final Four
plot92 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average 3pt Percent Impact Final Four
plot93 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Offensive Efficiency Impact Final Four
plot94 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Defensive Efficiency Impact Final Four
plot95 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Blocks Impact Final Four
plot96 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))

#Average Assists Impact Final Four
plot97 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact Final Four") +
  coord_flip() +
  theme(text = element_text(size = 5))
grid.arrange(plot92,plot93,plot94,plot95,plot96,plot97,ncol=3)

#EFG Percent Impact Championship
plot98 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact Championship") +
  coord_flip() +
  theme(text = element_text(size = 6))

#Average 3pt Percent Impact Championship
plot99 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact Championship") +
  coord_flip() +
  theme(text = element_text(size = 6))

#Offensive Efficiency Impact Championship
plot100 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact Championship") +
  coord_flip() +
  theme(text = element_text(size = 6))

#Defensive Efficiency Impact Championship Game
plot101 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact Championship Game") +
  coord_flip() +
  theme(text = element_text(size = 6))

#Average Blocks Impact Championship Game
plot102 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact Championship Game") +
  coord_flip() +
  theme(text = element_text(size = 6))

#Average Assists Impact Championship Game
plot103 <- StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact Championship Game") +
  coord_flip() +
  theme(text = element_text(size = 6))
grid.arrange(plot98,plot99,plot100,plot101,plot102,plot103,ncol=3)

#Make table to show most rankings
MMasseyOrdinals %>%
  group_by(SystemName) %>%
  summarise(Amount = n()) %>%
  arrange(desc(Amount)) %>%
  rename(`Ranking Name` = "SystemName",`Number of Rankings` = "Amount") %>%
  head(10) %>%
  kable(align = "cc")

MMasseyOrdinals <- MMasseyOrdinals %>%
  filter((SystemName == "SAG" |
            SystemName == "MOR" |
            SystemName == "POM") &
           Season >= 2009 & 
           RankingDayNum == 133) %>%
  pivot_wider(names_from = SystemName, values_from = OrdinalRank)


Rankings <- TourneyResults %>%
  filter(Season >= 2009) %>%
  left_join(MMasseyOrdinals, by = c("TeamID", "Season"))

#Modify dataframe
Rank <- CompactResults %>%
  filter(Season >= 2009) %>%
  left_join(MMasseyOrdinals, by = c("Season", "WTeamID" = "TeamID")) %>%
  left_join(MMasseyOrdinals, by = c("Season", "LTeamID" = "TeamID")) %>%
  rename( MOR_win = "MOR.x",
          POM_win = "POM.x",
          SAG_win = "SAG.x",
          MOR_lose = "MOR.y",
          POM_lose = "POM.y",
          SAG_lose = "SAG.y") %>%
  select(Season:LScore, MOR_win:SAG_win, MOR_lose:SAG_lose) %>%
  mutate(`MOR Higher Seed Win` = ifelse(MOR_win < MOR_lose, "Yes", "No")) %>%
  mutate(`POM Higher Seed Win` = ifelse(POM_win < POM_lose, "Yes", "No")) %>%
  mutate(`SAG Higher Seed Win` = ifelse(SAG_win < SAG_lose, "Yes", "No")) %>%
  mutate(TourneyRound = ifelse(DayNum %in% c(134, 135), "Play in",
                               ifelse(DayNum %in% c(136, 137), "First Round",
                                      ifelse(DayNum %in% c(138, 139), "Second Round",
                                             ifelse(DayNum %in% c(143, 144), "Sweet 16",
                                                    ifelse(DayNum %in% c(145, 146), "Elite 8",
                                                           ifelse(DayNum == 152, "Final Four", "Championship Game")))))))


MOR <- Rank %>%
  group_by(Season, `MOR Higher Seed Win`) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = `MOR Higher Seed Win`, values_from = Amount) %>%
  mutate(Percentage = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(Season)

POM <- Rank %>%
  group_by(Season, `POM Higher Seed Win`) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = `POM Higher Seed Win`, values_from = Amount) %>%
  mutate(Percentage = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(Season)

SAG <- Rank %>%
  group_by(Season, `SAG Higher Seed Win`) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = `SAG Higher Seed Win`, values_from = Amount) %>%
  mutate(Percentage = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(Season) 

higher_seed <- MOR %>%
  left_join(POM, by = "Season") %>%
  left_join(SAG, by = "Season") %>%
  rename(MOR_Percent = "Percentage.x",
         POM_Percent = "Percentage.y",
         SAG_Percent = "Percentage") %>%
  gather("Rankings","Percent", "MOR_Percent", "POM_Percent", "SAG_Percent")

higher_seed$Rankings <- as.factor(higher_seed$Rankings)
higher_seed$Season <- as.factor(higher_seed$Season)

#Percent Higher Ranked Wins in Tournament
plot104 <- higher_seed %>%
  ggplot(aes(x = Season, y = Percent, fill = Rankings)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_discrete(name = "Season") +
  scale_y_continuous(name = "Percent") +
  labs(x = "Year", y = "Percent", title = "Percent Higher Ranked Wins in Tournament") +
  coord_flip()
plot104

########################################################################################################
#Rankings on First Round
########################################################################################################

plot105 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on First Round") +
  coord_flip() +
  theme(text = element_text(size = 6))

plot106 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on First Round") +
  coord_flip() +
  theme(text = element_text(size = 6))

plot107 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on First Round") +
  coord_flip() +
  theme(text = element_text(size = 6))
grid.arrange(plot105,plot106,plot107, ncol = 3)

########################################################################################################
#Rankings on 2nd Round
########################################################################################################

plot108 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Second Round") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot109 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Second Round") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot110 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Second Round") +
  coord_flip() +
  theme(text = element_text(size = 7))
grid.arrange(plot108,plot109,plot110, ncol = 3)

########################################################################################################
#Rankings on Sweet 16
########################################################################################################

plot111 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot112 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot113 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Sweet 16") +
  coord_flip() +
  theme(text = element_text(size = 7))
grid.arrange(plot111,plot112,plot113, ncol = 3)

########################################################################################################
#Rankings on Elite 8
########################################################################################################

plot114 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot115 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot116 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Elite 8") +
  coord_flip() +
  theme(text = element_text(size = 7))
grid.arrange(plot114,plot115,plot116, ncol = 3)

########################################################################################################
#Rankings on Final 4
########################################################################################################

plot117 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Final Fou") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot118 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Final Fou") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot119 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Final Fou") +
  coord_flip() +
  theme(text = element_text(size = 7))
grid.arrange(plot117,plot118,plot119, ncol = 3)

########################################################################################################
#Rankings on Championship
########################################################################################################

plot120 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Championship") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot121 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Championship") +
  coord_flip() +
  theme(text = element_text(size = 7))

plot122 <- Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Championship") +
  coord_flip() +
  theme(text = element_text(size = 7))
grid.arrange(plot120,plot121,plot122, ncol = 3)









