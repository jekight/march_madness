library(tidyverse)
library(scales)
library(gghighlight)

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

#Create plot to show the schools with the most championships since 1985
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Wins", title = "Championship Game") +
  coord_flip()

#Create plot to show the schools that lost the most championship games since 1985
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Losses", title = "Championship Game") +
  coord_flip()

#Create plot to show the schools with the most Tournament wins since 1985
TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Wins", title = "Tournament Wins") +
  coord_flip()

#Create plot to show the schools with the most Tournament losses since 1985
TourneyResults %>%
  filter(Win == "No") %>%
  group_by(TeamName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(TeamName,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Number of Losses", title = "Tournament Losses") +
  coord_flip()

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
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Wins", title = "Championship Game") +
  coord_flip()

#Plot to show which seeds have loss the most since 1985
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Losses", title = "Championship Game") +
  coord_flip()

#Plot to show which seeds have loss the most since 1985
TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(18) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Wins", title = "Tournament Wins") +
  coord_flip()

#Plot to show which seeds have loss the most since 1985
TourneyResults %>%
  filter(Win == "No") %>%
  group_by(Seed) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(18) %>%
  ggplot(aes(x = reorder(Seed,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Seed Number", y = "Number of Losses", title = "Tournament Losses") +
  coord_flip()

#Plot to show which region has won the most since 1985
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Region,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Region", y = "Number of Wins", title = "Championship Game") +
  coord_flip()

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
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "Yes") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Wins", title = "Championship Game") +
  coord_flip()

#Plot to show which conferences have loss the most since 1985
TourneyResults %>%
  filter(TourneyRound == "Championship Game" & Win == "No") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Losses", title = "Championship Game") +
  coord_flip()


#Plot to show which conferences have won the most since 1985
TourneyResults %>%
  filter(Win == "Yes") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Wins", title = "Tournament Wins") +
  coord_flip()

#Plot to show which conferences have loss the most since 1985
TourneyResults %>%
  filter(Win == "No") %>%
  group_by(Conference) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(Conference,n), y = n)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "Conference", y = "Number of Losses", title = "Tournament Losses") +
  coord_flip()


#######################################################################################
#Find win percentage of top 10 teams in tourney
#######################################################################################

#Create new dataframe for winning percentage
schoolwin <- TourneyResults %>%
  select(Win:TeamName) %>%
  group_by(Win, TeamName) %>%
  summarise(Amount = n()) %>%
  pivot_wider(names_from = Win, values_from = Amount) %>%
  mutate('Win Percentage' = round(((Yes / (Yes + No))*100),1)) %>%
  arrange(desc(`Win Percentage`))

#Plot top ten winning percentages
schoolwin[0:10,] %>%
  ggplot(aes(x = reorder(TeamName,`Win Percentage`), y = `Win Percentage`)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Winning Percentage", title = "Winning Percentage in Tournament") +
  coord_flip()

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
schoolwin10[0:10,] %>%
  ggplot(aes(x = reorder(TeamName,`Win Percentage`), y = `Win Percentage`)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Winning Percentage", title = "Winning Percentage in Tournament Since 2009") +
  coord_flip()

#Team Name Map
schoolwin10 %>%
  ggplot(aes(No, Yes))+
  geom_text(aes(label = TeamName), size = 3)+
  labs(title = "Wins vs Losses Since 2009",
       x = "Number of Losses", y = "Number of Wins")+
  theme_minimal()

###############################################################################################################
#Find out how accurate the final rankings are
###############################################################################################################

MMasseyOrdinals %>%
  group_by(SystemName) %>%
  summarise(Amount = n()) %>%
  arrange(desc(Amount)) %>%
  head(10)


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

higher_seed %>%
  ggplot(aes(x = Season, y = Percent, fill = Rankings)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_discrete(name = "Season") +
  scale_y_continuous(name = "Percent") +
  labs(x = "Year", y = "Percent", title = "Percent Higher Ranked Wins") +
  coord_flip()

######################################################################################
#Team Statistics
######################################################################################

#Only want to look at last 10 years
TeamStats09 <- TeamStats %>%
  filter(Season >= 2009)

#Average score per game last 10 years
TeamStats09 %>%
  select(Season, WScore, LScore) %>%
  rename(Winner = "WScore", Loser = "LScore") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Score = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Score, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Score") +
  labs(x = "Year", y = "Score", title = "Average Score Per Game Since 2009") +
  coord_flip()

#Average field goals made per game last 10 years
TeamStats09 %>%
  select(Season, WFGM, LFGM) %>%
  rename(Winner = "WFGM", Loser = "LFGM") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FGM = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FGM, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FGM") +
  labs(x = "Year", y = "FGM", title = "Average Field Goals Made Per Game Since 2009") +
  coord_flip()

#Average 3 pointers made per game last 10 years
TeamStats09 %>%
  select(Season, WFGM3, LFGM3) %>%
  rename(Winner = "WFGM3", Loser = "LFGM3") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FGM3 = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FGM3, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FGM3") +
  labs(x = "Year", y = "FGM3", title = "Average 3 Pointers Made Per Game Since 2009") +
  coord_flip()

#Average free throws made per game last 10 years
TeamStats09 %>%
  select(Season, WFTM, LFTM) %>%
  rename(Winner = "WFTM", Loser = "LFTM") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(FTM = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = FTM, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "FTM") +
  labs(x = "Year", y = "FTM", title = "Average Free Throws Made Per Game Since 2009") +
  coord_flip()

#Average offensive rebounds per game last 10 years
TeamStats09 %>%
  select(Season, WOR, LOR) %>%
  rename(Winner = "WOR", Loser = "LOR") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(OR = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = OR, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "OR") +
  labs(x = "Year", y = "OR", title = "Average Offensive Rebounds Per Game Since 2009") +
  coord_flip()

#Average defensive rebounds per game last 10 years
TeamStats09 %>%
  select(Season, WDR, LDR) %>%
  rename(Winner = "WDR", Loser = "LDR") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(DR = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = DR, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "DR") +
  labs(x = "Year", y = "DR", title = "Average Defensive Rebounds Per Game Since 2009") +
  coord_flip()

#Average assists per game last 10 years
TeamStats09 %>%
  select(Season, WAst, LAst) %>%
  rename(Winner = "WAst", Loser = "LAst") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Ast = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Ast, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Ast") +
  labs(x = "Year", y = "Ast", title = "Average Assist Per Game Since 2009") +
  coord_flip()

#Average amount of turnovers per game last 10 years
TeamStats09 %>%
  select(Season, WTO, LTO) %>%
  rename(Winner = "WTO", Loser = "LTO") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(TO = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = TO, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "TO") +
  labs(x = "Year", y = "TO", title = "Average Turnovers Per Game Since 2009") +
  coord_flip()

#Average number of steals per game last 10 years
TeamStats09 %>%
  select(Season, WStl, LStl) %>%
  rename(Winner = "WStl", Loser = "LStl") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Stl = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Stl, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Stl") +
  labs(x = "Year", y = "Stl", title = "Average Steals Per Game Since 2009") +
  coord_flip()

#Average number of blocks per game last 10 years
TeamStats09 %>%
  select(Season, WBlk, LBlk) %>%
  rename(Winner = "WBlk", Loser = "LBlk") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(Blk = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = Blk, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "Blk") +
  labs(x = "Year", y = "Blk", title = "Average Blocks Per Game Since 2009") +
  coord_flip()

#Average number of personal fouls per game last 10 years
TeamStats09 %>%
  select(Season, WPF, LPF) %>%
  rename(Winner = "WPF", Loser = "LPF") %>%
  pivot_longer(cols = c("Winner","Loser"), names_to = "Outcome") %>%
  group_by(Season,Outcome) %>%
  summarise(PF = round(mean(value),1)) %>%
  ggplot(aes(x = Season, y = PF, fill = Outcome)) +
  geom_bar(stat="identity",color = "black", position = "dodge") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(name = "PF") +
  labs(x = "Year", y = "PF", title = "Average Personal Fouls Per Game Since 2009") +
  coord_flip()

######################################################################################################
#Stats before tourney 2009
######################################################################################################

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
BeforeTourney %>%
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
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2009")
  

#Three pointers made per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2009")

#EFG% average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2009")


#Field goals attempted per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2009")


#Offense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2009")


#Offensive rating average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2009")


#Defense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2009")


#Defensive Ratings average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2009")


######################################################################################################
#Stats before tourney 2013
######################################################################################################

#Average score month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2013")


#Three pointers made per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2013")

#EFG% average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2013")


#Field goals attempted per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2013")


######################################################################################################
#Stats before tourney 2018
######################################################################################################

#Average score month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2018")


#Three pointers made per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2018")

#EFG% average per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2018")


#Field goals attempted per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2018")


#Offense efficiency average per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2018")


#Offensive rating average per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2018")


#Defense efficiency average per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2018")


#Defensive Ratings average per game month before tournament
BeforeTourney %>%
  filter(Season == 2018) %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2018")


######################################################################################################
#Stats before tourney 2019
######################################################################################################

#Average score month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2019")


#Three pointers made per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2019")

#EFG% average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2019")


#Field goals attempted per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2019")


#Offense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2019")


#Offensive rating average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2019")


#Defense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2019")


#Defensive Ratings average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2019")


######################################################################################################
#Stats before tourney 2009 through 2019
######################################################################################################

#Average score month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Score", title = "Average Score Per Game 2009 Through 2019")


#Three pointers made per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Three Point Average Per Game 2009 Through 2019")

#EFG% average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average EFG Percent Per Game 2009 Through 2019")


#Field goals attempted per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "FGA", title = "Average Field Goals Attempted Per Game 2009 Through 2019")


#Offense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Efficiency Per Game 2009 Through 2019")


#Offensive rating average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Offensive Rating Per Game 2009 Through 2019")


#Defense efficiency average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defense Efficiency Per Game 2009 Through 2019")


#Defensive Ratings average per game month before tournament
BeforeTourney %>%
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
  labs(x = "Wins", y = "Percent", title = "Average Defensive Rating Per Game 2009 Through 2019")


######################################################################################################
#Stats tourney success
######################################################################################################

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


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win == "Yes") %>%
  ggplot() +
  geom_point(mapping = aes(x = Score_Season, y = Score_Tournament))

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win == "No") %>%
  ggplot() +
  geom_point(mapping = aes(x = Score_Season, y = Score_Tournament))


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot() +
  geom_point(mapping = aes(x = Score_Season, y = Score_Tournament, color = Win))

##############################################################################################################
#Stats Impact on First Round
##############################################################################################################

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on First Round") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on First Round") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on First Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on First Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on First Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "First Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on First Round") +
  coord_flip()

##############################################################################################################
#Stats Impact on Second Round 
##############################################################################################################

  
StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on Second Round") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on Second Round") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Second Round" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on Second Round") +
  coord_flip()

##############################################################################################################
#Stats Impact on Sweet 16 
##############################################################################################################


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on Tournament") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on Sweet 16") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on Sweet 16") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on Sweet 16") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on Sweet 16") +
  coord_flip()

##############################################################################################################
#Stats Impact on Elite 8 
##############################################################################################################


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on Tournament") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on Elite 8") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on Elite 8") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on Elite 8") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Elite 8" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on Elite 8") +
  coord_flip()

##############################################################################################################
#Stats Impact on Final Four 
##############################################################################################################


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on Tournament") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,avg3pt), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on Second Round") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on Final Four") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on Final Four") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on Final Four") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Final Four" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on Final Four") +
  coord_flip()

##############################################################################################################
#Stats Impact on Championship 
##############################################################################################################

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = EFG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "EFG Percent", title = "EFG Percent Impact on Tournament") +
  coord_flip()


StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,EFG), y = avg3pt,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Average 3pt Percent", title = "Average 3pt Percent Impact on Championship") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Off), y = Off,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Offensive Efficiency", title = "Offensive Efficiency Impact on Championship") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Def_eff), y = Def_eff,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Defensive Efficiency", title = "Defensive Efficiency Impact on Championship Game") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Blk), y = Blk,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Blocks", title = "Average Blocks Impact on Championship Game") +
  coord_flip()

StatsImpact %>%
  filter(Season == 2019 & TourneyRound == "Championship Game" & Win != "NA") %>%
  ggplot(aes(x = reorder(TeamName,Ast), y = Ast,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ast", title = "Average Assists Impact on Championship Game") +
  coord_flip()

#####################################################################################################
#Final Rankings
#####################################################################################################

Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on First Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Second Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Sweet 16") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Elite 8") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Final Four") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,MOR), y = MOR,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "MOR Ranking Impact on Championship Game") +
  coord_flip()

####################################################################################################

Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on First Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Second Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Sweet 16") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Elite 8") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Final Four") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,POM), y = POM,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "POM Ranking Impact on Championship Game") +
  coord_flip()

##################################################################################################
#SAG

Rankings %>%
  filter(Season == 2019 & TourneyRound == "First Round") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on First Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Second Round") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Second Round") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Sweet 16") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Sweet 16") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Elite 8") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Elite 8") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Final Four") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Final Four") +
  coord_flip()

Rankings %>%
  filter(Season == 2019 & TourneyRound == "Championship Game") %>%
  ggplot(aes(x = reorder(TeamName,SAG), y = SAG,fill = Win)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks= pretty_breaks()) +
  labs(x = "School", y = "Ranking", title = "SAG Ranking Impact on Championship Game") +
  coord_flip()

