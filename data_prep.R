library(tidyverse)


#Load in data
CompactResults <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
Teams <- read_csv("data2020/MDataFiles_Stage1/MTeams.csv")
TourneySeeds <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneySeeds.csv")
Conferences <- read_csv("data2020/MDataFiles_Stage1/MTeamConferences.csv")
ConferenceNames <- read_csv("data2020/MDataFiles_Stage1/Conferences.csv")
MMasseyOrdinals <- read_csv("data2020/MDataFiles_Stage1/MMasseyOrdinals.csv")
TeamStats <- read_csv("data2020/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")


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

#Filter from day 110
BeforeTourney <- BeforeTourney %>%
  left_join(Teams, by = "TeamID") %>%
  select(Season:DayNum, WFGM:LPF,TeamID:Score, Winner:TeamName) %>%
  filter(DayNum >= 103)


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

#Add TeamID to StatsImpact
StatsImpact <- StatsImpact %>%
  left_join(Teams, by = "TeamName")

#Join Conference and Conference Names
Conferences <- Conferences %>%
  left_join(ConferenceNames, by = "ConfAbbrev")

#Filter Seasons in Conference dataframe
Conferences <- Conferences %>%
  filter(Season >= 2003)

#Join Conference and StatsImpact
StatsImpact <- StatsImpact %>%
  left_join(Conferences, by = c("Season","TeamID"))

#Filter MMasseyOrdinals for POM last 20 days
POM_last20 <- MMasseyOrdinals %>%
  filter(SystemName == "POM" & RankingDayNum >= 110) %>%
  group_by(Season,TeamID) %>%
  summarise(POM = round(mean(OrdinalRank),1))

#Join MMMassey Ordinals to StatsImpact
StatsImpact <- StatsImpact %>%
  left_join(POM_last20, by = c("Season","TeamID"))

#Reorder Columns
StatsImpact <- StatsImpact[,c(1,22,2,26,27,3:21,23:25)]

#Take out unwanted columns
StatsImpact <- StatsImpact %>%
  select(Season:PF)

##############################################################################################

#Load in Tournament Seeds dataframe
TourneySeeds <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneySeeds.csv")

#Merge Dataframe with itself so that every combination of games can be created
Seeds <- TourneySeeds %>%
  inner_join(TourneySeeds, by = "Season")

#To only have each match up once and not have any match ups where a team plays themselves
#filter the games where the lower id team is always in the TeamID.x column
Seeds <- Seeds %>%
  filter(TeamID.x < TeamID.y)

#Create new feature Season_LowerID_HigherID
Seeds$ID <- paste(Seeds$Season, 
                          Seeds$TeamID.x,
                          Seeds$TeamID.y,sep = "_")

#Create dataframe TourneyResults and filter for seasons 2003 on
TourneyResults <- Seeds %>%
  filter(Season >= 2003)

#Separate Region from seed number
TourneyResults$Region_x <- substring(TourneyResults$Seed.x,1,1)
TourneyResults$Region_y <- substring(TourneyResults$Seed.y,1,1)

#Isolate seed number
TourneyResults$Seed_x <- as.numeric(substring(TourneyResults$Seed.x,2,3))
TourneyResults$Seed_y <- as.numeric(substring(TourneyResults$Seed.y,2,3))

#Join TourneyResults and StatsImpact
TourneyResults <- TourneyResults %>%
  left_join(StatsImpact, by = c("Season", "TeamID.x" = "TeamID")) %>%
  left_join(StatsImpact, by = c("Season", "TeamID.y" = "TeamID"))

#Load in compact results
CompactResults <- read_csv("data2020/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")

#Create a new feature for ID
CompactResults <- CompactResults %>%
  mutate(ID = ifelse(WTeamID < LTeamID, paste(CompactResults$Season,
                                              CompactResults$WTeamID,
                                              CompactResults$LTeamID,sep = "_" ), 
                     paste(CompactResults$Season,
                           CompactResults$LTeamID,
                           CompactResults$WTeamID,sep = "_" )))

#Only want lower ID team first and seasons 2003 and on
CompactResults <- CompactResults %>%
  mutate(Results = 1*(WTeamID<LTeamID)) %>%
  filter(Season >= 2003)

#Join CompactResults and TourneySeeds 
CompactResults <- CompactResults %>%
  inner_join(TourneySeeds, by = c("Season","WTeamID" = "TeamID")) %>%
  inner_join(TourneySeeds, by = c("Season","LTeamID" = "TeamID"))

#Filter out unneccessary columns
CompactResults <- CompactResults %>%
  select(ID,Results)

#Join TourneyResults and CompactResults
TourneyResults <- TourneyResults %>%
  left_join(CompactResults, by = "ID")

#Filter out unneccessary columns
TourneyResults <- TourneyResults %>%
  select(Season,TeamID.x,TeamID.y,ID,Seed_x:Seed_y,TeamName.x:Results)

#Rename some columns
TourneyResults <- TourneyResults %>%
  rename(Seed.x = "Seed_x", Seed.y = "Seed_y",Conference.x = "Description.x",
         Conference.y = "Description.y")

#Reorder Columns
TourneyResults <- TourneyResults[,c(1,4,51,2:3,7,29,8,30,9,31,5:6,10,32,
                                    11,33,12,34,13,35,14,36,15,37,16,38,
                                    17,39,18,40,19,41,20,42,21,43,22,44,
                                    23,45,24,46,25,47,26,48,27,49,28,50)]

#Save Dataframe
write_csv(TourneyResults,"/Users/jeremykight/Desktop/R Directory/march_madness/TourneyResults.csv")


#Normalize data
TourneyNormal <- read_csv("TourneyResults.csv")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

TourneyNormal$POM.x  <- normalize(TourneyNormal$POM.x)
TourneyNormal$POM.y  <- normalize(TourneyNormal$POM.y)

TourneyNormal$Seed.x  <- normalize(TourneyNormal$Seed.x)
TourneyNormal$Seed.y  <- normalize(TourneyNormal$Seed.y)

TourneyNormal$Score.x  <- normalize(TourneyNormal$Score.x)
TourneyNormal$Score.y  <- normalize(TourneyNormal$Score.y)

TourneyNormal$FGA.x  <- normalize(TourneyNormal$FGA.x)
TourneyNormal$FGA.y  <- normalize(TourneyNormal$FGA.y)

TourneyNormal$FGM2.x  <- normalize(TourneyNormal$FGM2.x)
TourneyNormal$FGM2.y  <- normalize(TourneyNormal$FGM2.y)

TourneyNormal$FGM3.x  <- normalize(TourneyNormal$FGM3.x)
TourneyNormal$FGM3.y  <- normalize(TourneyNormal$FGM3.y)

TourneyNormal$avg3pt.x  <- normalize(TourneyNormal$avg3pt.x)
TourneyNormal$avg3pt.y  <- normalize(TourneyNormal$avg3pt.y)

TourneyNormal$EFG.x  <- normalize(TourneyNormal$EFG.x)
TourneyNormal$EFG.y  <- normalize(TourneyNormal$EFG.y)

TourneyNormal$FTA.x  <- normalize(TourneyNormal$FTA.x)
TourneyNormal$FTA.y  <- normalize(TourneyNormal$FTA.y)

TourneyNormal$OR.x  <- normalize(TourneyNormal$OR.x)
TourneyNormal$OR.y  <- normalize(TourneyNormal$OR.y)

TourneyNormal$TO.x  <- normalize(TourneyNormal$TO.x)
TourneyNormal$TO.y  <- normalize(TourneyNormal$TO.y)

TourneyNormal$Poss.x  <- normalize(TourneyNormal$Poss.x)
TourneyNormal$Poss.y  <- normalize(TourneyNormal$Poss.y)

TourneyNormal$Off.x  <- normalize(TourneyNormal$Off.x)
TourneyNormal$Off.y  <- normalize(TourneyNormal$Off.y)

TourneyNormal$Opp_Score.x  <- normalize(TourneyNormal$Opp_Score.x)
TourneyNormal$Opp_Score.y  <- normalize(TourneyNormal$Opp_Score.y)

TourneyNormal$Def_eff.x  <- normalize(TourneyNormal$Def_eff.x)
TourneyNormal$Def_eff.y  <- normalize(TourneyNormal$Def_eff.y)

TourneyNormal$Def_rating.x  <- normalize(TourneyNormal$Def_rating.x)
TourneyNormal$Def_rating.y  <- normalize(TourneyNormal$Def_rating.y)

TourneyNormal$Blk.x  <- normalize(TourneyNormal$Blk.x)
TourneyNormal$Blk.y  <- normalize(TourneyNormal$Blk.y)

TourneyNormal$DR.x  <- normalize(TourneyNormal$DR.x)
TourneyNormal$DR.y  <- normalize(TourneyNormal$DR.y)

TourneyNormal$Ast.x  <- normalize(TourneyNormal$Ast.x)
TourneyNormal$Ast.y  <- normalize(TourneyNormal$Ast.y)

TourneyNormal$Stl.x  <- normalize(TourneyNormal$Stl.x)
TourneyNormal$Stl.y  <- normalize(TourneyNormal$Stl.y)

TourneyNormal$PF.x  <- normalize(TourneyNormal$PF.x)
TourneyNormal$PF.y  <- normalize(TourneyNormal$PF.y)

#Save Dataframe
write_csv(TourneyNormal,"/Users/jeremykight/Desktop/R Directory/march_madness/TourneyNormal.csv")

#Find the difference in features
TourneyNormalDiff <- TourneyNormal %>%
  mutate(POM_diff = POM.x - POM.y,
         Seed_diff = Seed.x - Seed.y,
         Score_diff = Score.x - Score.y,
         FGA_diff = FGA.x - FGA.y,
         FGM2_diff = FGM2.x - FGM2.y,
         FGM3_dff = FGM3.x - FGM3.y,
         Avg3pt_diff = avg3pt.x - avg3pt.y,
         EFG_diff = EFG.x - EFG.y,
         FTA_diff = FTA.x - FTA.y,
         OR_diff = OR.x - OR.y,
         TO_diff = TO.x - TO.y,
         POSS_diff = Poss.x - Poss.y,
         OFF_diff = Off.x - Off.y,
         OPP_SCORE_diff = Opp_Score.x - Opp_Score.y,
         DEF_EFF_diff = Def_eff.x - Def_eff.y,
         BLK_diff = Blk.x - Blk.y,
         DR_diff = DR.x - DR.y,
         AST_diff = Ast.x - Ast.y,
         STL_diff = Stl.x - Stl.y,
         PF_diff = PF.x - PF.y)

#Keep only selected columns
TourneyNormalDiff <- TourneyNormalDiff %>%
  select(Season:Conference.y,POM_diff:PF_diff)
     
#Save dataframe    
write_csv(TourneyNormalDiff,"/Users/jeremykight/Desktop/R Directory/march_madness/TourneyDiff.csv")
