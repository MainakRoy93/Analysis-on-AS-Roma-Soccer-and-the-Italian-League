library(dplyr)
library(tidyr)
library(tidyverse)

full_file <- read.csv("match_player_attributes_with_name.csv")

##### formation

get_formation <- function (row, desiredCols) {
  print(row[,desiredCols])
  defender_count <- 0
  midfielder_count <- 0
  attacker_count <- 0
  for (val in row[,desiredCols]) {
    if (val >= 2 & val <= 5) {
      defender_count = 1 + defender_count
    } else if (val >= 6 & val <= 8) {
      midfielder_count = 1 + midfielder_count
    } else if (val >= 9 & val <= 11) {
      attacker_count = attacker_count + 1
    }
  }
  
  formation<- paste(c(defender_count, midfielder_count, attacker_count), collapse = "_" )
  return(formation)
}


for(i in 1:nrow(full_file)) {
  row <- full_file[i,]
  home_player_Y <- 30:39
  away_player_Y <- 41:50
  home_formation<- paste(c(get_formation(row, home_player_Y)), collapse = "_" )
  full_file$home_formation[i]=home_formation
  away_formation<- paste(c(get_formation(row, away_player_Y)), collapse = "_" )
  full_file$away_formation[i]=away_formation
  
}

#Roma vs Juventus (Home)
HOMETEAM <- 'Roma'
AWAYTEAM <- 'Juventus'

full_file_ROMA <-full_file %>% filter(home_team_api_id=='Roma')%>% select("y1.15","y1.16","y1.17","y1.20","y1.21","y1.22"
                                                         ,"y1.25","y1.26","y1.27","y1.45",
                                                         "y1.46","y1.47","y1.50","y1.51",
                                                         "y1.52","y1.55","y1.56","y1.57","DETAILED_OUTCOME")

full_file <- full_file %>% filter(home_formation == '4_3_3' | away_formation == '4_3_3')
full_file_WIN <- ifelse((full_file$home_team_goal> full_file$away_team_goal) & (full_file$home_formation == '4_3_3'),"HOME_WIN",
                        ifelse((full_file$away_team_goal > full_file$home_team_goal) & (full_file$away_formation == '4_3_3'),"AWAY_WIN",
                              "DONT KNOW" ))
###SECOND TRY
full_file_WIN_TRY <- ifelse((full_file$home_team_goal> full_file$away_team_goal) & (full_file$home_formation == '4_3_3'),"HOME_WIN",
                        ifelse((full_file$away_team_goal > full_file$home_team_goal) & (full_file$away_formation == '4_3_3'),"AWAY_WIN",
                               ifelse((full_file$home_team_goal == full_file$away_team_goal),"TIE",
                                      ifelse((full_file$home_team_goal> full_file$away_team_goal) & (full_file$away_formation == '4_3_3'),"AWAY_LOSS"
                                             ,"HOME_LOSS"))))

full_file$FORMATION_OUTCOME <- full_file_WIN
###SECOND TRY
full_file$DETAILED_OUTCOME <- full_file_WIN_TRY

full_file_filtered <- full_file %>% select("y1.15","y1.16","y1.17","y1.20","y1.21","y1.22"
                                                ,"y1.25","y1.26","y1.27","y1.45",
                                                "y1.46","y1.47","y1.50","y1.51",
                                                "y1.52","y1.55","y1.56","y1.57","FORMATION_OUTCOME") 

full_file_filtered_DETAILED <- full_file %>% select("y1.15","y1.16","y1.17","y1.20","y1.21","y1.22"
                                           ,"y1.25","y1.26","y1.27","y1.45",
                                           "y1.46","y1.47","y1.50","y1.51",
                                           "y1.52","y1.55","y1.56","y1.57","DETAILED_OUTCOME")

full_file_filtered_DETAILED_LEFT_RIGHT <- full_file %>% select("y1","y1.1","y1.2","y1.5","y1.6","y1.7"
                                                    ,"y1.10","y1.11","y1.12","y1.30",
                                                    "y1.31","y1.32","y1.35","y1.36","y1.37",
                                                    "y1.40","y1.41","y1.42","DETAILED_OUTCOME")



RomaVsJuventus$OUTCOME <- as.factor(RomaVsJuventus$OUTCOME)
tDF <- as(full_file_filtered, "transactions")
inspect(tDF)
final_rules = apriori(tDF,parameter = list(support = 0.035, minlen = 2,target = "rules"),
                      appearance = list(rhs ="FORMATION_OUTCOME=HOME_WIN"))  
inspect(final_rules)

final_rules_away = apriori(tDF,parameter = list(support = 0.01, minlen = 2,maxlen=4,target = "rules"),
                      appearance = list(rhs ="FORMATION_OUTCOME=AWAY_WIN"))  
inspect(final_rules_away)

final_rules_dont = apriori(tDF,parameter = list(support = 0.05, minlen = 2,maxlen=4,target = "rules"),
                           appearance = list(rhs ="FORMATION_OUTCOME=DONT KNOW"))  
inspect(final_rules_dont)


##################
tDFD <- as(full_file_filtered_DETAILED, "transactions")
inspect(tDFD)
final_rulesD = apriori(tDFD,parameter = list(support = 0.035, minlen = 2,target = "rules"),
                      appearance = list(rhs ="DETAILED_OUTCOME=HOME_WIN"))  
inspect(final_rulesD)

final_rules_awayD = apriori(tDFD,parameter = list(support = 0.01, minlen = 2,maxlen=4,target = "rules"),
                           appearance = list(rhs ="DETAILED_OUTCOME=AWAY_WIN"))  
inspect(final_rules_awayD)

final_rules_dontD = apriori(tDFD,parameter = list(support = 0.005, minlen = 2,maxlen=4,target = "rules"),
                           appearance = list(rhs ="DETAILED_OUTCOME=TIE"))  
inspect(final_rules_dontD)

final_rules_dontHLD = apriori(tDFD,parameter = list(support = 0.005, minlen = 2,maxlen=4,target = "rules"),
                            appearance = list(rhs ="DETAILED_OUTCOME=HOME_LOSS"))  
inspect(final_rules_dontHLD)

####################DETAILED LEFT-RIGHT#############################
tDFD <- as(full_file_filtered_DETAILED_LEFT_RIGHT, "transactions")
inspect(tDFD)
final_rulesD = apriori(tDFD,parameter = list(support = 0.035, minlen = 2,target = "rules"),
                       appearance = list(rhs ="DETAILED_OUTCOME=HOME_WIN"))  
inspect(final_rulesD)

final_rules_awayD = apriori(tDFD,parameter = list(support = 0.005, minlen = 2,maxlen=4,target = "rules"),
                            appearance = list(rhs ="DETAILED_OUTCOME=AWAY_WIN"))  
inspect(final_rules_awayD)

final_rules_dontD = apriori(tDFD,parameter = list(support = 0.005, minlen = 2,maxlen=4,target = "rules"),
                            appearance = list(rhs ="DETAILED_OUTCOME=TIE"))  
inspect(final_rules_dontD)

final_rules_dontHLD = apriori(tDFD,parameter = list(support = 0.005, minlen = 2,maxlen=4,target = "rules"),
                              appearance = list(rhs ="DETAILED_OUTCOME=HOME_LOSS"))  
inspect(final_rules_dontHLD)

#############################
write.csv(full_file_ROMA,file="ROMA(Y).csv")
write.csv(full_file_filtered_DETAILED_LEFT_RIGHT, file="DIMO(X).csv")
