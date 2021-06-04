###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(quickpsy) #package to fit psychometric functions
require(cowplot) #design for data visualization
require(tidyverse)
theme_set(theme_cowplot()) #sets design parameters for data visualization
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"

###velH_Subject -1/1 = participant moves, wall is textured; desired response 1, acceptable [0.6;1]
###velH_Subject 0.5/-0.5 = wall moves; desired response -1, acceptable [-0.6;-1]
###velH_Subject 0.25/-0.25 = participant moves, but wall is blank; desired response 1, acceptable [0.6;1]; negative = wall moves left, positive = wall moves right

####the following function gets the current path of this script
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
# 
# Data_MainExperiment =  openxlsx::read.xlsx("Data/All Data_Final.xlsx", sheet = 3) #load main file
# Data_Selfmotion = openxlsx::read.xlsx("Data/All Data_Final.xlsx", sheet = 4) #load selfmotion file
# 
# Data_Selfmotion = Data_Selfmotion %>%
#   mutate(Judgement = round(Judgement,2),
#          Condition = case_when(
#            velH_Subject %in% c(0.5,-0.5) ~ "WallMoves",
#            velH_Subject %in% c(0.25,-0.25) ~ "BlankWall",
#            velH_Subject %in% c(1,-1) ~ "RegularCondition",
#          )) %>%
#   group_by(Condition,Participant) %>%
#   mutate(Mean_Judgement = mean(Judgement)) %>%
#   ungroup() %>%
#   mutate(ExpectedJudgement = case_when(
#            velH_Subject %in% c(-1,1) ~  "< -0.6",
#            velH_Subject %in% c(-0.25,0.25) ~ "< -0.6",
#            velH_Subject %in% c(-0.5,0.5) ~ " 0.6",
#   ),
#          Include = case_when(
#            velH_Subject %in% c(-1,1) & Mean_Judgement >= 0.6 ~ "yes",
#            velH_Subject %in% c(-0.25,0.25) & Mean_Judgement >= 0.6 ~ "yes",
#            velH_Subject %in% c(-0.5,0.5) & Mean_Judgement <= -0.6 ~ "yes",
#            TRUE ~ "no"
#          ))
# 
# 
# Data_MainExperiment = Data_MainExperiment %>%
#   mutate(Condition = case_when(
#            velH_Subject %in% c(0.5,-0.5) ~ "WallMoves",
#            velH_Subject %in% c(0.25,-0.25) ~ "BlankWall",
#            velH_Subject %in% c(1,-1) ~ "RegularCondition",
#            velH_Subject == 2 ~ "BlankWallStatic",
#            velH_Subject == 0 ~ "RegularWallStatic"),
#          Pest_Bigger = case_when(
#            Response_Interval == Pest_Interval ~ 1,
#            Response_Interval != Pest_Interval ~ 0,
#          ),
#          Direction = case_when(
#            velH < 0 ~ "left",
#            velH > 0 ~ "right",
#          ),
#          Difference = abs(velH_Pest)-abs(velH), #when
#          velH_Absolut = abs(velH),
#          Congruent = case_when(
#            velH*velH_Subject < 0 & velH_Subject != 2 ~ "incongruent",
#            velH*velH_Subject > 0 & velH_Subject != 2 ~ "congruent",
#            velH_Subject %in% c(0,2) ~ "1no motion"
#          ),
#          Difference_Percent = Difference/velH,
#          SelfMotionPresent = case_when(
#            velH_Subject %in% c(0,2) ~ 0,
#            TRUE ~ 1
#          )
#          ) %>%
#   group_by(Participant,Congruent,velH,Difference) %>%
#   mutate(Yes = sum(Pest_Bigger==1),
#          Total = length(velH_Subject)) %>%
#   ungroup() %>%
#   mutate(Static = case_when(
#          velH_Subject %in% c(0,2) ~ "Static",
#          TRUE ~ "Movement"),
#          Difference_Norm = Difference/abs(velH)) %>%
#   mutate(AngularVelocity = case_when(
#          Congruent == "Congruent" & velH %in% c(-6.6,6.6) ~ 27.6, #values from "Distance between Observer and Target"
#          Congruent == "Incongruent" & velH %in% c(-6.6,6.6) ~ 73.3,
#          Congruent == "Static" & velH %in% c(-6.6,6.6) ~ 46.6,
#          Congruent == "Congruent" & velH %in% c(-8,8) ~ 32.5,
#          Congruent == "Incongruent" & velH %in% c(-8,8) ~ 82.2,
#          Congruent == "Static" & velH %in% c(-8,8) ~ 56.2)) %>%
#   filter(!is.na(Participant))
# 
# 
# trials_before_Outlier1 = length(Data_MainExperiment$Participant)
# Data_MainExperiment = Data_MainExperiment %>%
#   filter(abs(velH_Pest) < abs(velH)*1.5)
# trials_after_Outlier1 = length(Data_MainExperiment$Participant)
# Precentage_lost_Outlier1 = (trials_before_Outlier1-trials_after_Outlier1)/trials_before_Outlier1
# 
# 
# 
# Data_MainExperiment2 = Data_MainExperiment %>%
#   group_by(Condition, Participant, Congruent, velH, velH_Subject, Start_Above) %>%
#   mutate(MeanVelocityOfLast10Trials = mean(velH_Pest[c(length(velH_Pest)-10:length(velH_Pest))])) %>%
#   slice(1) %>%
#   group_by(Condition, Participant, Congruent, velH, velH_Subject) %>%
#   mutate(DifferenceInMeanVelocity = MeanVelocityOfLast10Trials[1] - MeanVelocityOfLast10Trials[2],
#          IncludeOutliers = case_when(abs(DifferenceInMeanVelocity) < 3 ~ "Include",
#                              TRUE ~ "Exclude"))
# 
# Data_MainExperiment$IncludeOutliers = c()
# for (i in 1:length(Data_MainExperiment$Participant)){
#   Condition = Data_MainExperiment$Condition[i]
#   Participant = Data_MainExperiment$Participant[i]
#   Congruent = Data_MainExperiment$Congruent[i]
#   velH = Data_MainExperiment$velH[i]
#   velH_Subject = Data_MainExperiment$velH_Subject[i]
#   Start_Above = Data_MainExperiment$Start_Above[i]
#   Data_MainExperiment$IncludeOutliers[Data_MainExperiment$Condition == Condition &
#                                       Data_MainExperiment$Participant == Participant &
#                                       Data_MainExperiment$Congruent == Congruent &
#                                       Data_MainExperiment$velH == velH &
#                                       Data_MainExperiment$velH_Subject == velH_Subject &
#                                       Data_MainExperiment$Start_Above == Start_Above] =
#     Data_MainExperiment2$IncludeOutliers[Data_MainExperiment2$Condition == Condition &
#                                       Data_MainExperiment2$Participant == Participant &
#                                       Data_MainExperiment2$Congruent == Congruent &
#                                       Data_MainExperiment2$velH == velH &
#                                       Data_MainExperiment2$velH_Subject == velH_Subject &
#                                       Data_MainExperiment2$Start_Above == Start_Above]
#   print(i)
# }
# 
# length(Data_MainExperiment$Participant)
# Data_MainExperiment = Data_MainExperiment %>% filter(IncludeOutliers == "Include")
# length(Data_MainExperiment$Participant)
# 
# 
# Data_MainExperiment$Judgement = c()
# Data_MainExperiment$Include = c()
# for (i in 1:length(Data_Selfmotion$Participant)){
#   Judgement = Data_Selfmotion$Judgement[i]
#   Include = Data_Selfmotion$Include[i]
#   ID = Data_Selfmotion$Participant[i]
#   Condition = Data_Selfmotion$Condition[i]
#   Data_MainExperiment$Judgement[Data_MainExperiment$Condition == Condition & Data_MainExperiment$Participant == ID] = Judgement
#   Data_MainExperiment$Include[Data_MainExperiment$Condition == Condition & Data_MainExperiment$Participant == ID] = Include
# }
# Data_MainExperiment$Include[Data_MainExperiment$Condition == "RegularWallStatic" | Data_MainExperiment$Condition == "BlankWallStatic"] = "yes"
# 
# Data_GLM =
#   select(Data_MainExperiment,c(Participant,Congruent,velH,velH_Pest,Difference,Yes,Total,velH_Subject,SelfMotionPresent,Static,Condition,Include)) %>%
#   distinct()
# 
# QuickPsy = quickpsy::quickpsy(Data_MainExperiment,
#                                velH_Pest,Pest_Bigger,grouping = .(Condition,velH, Congruent,Participant), bootstrap = "none")
# 
# 
# Parameters = QuickPsy$par %>% filter(parn == "p1")
# Parameters$SD = (QuickPsy$par %>% filter(parn == "p2"))$par
# colnames(Parameters) = c(colnames(Parameters)[1:5],"Mean",colnames(Parameters)[7])
# Parameters = Parameters %>%
#   group_by(Condition,Congruent) %>%
#   mutate(Mean_Mean = mean(Mean),
#          Mean_SD = mean(SD),
#          Mean_Minus_velH = Mean-velH,
#          Mean_Mean_Minus_velH = mean(Mean_Minus_velH))
# 
# Parameters$gender = "woman"
# Parameters$gender[Parameters$Participant %in% sort(unique(Parameters$Participant))[16:30]] = "man"
# 
# Data_GLM$gender = "woman"
# Data_GLM$gender[Data_GLM$Participant %in% sort(unique(Data_GLM$Participant))[16:30]] = "man"
# 
# Data_MainExperiment$gender = "woman"
# Data_MainExperiment$gender[Data_MainExperiment$Participant %in% sort(unique(Data_MainExperiment$Participant))[16:30]] = "man"
# 
# Data_Selfmotion$gender = "woman"
# Data_Selfmotion$gender[Data_Selfmotion$Participant %in% sort(unique(Data_Selfmotion$Participant))[16:30]] = "man"
# 
# 
# save(QuickPsy, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/QuickPsy.RData"))
# save(Parameters, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/Parameters.RData"))
# save(Data_GLM, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/Data_GLM.RData"))
# save(Data_MainExperiment, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/Data_MainExperiment.RData"))
# save(Data_Selfmotion, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                         "/SavedVariables/Data_Selfmotion.RData"))

load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/QuickPsy.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Parameters.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Data_GLM.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Data_MainExperiment.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Data_Selfmotion.RData"))
