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

###the following function gets the current path of this script
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
# 
# Data_MainExperiment_NoOutlierAnalysis =  openxlsx::read.xlsx("Data/All Data_Final.xlsx", sheet = 3) #load main file
# 
# Data_MainExperiment_NoOutlierAnalysis = Data_MainExperiment_NoOutlierAnalysis %>%
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
#          Difference = abs(velH_Pest)-abs(velH),
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
# Data_MainExperiment_NoOutlierAnalysis2 = Data_MainExperiment_NoOutlierAnalysis %>%
#   group_by(Condition, Participant, Congruent, velH, velH_Subject, Start_Above) %>%
#   mutate(MeanVelocityOfLast10Trials = mean(velH_Pest[c(length(velH_Pest)-10:length(velH_Pest))])) %>%
#   slice(1) %>%
#   group_by(Condition, Participant, Congruent, velH, velH_Subject) %>%
#   mutate(DifferenceInMeanVelocity = MeanVelocityOfLast10Trials[1] - MeanVelocityOfLast10Trials[2],
#          IncludeOutliers = case_when(abs(DifferenceInMeanVelocity) < 3 ~ "Include",
#                              TRUE ~ "Exclude"))
# 
# Data_MainExperiment_NoOutlierAnalysis$gender = "woman"
# Data_MainExperiment_NoOutlierAnalysis$gender[Data_MainExperiment_NoOutlierAnalysis$Participant %in% sort(unique(Data_MainExperiment_NoOutlierAnalysis$Participant))[16:30]] = "man"
# 
# Data_GLM_NoOutlierAnalysis =
#   select(Data_MainExperiment,c(Participant,Congruent,velH,velH_Pest,Difference,Yes,Total,velH_Subject,SelfMotionPresent,Static,Condition,Include)) %>%
#   distinct()
# 
# save(Data_MainExperiment_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/Data_MainExperiment_NoOutlierAnalysis.RData"))
# save(Data_GLM_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                           "/SavedVariables/Data_GLM_NoOutlierAnalysis.RData"))

load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Data_MainExperiment_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Data_GLM_NoOutlierAnalysis.RData"))
