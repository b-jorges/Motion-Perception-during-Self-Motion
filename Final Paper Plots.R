###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(quickpsy) #package to fit psychometric functions
require(cowplot) #design for data visualiation
require(tidyverse)
theme_set(theme_cowplot()) #sets design parameters for data visualization
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"
source("Final Paper Data Preprocessing.r")
source("Final Paper Analysis.r")

####the following function gets the current path of this script 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory

###################plots Laurence
Laurence1 = ggplot(Parameters %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")), aes(Congruent,Mean_Minus_velH,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_point(aes(Congruent,Mean_Mean_Minus_velH), size = 8) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Accuracy - Full Stimulus") +
  ylab("PSE (m/s)") +
  theme(axis.title.x=element_blank()) +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  geom_point(aes(Congruent,Mean_Mean_Minus_velH), size = 4) +
#  coord_cartesian(ylim = c(-6.5,2)) +
  annotate("text", x = 1.5, y = 1, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent",2],2),
                                                  "]")) +
  # annotate("text", x = 1.5, y = 1, label = if(summary(mod1_Regular)$coef[20] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_Regular)$coef[20],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 2, y = 0.5, yend = 0.5) +
  annotate("text", x = 2, y = 2, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentincongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentincongruent",2],2),
                                                  "]")) +
  # annotate("text", x = 2, y = 2, label = if(summary(mod1_Regular)$coef[21] >= 0.001){paste0("p = ",
  #                                           round(summary(mod1_Regular)$coef[21],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 3, y = 1.5, yend = 1.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion"))$Mean_Minus_velH),
             linetype = 3) +
  geom_segment(aes(x = 1.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1, 
                   xend = 2.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1),
               size = 1,
               color = "black",
               linetype = 5) +
  geom_segment(aes(x = 2.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1, 
                   xend = 3.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1),
               size = 1,
               color = "black",
               linetype = 5)

Laurence2 = ggplot(Parameters %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")), aes(Congruent,SD,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ylab("SD Difference from Static (m/s)") +
  ggtitle("Precision - Full Stimulus") +
  theme(axis.title.x=element_blank()) +
#  coord_cartesian(ylim = c(0,5.5)) +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  geom_point(aes(Congruent,Mean_SD), size = 8) +
  annotate("text", x = 1.5, y = 7.5, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent:velH_Pest",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent:velH_Pest",2],2),
                                                  "]")) +
  # annotate("text", x = 1.5, y = 4, label = if(summary(mod1_Regular)$coef[23] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_Regular)$coef[23],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 2, y = 7, yend = 7) +
  annotate("text", x = 2, y = 9, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_Regular["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_Regular["Congruentincongruent:velH_Pest",2],2),
                                                "]")) +
  # annotate("text", x = 2, y = 5, label = if(summary(mod1_Regular)$coef[24] >= 0.001){paste0("p = ",
  #                                           round(summary(mod1_Regular)$coef[24],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 3, y = 8.5, yend = 8.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion"))$SD),
             linetype = 3)

Laurence3 = ggplot(Parameters %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")), aes(Congruent,Mean_Minus_velH,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Accuracy - Blank Wall") +
  ylab("PSE (m/s)") +
  theme(axis.title.x=element_blank()) +
  geom_point(aes(Congruent,Mean_Mean_Minus_velH), size = 8) +
#  coord_cartesian(ylim = c(-6.5,2)) +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  annotate("text", x = 1.5, y = 1, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Blankwall["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Blankwall["Congruentcongruent",2],2),
                                                  "]")) +
  # annotate("text", x = 1.5, y = 1, label = if(summary(mod1_BlankWall)$coef[20] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_BlankWall)$coef[20],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 2, y = 0.5, yend = 0.5) +
  annotate("text", x = 2, y = 2, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Blankwall["Congruentincongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Blankwall["Congruentincongruent",2],2),
                                                  "]")) +
  # annotate("text", x = 2, y = 2, label = if(summary(mod1_BlankWall)$coef[21] >= 0.001){paste0("p = ",
  #                                           round(summary(mod1_BlankWall)$coef[20],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 3, y = 1.5, yend = 1.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion"))$Mean_Minus_velH),
             linetype = 3) +
  geom_segment(aes(x = 1.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1, 
                   xend = 2.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1),
               size = 1,
               color = "black",
               linetype = 5) +
  geom_segment(aes(x = 2.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1, 
                   xend = 3.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1),
               size = 1,
               color = "black",
               linetype = 5)
  
Laurence4 = ggplot(Parameters %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")), aes(Congruent,SD,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Precision - Blank Wall") +
  ylab("SD (m/s)") +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  theme(axis.title.x=element_blank()) +
#  coord_cartesian(ylim = c(0,5.5)) +
  geom_point(aes(Congruent,Mean_SD), size = 8) +
  annotate("text", x = 1.5, y = 7.5, label = paste0("95% CI = [",
                                                    round(ConfidenceIntervals_Blankwall["Congruentcongruent:velH_Pest",1],2),
                                                    ";",
                                                    round(ConfidenceIntervals_Blankwall["Congruentcongruent:velH_Pest",2],2),
                                                    "]")) +
  # annotate("text", x = 1.5, y = 4, label = if(summary(mod1_BlankWall)$coef[23] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_BlankWall)$coef[23],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 2, y = 7, yend = 7) +
  annotate("text", x = 2, y = 9, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_Blankwall["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_Blankwall["Congruentincongruent:velH_Pest",2],2),
                                                "]")) +
  annotate("segment", x = 1, xend = 3, y = 8.5, yend = 8.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("BlankWall", "BlankWallStatic") & Congruent == "1no motion"))$SD),
             linetype = 3)

Laurence5 = ggplot(Parameters %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")), aes(Congruent,Mean_Minus_velH,color = Congruent)) +
  geom_point(alpha = 0.2, size = 3) +
  geom_point(aes(Congruent,Mean_Mean_Minus_velH), size = 8) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  ylab("PSE (m/s)") +
  theme(legend.position = "") +
  ggtitle("Accuracy - Moving Wall") +
  theme(axis.title.x=element_blank()) +
#  coord_cartesian(ylim = c(-6.5,2)) +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  annotate("text", x = 1.5, y = 1, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_WallMoves["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_WallMoves["Congruentcongruent",2],2),
                                                  "]")) +
  annotate("segment", x = 1, xend = 2, y = 0.5, yend = 0.5) +
  # annotate("text", x = 1.5, y = 1.25, label = if(summary(mod1_WallMoves)$coef[20] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_WallMoves)$coef[20],3))} else{"p < 0.001"}) +
  annotate("text", x = 2, y = 2, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent",1],2),
                                                ";",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent",2],2),
                                                "]")) +
  # annotate("text", x = 2.5, y = 1.75, label = if(summary(mod1_WallMoves)$coef[21] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_WallMoves)$coef[21],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 3, y = 1.5, yend = 1.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("WallMoves", "RegularWallStatic") & Congruent == "1no motion"))$Mean_Minus_velH),
             linetype = 3) +
  geom_segment(aes(x = 1.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1, 
                   xend = 2.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] - 1),
               size = 1,
               color = "black",
               linetype = 5) +
  geom_segment(aes(x = 2.5, 
                   y = (Parameters %>% 
                          filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                          group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1, 
                   xend = 3.5, 
                   yend = (Parameters %>% 
                             filter(Condition %in% c("RegularCondition", "RegularWallStatic") & Congruent == "1no motion") %>% 
                             group_by(Congruent))$Mean_Mean_Minus_velH[1] + 1),
               size = 1,
               color = "black",
               linetype = 5)

Laurence6 = ggplot(Parameters %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")), aes(Congruent,SD,color = Congruent)) +
  geom_point(alpha = 0.4, size = 3) +
  geom_point(aes(Congruent,Mean_SD), size = 8) +
  scale_x_discrete(labels = c("Static","Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Precision - Moving Wall") +
  ylab("SD (m/s)") +
  theme(axis.title.x=element_blank()) +
#  coord_cartesian(ylim = c(0,5.5)) +
  scale_color_manual(values = c(Red,BlauUB, "lightblue")) +
  annotate("text", x = 1.5, y = 7.5, label = paste0("95% CI = [",
                                                    round(ConfidenceIntervals_WallMoves["Congruentcongruent:velH_Pest",1],2),
                                                    ";",
                                                    round(ConfidenceIntervals_WallMoves["Congruentcongruent:velH_Pest",2],2),
                                                    "]")) +
  # annotate("text", x = 1.5, y = 3.75, label = if(summary(mod1_WallMoves)$coef[23] >= 0.001){paste0("p = ",
  #                                             round(summary(mod1_WallMoves)$coef[23],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 2, y = 7, yend = 7) +
  annotate("text", x = 2, y = 9, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent:velH_Pest",2],3),
                                                "]")) +
  # annotate("text", x = 2, y = 4.75, label = if(summary(mod1_WallMoves)$coef[24] >= 0.001){paste0("p = ",
  #                                           round(summary(mod1_WallMoves)$coef[24],3))} else{"p < 0.001"}) +
  annotate("segment", x = 1, xend = 3, y = 8.5, yend = 8.5) +
  geom_hline(yintercept = mean((Parameters %>% filter(Condition %in% c("WallMoves", "RegularWallStatic") & Congruent == "1no motion"))$SD),
             linetype = 3)

plot_grid(Laurence1,Laurence2, nrow = 1, labels = "AUTO")
ggsave("Figures/Main Plots Regular Condition.jpg", w = 10, h = 4.5)

plot_grid(Laurence3, Laurence4, Laurence5,Laurence6, nrow = 2, labels = "AUTO")
ggsave("Figures/Main Plots Control Conditions.jpg", w = 10, h = 8)
