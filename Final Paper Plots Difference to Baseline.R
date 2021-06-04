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


Parameters_Regular = Parameters %>%
  filter(Condition %in% c("RegularCondition", "RegularWallStatic")) %>% 
  group_by(velH,Participant) %>%
  filter(length(Mean) == 3) %>% 
  arrange(Participant,velH,Condition) %>%
  mutate(Baseline_PSE = Mean[Congruent == "1no motion"],
         Baseline_SD = SD[Congruent == "1no motion"],
         DifferenceToBaseline_PSE = Mean-Baseline_PSE,
         DifferenceToBaseline_SD = SD-Baseline_SD) %>% 
  group_by(Congruent) %>% 
  mutate(Mean_DifferenceToBaseline_PSE = mean(DifferenceToBaseline_PSE[Congruent != "1no motion"]),
         Mean_DifferenceToBaseline_SD = mean(DifferenceToBaseline_SD[Congruent != "1no motion"]))

###################plots Laurence
Laurence1 = ggplot(Parameters_Regular %>% filter(Congruent != "1no motion"), aes(Congruent,DifferenceToBaseline_PSE,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_PSE), size = 8) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Accuracy - Main Condition") +
  ylab("PSE Difference from Static (m/s)") +
  theme(axis.title.x=element_blank()) +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_PSE), size = 4) +
  annotate("text", x = 1, y = 2.5, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent",2],2),
                                                  "]")) +
  annotate("text", x = 2, y = 2.5, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentincongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentincongruent",2],2),
                                                  "]")) +
  geom_hline(yintercept = 0,
             linetype = 3) +
  annotate("segment",x = 0.6, xend = 1.4, y = -1, yend = -1, linetype = 2, size = 2) +
  annotate("segment",x = 1.6, xend = 2.4, y = 1, yend = 1, linetype = 2, size = 2)

Laurence2 = ggplot(Parameters_Regular %>% filter(Congruent != "1no motion"), aes(Congruent,DifferenceToBaseline_SD,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ylab("SD Difference from Static (m/s)") +
  ggtitle("Precision - Main Condition") +
  theme(axis.title.x=element_blank()) +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_SD), size = 8) +
  annotate("text", x = 1, y = 5.5, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent:velH_Pest",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Regular["Congruentcongruent:velH_Pest",2],2),
                                                  "]")) +
  annotate("text", x = 2, y = 5.5, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_Regular["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_Regular["Congruentincongruent:velH_Pest",2],2),
                                                "]")) +
  geom_hline(yintercept = 0,
             linetype = 3)


Parameters_Blank = Parameters %>%
  filter(Condition %in% c("BlankWall", "BlankWallStatic")) %>% 
  group_by(velH,Participant) %>%
  filter(length(Mean) == 3) %>% 
  arrange(Participant,velH,Condition) %>%
  mutate(Baseline_PSE = Mean[Congruent == "1no motion"],
         Baseline_SD = SD[Congruent == "1no motion"],
         DifferenceToBaseline_PSE = Mean-Baseline_PSE,
         DifferenceToBaseline_SD = SD-Baseline_SD) %>% 
  group_by(Congruent) %>% 
  mutate(Mean_DifferenceToBaseline_PSE = mean(DifferenceToBaseline_PSE[Congruent != "1no motion"]),
         Mean_DifferenceToBaseline_SD = mean(DifferenceToBaseline_SD[Congruent != "1no motion"]))


Laurence3 = ggplot(Parameters_Blank %>% filter(Congruent != "1no motion"), aes(Congruent,DifferenceToBaseline_PSE,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Accuracy - Blank Wall") +
  ylab("PSE Difference from Static (m/s)") +
  theme(axis.title.x=element_blank()) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_PSE), size = 8) +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  annotate("text", x = 1, y = 2, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Blankwall["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Blankwall["Congruentcongruent",2],2),
                                                  "]")) +
  annotate("text", x = 2, y = 2, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_Blankwall["Congruentincongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_Blankwall["Congruentincongruent",2],2),
                                                  "]")) +
  geom_hline(yintercept = 0,
             linetype = 3) +
  annotate("segment",x = 0.6, xend = 1.4, y = -1, yend = -1, linetype = 2, size = 2) +
  annotate("segment",x = 1.6, xend = 2.4, y = 1, yend = 1, linetype = 2, size = 2)
  
Laurence4 = ggplot(Parameters_Blank %>% filter(Congruent != "1no motion"), aes(Congruent,DifferenceToBaseline_SD,color = Congruent)) +
  geom_point(alpha = 0.5, size = 3) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Precision - Blank Wall") +
  ylab("SD Difference from Static (m/s)") +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  theme(axis.title.x=element_blank()) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_SD), size = 8) +
  annotate("text", x = 1, y = 3.5, label = paste0("95% CI = [",
                                                    round(ConfidenceIntervals_Blankwall["Congruentcongruent:velH_Pest",1],2),
                                                    ";",
                                                    round(ConfidenceIntervals_Blankwall["Congruentcongruent:velH_Pest",2],2),
                                                    "]")) +
  annotate("text", x = 2, y = 3.5, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_Blankwall["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_Blankwall["Congruentincongruent:velH_Pest",2],2),
                                                "]")) +
  geom_hline(yintercept = 0,
             linetype = 3)


Parameters_MovingWall = Parameters %>%
  filter(Condition %in% c("RegularWallStatic", "WallMoves")) %>% 
  group_by(velH,Participant) %>%
  filter(length(Mean) == 3) %>% 
  arrange(Participant,velH,Condition) %>%
  mutate(Baseline_PSE = Mean[Congruent == "1no motion"],
         Baseline_SD = SD[Congruent == "1no motion"],
         DifferenceToBaseline_PSE = Mean-Baseline_PSE,
         DifferenceToBaseline_SD = SD-Baseline_SD) %>% 
  group_by(Congruent) %>% 
  mutate(Mean_DifferenceToBaseline_PSE = mean(DifferenceToBaseline_PSE[Congruent != "1no motion"]),
         Mean_DifferenceToBaseline_SD = mean(DifferenceToBaseline_SD[Congruent != "1no motion"]))

Laurence5 = ggplot(Parameters_MovingWall %>% filter(Congruent != "1no motion"), 
                   aes(Congruent,DifferenceToBaseline_PSE,color = Congruent)) +
  geom_point(alpha = 0.2, size = 3) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_PSE), size = 8) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  ylab("PSE Difference from Static (m/s)") +
  theme(legend.position = "") +
  ggtitle("Accuracy - Moving Wall") +
  theme(axis.title.x=element_blank()) +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  annotate("text", x = 1, y = 2, label = paste0("95% CI = [",
                                                  round(ConfidenceIntervals_WallMoves["Congruentcongruent",1],2),
                                                  ";",
                                                  round(ConfidenceIntervals_WallMoves["Congruentcongruent",2],2),
                                                  "]")) +
  annotate("text", x = 2, y = 2, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent",1],2),
                                                ";",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent",2],2),
                                                "]")) +
  geom_hline(yintercept = 0,
             linetype = 3) +
  annotate("segment",x = 0.6, xend = 1.4, y = -1, yend = -1, linetype = 2, size = 2) +
  annotate("segment",x = 1.6, xend = 2.4, y = 1, yend = 1, linetype = 2, size = 2)

Laurence6 = ggplot(Parameters_MovingWall %>% filter(Congruent != "1no motion"), 
                   aes(Congruent,DifferenceToBaseline_SD,color = Congruent)) +
  geom_point(alpha = 0.4, size = 3) +
  geom_point(aes(Congruent,Mean_DifferenceToBaseline_SD), size = 8) +
  scale_x_discrete(labels = c("Same Direction", "Opposite Directions")) +
  theme(legend.position = "") +
  ggtitle("Precision - Moving Wall") +
  ylab("SD Difference from Static (m/s)") +
  theme(axis.title.x=element_blank()) +
  scale_color_manual(values = c(BlauUB, "lightblue")) +
  annotate("text", x = 1, y = 5.5, label = paste0("95% CI = [",
                                                    round(ConfidenceIntervals_WallMoves["Congruentcongruent:velH_Pest",1],2),
                                                    ";",
                                                    round(ConfidenceIntervals_WallMoves["Congruentcongruent:velH_Pest",2],2),
                                                    "]")) +
  annotate("text", x = 2, y = 5.5, label = paste0("95% CI = [",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent:velH_Pest",1],2),
                                                ";",
                                                round(ConfidenceIntervals_WallMoves["Congruentincongruent:velH_Pest",2],3),
                                                "]")) +
  geom_hline(yintercept = 0,
             linetype = 3)

plot_grid(Laurence1,Laurence2, nrow = 1, labels = "AUTO")
ggsave("Figures/Main Plots Regular Condition Difference.jpg", w = 9, h = 5)

plot_grid(Laurence3, Laurence4, Laurence5,Laurence6, nrow = 2, labels = "AUTO")
ggsave("Figures/Main Plots Control Conditions Difference.jpg", w = 9, h = 9)
