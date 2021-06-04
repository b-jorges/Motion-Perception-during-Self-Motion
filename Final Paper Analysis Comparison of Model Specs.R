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


######
mod1_Regular_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                            family = binomial(link = "probit"),
                            data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                            nAGQ = 0,
                            glmerControl(optimizer = "nloptwrap"))
summary(mod1_Regular_JND_Test_lalala1)
mod1_BlankWall_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
summary(mod1_BlankWall_Prereg)
mod1_WallMoves_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
summary(mod1_WallMoves_JND_Test_lalala1)


mod1_Regular_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
summary(mod1_Regular_Version2.0)
mod1_BlankWall_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
summary(mod1_BlankWall_Version2.0)
mod1_WallMoves_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
summary(mod1_WallMoves_Version2.0)


mod1_Regular_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
summary(mod1_Regular_Version3.0)
mod1_BlankWall_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
summary(mod1_BlankWall_Version3.0)
mod1_WallMoves_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
summary(mod1_WallMoves_Version3.0)


mod1_Regular_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
summary(mod1_Regular_Version4.0)
mod1_BlankWall_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
summary(mod1_BlankWall_Version4.0)
mod1_WallMoves_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
summary(mod1_WallMoves_Version4.0)




anova(mod1_Regular_Prereg,
      mod1_Regular_Version2.0,
      mod1_Regular_Version3.0,
      mod1_Regular_Version4.0)
anova(mod1_BlankWall_Prereg,
      mod1_BlankWall_Version2.0,
      mod1_BlankWall_Version3.0,
      mod1_BlankWall_Version4.0)
anova(mod1_WallMoves_Prereg,
      mod1_WallMoves_Version2.0,
      mod1_WallMoves_Version3.0,
      mod1_WallMoves_Version4.0)


anova(mod1_Regular_Version2.0,
      mod1_Regular_Version3.0,
      mod1_Regular_Version4.0)
anova(mod1_BlankWall_Version2.0,
      mod1_BlankWall_Version3.0,
      mod1_BlankWall_Version4.0)
anova(mod1_WallMoves_Version2.0,
      mod1_WallMoves_Version3.0,
      mod1_WallMoves_Version4.0)


anova(mod1_Regular_Version3.0,
      mod1_Regular_Version4.0)
anova(mod1_BlankWall_Version3.0,
      mod1_BlankWall_Version4.0)
anova(mod1_WallMoves_Version2.0,
      mod1_WallMoves_Version4.0)
