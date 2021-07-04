###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
theme_set(theme_cowplot()) #sets design parameters for data visualization
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Final Paper Data Preprocessing.r")
set.seed(12312)

######For Appendix B: Deviations from original analysis plan
#Original model, Equation 10
mod1_Regular_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                            family = binomial(link = "probit"),
                            data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                            nAGQ = 0,
                            glmerControl(optimizer = "nloptwrap"))
mod1_BlankWall_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
mod1_WallMoves_Prereg = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference| Participant) + (Difference| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))


#Equation 11
mod1_Regular_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
mod1_BlankWall_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
mod1_WallMoves_Version2.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest| Participant) + (velH_Pest| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))


#Equation 12
mod1_Regular_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
mod1_BlankWall_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))
mod1_WallMoves_Version3.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference + Congruent| Participant) + (Difference + Congruent| velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))


#Equation 13
mod1_Regular_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                family = binomial(link = "probit"),
                                data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
                                nAGQ = 0,
                                glmerControl(optimizer = "nloptwrap"))
mod1_BlankWall_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))
mod1_WallMoves_Version4.0 = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent| Participant) + (velH_Pest + Congruent| velH),
                                       family = binomial(link = "probit"),
                                       data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
                                       nAGQ = 0,
                                       glmerControl(optimizer = "nloptwrap"))


#model comparisons
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
