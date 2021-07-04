###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(cowplot) #design for data visualiation
require(tidyverse)
theme_set(theme_cowplot()) #sets design parameters for data visualization
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Final Paper Data Preprocessing.r")


#################################################
#                                               #
#          Preregistered Analyses               #
#                                               #
#################################################

#################################################
###################Main Condition####################
#################################################

#######Model comparisons JND
# mod1_Regular_JND_Test = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 0,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_JND_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/mod1_Regular_JND_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_JND_Test.RData"))

# mod1_Regular_JND_Null = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 0,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_JND_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                           "/SavedVariables/mod1_Regular_JND_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_JND_Null.RData"))

anova(mod1_Regular_JND_Test,mod1_Regular_JND_Null)
summary(mod1_Regular_JND_Test)
summary(mod1_Regular_JND_Null)


paste0(round(summary(mod1_Regular_JND_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test)$coef[5],2), ")")

paste0(round(summary(mod1_Regular_JND_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test)$coef[6],2), ")")

paste0(round(summary(mod1_Regular_JND_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test)$coef[7],2), ")")

paste0(round(summary(mod1_Regular_JND_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test)$coef[8],2), ")")


#######Model comparisons PSE
# mod1_Regular_PSE_Test = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                               family = binomial(link = "probit"),
#                               data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                               nAGQ = 0,
#                               glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_PSE_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                            "/SavedVariables/mod1_Regular_PSE_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_PSE_Test.RData"))

# mod1_Regular_PSE_Null = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                               family = binomial(link = "probit"),
#                               data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                               nAGQ = 0,
#                               glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_PSE_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                             "/SavedVariables/mod1_Regular_PSE_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_PSE_Null.RData"))

anova(mod1_Regular_PSE_Test,mod1_Regular_PSE_Null)
summary(mod1_Regular_PSE_Test)


paste0(round(summary(mod1_Regular_PSE_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test)$coef[5],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test)$coef[8],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test)$coef[6],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test)$coef[7],2), ")")




#################################################
###################Blank Wall####################
#################################################


# mod1_BlankWall_JND_Test = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_JND_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                              "/SavedVariables/mod1_BlankWall_JND_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_JND_Test.RData"))

# mod1_BlankWall_JND_Null = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                                 nAGQ = 0,
#                                 glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_JND_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                              "/SavedVariables/mod1_BlankWall_JND_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_JND_Null.RData"))

anova(mod1_BlankWall_JND_Test,mod1_BlankWall_JND_Null)

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[8],2), ")")




# mod1_BlankWall_PSE_Test = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                                 nAGQ = 0,
#                                 glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_PSE_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                               "/SavedVariables/mod1_BlankWall_PSE_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_PSE_Test.RData"))

# mod1_BlankWall_PSE_Null = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                                 nAGQ = 0,
#                                 glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_PSE_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                               "/SavedVariables/mod1_BlankWall_PSE_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_PSE_Null.RData"))

anova(mod1_BlankWall_PSE_Test,mod1_BlankWall_PSE_Null)

paste0(round(summary(mod1_BlankWall_PSE_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test)$coef[8],2), ")")


#################################################
###################Wall moves####################
#################################################

# mod1_WallMoves_JND_Test = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_JND_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                "/SavedVariables/mod1_WallMoves_JND_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_JND_Test.RData"))

# mod1_WallMoves_JND_Null = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_JND_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                "/SavedVariables/mod1_WallMoves_JND_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_JND_Null.RData"))

anova(mod1_WallMoves_JND_Test,mod1_WallMoves_JND_Null)

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test)$coef[8],2), ")")





# mod1_WallMoves_PSE_Test = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_PSE_Test, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                 "/SavedVariables/mod1_WallMoves_PSE_Test.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_PSE_Test.RData"))

# mod1_WallMoves_PSE_Null = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_PSE_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                 "/SavedVariables/mod1_WallMoves_PSE_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_PSE_Null.RData"))

anova(mod1_WallMoves_PSE_Test,mod1_WallMoves_PSE_Null)

paste0(round(summary(mod1_WallMoves_PSE_Test)$coef[1],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test)$coef[5],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test)$coef[2],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test)$coef[6],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test)$coef[3],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test)$coef[7],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test)$coef[4],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test)$coef[8],2), ")")

summary(mod1_WallMoves_PSE_Test)


#################################################
#                                               #
#            Exploratory Analyses               #
#                                               #
#################################################

########Exploratory Analysis
# mod1_Regular = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 0,
#                      glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_Regular = confint(mod1_Regular,method = "boot")
# save(mod1_Regular, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/mod1_Regular.RData"))
# save(ConfidenceIntervals_Regular, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_Regular.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_Regular.RData"))

DataFrameRegular = round(data.frame(
                              RegressionCoefficient = summary(mod1_Regular)$coef[1:6],
                              SE = summary(mod1_Regular)$coef[7:12],
                              CI_Lower = ConfidenceIntervals_Regular[(length(ConfidenceIntervals_Regular[,1])-5):length(ConfidenceIntervals_Regular[,1]),1],
                              CI_Higher = ConfidenceIntervals_Regular[(length(ConfidenceIntervals_Regular[,1])-5):length(ConfidenceIntervals_Regular[,1]),2]),2)

DataFrameRegular = DataFrameRegular %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameRegular) = rownames(summary(mod1_Regular)$coef)
write.csv(DataFrameRegular,file = "./Tables/Excel_DataFrameRegular.csv")

########white wall, no induced motion
# mod1_BlankWall = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_Blankwall = confint(mod1_BlankWall,method = "boot")
# save(mod1_BlankWall, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                  "/SavedVariables/mod1_BlankWall.RData"))
# save(ConfidenceIntervals_Blankwall, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_Blankwall.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_Blankwall.RData"))


DataFrameBlankWall = round(data.frame(
  RegressionCoefficient = summary(mod1_BlankWall)$coef[1:6],
  SE = summary(mod1_BlankWall)$coef[7:12],
  CI_Lower = ConfidenceIntervals_Blankwall[(length(ConfidenceIntervals_Blankwall[,1])-5):length(ConfidenceIntervals_Blankwall[,1]),1],
  CI_Higher = ConfidenceIntervals_Blankwall[(length(ConfidenceIntervals_Blankwall[,1])-5):length(ConfidenceIntervals_Blankwall[,1]),2]),2)

DataFrameBlankWall = DataFrameBlankWall %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameBlankWall) = rownames(summary(mod1_BlankWall)$coef)
write.csv(DataFrameBlankWall,file = "./Tables/Excel_DataFrameBlankWall.csv")


########wall moves, only induced motion
# mod1_WallMoves = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                        nAGQ = 0,
#                        glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_WallMoves = confint(mod1_WallMoves,method = "boot")
# save(mod1_WallMoves, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                    "/SavedVariables/mod1_WallMoves.RData"))
# save(ConfidenceIntervals_WallMoves, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_WallMoves.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_WallMoves.RData"))


DataFrameWallMoves = round(data.frame(
  RegressionCoefficient = summary(mod1_WallMoves)$coef[1:6],
  SE = summary(mod1_WallMoves)$coef[7:12],
  CI_Lower = ConfidenceIntervals_WallMoves[(length(ConfidenceIntervals_WallMoves[,1])-5):length(ConfidenceIntervals_WallMoves[,1]),1],
  CI_Higher = ConfidenceIntervals_WallMoves[(length(ConfidenceIntervals_WallMoves[,1])-5):length(ConfidenceIntervals_WallMoves[,1]),2]),2)

DataFrameWallMoves = DataFrameWallMoves %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameWallMoves) = rownames(summary(mod1_WallMoves)$coef)
write.csv(DataFrameWallMoves,file = "./Tables/Excel_DataFrameWallMoves.csv")