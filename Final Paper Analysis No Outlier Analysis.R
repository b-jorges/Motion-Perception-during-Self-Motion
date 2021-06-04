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
source("Final Paper Data Preprocessing No Outlier Analysis.r")


#################################################
#                                               #
#          Preregistered Analyses               #
#                                               #
#################################################

#################################################
###################Main Condition####################
#################################################

########Model comparisons JND
# mod1_Regular_JND_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest  | Participant) + (velH_Pest | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_JND_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                        "/SavedVariables/mod1_Regular_JND_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_JND_Test_NoOutlierAnalysis.RData"))

# mod1_Regular_JND_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest  | Participant) + (velH_Pest | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_JND_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                           "/SavedVariables/mod1_Regular_JND_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_JND_Null_NoOutlierAnalysis.RData"))

anova(mod1_Regular_JND_Test_NoOutlierAnalysis,mod1_Regular_JND_Null_NoOutlierAnalysis)
summary(mod1_Regular_JND_Test_NoOutlierAnalysis)



paste0(round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_Regular_JND_Test_NoOutlierAnalysis)$coef[8],2), ")")


########Model comparisons PSE
# mod1_Regular_PSE_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest  | Participant) + (velH_Pest | velH),
#                               family = binomial(link = "probit"),
#                               data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_PSE_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                            "/SavedVariables/mod1_Regular_PSE_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_PSE_Test_NoOutlierAnalysis.RData"))

# mod1_Regular_PSE_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest  | Participant) + (velH_Pest | velH),
#                               family = binomial(link = "probit"),
#                               data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_Regular_PSE_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                             "/SavedVariables/mod1_Regular_PSE_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_PSE_Null_NoOutlierAnalysis.RData"))

anova(mod1_Regular_PSE_Test_NoOutlierAnalysis,mod1_Regular_PSE_Null_NoOutlierAnalysis)
summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)


paste0(round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_Regular_PSE_Test_NoOutlierAnalysis)$coef[8],2), ")")


#################################################
###################Blank Wall####################
#################################################


# mod1_BlankWall_JND_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest | Participant) + (velH_Pest | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_JND_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                              "/SavedVariables/mod1_BlankWall_JND_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_JND_Test_NoOutlierAnalysis.RData"))

# mod1_BlankWall_JND_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest | Participant) + (velH_Pest | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_JND_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                              "/SavedVariables/mod1_BlankWall_JND_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_JND_Null_NoOutlierAnalysis.RData"))

anova(mod1_BlankWall_JND_Test_NoOutlierAnalysis,mod1_BlankWall_JND_Null_NoOutlierAnalysis)

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[8],2), ")")




# mod1_BlankWall_PSE_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest | Participant) + (velH_Pest | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_PSE_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                               "/SavedVariables/mod1_BlankWall_PSE_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_PSE_Test_NoOutlierAnalysis.RData"))

# mod1_BlankWall_PSE_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest | Participant) + (velH_Pest | velH),
#                                 family = binomial(link = "probit"),
#                                 data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_BlankWall_PSE_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                               "/SavedVariables/mod1_BlankWall_PSE_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_PSE_Null_NoOutlierAnalysis.RData"))

anova(mod1_BlankWall_PSE_Test_NoOutlierAnalysis,mod1_BlankWall_PSE_Null_NoOutlierAnalysis)

paste0(round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_PSE_Test_NoOutlierAnalysis)$coef[8],2), ")")


#################################################
###################Wall moves####################
#################################################

# mod1_WallMoves_JND_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static*velH_Pest + (velH_Pest  | Participant) + (velH_Pest  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_JND_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                "/SavedVariables/mod1_WallMoves_JND_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_JND_Test_NoOutlierAnalysis.RData"))

# mod1_WallMoves_JND_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Static + velH_Pest + (velH_Pest  | Participant) + (velH_Pest  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_JND_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                "/SavedVariables/mod1_WallMoves_JND_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_JND_Null_NoOutlierAnalysis.RData"))

anova(mod1_WallMoves_JND_Test_NoOutlierAnalysis,mod1_WallMoves_JND_Null_NoOutlierAnalysis)

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_BlankWall_JND_Test_NoOutlierAnalysis)$coef[8],2), ")")





# mod1_WallMoves_PSE_Test_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent + velH_Pest + (velH_Pest  | Participant) + (velH_Pest  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_PSE_Test_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                 "/SavedVariables/mod1_WallMoves_PSE_Test_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_PSE_Test_NoOutlierAnalysis.RData"))

# mod1_WallMoves_PSE_Null_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ velH_Pest + (velH_Pest  | Participant) + (velH_Pest  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# save(mod1_WallMoves_PSE_Null_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                 "/SavedVariables/mod1_WallMoves_PSE_Null_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_PSE_Null_NoOutlierAnalysis.RData"))

anova(mod1_WallMoves_PSE_Test_NoOutlierAnalysis,mod1_WallMoves_PSE_Null_NoOutlierAnalysis)

paste0(round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[1],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[5],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[2],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[6],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[3],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[7],2), ")")

paste0(round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[4],2), " (SE = ", 
       round(summary(mod1_WallMoves_PSE_Test_NoOutlierAnalysis)$coef[8],2), ")")


#################################################
#                                               #
#            Exploratory Analyses               #
#                                               #
#################################################

#########Exploratory Analysis JNDs
# mod1_Regular_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest  | Participant) + (velH_Pest | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_Regular_NoOutlierAnalysis = confint(mod1_Regular_NoOutlierAnalysis,method = "boot")
# save(mod1_Regular_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/mod1_Regular_NoOutlierAnalysis.RData"))
# save(ConfidenceIntervals_Regular_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_Regular_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_Regular_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_Regular_NoOutlierAnalysis.RData"))


DataFrameRegular_NoOutlierAnalysis = round(data.frame(
                              RegressionCoefficient = summary(mod1_Regular_NoOutlierAnalysis)$coef[1:6],
                              SE = summary(mod1_Regular_NoOutlierAnalysis)$coef[7:12],
                              CI_Lower = ConfidenceIntervals_Regular_NoOutlierAnalysis[(length(ConfidenceIntervals_Regular_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_Regular_NoOutlierAnalysis[,1]),1],
                              CI_Higher = ConfidenceIntervals_Regular_NoOutlierAnalysis[(length(ConfidenceIntervals_Regular_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_Regular_NoOutlierAnalysis[,1]),2]),2)

DataFrameRegular_NoOutlierAnalysis = DataFrameRegular_NoOutlierAnalysis %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameRegular_NoOutlierAnalysis) = rownames(summary(mod1_Regular_NoOutlierAnalysis)$coef)
write.csv(DataFrameRegular_NoOutlierAnalysis,file = "./Tables/Excel_DataFrameRegular_NoOutlierAnalysis.csv")



#########white wall, no induced motion
# mod1_BlankWall_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest | Participant) + (velH_Pest | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_Blankwall_NoOutlierAnalysis = confint(mod1_BlankWall_NoOutlierAnalysis,method = "boot")
# save(mod1_BlankWall_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                  "/SavedVariables/mod1_BlankWall_NoOutlierAnalysis.RData"))
# save(ConfidenceIntervals_Blankwall_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_Blankwall_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_BlankWall_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_Blankwall_NoOutlierAnalysis.RData"))


DataFrameBlankWall_NoOutlierAnalysis = round(data.frame(
  RegressionCoefficient = summary(mod1_BlankWall_NoOutlierAnalysis)$coef[1:6],
  SE = summary(mod1_BlankWall_NoOutlierAnalysis)$coef[7:12],
  CI_Lower = ConfidenceIntervals_Blankwall_NoOutlierAnalysis[(length(ConfidenceIntervals_Blankwall_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_Blankwall_NoOutlierAnalysis[,1]),1],
  CI_Higher = ConfidenceIntervals_Blankwall_NoOutlierAnalysis[(length(ConfidenceIntervals_Blankwall_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_Blankwall_NoOutlierAnalysis[,1]),2]),2)

DataFrameBlankWall_NoOutlierAnalysis = DataFrameBlankWall_NoOutlierAnalysis %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameBlankWall_NoOutlierAnalysis) = rownames(summary(mod1_BlankWall_NoOutlierAnalysis)$coef)
write.csv(DataFrameBlankWall_NoOutlierAnalysis,file = "./Tables/Excel_DataFrameBlankWall_NoOutlierAnalysis.csv")


#########wall moves, only induced motion
# mod1_WallMoves_NoOutlierAnalysis = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest  | Participant) + (velH_Pest  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM_NoOutlierAnalysis %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")),
#                      nAGQ = 1,
#                      glmerControl(optimizer = "nloptwrap"))
# ConfidenceIntervals_WallMoves_NoOutlierAnalysis = confint(mod1_WallMoves_NoOutlierAnalysis,method = "boot")
# save(mod1_WallMoves_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                    "/SavedVariables/mod1_WallMoves_NoOutlierAnalysis.RData"))
# save(ConfidenceIntervals_WallMoves_NoOutlierAnalysis, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                       "/SavedVariables/ConfidenceIntervals_WallMoves_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/mod1_WallMoves_NoOutlierAnalysis.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/ConfidenceIntervals_WallMoves_NoOutlierAnalysis.RData"))


DataFrameWallMoves_NoOutlierAnalysis = round(data.frame(
  RegressionCoefficient = summary(mod1_WallMoves_NoOutlierAnalysis)$coef[1:6],
  SE = summary(mod1_WallMoves_NoOutlierAnalysis)$coef[7:12],
  CI_Lower = ConfidenceIntervals_WallMoves_NoOutlierAnalysis[(length(ConfidenceIntervals_WallMoves_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_WallMoves_NoOutlierAnalysis[,1]),1],
  CI_Higher = ConfidenceIntervals_WallMoves_NoOutlierAnalysis[(length(ConfidenceIntervals_WallMoves_NoOutlierAnalysis[,1])-5):length(ConfidenceIntervals_WallMoves_NoOutlierAnalysis[,1]),2]),2)

DataFrameWallMoves_NoOutlierAnalysis = DataFrameWallMoves_NoOutlierAnalysis %>% 
  mutate(Signifant = case_when(
    CI_Lower*CI_Higher > 0 ~ "*",
    TRUE ~ "n.s."
  ))
rownames(DataFrameWallMoves_NoOutlierAnalysis) = rownames(summary(mod1_WallMoves_NoOutlierAnalysis)$coef)
write.csv(DataFrameWallMoves_NoOutlierAnalysis,file = "./Tables/Excel_DataFrameWallMoves_NoOutlierAnalysis.csv")