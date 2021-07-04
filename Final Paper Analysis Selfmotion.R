###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(lme4) #package for statistical analysis 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Final Paper Data Preprocessing.r")

#########regular results
# Include = glmer(cbind(Yes, Total - Yes) ~ Include*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                              family = binomial(link = "probit"),
#                              data = Data_GLM,
#                              nAGQ = 0,
#                              glmerControl(optimizer = "nloptwrap"))
# 
# Include_Null = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                                   family = binomial(link = "probit"),
#                                   data = Data_GLM,
#                                   nAGQ = 0,
#                                   glmerControl(optimizer = "nloptwrap"))
# 
# save(Include, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                               "/SavedVariables/Include.RData"))
# save(Include_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                               "/SavedVariables/Include_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Include.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Include_Null.RData"))

anova(Include, Include_Null)
