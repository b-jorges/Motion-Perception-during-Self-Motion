###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(lme4) #package for statistical analysis 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Final Paper Data Preprocessing.r")


#########regular results
mod1_Regular_Include = glmer(cbind(Yes, Total - Yes) ~ Include*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
                             family = binomial(link = "probit"),
                             data = Data_GLM,
                             nAGQ = 0,
                             glmerControl(optimizer = "nloptwrap"))

mod1_Regular_Include_Null = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
                                  family = binomial(link = "probit"),
                                  data = Data_GLM,
                                  nAGQ = 0,
                                  glmerControl(optimizer = "nloptwrap"))

anova(mod1_Regular_Include, mod1_Regular_Include_Null)
