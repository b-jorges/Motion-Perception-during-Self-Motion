###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(lme4) #package for statistical analysis 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
source("Final Paper Data Preprocessing.r")

######Figure 2
ggplot(Data_Selfmotion %>% 
         mutate(Condition2 = case_when(
           Condition == "BlankWall" ~ "2BlankWall",
           Condition == "RegularCondition" ~ "1RegularCondition",
           Condition == "WallMoves" ~ "3WallMoves")),aes(Condition2,Judgement)) +
  geom_jitter(alpha = 0.2, width = 0.05) +
  geom_flat_violin(size=1) + 
  stat_summary(fun = "mean", geom = "point",size = 6, aes(group=c(Participant)), shape = 95) +
  stat_summary(fun = "mean", geom = "point",size = 4, aes(group=0), shape = 16, color = "black") + 
  annotate("segment", x = 0.6, xend = 1.4, y = 0.6, yend = 0.6, linetype = 2, size = 1.5) + 
  annotate("segment", x = 1.6, xend = 2.4, y = 0.6, yend = 0.6, linetype = 2, size = 1.5) + 
  annotate("segment", x = 2.6, xend = 3.4, y = -0.6, yend = -0.6, linetype = 2, size = 1.5) +
  scale_x_discrete(labels = c("Regular Condition","Blank Wall","Moving Wall"), name = NULL)
ggsave("Figures/(Figure 2) Selfmotion.jpg",w = 6, h = 6)


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
