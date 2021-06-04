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
source("Utilities/Funs.R")
source("Final Paper Data Preprocessing.r")
require(brms)


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
ggsave("Figures/Selfmotion.jpg",w = 6, h = 6)

# #########regular results
# mod1_Regular_Include = glmer(cbind(Yes, Total - Yes) ~ Include*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                      family = binomial(link = "probit"),
#                      data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")))
# summary(mod1_Regular_Include)
# 
# ########white wall, no induced motion
# mod1_BlankWall_Include = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest*Include + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic") & Participant != "Benjamin"))
# summary(mod1_BlankWall_Include)
# 
# ########wall moves, only induced motion
# mod1_WallMoves_Include = glmer(cbind(Yes, Total - Yes) ~ Congruent*velH_Pest*Include + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                        family = binomial(link = "probit"),
#                        data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic") & Participant != "Benjamin"))
# summary(mod1_WallMoves_Include)


# ####################Gender diffs???
# #########regular results
# mod1_Regular_Gender = glmer(cbind(Yes, Total - Yes) ~ gender*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH),
#                              family = binomial(link = "probit"),
#                              data = Data_GLM %>% filter(Condition %in% c("RegularCondition", "RegularWallStatic")))
# summary(mod1_Regular_Gender)
# 
# ########white wall, no induced motion
# mod1_BlankWall_Gender = glmer(cbind(Yes, Total - Yes) ~ gender*Congruent*velH_Pest + (velH_Pest + Congruent | Participant) + (velH_Pest + Congruent | velH),
#                                family = binomial(link = "probit"),
#                                data = Data_GLM %>% filter(Condition %in% c("BlankWall", "BlankWallStatic")))
# summary(mod1_BlankWall_Gender)
# 
# ########wall moves, only induced motion
# mod1_WallMoves_Gender = glmer(cbind(Yes, Total - Yes) ~ gender*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent  | velH),
#                                family = binomial(link = "probit"),
#                                data = Data_GLM %>% filter(Condition %in% c("WallMoves", "RegularWallStatic")))
# summary(mod1_WallMoves_Gender)


priors <- c(prior(normal(2.5, 3), class = Intercept), #intercept
            prior(normal( -0.2, 3), coef = "Includeyes"), #Includeyes
            prior(normal( 0.2, 3), coef = "Congruentincongruent"), #Congruent:incongruent
            prior(normal( 0.5, 3), coef = "velH_Pest"), #velH_Pest
            prior(normal( 0, 3), coef = "Includeyes:Congruentincongruent"), #Includeyes-Congruent:incongruent
            prior(normal( 0, 3), coef = "Includeyes:velH_Pest"), #Includeyes-velH_Pest
            prior(normal( 0.2, 3), coef = "Congruentincongruent:velH_Pest"), #Conruent:congruent-velH_Pest
            prior(normal( 0, 3), coef = "Includeyes:Congruentincongruent:velH_Pest"), #All interactions
            prior(normal( 0.5, 2), class = sd),
            prior(lkj(4), class = cor))

# Bayesian_Include = brm(bf(Pest_Bigger ~ Include*Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH)),
#             family = bernoulli(),
#             data = Data_MainExperiment,
#             cores = 4,
#             warmup = 2000,
#             iter = 20000,
#             control = list(adapt_delta = 0.9),
#             save_all_pars = TRUE,
#             prior = priors)
# save(Bayesian_Include, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                                        "/SavedVariables/Bayesian_Include.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Bayesian_Include.RData"))


priors_Null <- c(prior(normal(2.5, 3), class = Intercept), #intercept
            prior(normal( 0.2, 3), coef = "Congruentincongruent"), #Congruent:incongruent
            prior(normal( 0.5, 3), coef = "velH_Pest"), #velH_Pest
            prior(normal( 0.2, 3), coef = "Congruentincongruent:velH_Pest"), #Conruent:congruent-velH_Pest
            prior(normal( 0.5, 2), class = sd),
            prior(lkj(4), class = cor))

# Bayesian_Include_Null = brm(bf(Pest_Bigger ~ Congruent*velH_Pest + (velH_Pest + Congruent  | Participant) + (velH_Pest + Congruent | velH)),
#                               family = bernoulli(),
#                               data = Data_MainExperiment,
#                               cores = 4,
#                               warmup = 2000,
#                               iter = 20000,
#                               control = list(adapt_delta = 0.9),
#                               save_all_pars = TRUE,
#                               prior = priors_Null)
# save(Bayesian_Include_Null, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                                      "/SavedVariables/Bayesian_Include_Null.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/Bayesian_Include_Null.RData"))


# margLogLik_TestModel_Selfmotion <- brms::bridge_sampler(Bayesian_Include, silent = TRUE)
# margLogLik_NullModel_Selfmotion <- brms::bridge_sampler(Bayesian_Include_Null, silent = TRUE)
# save(margLogLik_TestModel_Selfmotion, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                               "/SavedVariables/margLogLik_TestModel_Selfmotion.RData"))
# save(margLogLik_NullModel_Selfmotion, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),
#                               "/SavedVariables/margLogLik_NullModel_Selfmotion.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_TestModel_Selfmotion.RData"))
load(file=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/SavedVariables/margLogLik_NullModel_Selfmotion.RData"))

BF_ln <- brms::bayes_factor(margLogLik_NullModel_Selfmotion,margLogLik_TestModel_Selfmotion)
BF_ln