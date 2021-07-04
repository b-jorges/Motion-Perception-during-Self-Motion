###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(cowplot) #design for data visualiation
require(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set path of this script as working directory
theme_set(theme_cowplot()) #sets design parameters for data visualization
source("Final Paper Data Preprocessing.r")


###################################
######PSE Model
###################################

Parameters = Parameters %>% 
  mutate(Static = case_when(
                Condition %in% c("RegularWallStatic","BlankWallStatic") ~ "Static",
                TRUE ~ "Movement"
                  ),
        WallMaterial = case_when(
                Condition %in% c("BlankWall","BlankWallStatic") ~ "Blank",
                TRUE ~ "Textured"
        )
  )

Parameters = Parameters %>%
  #only continue with participants where we have data from all staircases
  group_by(Participant) %>%
  filter(length(Congruent) > 15) %>% 
  mutate(ConditionSelfmotion = case_when(
    Congruent == "1no motion" ~ 0,
    Congruent == "congruent" & Condition %in% c("BlankWall","RegularCondition") ~ -1,
    Congruent == "incongruent" & Condition %in% c("BlankWall","RegularCondition") ~ 1,
    Condition == "WallMoves" ~ 0),
    
    #should motion be induced (no = 0) and should it make for speed overestimation (1) or underrstimation (-1)
    ConditionInducedMotion = case_when( 
      Congruent == "1no motion" ~ 0,
      #if the wall moves in the same direction as the target, induced motion would lead the target to be perceived as slower
      Congruent == "congruent" & Condition == "WallMoves" ~ -1,
      
      #selfmotion in the same direction as the target would lead the background to move in the opposite direction, 
      #thus motion should be induced in the same direction as the target and the target should be perceived as faster
      Congruent == "congruent" & Condition %in% c("RegularCondition","BlankWall") ~ 1,
      
      #if the wall moves in the opposite direction of the target, induced motion would lead the target to be perceived as faster
      Congruent == "incongruent" & Condition == "WallMoves" ~ 1,
      
      #selfmotion in the same direction as the target would lead the background to move in the opposite direction, 
      #thus motion should be induced in the same direction as the target and the target should be perceived as faster
      Congruent == "incongruent" & Condition %in% c("RegularCondition","BlankWall") ~ -1),
    
    #effect of self-motion should occur whenever the participant is moving, but not when the wall is moving
    SelfmotionPresent = case_when(
      Condition %in% c("BlankWallStatic", "RegularConditionStatic", "WallMoves") ~ 0,
      TRUE ~ 1),
    
    #Induced motion should happen when the wall moves, but also when the participant moves and the wall is textured
    InducedMotionPresent = case_when( #effect of induced motion
      Condition %in% c("WallMoves", "RegularCondition") ~ 1,
      TRUE ~ 0))  %>% 
  mutate(Static = case_when(
    Condition %in% c("RegularWallStatic","BlankWallStatic") ~ "Static",
    TRUE ~ "Movement"
  ),
  WallMaterial = case_when(
    Condition %in% c("BlankWall","BlankWallStatic") ~ "Blank",
    TRUE ~ "Textured"
  )
  ) %>% 
  
  #Define baseline for each condition (blank wall and texture wall/participants/target velocity seperately)
  group_by(Participant,velH, WallMaterial) %>% 
  mutate(Baseline_PSE = Mean[Static == "Static"],
         Baseline_JND = SD[Static == "Static"],
         )



#Fit PSEs based on the effect of self-motion and induced motion (two parameters)
#See Equation 8 in paper
FitPSEs = function(x,Mean,Baseline_PSE,ConditionSelfmotion,ConditionInducedMotion,SelfmotionPresent,InducedMotionPresent){
  SelfmotionEffect = x[1]
  InducedMotionEffect = x[2]
  (mean((Mean - (Baseline_PSE +
    ConditionSelfmotion*SelfmotionEffect*SelfmotionPresent + 
      ConditionInducedMotion*InducedMotionEffect*InducedMotionPresent))^2))^0.5
}



InitialParameters_PSE = c(0,0)

#fit the model for each participant seperately and output the effect of self-motion, 
#the effeect of induced motion, the Root Mean Squared Error and the predictions (to be compared against actual performance)
Model_PSE = Parameters %>%
  group_by(Participant) %>%
  mutate(SelfmotionEffect = optim(InitialParameters_PSE,
                                  FitPSEs,
                                  Mean = Mean,
                                  Baseline_PSE = Baseline_PSE,
                                  ConditionSelfmotion = ConditionSelfmotion,
                                  ConditionInducedMotion = ConditionInducedMotion,
                                  SelfmotionPresent = SelfmotionPresent,
                                  InducedMotionPresent = InducedMotionPresent)$par[1],
         InducedMotionEffect = optim(InitialParameters_PSE,
                                     FitPSEs,
                                     Mean = Mean,
                                     Baseline_PSE = Baseline_PSE,
                                     ConditionSelfmotion = ConditionSelfmotion,
                                     ConditionInducedMotion = ConditionInducedMotion,
                                     SelfmotionPresent = SelfmotionPresent,
                                     InducedMotionPresent = InducedMotionPresent)$par[2],
         RMSE_PSE = optim(InitialParameters_PSE,
                          FitPSEs,
                          Mean = Mean,
                          Baseline_PSE = Baseline_PSE,
                          ConditionSelfmotion = ConditionSelfmotion,
                          ConditionInducedMotion = ConditionInducedMotion,
                          SelfmotionPresent = SelfmotionPresent,
                          InducedMotionPresent = InducedMotionPresent)$value,
         Predictions_PSE = Baseline_PSE +
           ConditionSelfmotion*SelfmotionEffect*SelfmotionPresent + 
           ConditionInducedMotion*InducedMotionEffect*InducedMotionPresent) %>%
  group_by(Participant,Congruent,Condition,velH) %>% 
  slice(1)

###############
#####Figure 5
###############
ModelFitPSEs = ggplot(Model_PSE %>% filter(Congruent != "1no motion"),aes(Predictions_PSE,Mean,color = Condition)) +
  geom_point() +
  coord_cartesian(xlim = c(1.5,9), ylim = c(1.5,9)) +
  annotate("segment",x = 0, xend = 9, y = 0, yend = 9, linetype = 2) +
  scale_color_manual(name = "Condition",
                     values = c(Red,Yellow,BlauUB),
                     labels = c("Blank Wall", "Regular Condition", "Moving Wall")) +
  ylab("Fitted PSEs (m/s)") +
  xlab("Model Predictions (PSEs)") +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid")) +
  ggtitle("A. Model Fit (PSEs)")

SelfmotionPSEs = ggplot(Model_PSE,aes(SelfmotionEffect)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Effect of Selfmotion (m/s)") +
  ylab("Density") +
  ggtitle("B. Self-Motion")

InducedMotionPSEs = ggplot(Model_PSE,aes(InducedMotionEffect)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Effect of Induced Motion (m/s)") +
  ylab("Density") +
  ggtitle("C. Induced Motion")

RightPart_PSEs = plot_grid(SelfmotionPSEs,InducedMotionPSEs,ncol = 1)

plot_grid(ModelFitPSEs,RightPart_PSEs)
ggsave("Figures/(Figure 5) Model_PSEs.jpg", w = 10, h = 8)

#get some summary statistics on RMSE, effect of self-motion and effect of induced motion
round(mean(Model_PSE$RMSE_PSE),2)
round(median(Model_PSE$RMSE_PSE),2)

round(mean(Model_PSE$SelfmotionEffect),2)
round(median(Model_PSE$SelfmotionEffect),2)

round(mean(Model_PSE$InducedMotionEffect),2)
round(median(Model_PSE$InducedMotionEffect),2)



##########################
######JND Model
##########################
Parameters = Parameters %>% 
  #values from the r script "Distance between Observer and Target"
  mutate(AngularVelocity = case_when(
    Congruent == "congruent" & velH == 6.6 & Condition %in% c("BlankWall", "RegularCondition") ~ 27.6, 
    Congruent == "incongruent" & velH == 6.6 & Condition %in% c("BlankWall", "RegularCondition") ~ 73.3, 
    Congruent == "congruent" & velH == 6.6 & Condition %in% c("BlankWallStatic", "RegularConditionStatic", "WallMoves") ~ 46.6, 
    Congruent == "incongruent" & velH == 6.6 & Condition %in% c("BlankWallStatic", "RegularConditionStatic", "WallMoves") ~ 46.6, 
    Congruent == "congruent" & velH == 8 & Condition %in% c("BlankWall", "RegularCondition") ~ 32.5,
    Congruent == "incongruent" & velH == 8 & Condition %in% c("BlankWall", "RegularCondition") ~ 82.2,
    Congruent == "congruent" & velH == 8 & Condition %in% c("BlankWallStatic", "RegularConditionStatic", "WallMoves") ~ 56.2,
    Congruent == "incongruent" & velH == 8 & Condition %in% c("BlankWallStatic", "RegularConditionStatic", "WallMoves") ~ 56.2,
    Congruent == "1no motion" & velH == 6.6 ~ 46.6,
    Congruent == "1no motion" & velH == 8 ~ 56.2))

####model, see Equation 9
FitJNDs = function(x,SD,Baseline_JND,AngularVelocity,SelfmotionPresent){
  SelfmotionEffect = x[1]
  WF = x[2]
  (mean((SD - (Baseline_JND +
                  SelfmotionEffect*SelfmotionPresent + 
                WF*AngularVelocity))^2))^0.5
  }


#fit the model for each participant seperately and output the effect of self-motion, 
#effects relative to Weber Fractions, the Root Mean Squared Error and the predictions (to be compared against actual performance)
Model_JND = Parameters %>%
  group_by(Participant) %>%
  mutate(SelfmotionEffect = optim(c(0,0),
                                  FitJNDs,
                                  Baseline_JND = Baseline_JND, 
                                  AngularVelocity = AngularVelocity,
                                  SelfmotionPresent = SelfmotionPresent,
                                  SD = SD)$par[1],
         WF = optim(c(0,0),
                    FitJNDs,
                    Baseline_JND = Baseline_JND, 
                    AngularVelocity = AngularVelocity,
                    SelfmotionPresent = SelfmotionPresent,
                    SD = SD)$par[2],
         RMSE_JND = optim(c(0,0),
                          FitJNDs,
                          Baseline_JND = Baseline_JND, 
                          AngularVelocity = AngularVelocity,
                          SelfmotionPresent = SelfmotionPresent,
                          SD = SD)$value,
         Predictions_JND = Baseline_JND +
           SelfmotionEffect*SelfmotionPresent + 
           WF*AngularVelocity) %>%
  group_by(Participant,Congruent,Condition,velH) %>% 
  slice(1)



###############
#####Figure 6
###############
ModelFitJNDs = ggplot(Model_JND %>% filter(Congruent != "1no motion"),aes(Predictions_JND,SD,color = Condition)) +
  geom_point() +
  annotate("segment",x = 0, xend = 9, y = 0, yend = 9, linetype = 2) +
  coord_cartesian(xlim = c(0,5.75), ylim = c(0,5.75)) +
  scale_color_manual(name = "Condition",
                     values = c(Red,Yellow,BlauUB),
                     labels = c("Blank Wall", "Regular Condition", "Moving Wall")) +
  ylab("Fitted SDs (m/s)") +
  xlab("Model-Predicted SDs (m/s)") +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill="lightblue", 
                                         size=0.5, linetype="solid")) +
  ggtitle("A. Model Fit (SDs)")

SelfmotionJNDs = ggplot(Model_JND,aes(SelfmotionEffect)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Effect of Selfmotion (m/s)") +
  ylab("Density") +
  ggtitle("B. Self-Motion")

WF_JNDs = ggplot(Model_JND,aes(WF)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2) +
  xlab("Effect of Angular Velocity") +
  ylab("Density") +
  ggtitle("C. Angular Velocity")

RightPart_JNDs = plot_grid(SelfmotionJNDs,WF_JNDs,ncol = 1)

plot_grid(ModelFitJNDs,RightPart_JNDs)
ggsave("Figures/(Figure 6) Model_JNDs.jpg", w = 10, h = 8)

round(mean(Model_JND$RMSE_JND),2)
round(median(Model_JND$RMSE_JND),2)

round(mean(Model_JND$SelfmotionEffect),3)
round(median(Model_JND$SelfmotionEffect),3)

round(mean(Model_JND$WF),4)
round(median(Model_JND$WF),4)



###model overall ... correlation between compensation and selfmotion  on JNDs
Model_JND$Compensation = 1-Model_PSE$SelfmotionEffect
cor.test((Model_JND %>% filter(Condition %in% c("RegularCondition", "BlankWall")) %>%  group_by(Participant,WallMaterial) %>% slice(1))$Compensation,
         (Model_JND %>% filter(Condition %in% c("RegularCondition", "BlankWall")) %>% group_by(Participant,WallMaterial) %>% slice(1))$SelfmotionEffect)