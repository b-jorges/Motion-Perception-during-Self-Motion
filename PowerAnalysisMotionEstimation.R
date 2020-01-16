###Pull the whole repository and paste the path to the the local GitHub repository here:
require(dplyr)
require(tidyverse)
require(lme4)
library(PearsonDS)
theme_set(theme_cowplot())
set.seed(912)



#####This is the function from which I choose which comparison values we put into the modelling part: 
#super high curtosis because we will get many values around the PSE
DataFrame3 = data.frame(x = rcauchy(1000,1,0.02),
                        y = rnorm(1000,1,0.1))
ggplot(DataFrame3, aes(x)) + ####just to get an idea of what this function looks like 
  geom_density() +
  coord_cartesian(xlim=c(0.7,1.3))

ID = paste0("s",1:16)
Motion = c(-2,0,2) ##motion left, right, and static ... this will be factors that are supposed to represent congruent/incongruent
velH = c(-8,-6.6, 6.6,8) ##target motion to the left (neg values) and to the right (pos values)
reps = seq(1,55,1) ##we have two PESTS, and between 20 and 35 reps for each, averages for 55 for both together

SimulatePsychometricFunction = function(ID,Motion,velH, reps){
  Psychometric = expand.grid(ID=ID, Motion=Motion, velH=velH, reps = reps)
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,0.15),
           SD_Factor_ID = rnorm(1,1,0.15))
  
  Psychometric = Psychometric %>%
    group_by(ID,Motion,velH) %>%
    mutate(
      EffectOfSelfMotion.Accuracy = (2*velH/3+Motion/8)*abs(PSE_Factor_ID), ###get PSE: +/- 1/8 of selfmotion, aaaand some inter-subject variability (PSE_Factor_ID)
      velH_pest_factor = rcauchy(length(reps),1,0.02), ###get vector of presented stimulus velocities in accordance with staircase (lots of values around PSE, fewer in the periphery)
      velH_shown=EffectOfSelfMotion.Accuracy*velH_pest_factor, ###translates from values around 1 to values around PSE
      ####Get SD for cumulative Gaussian: 0.15*PSE + 0.05*PSE if selfmotion is present, aaaand some inter-subject variability (SD_Factor_ID)
      EffectOfSelfMotion.Precision = (abs(EffectOfSelfMotion.Accuracy*0.1)+abs(EffectOfSelfMotion.Accuracy*0.033*(Motion!=0)))*SD_Factor_ID,
      ####cram everything into a cumulative Gaussian (pnorm()):
      AnswerProbability = pnorm(velH_shown,EffectOfSelfMotion.Accuracy,EffectOfSelfMotion.Precision),
      ####Get difference between target velocity and PEST velocity
      Difference = velH_shown-velH,
      ##get binary answer from probability for each trial
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  
  ###prepare for glmer() - needs sum of YES/Total per stimulus strength and condition
  Psychometric = Psychometric %>%
    group_by(ID,Motion,velH,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(Motion))
  
  Psychometric
}

Psychometric = Psychometric %>%
  filter(abs(Difference) < 5)

ggplot(Psychometric,aes(Difference,Answer,color=as.factor(Motion))) +
  binomial_smooth() +
  facet_grid(velH~ID) +
  xlab("Ratio Target/Comparison") +
  ylab("Probability Target Bigger")


Analyze_Pychometric_Precision = function(Psychometric){
  Psychometric = 
    select(Psychometric,c(ID,Motion,Yes,Total,Difference,velH)) %>%
    distinct() %>%
    filter(abs(Difference) < 5)
  
  Psychometric = Psychometric %>% ###for this test, we can collapse right and leftward motion because we expect the same effect in terms of thresholds for both
    mutate(
      MotionCondition = case_when(
        Motion == 0 ~ "No",
        Motion != 0 ~ "Yes"
      )
    )
  
  ###precision = slope for Difference (the steeper the slope, the more sensitive) ... 
  ###so if there is an interaction between Motion Condition and Difference, that means that it changes the slope
  mod1 = glmer(cbind(Yes, Total - Yes) ~ as.factor(MotionCondition)*Difference + (1 | ID)  + (1 | velH), 
               family = binomial(link = "probit"), 
               data = Psychometric)
  mod2 = glmer(cbind(Yes, Total - Yes) ~ as.factor(MotionCondition) + Difference + (1 | ID)  + (1 | velH),
               family = binomial(link = "probit"), 
               data = Psychometric)
  anova(mod1,mod2)$`Pr(>Chisq)`[2] ##Model 1 beats model 2?
}

Analyze_Pychometric_Accuracy = function(Psychometric){
  Psychometric = 
    select(Psychometric,c(ID,Motion,Yes,Total,Difference, velH)) %>%
    distinct()
  
  mod1 = glmer(cbind(Yes, Total - Yes) ~ Motion + Difference + (1 | ID)  + (1 | velH),
               family = binomial(link = "probit"), 
               data = Psychometric)
  
  mod2 = glmer(cbind(Yes, Total - Yes) ~ Difference + (1 | ID) + (1 | velH),
               family = binomial(link = "probit"), 
               data = Psychometric)
  anova(mod1,mod2)$`Pr(>Chisq)`[2] ##Model 1 beats model 2
}

Power_Precision = c()
nIterations = 100
out <- replicate(nIterations, {
  Analyze_Pychometric_Precision(SimulatePsychometricFunction(ID=ID, Motion=Motion, velH=velH, reps=reps))})
hist(out) ###Distribution of p values
Power_Precision = mean(out < 0.05) ###Power is the times the difference between the two models is significant
Power_Precision ###This is the power for the JNDs

Power_Accuracy = c()
nIterations = 100
out2 <- replicate(nIterations, {
  Analyze_Pychometric_Accuracy(SimulatePsychometricFunction(ID=ID, Motion=Motion, velH=velH, reps=reps))})
hist(out2) ###Distribution of p values
Power_Accuracy = mean(out2 < 0.05) ###Power is the times the difference between the two models is significant
Power_Accuracy ###This is the power for the PSEs
