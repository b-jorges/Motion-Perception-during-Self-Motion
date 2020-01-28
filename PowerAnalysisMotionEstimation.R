###Pull the whole repository and paste the path to the the local GitHub repository here:
require(dplyr)
require(tidyverse)
require(lme4)

theme_set(theme_cowplot())
set.seed(912)



#####This is the function from which I choose which comparison values we put into the modelling part: 
#super high curtosis because we will get many values around the PSE
DataFrame3 = data.frame(x = rcauchy(1000,1,0.05),
                        y = rnorm(1000,1,0.1))
ggplot(DataFrame3, aes(x)) + ####just to get an idea of what this function looks like 
  geom_density() +
  coord_cartesian(xlim=c(0.5,1.5))

ID = paste0("s",1:16)
Motion = c(-1,0,1) ##motion left, right, and static ... this will be factors that are supposed to represent congruent/incongruent
velH = c(-8,-6.6, 6.6,8) ##target motion to the left (neg values) and to the right (pos values)
reps = seq(1,55,1) ##we have two PESTS, and between 20 and 35 reps for each, averages for 55 for both together

SimulatePsychometricFunction = function(ID,Motion,velH, reps,PSE_Diff, JND_Diff){
  Psychometric = expand.grid(ID=ID, Motion=Motion, velH=velH, reps = reps)
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%#
    mutate(PSE_Factor_ID = rnorm(1,1,0.1),
           SD_Factor_ID = rnorm(1,1,0.1))
 
  Psychometric = Psychometric %>%
    mutate(
      EffectOfSelfMotion.Accuracy = (2*velH/3+Motion*PSE_Diff)*abs(PSE_Factor_ID), ###get PSE: +/- 1/8 of selfmotion, aaaand some inter-subject variability (PSE_Factor_ID)
      velH_pest_factor = rcauchy(length(reps),1,0.06), ###get vector of presented stimulus velocities in accordance with staircase (lots of values around PSE, fewer in the periphery)
      velH_shown=EffectOfSelfMotion.Accuracy*velH_pest_factor, ###translates from values around 1 to values around PSE
      ####Get SD for cumulative Gaussian: 0.15*PSE + 0.05*PSE if selfmotion is present, aaaand some inter-subject variability (SD_Factor_ID)
      EffectOfSelfMotion.Precision = (abs(EffectOfSelfMotion.Accuracy*0.1)+abs(EffectOfSelfMotion.Accuracy*JND_Diff*(Motion!=0)))*SD_Factor_ID,
      ####cram everything into a cumulative Gaussian (pnorm()):
      AnswerProbability = pnorm(abs(velH_shown),abs(EffectOfSelfMotion.Accuracy),EffectOfSelfMotion.Precision),
      ####Get difference between target velocity and PEST velocity
      Difference = abs(velH_shown) - abs(velH),
      ##get binary answer from probability for each trial
      Answer = as.numeric(rbernoulli(length(AnswerProbability),AnswerProbability))
    )
  

  ###prepare for glmer() - needs sum of YES/Total per stimulus strength and condition
  Psychometric = Psychometric %>%
    group_by(ID,Motion,velH,Difference) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(Motion)) 
  
  Psychometric =  Psychometric %>% 
    mutate(Congruent = case_when(
    velH*Motion < 0 ~ "incongruent",
    velH*Motion > 0 ~ "congruent",
    velH*Motion == 0 ~ "1no motion"
                                )
          )
}

Psychometric = Psychometric %>%
  filter(abs(Difference) < 5)

ggplot(Psychometric,aes(Difference,Answer,color=Congruent)) +
  binomial_smooth() +
  facet_grid(velH~ID) +
  xlab("Ratio Target/Comparison") +
  ylab("Probability Target Bigger") +
  coord_cartesian(xlim = c(-5,-1))


Analyze_Pychometric_Precision = function(Psychometric){
  
  TimeBeginning = Sys.time()
  
  Psychometric = 
    select(Psychometric,c(ID,Motion,Yes,Total,Difference,velH)) %>%
    distinct() %>%
    group_by(ID,Motion,velH) %>%
    filter(abs(Difference) < 5)
  
  Psychometric = Psychometric %>% ###for this test, we can collapse right and leftward motion because we expect the same effect in terms of thresholds for both
    mutate(
      MotionCondition = case_when(
        Motion == 0 ~ 0,
        Motion != 0 ~ 1
      )
    )
  
  ###precision = slope for Difference (the steeper the slope, the more sensitive) ... 
  ###so if there is an interaction between Motion Condition and Difference, that means that it changes the slope
  mod1 = glmer(cbind(Yes, Total - Yes) ~ as.factor(MotionCondition)*Difference + (Difference  | ID) + (Difference  | velH), 
               family = binomial(link = "probit"), 
               data = Psychometric,
               nAGQ = 0,
               control = glmerControl(optimizer = "nloptwrap"))
  
  mod2 = glmer(cbind(Yes, Total - Yes) ~ as.factor(MotionCondition) + Difference + (Difference  | ID) + (Difference  | velH), 
               family = binomial(link = "probit"), 
               data = Psychometric,
               nAGQ = 0,
               control = glmerControl(optimizer = "nloptwrap"))
  
  print(TimeBeginning - Sys.time())
  
  p = anova(mod1,mod2)$`Pr(>Chisq)`[2] ##Model 1 beats model 2
  
  print(p)
  p
}

Analyze_Pychometric_Accuracy = function(Psychometric){
  
  TimeBeginning = Sys.time()
  
  Psychometric = 
    select(Psychometric,c(ID,Motion,Yes,Total,Difference, velH,Congruent)) %>%
    distinct() %>%
    filter(Difference < -5 & Difference > 1)
  
  mod1 = glmer(cbind(Yes, Total - Yes) ~ Congruent + (Difference  | ID)  + (Difference  | velH),
               family = binomial(link = "probit"), 
               data = Psychometric,
               nAGQ = 0,
               control = glmerControl(optimizer = "nloptwrap"))
  
  mod2 = glmer(cbind(Yes, Total - Yes) ~ (Difference  | ID) + (Difference  | velH),
               family = binomial(link = "probit"), 
               data = Psychometric,
               nAGQ = 0,
               control = glmerControl(optimizer = "nloptwrap"))
  
  print(TimeBeginning - Sys.time())
  
  p = anova(mod1,mod2)$`Pr(>Chisq)`[2] ##Model 1 beats model 2
  
  print(p)
  p
}


######power for very low estimates of effect size: difference of 0.1 m/s in PSEs, and JNDs 1/4 higher when self-motion is simulated
PowerPerN_Precision = c()
for (i in c(10,12,14,16,18,20)){
  ID = paste0("s",1:i)
  Power_Precision = c()
  nIterations = 100
  out <- replicate(nIterations, {
    Analyze_Pychometric_Precision(SimulatePsychometricFunction(ID=ID, Motion=Motion, velH=velH, reps=reps, PSE_Diff = 1/8, JND_Diff = 0.025))})
  hist(out) ###Distribution of p values
  Power_Precision = mean(out < 0.05) ###Power is the times the difference between the two models is significant
  PowerPerN_Precision = c(PowerPerN_Precision,Power_Precision) ###This is the power for the JNDs
  paste0("For ", i, " subjects: ", Power_Precision)
}

PowerPerN_Accuracy = c()
for (i in c(10,12,14,16,18,20)){
  ID = paste0("s",1:i)
  Power_Accuracy = c()
  nIterations = 100
  out2 <- replicate(nIterations, {
    Analyze_Pychometric_Accuracy(SimulatePsychometricFunction(ID=ID, Motion=Motion, velH=velH, reps=reps, PSE_Diff = 1/8, JND_Diff = 0.025))})
  hist(out2) ###Distribution of p values
  Power_Accuracy = mean(out2 < 0.05) ###Power is the times the difference between the two models is significant
  PowerPerN_Accuracy = c(PowerPerN_Accuracy,Power_Accuracy) ###This is the power for the PSEs
  paste0("For ", i, " subjects: ", Power_Accuracy)
}