###Pull the whole repository and paste the path to the the local GitHub repository here:
setwd("C:/Users/bjoer/Documents/GitHub/Motion-Perception-during-Self-Motion") 

require(ggplot2)
require(dplyr)
require(lme4)
source("parabolic.r")
source("functions.r")
source("colourschemes.r")
require(tidyverse)
require(quickpsy)
require(cowplot)
require(MixedPsy)
theme_set(theme_cowplot())

set.seed(81)

levelsFix1 = c("NoMotion+NoMotion","MotionCongruent+NoMotion","MotionIncongruent+NoMotion")
bFix1 = c(1,0.9,1.1) ####this is perceived time to contact/actual time to contact
Intercept = 0
sdSubject = 0.08
sdError = 0.08
nParticipants = 10
nTrials = 210
sdCondition = c(0,1,1)

nIterations = 100
Power_Timing1=c()
Power_Timing2=c()

sim1_within_Dataset <- function(bFix1, levelsFix1, Intercept, sdSubject, sdError, nParticipants, nTrials) {
  Subject <- rep(1:(nParticipants), each=nTrials*length(bFix1)) #Vector with subject IDs
  Fix1 <- rep(levelsFix1, nParticipants*nTrials) #Vector with IDs for first fixed effect
  
  # random effects per subject
  S.re <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdSubject)
  
  # epsilons----------------------------------------------------------------------------------------
  eps <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdError)
  
  #random effects per condition
  C.re <- sdCondition*rnorm(nParticipants*nTrials*length(levelsFix1), 0, 0.05)

  # put it all together
  Response <- Intercept + bFix1*(Fix1==Fix1) +
    S.re[Subject] + eps + C.re
  
  # put into a data frame
  mydata <- data.frame(Subject = paste('s',Subject, sep=''), 
                       Fix1=Fix1,
                       Response = Response)
}

LMM_Analysis_Sim1 = function(mydata) {
  # analyze looking at interaction term with LR test
fit1 <- lmer(Response ~ Fix1 + (1|Subject), data=mydata, REML = FALSE)
fit2 <- lmer(Response ~ (1|Subject), data=mydata, REML = FALSE)
summary(fit1)
anova(fit2,fit1)$`Pr(>Chisq)`[2]
}

LMM_Analysis_Sim1(sim1_within_Dataset(bFix1 = bFix1, levelsFix1 = levelsFix1, Intercept = 0, sdSubject = sdSubject, 
                                      sdError = sdError, nParticipants = nParticipants, nTrials = nTrials))

#####Power analysis for accuracy in motion prediction
out1 <- replicate(nIterations, {
  sim_within1(bFix1 = bFix1, levelsFix1 = levelsFix1, Intercept = 0, sdSubject = sdSubject, 
             sdError = sdError, nParticipants = nParticipants, nTrials = nTrials)})
hist(out1)
Power_Timing1 = c(Power_Timing1,mean(out1 < 0.05))
Power_Timing1
#####


levelsFix1 = c("NoMotion+NoMotion","MotionCongruent+NoMotion","MotionIncongruent+NoMotion")
bFix1 = c(1,0.9,1.1) ####this is actual time to contact/perceived time to contact
Intercept = 0
sdSubject = 0.1
sdError = 0.1
nParticipants = 10
nTrials = 80
sdCondition = c(0,1,1)


sim_within2 <- function(bFix1, levelsFix1, Intercept, sdSubject, sdError, nParticipants, nTrials) {
  Subject <- rep(1:(nParticipants), each=nTrials*length(bFix1)) #Vector with subject IDs
  Fix1 <- rep(levelsFix1, nParticipants*nTrials) #Vector with IDs for first fixed effect
  
  # random effects per subject
  S.re <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdSubject)
  
  # epsilons----------------------------------------------------------------------------------------
  eps <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdError)
  
  #random effects per condition
  C.re <- sdCondition*rnorm(nParticipants*nTrials*length(levelsFix1), 0, 0.05)
  
  # put it all together
  Response <- Intercept + bFix1*(Fix1==Fix1) +
    S.re[Subject] + eps + C.re
  
  # put into a data frame
  mydata <- data.frame(Subject = paste('s',Subject, sep=''), 
                       Fix1=Fix1,
                       Response = Response)

  mydata = mydata %>%
    group_by(Subject,Fix1) %>%
    mutate(Mean = mean(Response))
  
  mydata$PrecisionParameter=mydata$Response-mydata$Mean
  
  # analyze looking at interaction term with LR test
  fit1 <- lmer(abs(PrecisionParameter) ~ Fix1 + (1|Subject), data=mydata, REML = FALSE)
  fit2 <- lmer(abs(PrecisionParameter) ~ (1|Subject), data=mydata, REML = FALSE)
  summary(fit1)
  anova(fit2,fit1)$`Pr(>Chisq)`[2]
}

#####Power analysis for precision in motion prediction
out2 <- replicate(nIterations, {
  sim_within2(bFix1 = bFix1, levelsFix1 = levelsFix1, Intercept = 0, sdSubject = sdSubject, 
             sdError = sdError, nParticipants = nParticipants, nTrials = nTrials)})
hist(out2)
Power_Timing2 = c(Power_Timing2,mean(out2 < 0.05))
Power_Timing2
#####




#we have two velocities
#7 comparison values
#three motion thingies (-2,0,2)
#lets say 15 reps per condition
#10 subjects

SimulatePsychometricFunction = function(ID,Motion,velH,rep,Comparison){
  Psychometric = c()
  for (ID_i in ID){
    for (Motion_i in Motion){
      for (velH_i in velH){
        for (rep_i in rep){
          EffectOfSelfMotion.Accuracy = velH_i+Motion_i/10 ###new PSE in function of self-motion ... gross approximation because it should be lower for lower velH, but whatever
          velH_shown=velH_i*Comparison
          EffectOfSelfMotion.Precision = velH_i*0.15+velH_i*0.1*(Motion_i!=0)
          #        AddedNoise = rnorm(7,0,0.06)
          
          #draw probability for yes/no from simulated psychometric function
          AnswerProbability = pnorm(velH_shown,EffectOfSelfMotion.Accuracy,EffectOfSelfMotion.Precision) #+AddedNoise 
          data_temp = data.frame(AnswerProbability = AnswerProbability, 
                                 rep = rep_i,
                                 velH = velH_i,
                                 velH_shown = velH_i*Comparison, 
                                 Motion = Motion_i,
                                 Comparison = Comparison,
                                 ID = ID_i,
                                 PSE = velH_i+Motion_i/4,
                                 SD = velH_i/5+0.1*(Motion_i!=0))
          Psychometric = rbind(Psychometric,data_temp)
        }
      }
    }
  }
  
  Psychometric = Psychometric %>%
    group_by(ID) %>%
    mutate(AnswerProbability = AnswerProbability + rnorm(length(ID),0,0.06))
  Psychometric$Answer = as.numeric(rbernoulli(length(Psychometric$AnswerProbability),Psychometric$AnswerProbability))
  
  Psychometric = Psychometric %>%
    group_by(ID,Motion,velH,Comparison) %>%
    mutate(Yes = sum(Answer==1),
           Total = length(Motion))
  
  Psychometric = 
    select(Psychometric,c(ID,Motion,velH,Comparison,Yes,Total)) %>%
    distinct()
  
  mod1 = glmer(cbind(Yes, Total - Yes) ~ as.factor(Motion) + (1 + Comparison | ID),
               family = binomial(link = "probit"), 
               data = Psychometric)
  mod2 = glmer(cbind(Yes, Total - Yes) ~ (1 + Comparison | ID),
               family = binomial(link = "probit"), 
               data = Psychometric)
  
  anova(mod1,mod2)$`Pr(>Chisq)`[2] ##Model 1 beats model 2
}


ID = c("s01","s02","s03","s04","s05","s06","s07","s08","s09","s10")
Motion = c(-2,0,2)
velH = c(6.6,8)
rep = seq(1,15,1)
Comparison = c(0.7,0.8,0.9,1,1.1,1.2,1.3)
Power_Timing3 = c()

out3 <- replicate(nIterations, {
  SimulatePsychometricFunction(ID=ID, Motion=Motion, velH=velH, rep=rep, Comparison = Comparison)})
hist(out3)
Power_Timing3 = c(Power_Timing3,mean(out2 < 0.05))
Power_Timing3


Psychometric2 = expand.grid(ID=ID, Motion=Motion, velH=velH, rep = rep)

Psychometric2 = Psychometric2 %>%
  group_by(ID,Motion,velH) %>%
  mutate(EffectOfSelfMotion.Accuracy = velH+Motion/4,
         )

###Estimate Weber fractions and PSEs
Psychometric = c()
for (ID_i in ID){
  for (Motion_i in Motion){
    for (velH_i in velH){
      for (rep_i in rep){
        EffectOfSelfMotion.Accuracy = velH_i+Motion_i/4 ###new PSE in function of self-motion ... gross approximation because it should be lower for lower velH, but whatever
        velH_shown=velH_i*Comparison
        EffectOfSelfMotion.Precision = velH_i*0.15+velH_i*0.1*(Motion_i!=0)
#        AddedNoise = rnorm(7,0,0.06)
        
        #draw probability for yes/no from simulated psychometric function
        AnswerProbability = pnorm(velH_shown,EffectOfSelfMotion.Accuracy,EffectOfSelfMotion.Precision) #+AddedNoise 
        data_temp = data.frame(AnswerProbability = AnswerProbability, 
                               rep = rep_i,
                               velH = velH_i,
                               velH_shown = velH_i*Comparison, 
                               Motion = Motion_i,
                               Comparison = Comparison,
                               ID = ID_i,
                               PSE = velH_i+Motion_i/4,
                               SD = velH_i/5+0.1*(Motion_i!=0))
        Psychometric = rbind(Psychometric,data_temp)
      }
    }
  }
}

Psychometric = Psychometric %>%
  group_by(ID) %>%
  mutate(AnswerProbability = AnswerProbability + rnorm(length(ID),0,0.06))
Psychometric$Answer = as.numeric(rbernoulli(length(Psychometric$AnswerProbability),Psychometric$AnswerProbability))

fit1 <- quickpsy(Psychometric, velH_shown, Answer, grouping=.(ID,Motion,velH))
Temp = fit1$par
Temp[Temp$parn == "p1",]$par
Temp[Temp$parn == "p2",]$par
TempTest = cbind(Temp[Temp$parn == "p1",],sd=Temp[Temp$parn == "p2",]$par)

#######Modelling the temporal responses
Distances = c(0.5*6.6,0.6*6.6,0.7*6.6,0.5*8,0.6*8,0.7*8)
TempTest$OcclusionTime = 0.5
Temp2 = TempTest
Temp2$OcclusionTime = 0.6
Temp3 = TempTest
Temp3$OcclusionTime = 0.7
Data = rbind(TempTest,Temp2,Temp3)

Data = select(Data,c(ID,Motion,velH,par,sd,OcclusionTime)) %>% 
  slice(rep(row_number(), 35))

Data$Distance=Data$velH*Data$OcclusionTime

Data$Timing_Modelled = Data$Distance/rnorm(length(Data$Distance),Data$par,Data$sd)
Data$Ratio = Data$Timing_Modelled/Data$OcclusionTime ##which way around? bigger than 1 means overshoot

c("NoMotion+NoMotion","MotionCongruent+NoMotion","MotionIncongruent+NoMotion")
Data$Condition[Data$Motion == -2] = "MotionIncongruent+NoMotion"
Data$Condition[Data$Motion == 0] = "NoMotion+NoMotion"
Data$Condition[Data$Motion == 2] = "MotionCongruent+NoMotion"


#####Get Actual Responses
Subject <- rep(1:(nParticipants), each=nTrials*length(bFix1)) #Vector with subject IDs
Fix1 <- rep(levelsFix1, nParticipants*nTrials) #Vector with IDs for first fixed effect

# random effects per subject
S.re <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdSubject)

# epsilons----------------------------------------------------------------------------------------
eps <- rnorm(nParticipants*nTrials*length(levelsFix1), 0, sdError)

#random effects per condition
C.re <- sdCondition*rnorm(nParticipants*nTrials*length(levelsFix1), 0, 0.05)

# put it all together
Response <- Intercept + bFix1*(Fix1==Fix1) +
  S.re[Subject] + eps + C.re

# put into a data frame
mydata <- data.frame(Subject = paste('s0',Subject, sep=''), 
                     Fix1=Fix1,
                     Response = Response)

mydata = arrange(mydata,Subject,Fix1)
colnames(mydata) = c("ID","Condition","Ratio")
mydata$Simulated = "Measured"
Data = data.frame(arrange(Data,ID,Condition))
Data$Simulated = "Simulated"
DataForPlot = rbind(select(mydata,ID,Condition,Ratio,Simulated),select(Data,ID,Condition,Ratio,Simulated))
DataForPlot = filter(DataForPlot,abs(Ratio-1) < 1) %>%
  mutate(
    SelfMotion = case_when(
    Condition == "MotionIncongruent+NoMotion" ~ "-2",
    Condition == "NoMotion+NoMotion" ~ "0",
    Condition == "MotionCongruent+NoMotion" ~ "2"
    )
  )

DataForPlot$ID[DataForPlot$ID == "s010"] = "s10"


ggplot(DataForPlot, aes(x = SelfMotion, y = Ratio, col = Simulated)) +
  geom_violin() +
  facet_wrap(.~ID)

 




b <- read.table(header=T,"PilotLaurence.txt")
c <- read.table(header=T,"PilotBjorn_2D.txt")
d <- read.table(header=T,"PilotBjorn_3D.txt")
e <- read.table(header=T,"PilotMeaghan.txt")
f <- read.table(header=T,"PilotLaurence2.txt")
g <- read.table(header=T,"PilotRachel.txt")
h <- read.table(header=T,"PilotAbi.txt")
i <- read.table(header=T,"PilotJohn.txt")
j <- read.table(header=T,"PilotJohn.txt")


b$id = "Laurence"
c$id = "Bjorn"
d$id = "Bjorn2"
e$id = "Meaghan"
b$Start_Above = 1
c$Start_Above = 1
f$id = "Laurence2"
g$id = "Rachel"
h$id = "Abi"
i$id = "John"
j$id = "Bob"

a = rbind(b,c,d,e,f,g,h,i,j)

a = a %>%
  mutate(
    Pest_Bigger = case_when(
      Response_Interval == Pest_Interval ~ 1,
      Response_Interval != Pest_Interval ~ 0,
    ),
    Direction = case_when(
      velH < 0 ~ "left",
      velH > 0 ~ "right",
    ),
    Difference = abs(velH_Pest)-abs(velH),
    velH_Absolut = abs(velH),
    Congruent = case_when(
      velH*velH_Subject < 0 ~ "incongruent",
      velH*velH_Subject > 0 ~ "congruent",
      velH*velH_Subject == 0 ~ "1no motion"
    ),
    Difference_Percent = Difference/velH
  ) %>%
  filter(abs(velH_Pest) < abs(velH)*1.5)

ggplot(a, aes(velH_Pest,Pest_Bigger, color = as.factor(velH_Subject))) +
  binomial_smooth() +
  facet_wrap(Direction~velH) +
  theme_cowplot(12)
ggsave("PsychometricFunctionsRaw.jpg", w = 10, h = 6)

ggplot(a[a$id == "Bob",], aes(Difference,Pest_Bigger, color = Congruent)) +
  binomial_smooth() +
  facet_wrap(.~velH)

ggplot(a[a$id %in% c("Meaghan", "Bjorn2", "Laurence2", "John", "Abi"),], aes(Difference,Pest_Bigger, color = Congruent)) +
  binomial_smooth() +
  facet_(id~velH)

lala = quickpsy(a[a$id == "Bjorn",], Difference, Pest_Bigger, grouping = .(velH_Subject,velH))
lala2 = quickpsy(a[a$id == "Laurence",], Difference, Pest_Bigger, grouping = .(velH_Subject,velH))
lala3 = quickpsy(a, Difference, Pest_Bigger, grouping = .(Congruent,velH))
lala4 = quickpsy(a[a$id == "Bjorn2",], Difference, Pest_Bigger, grouping = .(Congruent,velH))
lala5 = quickpsy(a[a$id == "Meaghan",], Difference, Pest_Bigger, grouping = .(Congruent,velH))
lala6 = quickpsy(a[a$id %in% c("Meaghan", "Bjorn2", "Laurence2", "John", "Abi", "Bob"),], Difference, Pest_Bigger, grouping = .(Congruent,velH))
lala7 = quickpsy(a[a$id == "Laurence2",], Difference, Pest_Bigger, grouping = .(Congruent,velH))
lala8 = quickpsy(a[a$id == "Rachel",], Difference, Pest_Bigger, grouping = .(Congruent,velH))

plot(lala5)
plot(lala7) + xlim(c(-5,5))
plot(lala8) + xlim(c(-5,5))

lala3$parcomparisons

plot(lala6)
plot(lala4)
plot(lala5)



Hoho6 = lala6$parcomparisons
Hoho6[Hoho6$Congruent != Hoho6$Congruent2 & Hoho6$velH == Hoho6$velH2,] %>%
  arrange(.,velH,parn)


ggplot(a[a$id == "Bjorn",], aes(PestNr,Step_Size_OnTrial), col = as.factor(velH)) +
  geom_linN2 -e() +
  facet_wrap(.~Condition) +
  xlab("Trial") +
  ylab("Abs. Step Size (m/s)") +
  ggtitle("Step sizes per trial for each pest") +
  theme_cowplot(12) +
  coord_cartesian(ylim = c(0,1.3))
ggsave("StepSizeBjorn.jpg", w = 10, h = 10)

ggplot(a[a$id == "Laurence",], aes(PestNr,Step_Size_OnTrial), col = as.factor(velH)) +
  geom_line() +
  facet_wrap(.~Condition) +
  xlab("Trial") +
  ylab("abs. Step Size (m/s)") +
  ggtitle("Step sizes per trial for each pest") +
  theme_cowplot(12) +
  coord_cartesian(ylim = c(0,1.3))
ggsave("StepSizeLaurence.jpg", w = 10, h = 10)

ggplot(a[a$id == "Bjorn",], aes(PestNr,Step_Size_OnTrial), col = as.factor(velH)) +
  geom_line() +
  facet_wrap(.~Condition) +
  xlab("Trial") +
  ylab("Abs. Step Size (m/s)") +
  ggtitle("Step sizes per trial for each pest") +
  theme_cowplot(12) +
  coord_cartesian(ylim = c(0,0.3))
ggsave("StepSizeBjornUpClose.jpg", w = 10, h = 10)

ggplot(a[a$id == "Bjorn2",], aes(PestNr,Step_Size_OnTrial), col = as.factor(velH)) +
  geom_line() +
  facet_wrap(.~Condition) +
  xlab("Trial") +
  ylab("abs. Step Size (m/s)") +
  ggtitle("Step sizes per trial for each pest") +
  theme_cowplot(12) +
  coord_cartesian(ylim = c(0,0.3))
ggsave("StepSizeLaurenceUpClose.jpg", w = 10, h = 10)

plot(lala) +
  xlab("Difference between Velocity(Pest) and Velocity(Target)") +
  ylab("Probability of Pest larger") +
  theme_cowplot(12) +
  
ggsave("PsychometricFunctionsBjorn.jpg", w = 10, h = 10)

plot(lala2) +
  xlab("Difference between Velocity(Pest) and Velocity(Target)") +
  ylab("Probability of Pest larger") +
  theme_cowplot(12)
  
ggsave("PsychometricFunctionsLaurence.jpg", w = 10, h = 10)

plot(lala3) +
  xlab("Difference between Velocity(Pest) and Velocity(Target)") +
  ylab("Probability of Pest larger") +
  theme_cowplot(12) +
  facet_grid(Direction~velH_Absolut)
ggsave("PsychometricFunctionsFittedSigmoids.jpg", w = 10, h = 10)

Hoho = lala$parcomparisons
Hoho[Hoho$velH_Subject != Hoho$velH_Subject2 & Hoho$velH == Hoho$velH2,]

Hoho2 = lala2$parcomparisons
Hoho2[Hoho2$velH_Subject != Hoho2$velH_Subject2 & Hoho2$velH == Hoho2$velH2,]

Hoho3 = lala3$parcomparisons
Hoho3[Hoho3$Congruent != Hoho3$Congruent2 & Hoho3$velH == Hoho3$velH2,]

a = a %>%
  group_by(id,Congruent,velH,Difference) %>%
  mutate(Yes = sum(Pest_Bigger==1),
         Total = length(velH_Subject))

Data_GLM = 
  select(a,c(id,Congruent,velH,Difference,Yes,Total,velH_Subject)) %>%
  distinct()

Data_GLM = Data_GLM %>%
  mutate(
    Static = case_when(
      velH_Subject == 0 ~ 1,
      velH_Subject != 0 ~ 0
    )
  )

ggplot(a[a$id %in% c("Meaghan", "Bjorn2", "Laurence2", "John", "Abi", "Bob"),], aes ( x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(id~velH)


mod1 = glmer(cbind(Yes, Total - Yes) ~ Congruent + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("Meaghan", "Bjorn2", "Abi", "John", "Bob"),])
mod2 = glmer(cbind(Yes, Total - Yes) ~ (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("Meaghan", "Bjorn2", "Abi", "John", "Bob"),])

mod3 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("Meaghan", "Bjorn2", "Abi", "John", "Bob"),])
mod4 = glmer(cbind(Yes, Total - Yes) ~ Congruent + Difference + (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("Meaghan", "Bjorn2", "Abi", "John", "Bob"),])

summary(mod1)
coef(mod1)
anova(mod1,mod2)
summary(mod3)
anova(mod4,mod3)

newdfr = expand.grid(Difference = seq(from = -6, to = 2, by = 0.5),
                     Congruent = c("1no motion", "congruent", "incongruent"),
                     id =  c("Meaghan", "Bjorn2", "Abi", "John", "Bob", "Laurence2"),
                     velH = c(-8,-6.6,6.6,8))

newdfr$response = predict(mod3,type = "response", newdata = newdfr)
ggplot(newdfr,aes(x=Difference, y=response, col = Congruent)) +
  geom_line(size=1) +
  facet_grid(id~velH)

lala10 = quickpsy(a[a$id %in% c("Meaghan", "Bjorn2", "Laurence2", "John", "Abi", "Bob"),], Difference, Pest_Bigger, grouping = .(id,Congruent,velH))
PSEsJNDs = data.frame(lala10$par)
PSEsJNDs = PSEsJNDs %>%
  mutate(Congruent2 =
           case_when(
               Congruent == "incongruent" ~ "1incongruent",
               Congruent =="congruent" ~ "3congruent",
               Congruent =="1no motion" ~ "2no motion"
             )
          )

QuickPsyCurves = lala10$curves

ggplot(QuickPsyCurves, aes(x, y, color = Congruent)) +
         geom_line() +
         coord_cartesian(xlim = c(-5,1)) +
         facet_grid(id~velH)
         
       
ggplot(PSEsJNDs[PSEsJNDs$parn == "p1",], aes(Congruent2,par, fill = Congruent2)) +
  geom_bar(stat="identity",color="black") +
  facet_grid(id~velH) +
  ylab(label = "PSEs (m/s)") +
  xlab(label = "") +
  ggtitle(label = "PSE") +
  scale_x_discrete(labels = c('incongruent','no motion','congruent')) +
  theme(legend.position = "none")

ggplot(PSEsJNDs[PSEsJNDs$parn == "p2",], aes(Congruent2,par, fill = Congruent2)) +
  geom_bar(stat="identity",color="black") +
  facet_grid(id~velH) +
  ylab(label = "SD (m/s)") +
  xlab(label = "") +
  ggtitle(label = "Thresholds") +
  scale_x_discrete(labels = c('incongruent','no motion','congruent')) +
  theme(legend.position = "none") 

PSEsJNDsWithLaurence = PSEsJNDs

PSEsJNDs = PSEsJNDs %>%
  filter(id != "Laurence2") %>%
  group_by(Congruent, parn) %>%
  mutate(mean = mean(par),
         SD = sd(par),
         n = length(par),
         SE = SD/(n)^0.5)

ggplot(PSEsJNDs[PSEsJNDs$parn == "p1" & PSEsJNDs$velH == 6.6 & PSEsJNDs$id == "Meaghan",], aes(Congruent2,mean, fill = Congruent2)) +
  geom_bar(stat="identity",color="black") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE),
                size=.3,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  ylab(label = "PSEs (m/s)") +
  xlab(label = "") +
  ggtitle(label = "PSE") +
  scale_x_discrete(labels = c('incongruent','no motion','congruent')) +
  theme(legend.position = "none")

ggplot(PSEsJNDs[PSEsJNDs$parn == "p2" & PSEsJNDs$velH == 6.6 & PSEsJNDs$id == "Meaghan",], aes(Congruent2,mean, fill = Congruent2)) +
  geom_bar(stat="identity",color="black") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE),
                size=.3,    # Thinner lines
                width=.3,
                position=position_dodge(.9)) +
  ylab(label = "SD (m/s)") +
  xlab(label = "") +
  ggtitle(label = "Thresholds") +
  scale_x_discrete(labels = c('incongruent','no motion','congruent')) +
  theme(legend.position = "none") 


timeseries = seq(0,0.5,0.01)
pnorm(timeseries, mean = 0.25, sd = 0.08)
dnorm(timeseries, mean = 0.25, sd = 0.08)
plot(timeseries,pnorm(timeseries, mean = 0.25, sd = 0.08))
plot(timeseries,dnorm(timeseries, mean = 0.25, sd = 0.08))
