######you will have to install each of these packages once with the command install.packages("dplyr") and so on
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(quickpsy) #package to fit psychometric functions
require(cowplot) #design for data visualiation
theme_set(theme_cowplot()) #sets design parameters for data visualization

####the following function gets the current path of this script 
Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}
setwd(Where_Am_I()) #set path of this script as working directory

source("Utilities/parabolic.r") #load a bunch of custom functions from the file "parabolic.r" in the folder "Utilities"

b <- read.table(header=T,"PilotData/Discarded/Pilots02_2D.txt") #this loads a text file into R, in this case all data for one participant
c <- read.table(header=T,"PilotData/Discarded/Pilots01_2D.txt")
d <- read.table(header=T,"PilotData/Pilots01_3D.txt")
e <- read.table(header=T,"PilotData/pilots03.txt")
f <- read.table(header=T,"PilotData/Pilots02_3D.txt")
g <- read.table(header=T,"PilotData/Discarded/Pilots07.txt")
h <- read.table(header=T,"PilotData/pilots04.txt")
i <- read.table(header=T,"PilotData/Pilots06.txt")
j <- read.table(header=T,"PilotData/Pilots05.txt")
k <- read.table(header=T,"PilotData/Pilots08.txt")


b$id = "s02_2D" #add IDs for each data file we loaded
c$id = "s01_2D"
d$id = "s01"
e$id = "s03"
b$Start_Above = 1
c$Start_Above = 1
f$id = "s02"
g$id = "s07"
h$id = "s04"
i$id = "s05"
j$id = "s06"
k$id = "s08"

a = rbind(b,c,d,e,f,g,h,i,j,k) #combine the datasets from the different participants into one dataframe

###a bunch of transformations necessary for this dataset
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
    Difference_Percent = Difference/velH,
    SelfMotionPresent = case_when(
      velH_Subject == 0 ~ 0,
      velH_Subject != 0 ~ 1
    )
  ) %>%
  filter(abs(velH_Pest) < abs(velH)*1.5)

a = a %>%
  group_by(id,Congruent,velH,Difference) %>%
  mutate(Yes = sum(Pest_Bigger==1),
         Total = length(velH_Subject))

Data_GLM = 
  select(a,c(id,Congruent,velH,Difference,Yes,Total,velH_Subject,SelfMotionPresent,Pest_Bigger)) %>%
  distinct()

Data_GLM = Data_GLM %>%
  mutate(
    velH_Factor = case_when(
      velH == -8 ~ "-8 m/s",
      velH == -6.6 ~ "-6.6 m/s",
      velH == 6.6 ~ "6.6 m/s",
      velH == 8 ~ "8 m/s")
  )
###


###use the quickpsy package to fit the psychometric functions. You can see the arguments of the quickpsy function here:

FittedPsychometricFunctions_WithOutliers = quickpsy(Data_GLM,Difference,Pest_Bigger, #these are the basic values: name of the dataframe, name of column that indicates the stimulus strength, and name of the column that  contains the responses
                                                    grouping = .(Congruent,id,velH_Factor), #these are the different conditions
                                                    bootstrap = "none")$par


plot(FittedPsychometricFunctions_WithOutliers) #a quick way to plot all psychometric functions fitted with quickpsy

#you can look at the PSEs and JNDs here; p1 coresponds to PSEs and p2 corresponds to JNDs:
FittedPsychometricFunctions_WithOutliers



#everything below this here is probably not very important for you right now:)
##################

Data_GLM$PSEsFirstPass = 1
for (i in (1:length(Data_GLM$id))){
  print(i)
  Data_GLM$PSEsFirstPass[i] = FittedPsychometricFunctions_WithOutliers$par[
                              FittedPsychometricFunctions_WithOutliers$id == Data_GLM$id[i] & 
                              FittedPsychometricFunctions_WithOutliers$Congruent == Data_GLM$Congruent[i] &
                              FittedPsychometricFunctions_WithOutliers$velH_Factor == Data_GLM$velH_Factor[i] &
                              FittedPsychometricFunctions_WithOutliers$parn == "p1"]
}

#Outlier Analysis
Data_GLM = Data_GLM %>%
  group_by(id,Congruent,velH) %>%
  mutate(CriterionUpper = PSEsFirstPass+2.5,
         CriterionLower = PSEsFirstPass-2.5
         ) %>%
  filter(Difference > CriterionLower & Difference < CriterionUpper)

Data_GLM2 = Data_GLM[Data_GLM$id %in% c("s01","s02", "s03", "s04", "s05", "s06", "s08"),]
Data_GLM2$id = "Across All"
FittedPsychometricFunctions_WithoutOutliers = quickpsy(rbind(Data_GLM[Data_GLM$id %in% 
                                                                        c("s01","s02", "s03", "s04", "s05", "s06", "s08"),],
                                                             Data_GLM2),
                                 Difference,Pest_Bigger,grouping = .(Congruent,velH,id), bootstrap = "none")

####plot full psychometric functions
plot(FittedPsychometricFunctions_WithoutOutliers) +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB,LightBlauUB),
                     labels = c("No Motion","Same Direction","Opposite Directions")) +
  scale_x_continuous("Difference between Comparison and Test (m/s)") +
  scale_y_continuous("Probability to choose Test",breaks = c(0.2,0.5,0.8)) +
  geom_vline(linetype = 2, xintercept = 0, color = "grey") +
  geom_hline(linetype = 2, yintercept = 0.5, color = "grey") +
  theme(legend.position = "top") +
  ggtitle("Full psychometric functions")
ggsave("Poster VOR/All Psychometric Functions.jpg", w = 7, h = 10)



ggplot(FittedPsychometricFunctions_WithoutOutliers$curves, aes(x,y,color=Congruent)) +
  geom_line(size = 1) +
  geom_segment(aes(x = par, y = 0, xend = par, yend = 0.5),
               data = FittedPsychometricFunctions_WithoutOutliers$par %>% filter(parn == "p1" ),
               size = 1) +
  facet_grid(id~velH) + 
  geom_point(data = rbind(Data_GLM2,Data_GLM) %>% filter(id %in% c("Across All", "s01","s02", "s03", "s04", "s05", "s06", "s08")), aes(Difference,Yes/Total,color = Congruent), alpha = 0.2, size = 0.7) +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB,LightBlauUB),
                     labels = c("No Motion","Same Direction","Opposite Directions")) +
  scale_x_continuous("Difference between Comparison and Test (m/s)") +
  scale_y_continuous("Probability to choose Test",breaks = c(0.2,0.5,0.8)) +
  geom_vline(linetype = 2, xintercept = 0, color = "grey") +
  geom_hline(linetype = 2, yintercept = 0.5, color = "grey") +
  theme(legend.position = "top") +
  ggtitle("Full psychometric functions")
ggsave("Poster VOR/All Psychometric Functions.jpg", w = 7, h = 10)

####get dataframe with means
Parameters_Mean = rbind(FittedPsychometricFunctions_WithoutOutliers$par %>%
                          filter(parn == "p1"),
                        FittedPsychometricFunctions_WithoutOutliers$par %>% 
                     group_by(Congruent,parn) %>%
                     filter(parn == "p1") %>%
                     mutate(par = median(par),
                            id = "Mean") %>%
                     slice(1)
                   )

Parameters_SD = rbind(FittedPsychometricFunctions_WithoutOutliers$par %>%
#                        mutate(SelfMotionPresent = case_when(
#                          Congruent == "1no motion"  ~ "No Selfmotion",
#                          TRUE ~ "Selfmotion")) %>%
                        filter(parn == "p2"),
                      FittedPsychometricFunctions_WithoutOutliers$par %>%
#                         mutate(SelfMotionPresent = case_when(
#                            Congruent == "1no motion"  ~ "No Selfmotion",
#                            TRUE ~ "Selfmotion")) %>%
                          group_by(Congruent,parn) %>%
                          filter(parn == "p2") %>%
                          mutate(par = median(par),
                                 id = "Mean") %>%
                          slice(1)
)

ggplot(Parameters_SD %>% filter(id != "Mean"),aes(Congruent,par, color = Congruent, shape = as.factor(velH))) +
  geom_point(alpha = 0.2, size = 3) +
  geom_point(data = Parameters_SD %>% filter(id == "Mean"), aes(Congruent,par), size = 8) +
  xlab("") +
  ylab("SD (m/s)") +
  coord_cartesian(ylim = c(0,4.2)) +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB,LightBlauUB)) +
  scale_x_discrete(labels= c("No Motion", "Same\nDirection", "Opposite\nDirections")) +
  theme(legend.position = "") +
  geom_hline(yintercept = (Parameters_SD %>% 
                             filter(id == "Mean" & Congruent == "1no motion") %>% 
                             group_by(Congruent))$par, linetype = 3, size = 1) +
  ggtitle("Precision") +
  geom_segment(aes(x = 1, y = 3.7, xend = 2, yend = 3.7), size = 1, color = "black") +
  annotate("text",x = 1.5,y = 3.85, label = "n.s.") +
  geom_segment(aes(x = 1, y = 4.1, xend = 3, yend = 4.1), size = 1, color = "black") +
  annotate("text",x = 2,y = 4.25, label = "n.s.")
ggsave("Poster VSS/SDs Poster VSS.jpg",w=5,h=5)

ggplot(Parameters_Mean %>% filter(id != "Mean"),aes(Congruent,par, color = Congruent, shape = as.factor(velH))) +
  geom_point(alpha = 0.2, size = 3) +
  geom_point(data = Parameters_Mean %>% filter(id == "Mean"), aes(Congruent,par), size = 8) +
  xlab("") +
  ylab("PSE (m/s)") +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB,LightBlauUB)) +
  scale_x_discrete(labels= c("No Motion", "Same\nDirection", "Opposite\nDirections")) +
  theme(legend.position = "") +
  geom_hline(yintercept = (Parameters_Mean %>% 
                             filter(id == "Mean" & Congruent == "1no motion") %>% 
                             group_by(Congruent))$par, linetype = 3, size = 1) +
  ggtitle("Accuracy") +
  geom_segment(aes(x = 1, y = 3.9, xend = 2, yend = 3.9), size = 1, color = "black") +
  geom_segment(aes(x = 1, y = 4.4, xend = 3, yend = 4.4), size = 1, color = "black") +
  geom_segment(aes(x = 1.5, 
                   y = (Parameters_Mean %>% 
                                 filter(id == "Mean" & Congruent == "1no motion") %>% 
                                 group_by(Congruent))$par - 1, 
                   xend = 2.5, 
                   yend = (Parameters_Mean %>% 
                         filter(id == "Mean" & Congruent == "1no motion") %>% 
                         group_by(Congruent))$par - 1), 
                   size = 1,
                   color = "black",
                   linetype = 5) +
  geom_segment(aes(x = 2.5, 
                   y = (Parameters_Mean %>% 
                          filter(id == "Mean" & Congruent == "1no motion") %>% 
                          group_by(Congruent))$par + 1, 
                   xend = 3.5, 
                   yend = (Parameters_Mean %>% 
                             filter(id == "Mean" & Congruent == "1no motion") %>% 
                             group_by(Congruent))$par + 1), 
                   size = 1,
                   color = "black",
                   linetype = 5) +
  geom_segment(aes(x = 1, y = 4.4, xend = 3, yend = 4.4), size = 1, color = "black") +
  annotate("text",x = 1.5,y = 4.2, label = "n.s.") +
  annotate("text",x = 2,y = 4.6, label = "*")
ggsave("Poster VSS/PSEs Poster VSS.jpg",w=5,h=5)



##########Statistical analysis
mod1 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + 
               (Congruent + Difference | id) + (Congruent + Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01", "s02", "s03", "s04", "s05", "s06", "s08"),],
             glmerControl(optimizer = "bobyqa"),
             nAGQ = 0)
mod2 = glmer(cbind(Yes, Total - Yes) ~ Congruent + Difference + 
               (Congruent + Difference | id) + (Congruent + Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01", "s02", "s03", "s04", "s05", "s06", "s08"),],
             glmerControl(optimizer = "bobyqa"),
             nAGQ = 0)
anova(mod1,mod2)
summary(mod1)

mod3 = glmer(cbind(Yes, Total - Yes) ~ Congruent + (Congruent + Difference | id) + (Congruent + Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01", "s02", "s03", "s04", "s05", "s06", "s08"),],
             glmerControl(optimizer = "bobyqa"),
             nAGQ = 0)
mod4 = glmer(cbind(Yes, Total - Yes) ~ (Congruent + Difference | id)  + (Congruent + Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01", "s02", "s03", "s04", "s05", "s06", "s08"),],
             glmerControl(optimizer = "bobyqa"),
             nAGQ = 0)
anova(mod3,mod4)
summary(mod3)



############Predictions
source("Utilities/PowerFunctions.r")

Predictions = expand.grid(Difference = rep(seq(-3,3,0.01),3))
Predictions$Motion = c(rep("No Motion",length(Predictions$Difference)/3),
                       rep("Same Direction",length(Predictions$Difference)/3),
                       rep("Opposite Direction",length(Predictions$Difference)/3))
Predictions = Predictions %>%
  mutate(Response = case_when(
    Motion == "No Motion" ~ pnorm(Difference,0,1),
    Motion == "Same Direction" ~ pnorm(Difference,0.4,1.3),
    Motion == "Opposite Direction" ~ pnorm(Difference,-0.4,1.3)))

ggplot(Predictions, aes(Difference,Response,color = Motion)) +
  geom_line(size = 2) +
  scale_color_manual(name = "",
                     values = c(Red,BlauUB,LightBlauUB),
                     labels = c("No Motion","Same\nDirection","Opposite\nDirection")) +
  xlab("Difference between Comparison and Test (m/s)") +
  ylab("Probability Test Faster") + 
  theme(legend.position = "",
        axis.title.x = element_text(size = rel(0.9))) +
  ggtitle("Predicted Psychometric Functions") +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey") +
  annotate("text", x = 2.2, y = 0.6, label = "No Motion\n(steeper slope,\nnot shifted)", color = Red, size = 6) +
  annotate("text", x = 1.3, y = 0.2, label = "Opposite Directions\n(shifted right)", color = LightBlauUB, size = 6) +
  annotate("text", x = -1, y = 0.8, label = "Same Direction\n(shifted left)", color = BlauUB, size = 6)
ggsave("Poster VSS/Predictions VOR.jpg",w=5,h=7)
