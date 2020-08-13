###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr) #package for data structure manipulation
require(lme4) #package for statistical analysis 
require(ggplot2) #package for data visualization
require(quickpsy) #package to fit psychometric functions
require(cowplot) #design for data visualiation
theme_set(theme_cowplot()) #sets design parameters for data visualization


###velH_Subject 0.5/-0.5 = wall moves
###velH_Subject 0.25/-0.25 = participant moves, but wall is blank

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

b <- read.table(header=T,"Data/Sergio/S01 MainExperiment.txt") #this loads a text file into R

b$id = "S01" #add IDs for each data file we loaded

a = b #combine all data sets into one dataframe

a = a %>% ####several transformations necessary for this data analysis
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
      velH*velH_Subject < 0 & velH_Subject != 2 ~ "incongruent",
      velH*velH_Subject > 0 & velH_Subject != 2 ~ "congruent",
      velH_Subject %in% c(0,2) ~ "1no motion"
    ),
    Difference_Percent = Difference/velH,
    SelfMotionPresent = case_when(
      velH_Subject %in% c(0,2) ~ 0,
      TRUE ~ 1
    )
  ) %>%
  filter(abs(velH_Pest) < abs(velH)*1.5)

a = a %>%
  group_by(id,Congruent,velH,Difference) %>%
  mutate(Yes = sum(Pest_Bigger==1),
         Total = length(velH_Subject))
a$Condition
a = a %>%
  mutate(
    Static = case_when(
      velH_Subject %in% c(0,2) ~ "Static",
      velH_Subject != 0 ~ "Movement"
    ),
    Condition = case_when(
      velH_Subject %in% c(0.5,-0.5) ~ "WallMoves",
      velH_Subject %in% c(0.25,-0.25) ~ "BlankWall",
      velH_Subject %in% c(1,-1) ~ "RegularCondition",
      velH_Subject == 2 ~ "BlankWallStatic",
      velH_Subject == 0 ~ "RegularWallStatic"
    )
  )

Data_GLM = 
  select(a,c(id,Congruent,velH,Difference,Yes,Total,velH_Subject,SelfMotionPresent,Static,Condition)) %>%
  distinct()


#####Plot data raw data for all subjects (tested in 3D)
ggplot(a %>% filter(Condition %in% c("WallMoves","RegularWallStatic")), 
       aes (x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(.~velH)
ggsave("PlotsPilotData.jpg", w=10, h=10)

ggplot(a %>% filter(Condition %in% c("RegularCondition","RegularWallStatic")), 
       aes (x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(.~velH)
ggplot(a %>% filter(Condition %in% c("BlankWall","BlankWallStatic")), 
       aes (x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(.~velH)

mod1 = glmer(cbind(Yes, Total - Yes) ~ as.factor(SelfMotionPresent)*Difference + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
mod2 = glmer(cbind(Yes, Total - Yes) ~ Difference + (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
anova(mod1,mod2)
summary(mod1)

mod3 = glmer(cbind(Yes, Total - Yes) ~ Congruent + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
mod4 = glmer(cbind(Yes, Total - Yes) ~ (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
anova(mod3,mod4)
summary(mod3)


c <- read.table(header=T,"Data/Sergio/S01 SelfMotion.txt") #this loads a text file into R
c = c %>%
  group_by(subject,velH_Subject) %>% 
  mutate(Mean_Judgement = mean(Judgement))
