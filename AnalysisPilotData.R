###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
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

b <- read.table(header=T,"PilotData/Discarded/Pilots02_2D.txt") #this loads a text file into R
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
d$id = "s01_3D"
e$id = "s03"
b$Start_Above = 1
c$Start_Above = 1
f$id = "s02_3D"
g$id = "s07"
h$id = "s04"
i$id = "s05"
j$id = "s06"
k$id = "s08"

a = rbind(b,c,d,e,f,g,h,i,j,k) #combine all data sets into one dataframe

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
  select(a,c(id,Congruent,velH,Difference,Yes,Total,velH_Subject,SelfMotionPresent)) %>%
  distinct()

Data_GLM = Data_GLM %>%
  mutate(
    Static = case_when(
      velH_Subject == 0 ~ 1,
      velH_Subject != 0 ~ 0
    )
  )

#####Plot data raw data for all subjects (tested in 3D)
ggplot(a[a$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06", "s07"),], 
       aes ( x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(id~velH)
ggsave("PlotsPilotData.jpg", w=10, h=10)

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