###Pull the whole repository. The code should work as long as the structure of the repository is not altered.
require(dplyr)
require(lme4)
require(ggplot2)
require(quickpsy)

Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)}

setwd(Where_Am_I())

source("Utilities/parabolic.r")

b <- read.table(header=T,"PilotData/Discarded/Pilots02_2D.txt")
c <- read.table(header=T,"PilotData/Discarded/Pilots01_2D.txt")
d <- read.table(header=T,"PilotData/Pilots01_3D.txt")
e <- read.table(header=T,"PilotData/pilots03.txt")
f <- read.table(header=T,"PilotData/Pilots02_3D.txt")
g <- read.table(header=T,"PilotData/Discarded/Pilots07.txt")
h <- read.table(header=T,"PilotData/pilots04.txt")
i <- read.table(header=T,"PilotData/Pilots06.txt")
j <- read.table(header=T,"PilotData/Pilots05.txt")


b$id = "s02_2D"
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