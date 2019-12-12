###Pull the whole repository and paste the path to the the local GitHub repository here:
require(dplyr)
require(lme4)

Where_Am_I <- function(path=T){
  if (path == T){
    dirname(rstudioapi::getSourceEditorContext()$path)
  }
  else {
    rstudioapi::getSourceEditorContext()$path
  }
}

setwd(Where_Am_I())

b <- read.table(header=T,"PilotData/PilotLaurence.txt")
c <- read.table(header=T,"PilotData/PilotBjorn_2D.txt")
d <- read.table(header=T,"PilotData/PilotBjorn_3D.txt")
e <- read.table(header=T,"PilotData/PilotMeaghan.txt")
f <- read.table(header=T,"PilotData/PilotLaurence2.txt")
g <- read.table(header=T,"PilotData/PilotRachel.txt")
h <- read.table(header=T,"PilotData/PilotAbi.txt")
i <- read.table(header=T,"PilotData/PilotJohn.txt")
j <- read.table(header=T,"PilotData/PilotBob.txt")


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

#####Plot data raw data for all subjects included in analysis
ggplot(a[a$id %in% c("Meaghan", "Bjorn2", "John", "Abi", "Bob"),], aes ( x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(id~velH)
ggsave("PlotsPilotData.jpg", w=10, h=10)

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
