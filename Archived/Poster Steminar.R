###Pull the whole repository and paste the path to the the local GitHub repository here:
require(dplyr)
require(lme4)
require(ggplot2)
require(quickpsy)
require(lemon)

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

#####Plot data raw data for all subjects (tested in 3D)
ggplot(a[a$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06", "s07"),], 
       aes ( x = Difference, y = Pest_Bigger, col = as.factor(Congruent))) +
  binomial_smooth() +
  facet_grid(id~velH)
ggsave("PlotsPilotData.jpg", w=10, h=10)

mod1 = glmer(cbind(Yes, Total - Yes) ~ Congruent + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
mod2 = glmer(cbind(Yes, Total - Yes) ~ (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
summary(mod1)
anova(mod1,mod2)

mod3 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Difference | id) + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
mod4 = glmer(cbind(Yes, Total - Yes) ~ Congruent + Difference + (Difference | id)  + (Difference | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s01_3D", "s02_3D", "s03", "s04", "s05", "s06"),])
summary(mod3)
anova(mod4,mod3)


#############Plot SDs and PSEs
lala10 = quickpsy(a[a$id %in% c("Meaghan", "Bjorn2", "Laurence2", "John", "Abi", "Bob"),], Difference, Pest_Bigger, grouping = .(id,Congruent,velH))
PSEsJNDs = data.frame(lala10$par)

PSEsJNDs = PSEsJNDs %>%
  mutate(Congruent2 =
           case_when(
             Congruent == "incongruent" ~ "1incongruent",
             Congruent =="congruent" ~ "3congruent",
             Congruent =="1no motion" ~ "2no motion"
           )
  ) %>%
  group_by(id,Congruent2,parn) %>%
  mutate(ParametersPerIDAndCongruent = mean(par))

Parameters =  PSEsJNDs %>%
  group_by(id,Congruent2,parn) %>%
  filter(id != "s02_3D") %>%
  slice(1)

PSEsJNDs_Means = PSEsJNDs %>%
  group_by(Congruent,parn) %>%
  filter(id != "s02_3D") %>%
  mutate(ParametersPerIDAndCongruent = mean(par)) %>%
  slice(1)

PSEsJNDs_Means$id = "Mean"

Parameters = rbind(Parameters,PSEsJNDs_Means)

fgh = ggplot(Parameters[Parameters$parn == "p1",], aes(id,ParametersPerIDAndCongruent, fill = Congruent2)) +
  geom_bar(stat="identity",color="black",position = "dodge") +
  ylab(label = "PSE (m/s)") +
  xlab(label = "") +
  ggtitle(label = "Biases") +
  scale_x_discrete(labels = c("Mean","s01", "s03", "s04", "s05", "s06")) +
  scale_fill_manual(labels = c("Incongruent", "No Motion", "Congruent"),
                    values = c(BlauUB, Yellow, Red)) +
  theme(legend.position = "",
        legend.title = element_blank())

ghi = ggplot(Parameters[Parameters$parn == "p2",], aes(id,ParametersPerIDAndCongruent, fill = Congruent2)) +
  geom_bar(stat="identity",color="black",position = "dodge") +
  ylab(label = "SD (m/s)") +
  xlab(label = "") +
  ggtitle(label = "Sensitivity") +
  scale_x_discrete(labels = c("Mean","s01", "s03", "s04", "s05", "s06")) +
  scale_fill_manual(labels = c("Incongruent", "No Motion", "Congruent"),
                    values = c(BlauUB, Yellow, Red)) +
  theme(legend.title = element_blank())

legend <- g_legend(ghi)

ghi = ghi + theme(legend.position = "")

ParametersPlot = plot_grid(fgh,ghi,nrow = 2)
plot_grid(ParametersPlot,legend,rel_widths = c(1,0.2))

ggsave("Parameters.jpg", w=8, h=6)


###############Psychometric Functions
mod4 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Congruent | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s06"),])

newdfr2 = expand.grid(Difference = seq(from = -6, to = 2, by = 0.5),
                      Congruent = c("2no motion", "3congruent", "1incongruent"),
                      id = c("s06"),
                      velH = c(-8,-6.6,6.6,8))

newdfr2$response = predict(mod4,type = "response", newdata = newdfr2)
plot1 = ggplot(newdfr2,aes(x=Difference, y=response, col = Congruent)) +
  geom_line(size=1) +
  facet_grid(id~velH) +
  geom_hline(yintercept = 0.5, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  scale_color_manual(name = "",
                     values = c(BlauUB,Yellow,Red),
                     labels = c("No Motion","Congruent", "Incongruent")) +
  xlab("Difference (m/s)") +
  ylab("Response Probability") +
  theme(legend.position = "")

mod4 = glmer(cbind(Yes, Total - Yes) ~ Congruent*Difference + (Congruent | velH),
             family = binomial(link = "probit"), 
             data = Data_GLM[Data_GLM$id %in% c("s03"),])

newdfr2 = expand.grid(Difference = seq(from = -6, to = 2, by = 0.5),
                      Congruent = c("2no motion", "3congruent", "1incongruent"),
                      id = c("s03"),
                      velH = c(-8,-6.6,6.6,8))

newdfr2$response = predict(mod4,type = "response", newdata = newdfr2)
plot2 = ggplot(newdfr2,aes(x=Difference, y=response, col = Congruent)) +
  geom_line(size=1) +
  facet_grid(id~velH) +
  geom_hline(yintercept = 0.5, color = "grey") +
  geom_vline(xintercept = 0, color = "grey") +
  scale_color_manual(name = "",
                     values = c(BlauUB,Yellow,Red),
                     labels = c("No Motion","Congruent", "Incongruent")) +
  xlab("Difference (m/s)") +
  ylab("Response Probability") +
  theme(legend.position = "")

plot3 = plot_grid(plot1,plot2, nrow = 2)
plot_grid(plot3,legend,rel_widths = c(1,0.2))
ggsave("PsychometricFunctions.jpg", w=8, h=6)


###############Psychometric example functions
######

StimulusStrength = seq(-4,4,0.01)
Regular = pnorm(StimulusStrength,0,1)
Bias = pnorm(StimulusStrength,1,1)
Sensitivity = pnorm(StimulusStrength,0,2)
PsychometricFunctions = data.frame(Value = c(Regular, Bias, Sensitivity), 
                                   StimulusStrength = rep(StimulusStrength,3), 
                                   Which = c(rep("Base",length(StimulusStrength)),
                                           rep("Bias",length(StimulusStrength)),
                                           rep("Sensitivity Decrease",length(StimulusStrength))))

ggplot(PsychometricFunctions,aes(StimulusStrength,Value,color = Which)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("black", Turquoise, LightRed)) +
  theme(legend.title = element_blank()) +
  ylab("Probability") +
  xlab("Stimulus Strength") +
  scale_y_continuous(position = "right") +
  theme(legend.position = "left")
ggsave("PsychometricFunctionExample.jpg", w=8, h=4)
  
