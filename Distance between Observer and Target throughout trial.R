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

#######################transform physical speeds into observer centric speeds (= radial velocity)
TimeSeries = seq(0,0.5,0.01)
vx = c(-8,-6.6,6.6,8)
ObserverMotion = c(-0.25,0,0.25)
Conversion = expand.grid(TimeSeries,vx,ObserverMotion)
colnames(Conversion) = c("TimeSeries", "vx", "ObserverMotion")

Conversion = Conversion %>%
  mutate(ObserverInSpace = case_when(
    ObserverMotion == 0 ~ 0,
    ObserverMotion == 0.25 ~ -0.5 + pnorm(TimeSeries,abs(ObserverMotion),0.08)*2,
    ObserverMotion == -0.25 ~ 0.5 -pnorm(TimeSeries,abs(ObserverMotion),0.08)*2),
    TargetInSpace = -(- ObserverMotion*4 + vx * 0.5)/2 + vx*TimeSeries,
    Distance = ((ObserverInSpace-TargetInSpace)^2 + 8^2)^0.5,
    Angle = RadiansToDegree(atan((ObserverInSpace-TargetInSpace)/8)),
    AngleVelocity = abs(Angle - lag(Angle, n = 1))/0.01) %>%
  #  filter(AngleVelocity < 100 & AngleVelocity > 0 ) %>%
  mutate(Congruent = case_when(
    ObserverMotion*vx == 0 ~ "Static",
    ObserverMotion*vx < 0 ~ "Incongruent",
    ObserverMotion*vx > 0 ~ "Congruent")
  )



ggplot(Conversion, aes(TimeSeries,Distance,col = as.factor(Congruent))) +
  geom_point(size=3)
