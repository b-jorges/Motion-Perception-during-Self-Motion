VisualAngle_NoOffset = RadiansToDegree(DegreeToRadians(90)- atan(15/(2+0.065/2)) - atan((2-0.065/2)/15))

Offsets = seq(0.05,0.00,-0.0025)
Stereo_Differences = c()

for (Offset in Offsets){

  VisualAngle_Offset = DegreeToRadians(180) - atan((2-0.065/2-Offset/2)/15) - DegreeToRadians(90) - atan(15/(2+0.065/2+Offset/2))
  
  Stereo_Differences = c(Stereo_Differences,RadiansToDegree(VisualAngle_Offset) - VisualAngle_NoOffset)
  
}

Stereo_Differences_ArcSec = Stereo_Differences*3600


VisualAngle_Offset = RadiansToDegree(DegreeToRadians(180) - atan((2-0.065/2-0.05/2)/15) - DegreeToRadians(90) - atan(15/(2+0.065/2+0.05)))

