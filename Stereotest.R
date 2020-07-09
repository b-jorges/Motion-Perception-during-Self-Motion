#see also https://github.com/b-jorges/Motion-Perception-during-Self-Motion/blob/master/Writeups/Stereotest.docx


#15m distance to target
#0.065m interocular distance assumed
#offets between 0.05 and 0m shown


VisualAngle_NoOffset = RadiansToDegree(DegreeToRadians(90)- atan(15/(2+0.065/2)) - atan((2-0.065/2)/15))

Offsets = seq(0.025,0.00,-0.00125)
Stereo_Differences = c()

for (Offset in Offsets){

  VisualAngle_Offset = DegreeToRadians(180) - atan((2-0.065/2-Offset/2)/15) - DegreeToRadians(90) - atan(15/(2+0.065/2+Offset/2))
  
  Stereo_Differences = c(Stereo_Differences,RadiansToDegree(VisualAngle_Offset) - VisualAngle_NoOffset)
  
}

Stereo_Differences_ArcSec = Stereo_Differences*3600
Ratio = round(Offsets[1:20]/Stereo_Differences_ArcSec[1:20],5)

Stereo_Differences_ArcSec = Offsets/0.00007
Offsets = 0.00007*Stereo_Differences_ArcSec

VisualAngle_Offset = RadiansToDegree(DegreeToRadians(180) - atan((2-0.065/2-0.05/2)/15) - DegreeToRadians(90) - atan(15/(2+0.065/2+0.05)))

