###Plots 
library(ggplot2)
ggplot(MasterData, aes(x=SeverityClass, y= LitterDepth)) +
  geom_boxplot(fill="steelblue")


ggplot(MasterData, aes(x=SeverityTrend, y= UnderstoryCover)) +
  geom_boxplot(fill="steelblue")
###Interesting one - Same high is much higher than all others

ggplot(MasterData, aes(x=SeverityTrend, y= CanopyCover)) +
  geom_boxplot(fill="steelblue")
## huge influence of high/high here

ggplot(MasterData, aes(x=SeverityTrend, y= ConSeedling)) +
  geom_boxplot(fill="steelblue")
#slightly different trends in health between oak and con health

ggplot(MasterData, aes(x=SeverityTrend, y= UndOakCover)) +
  geom_boxplot(fill="steelblue")
