#Factor Analysis of independent variables

#sample code from Katie
#MFA_fa <- fa(mes_all[c(23:27, 39:43)], nfactors=3)
#fa.diagram(MFA_fa)
#MFA_fa

getwd()
MasterData<- read.csv("MasterData.csv", header=TRUE)

installed.packages("psych")
library(psych)

library(GPArotation)


#Mixed Con Only 
MixedCon<- subset(MasterData, TrueERU=="Mixed Conifer")
MixedConIndependent<- MixedCon[,c(17:29, 30:32, 34:35, 50:76, 101,103,
                                  105, 107, 109, 111, 113, 115, 117,
                                  119, 121, 123)]
MixedConIndependent$NO3N<- as.numeric(MixedConIndependent$NO3N)
#NO3N has a DIV/0 error in Excel that's importing oddly and confusing R, so that's why you need to force it to be numeric here

#First attempt- 6 factors
MixedCon_cor<- cor(MixedConIndependent, use="pairwise.complete.obs")
MixedCon_FA1<- fa(MixedCon_cor,nfactors=6) 
fa.diagram(MixedCon_FA1,cex=2, rsize= .5, e.size =1)

#The missing values (NAs) in the data are a problem. There are ways to deal with missing data, but for soil categories there is likely just too much to work around. I'd subset plots with soil data (& other partial data) out & do analyses just on that subset
#This isn't running happily because there are too many variables in the factor analysis compared to the sample size....you'll need to condense into fewer variables
#you want plot spp IV values to use as independent variables


#fire(?) variables
#Ash[25], LitterDepth & DuffDepth [27:28], 10hr, 100hr, 1000hr, 10000hr, Cones [92:96]
Master_FA1<- fa(MasterData[,c(25, 27:28, 92:96)], nfactors=3) 
fa.diagram(Master_FA1)
#the line between MR1 & MR2 is telling you those factors have a 0.4 correlation, which might be higher than you want, 2 factors may be better or this might not be a good list of variables to use, but you get the idea



#nlme & gls
library(nlme) #for mixed models (use gls rather than lm to more easily change lines of code from mixed/not mixed models)
model1.gls <- gls(TreeSurvivorship ~ LitterDepth + Slope, data=MasterData, na.action=na.omit)
anova(model1.gls)
#LitterDepth p=0.0266, others NS

model1.lme <- lme(TreeSurvivorship ~ LitterDepth + Slope, random=~1|TrueERU, data=MasterData, na.action=na.omit)
anova(model1.lme)
#LitterDepth p=0.0292, others NS

anova(model1.lme, model1.gls)
#results here show that these two models are not significantly different; so specifying TrueERU as a random effect didn't matter here
#I'd consider keeping all your plot data together rather than seperating by ERU to see when/how it matters. You may want to split ERUs apart for some analyses/variables, but when you can run your whole dataset together you'll have more df

model2.gls <- gls(TreeSurvivorship ~ Ash + TopoPosition + TrueERU, data=MasterData, na.action=na.omit)
anova(model2.gls)
#Ash p=0.0297, others NS

model3.gls <- gls(OakSeedling ~ LitterDepth + TopoPosition + TrueERU, data=MasterData, na.action=na.omit)
anova(model3.gls)
#seedlings are often so variable they're tough to explain, but I think you'll be able to find some relationships with these oaks


#glm with con health and fire severity
model4.gls<- gls(ConHealth ~ BARBR + BighornRBR + LitterDepth, data=MasterData, na.action = na.omit)
anova(model4.gls)
# Bighorn RBR p value of .06, BArbr p value of .5

#glm Oak seedlings vs nutrients
model5.gls<- gls(OakSeedling~ DuffDepth + CanopyCover +NH4N + OrganicMatter + EEMT,
                 data = MasterData, na.action= na.omit )
anova(model5.gls)
#canopy cover p value of .01, others ns

#dead BA versus RBR
model6.gls<- gls(DeadBA ~ BARBR + BighornRBR, data=MasterData, na.action= na.omit)
anova(model6.gls)
#neither is significant 

#glm con seedlings
model7.gls<- gls(ConSeedling~ TreeSurvivorship + ConHealth + BARBR, data= MasterData,
                 na.action=na.omit)
anova(model7.gls)
#none are significant


model8.gls<- gls(TreeSurvivorship~ BARBR + BighornRBR, data=MasterData, na.action=na.omit)
anova(model8.gls)
#BARBR p value of 0.06

hist(MasterData$UnderstoryCover)
#Understory cover negative log distribution
hist(MasterData$pH)

#Understory cover
model9.gls<- gls(UnderstoryCover~ BighornRBR + BARBR+ LiveTreesHA, data=MasterData,
                 na.action= na.omit)
anova(model9.gls)
# all 3 are significant 

#FERI & EEMT
model10.gls<- gls(TreeSurvivorship ~ FERI + EEMT, data=MasterData, na.action= na.omit)
anova(model10.gls)
# EEMT sig

model11.gls<- gls(ConHealth ~ FERI + EEMT, data= MasterData, na.action=na.omit)
anova(model11.gls)
#both significant- FERI makes sense because con health being driven by Bighorn (which
# is overweighted in FERI. Tree survivorship driven by Bullock/aspen- FERI has higher F-value tho

model12.gls<- gls(OakHealth ~ FERI + EEMT, data= MasterData, na.action=na.omit)
anova(model12.gls)
# neither significant 

plot(MasterData$BARBR, MasterData$UnderstoryCover)

plot(MasterData$BighornRBR, MasterData$UnderstoryCover)
#As bullock/aspen goes up, so does understory. As bighorn goes up, understory goes down

plot(Nutrients$Kfsgeomean, Nutrients$OakSeedling)

model13.gls<- gls(TreeSurvivorship ~ MR1 + MR2, data= data_nutrientfa, na.action=na.omit)
anova(model13.gls)
#MR2 is significant (NO3N, N, C, OM, etc) for conifer health and oak health
#niether is sig for survivorship

model14.gls<- gls(AspSeedlings ~ MR1 + MR2, data= data_nutrientfa, na.action=na.omit)
anova(model14.gls)
#MR2 significant for Aspen seedlings, none others 


#Trends
model15.gls<- gls(TreeSurvivorship~  BASeverity + BighornSeverity, 
                  data=MasterData, na.action=na.omit)
anova(model15.gls)


model16.gls<- gls(UnderstoryCover ~ SeverityTrend, data=MasterData, na.action=na.omit )
anova(model16.gls)

###Plots 
library(ggplot2)
ggplot(MasterData, aes(x=SeverityTrend, y= TreeSurvivorship)) +
  geom_boxplot(fill="steelblue")


ggplot(MasterData, aes(x=SeverityTrend, y= UnderstoryCover)) +
  geom_boxplot(fill="steelblue")
###Interesting one - Same high is much higher than all others

ggplot(MasterData, aes(x=SeverityClass, y= LiveTreesHA)) +
  geom_boxplot(fill="steelblue", outlier.shape=NA)
