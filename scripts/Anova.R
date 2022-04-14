#Anovas


getwd()
MasterData<- read.csv("MasterData.csv", header=TRUE)


Catalina<- subset(MasterData, Mountain=="Catalina")

Severity.aov= aov(TreeSurvivorship~ SeverityTrend, data=MasterData, singular.ok=singular.ok)
summary(Severity.aov)


TukeyHSD(Severity.aov)
#Significant differences - LU-HH, UL-HH, UU-HH, LU-LH, UL-LH, UL-UH, UU-LH, UH-LU
#

plot(TukeyHSD(Severity.aov), las=2, cex=.5)
pdf(file= "tree survivor anova.pdf", useDingbats = FALSE, height=14, width=14)
plot(TukeyHSD(Severity.aov), las=2, cex=.5)
dev.off()


##########################################################
#Nutrient Anova 

Nutrients<- subset(MasterData, subset= HydroPlot =="Yes"| HydroPlot=="Soil Only")

Nutrients.aov= aov(DuffDepth ~ SeverityClass, data=Nutrients, singular.ok=singular.ok)
summary(Nutrients.aov)
#no significant differences with Carbon, Nitrogen, pH, OM, BrayIP, litter depth (p=0.06)
#duff depth significant for LU-HH, UL-HH, LU-HL, UU-HL, LU-LL, UU-LL, UL-LL, UU-UH
TukeyHSD(Nutrients.aov)
######################################################################
#Understory Cover Anova

Understory.aov= aov(UnderstoryCover ~ SeverityClass, data=MasterData, singular.ok=singular.ok)
summary(Understory.aov)

TukeyHSD(Understory.aov)
#significant differences -LH-HH, LL-HH, LU-HH, UL-HH, UU-HH

#####################################################################
#Canopy Cover

Canopy.aov= aov(CanopyCover~SeverityClass, data=MasterData, singular.ok=singular.ok)
summary(Canopy.aov)

TukeyHSD(Canopy.aov)
#significant differences - LU-HH, UL-HH, UU-HH, LU-HL, UL-HL, UL-HU, UL-LH, UL-LL

##########################################################################
#Seedlings
OakSeed.aov= aov(OakSeedling~ SeverityClass,  data=MasterData, singular.ok=singular.ok)
summary(OakSeed.aov)
#no sig differences- does assume normality and very zero inflated

ConSeed.aov= aov(ConSeedling~ SeverityClass,  data=MasterData, singular.ok=singular.ok)
summary(ConSeed.aov)
#no sig differences- does assume normality and very zero inflated

#########################################################################
#Con and Oak Health
ConHealth.aov= aov(ConHealth ~SeverityClass,data=MasterData, singular.ok=singular.ok )
summary(ConHealth.aov)

TukeyHSD(ConHealth.aov)
#significant differences- UH-HU, UU-UH

OakHealth.aov= aov(OakHealth ~SeverityClass,data=MasterData, singular.ok=singular.ok )
summary(OakHealth.aov)

#not significant, but 44 obs deleted due to NAs

#Eru and Fire history on Tree Survivorship
TreeSur.aov= aov(TreeSurvivorship ~ SeverityClass * TrueERU, data=MasterData, singular.ok=singular.ok)
summary(TreeSur.aov)
#ERU not significant as additive or interactive effect. Severity Class is


#Eru and Fire history on Understory Cover
UnderstoryERU.aov= aov(UnderstoryCover ~ SeverityClass * TrueERU, data=MasterData, singular.ok=singular.ok)
summary(UnderstoryERU.aov)
#ERU not significant as additive or interactive effect. Severity Class is 

#Infiltration
Ksat.aov= aov(Kfsgeomean ~ SeverityClass, data= Nutrients, singular.ok= singular.ok)
summary(Ksat.aov)
#nothing significant

