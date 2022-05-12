AllCores<- read.csv("CoresFuleRahner.csv", header=TRUE)
NewDetrended<- read.csv("DetrendedCores.csv", header=TRUE)
library(dplR)
library(ggplot2)
library(reshape2)
library(gridExtra)

#Subset all data by Species and burn severity 

#PIAZ= Arizona and Ponderosa Pine
#PSME= Douglas Fir
#PIST= Southwestern White Pine

#Subset PIPO and PIAZ for severity unburned:
PIAZU<-NewDetrended[,c(1,12,25,27,30,33,51:53,60)]

PIAZU<-PIAZU[-c(1:137,209),]

#Subset PIPO and PIAZ for severity low
PIAZL<-NewDetrended[,c(1,10,15,22,42,44,45)]

PIAZL<-PIAZL[-c(1:137,209),]

#Subset PIPO and PIAZ for severity High
PIAZH<-NewDetrended[,c(1,3,4,7,8,35,37,40)]
PIAZH<-PIAZH[-c(1:137,209),]
#subset PSME Unburnt
PSMEU<-NewDetrended[,c(1,11,14,26,28,32,49,55,61)]
PSMEU<-PSMEU[-c(1:137,209),]
#subset PSME low severity
PSMEL<-NewDetrended[,c(1,18,23,24,47,56,57)]
PSMEL<-PSMEL[-c(1:137,209),]
#subset PSME high severity
PSMEH<-NewDetrended[,c(1,2,6,9,16,20,36,39,41)]
PSMEH<-PSMEH[-c(1:137,209),]
#subset PIST Unburned
PISTU<-NewDetrended[,c(1,13,29,31,50,54,62)]
PISTU<-PISTU[-c(1:137,209),]
#subset PIST Low severity
PISTL<-NewDetrended[,c(1,17,21,43,46,48,58,59)]
PISTL<-PISTL[-c(1:137,209),]
#subset PIST High severity
PISTH<-NewDetrended[,c(1,5,19,34,38)]
PISTH<-PISTH[-c(1:137,209),]

#Calculate means of each species/burn severity to map on top of each other

#PSME Means  
MeanPSMEH<-rowMeans(PSMEH[,c(2:9)], na.rm=TRUE)
MeanPSMEL<-rowMeans(PSMEL[,c(2:7)], na.rm=TRUE)
MeanPSMEU<-rowMeans(PSMEU[, c(2:9)], na.rm=TRUE)


MeanPSME<-cbind(PSMEU$X, MeanPSMEH, MeanPSMEL, MeanPSMEU )
View(MeanPSME)

MeanPSME<- MeanPSME[,c(1:4)]

#melt means
MeltPSME<-melt(MeanPSME, id.var="X")
View(MeltPSME)

#create Graph
png("outputs/MeanPSME.png", units= "in", width=20, height= 4, res=200)
psmeplot<- ggplot(MeltPSME, aes(x=X, y=value))+
  geom_line(aes(colour=variable))+
  geom_vline(xintercept=2003, colour="darkred", linetype="longdash")+
  ggtitle("Mean PSME RWI by Severity")+
  xlab("Year")+
  ylab("RWI")
dev.off()

###################################################
#PIAZ Means
MeanPIAZL<-rowMeans(PIAZL[,c(2:7)], na.rm=TRUE)
MeanPIAZH<-rowMeans(PIAZH[,c(2:8)], na.rm=TRUE)
MeanPIAZU<- rowMeans(PIAZU[,c(2:10)], na.rm=TRUE)
#combine means
MeanPIAZ1<-cbind(PIAZU, MeanPIAZH, MeanPIAZL, MeanPIAZU)

MeanPIAZ<-MeanPIAZ1[,c(1, 11:13)]
#melt means
MeltPIAZ<-melt(MeanPIAZ, id.var="X")
View(MeltPIAZ)
png("outputs/MeanPIAZ.png", units= "in", width=20, height= 4, res=200)
piazplot<-ggplot(MeltPIAZ, aes(x=X, y=value))+
  geom_line(aes(colour=variable))+
  geom_vline(xintercept=2003, colour="darkred", linetype="longdash")+
  ggtitle("Mean PIAZ RWI by Severity")+
  xlab("")+
  ylab("RWI")
dev.off()
###############################################################
#PIST Means
MeanPISTL<-rowMeans(PISTL[,c(2:8)], na.rm=TRUE)
MeanPISTH<-rowMeans(PISTH[,c(2:5)], na.rm=TRUE)
MeanPISTU<- rowMeans(PISTU[,c(2:7)], na.rm=TRUE)
#combine means
MeanPIST1<-cbind(PISTU, MeanPISTH, MeanPISTL, MeanPISTU)

MeanPIST<-MeanPIST1[,c(1, 8:10)]

#melt means
MeltPIST<-melt(MeanPIST, id.var="X")
png("outputs/MeanPIST.png", units= "in", width=20, height= 4, res=200)
pistplot<-ggplot(MeltPIST, aes(x=X, y=value))+
  geom_line(aes(colour=variable))+
  geom_vline(xintercept=2003, colour="darkred", linetype="longdash")+
  xlab("")+
  ylab("RWI")+
  ggtitle("Mean PIST RWI by Severity")
dev.off()

#plot all of them

png("outputs/AllMeans.png", units="in", width=20, height=12, res=200)
grid.arrange(pistplot, piazplot, psmeplot, ncol=1)
dev.off()