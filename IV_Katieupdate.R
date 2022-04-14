#create Importance Values for each species for each plot

#IV= relative BA + relative density + relative height

# IV= (sum of species BA/sum of BA)*100 + (sum of species tree count/sum of 
#count)*100 + (sum of species heights/sum of heights)*100

#Final result will be ~15 columns with species headers "Alligator Juniper IV", "Arizona
# Pine IV". Plots with only one species will only have one column filled out. 

getwd()
AliveTrees<- read.csv("AllTreesIV.csv", header=TRUE)



#Create a variable thot contains both plot & species
AliveTrees$PlotSp <- paste(AliveTrees$Plot, AliveTrees$Species)

# SpeciesBA is sum of species BA by plot
SpeciesBA<- aggregate(x=AliveTrees$BA, by= list(AliveTrees$Species, 
                                                AliveTrees$Plot), 
                                         FUN=sum)

#Sum BA- sum of total BA by plot
SumBA<- aggregate(x=AliveTrees$BA, by= list(AliveTrees$Plot), FUN=sum)


#Sum BA- sum of total BA by plot
SumBA<- aggregate(x=AliveTrees$BA, by= list(AliveTrees$Plot), FUN=sum)

#SpeciesCount- Count of all trees by species per plot
SpeciesCount<- aggregate (x=AliveTrees$XCoord, by = list(AliveTrees$Plot, 
          AliveTrees$Species), FUN= length)
                                                          

#SumCount- sum of all trees in plot
SumCount<- aggregate (x=AliveTrees$XCoord, by = list(AliveTrees$Plot
                                                         ), FUN= length)
#SpeciesHeight- total height by species per plot
SpeciesHeight<- aggregate(x=AliveTrees$TreeHeight, by= list(AliveTrees$Species, AliveTrees$Plot), 
                          FUN=sum)

#SumHeight- sum of all tree heights per plot
SumHeight<- aggregate(x=AliveTrees$TreeHeight, by= list(AliveTrees$Plot), FUN=sum)

#Name summary dataset columns
colnames(SpeciesBA) <- c("Species", "Plot", "SpBA")
colnames(SumBA) <- c("Plot", "PlotBA")
colnames(SpeciesCount) <- c("Plot", "Species", "SpCount")
colnames(SumCount) <- c("Plot", "PlotCount")
colnames(SpeciesHeight) <- c("Species", "Plot", "SpHt")
colnames(SumHeight) <- c("Plot", "PlotHt")

#Create datasets
BA <- merge(SpeciesBA, SumBA, by="Plot", all=TRUE)
Count <- merge(SpeciesCount, SumCount, by="Plot", all=TRUE)
Height <- merge(SpeciesHeight, SumHeight, by="Plot", all=TRUE)

#Create a variable that contains both plot & species
BA$PlotSp <- paste(BA$Plot, BA$Species)
Count$PlotSp <- paste(Count$Plot, Count$Species)
Height$PlotSp <- paste(Height$Plot, Height$Species)

#Create a dataset with all data
#Do this in two steps to check merges as we go
#Redundant columns are renamed in merges (so you get Plot, Plot.x, Plot.y); these should match, if they don't there is a merge error & the incorrect Plot column should help you figure out which merge created the error
temp <- merge(BA, Count, by="PlotSp", all=TRUE)
ALL <- merge(temp, Height, by="PlotSp", all=TRUE)

#Name summary dataset rows so you can easily track
row.names(ALL) <-paste(ALL$PlotSp)

# IV= (sum of species BA/sum of BA) + (sum of species tree count/sum of 
#count) + (sum of species heights/sum of heights)
ALL$IV<- (ALL$SpBA/ALL$PlotBA)*100 + (ALL$SpCount/ALL$PlotCount)*100 +(ALL$SpHt/ALL$PlotHt)*100



