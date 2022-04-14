#EMily Fule, Spring 2022
#efule@email.arizona.edu
#Script for PCA (Principal Component Analysis)
#PCA " is commonly used for dimensionality reduction by projecting each data point 
# onto only the first few principal components to obtain lower-dimensional data while 
# preserving as much of the data's variation as possible. The first principal component 
# can equivalently be defined as a direction that maximizes the variance of the projected data"

# I am using PCA because I have a complex ecological data set that has many variables that can 
# considered both as dependent and independent. This is to look for trends, correlations, and clustering
# within and between ecosystem types

library(factoextra)
library(corrplot)
#kmeans, #prcomp

#######################################################################################
##PCA with EEMT, tree survivor, dead BA, understory cover, live BA, litter/duff depth

#Subset of MasterData that includes the variables: EEMT(a measure of productivity), tree survivorship,
# dead basal area, understory cover, live basal area, and litter and duff depth, and distance to a seed source
Ecol.numeric<- MasterData [,c(6, 22:25, 34:36, 98, 108, 112, 116, 120, 124, 130, 135)]

#create PCA
PCAEcol<- prcomp(Ecol.numeric, scale=TRUE)

#visualiztion of eigen values. Eigen values are assigned for each dimension, and describe the amount
# of variance explained by each dimension. A desirable result is about 70% of variance explained by
# roughly 5 dimensions
fviz_eig(PCAEcol)

# raw Eigen values
eig.val.ecol<- get_eigenvalue(PCAEcol)
eig.val.ecol

# vizualition of the PCA with variables. Variables opposite of each other are anti-correlated, variables
# grouped together are correlated. The longer the arrow (further from center) indicates a stronger 
# association with either dimension 1 or dimension 2
fviz_pca_var(PCAEcol, repel=TRUE)
## a lot of this is not novel- distance to seed source is opposite from live tree. Understory ang cover is opposite from und locust and shrub,
#cause angiosperms aren't great after fire. Tree survivorship is associated with canopy cover, litter/duff depth

# the plots and variables graphed on top of each other
fviz_pca_biplot(PCAEcol)

#correlation plot for cos2 values
var<- get_pca_var(PCAEcol)
corrplot(var$cos2)
# this plot shows which variables are contributing to which dimensions. Interesting that EEMT is not
# in the same dimensions as all the other ecology variables that are indicative of productivity
##########################################################################################
#=Nutrient PCA
# Nutrients is a data set containing only plots where nutrient
#analysis was conducted. PCA cannot accept any null values so this
#does limit the analysis a little
Nutrients<- subset(MasterData, subset= HydroPlot =="Yes"| HydroPlot=="Soil Only")
#Subset of Nutrients ( a subset of Master Data that only includes plots with nutrient analysis)
# PCA includes EEMT, tree survivorship, dead BA, nutrients, shrub and oak understory
Nutrient.numeric<- Nutrients[, c(6, 22:24, 34:39, 57:81 ,83, 116, 119, 135)]


PCANutrient<- prcomp(Nutrient.numeric, scale=TRUE)

#eigen values for PCA
eig.val.nutrient<- get_eigenvalue(PCANutrient)
eig.val.nutrient
##top 5 dimensions account for 68% of variation

#corelation plot for Nutrient PCA. So many variables and dimensions that this is hard to understand
varnutrient<- get_pca_var(PCANutrient)
corrplot(varnutrient$cos2, title= "Correlation Plot for Nutrient PCA")

fviz_pca_biplot(PCANutrient, geom="point", repel=TRUE)
#groups <- as.factor(data$ecosystem_type)
#fviz_pca_ind(data.pca,
#            col.ind = groups, # color by groups
#             legend.title = "Groups",
#             repel = TRUE)

# Visualization of Plots by ecosystem type 
ERU<- as.factor(Nutrients$TrueERU)
png("outputs/Nutrient PCA by ERU.png", units = "in", width = 7, height = 5, res = 200)
fviz_pca_ind(PCANutrient, col.ind=ERU, title= "Nutrient PCA by ERU" ,legend.title="Ecosystem Type", repel=TRUE )
dev.off()

# this is showing that Mixed Conifer is being highly influenced by the variables in dimension 1,
# which from the Correlation plot are soil type (sand/silt/clay) and Ca, Mg, and K



########################################################################################

##Nutrient and Tree IV PCA - PCA 4
## Variables included: EEMT, Tree Survivorship, Canopy Cover, Dead BA, Understory Cover, C, N, 
# pH, CN Ratio, Average Sand, Ca, NO4N, White Fir IV, AZ Pine IV, Silver Leaf Oak IV, Doug Fir IV, 
#Emory Oak IV
#Includes all plots with nutrient analysis

NutrientsTrees<- Nutrients[, c(6, 23, 24, 36, 39, 57:59, 61, 74, 83, 136:138, 142 )]
PCANutTree<- prcomp(NutrientsTrees, scale=TRUE)
get_eigenvalue(PCANutTree)
## 6 Dimensions explain 84% of variation!!

NutTreevar<- get_pca_var(PCANutTree)

corrplot(NutTreevar$cos2, title= "Correlation plot for Nutrients and Tree IV PCA")

png("outputs/Nutrient and Tree IV PCA.png", units= "in", width=7, height= 5, res=200)
fviz_pca_biplot(PCANutTree, col.ind=ERU, repel=TRUE, title ="PCA of Nutrients and Tree IV", 
                legend.title="Ecosystem Type")
dev.off()
fviz_pca_ind(PCANutTree, col.ind=ERU, repel=TRUE)
######################################################################################
##PCA Tree IV 
##Following script dropped C5 and P9 to get rid of locust and Aspen outliers
##Includes all plots except C5 and P9
MasterData<- read.csv("MasterData.csv", header=TRUE)

TreeIV<- MasterData[-c(1,5), c(6, 22:24, 135:147, 149:153)]
PCAIV<- prcomp(TreeIV, scale=TRUE)
eig.val.IV<- get_eigenvalue(PCAIV)
eig.val.IV

fviz_pca_ind(PCAIV, repel=TRUE, title= "Tree IV Biplot")
fviz_pca_biplot(PCAIV, repel=TRUE)
##C5a huge outlier in this data. So is P9. Might consider dropping both of them? 
# not super useful on its own, so many of these values are 0 and only a few tree species
# are consistently important/ present

######################################################################################
#PCA of nutrients and fire severity, color coded by severity class
#PCA 5

MasterData<- read.csv("MasterData.csv", header=TRUE)

Hydro<-subset(MasterData, HydroPlot=="Yes")
# A subset where hydrology variables were measured (saturated conductivity and sorptivity
# EEMT, FERI, bighorn and bullock RBR, Kfs, S, nutrients)
Hydro2<- Hydro[,c(6, 7, 16,17, 41, 42, 57:62, 74, 83, 77, 78 )]
PCAHydro<-prcomp(Hydro2, scale=TRUE)

eig.val.hydro<- get_eigenvalue(PCAHydro)
eig.val.hydro

#eigen values are high- 5 dimensions are 82% 

Hydrovar<- get_pca_var(PCAHydro)
corrplot(Hydrovar$cos2)


Severity<- as.factor(Hydro$SeverityTrend)
png("outputs/Nutrient and Fire Severity Biplot.png", units= "in", width=7, height= 5, res=200)
fviz_pca_biplot(PCAHydro, repel=TRUE, col.ind=Severity, title="Biplot for Nutrients and Fire severity",
                legend.title= "Severity Trend")
dev.off()

########################################################################################
