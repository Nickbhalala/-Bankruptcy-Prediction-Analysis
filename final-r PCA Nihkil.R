#Author:  Joe Larson 
#Date:  June 1, 2021 
#Libraries
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(DescTools) #VIF Function
library(leaps) #Best Set Linear Regression Functions
library(psych)
library(REdaS)
library(dplyr)
library(ggplot2)
library(factoextra)
library("corrplot")
################################################################################

#Read in Dataset

#Set Working Directory
setwd('C:/Users/josep/Downloads')

training_values <- read.csv(file="data.csv", header=TRUE, sep=",")

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(training_values))
#0 total missing
################################################################################

library(psych)
describe(training_values)

#Create new subsets of data

training_datasets2 <- training_values[,c(1:94,96)]

library(Hmisc)
describe(training_datasets2)
################################################################################
# PCA_Plot functions


PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}
################################################################################

#Testing KMO Sampling Adequacy
#Tests sample size reliability
KMO(training_datasets2)



#Test Bartlett's Test of Sphericity
#testing for shared variance
library(REdaS)
bart_spher(training_datasets2)

#Test for Reliability Analysis using Cronbach's Alpha
#Assesses consistency of each factor / component 
alpha(training_datasets2,check.keys=TRUE)

#initial chronbach's alpha of all data in dataset
#raw_alpha = -0.066
#Chronbach's alpha analysis showed a reliability analysis with an alpha = -0.07 

################################################################################
training_p = prcomp(training_datasets2, center=T, scale=T)
training_p

#Check Scree Plot
plot(training_p ,main = "Bankrupt", xlab="Components")
abline(1, 0)

#enhanced scree plot

training_p %>% fviz_eig()
training_p2 = psych::principal(training_datasets2, rotate="varimax", nfactors=3, scores=TRUE)
training_p2
print(training_p2$loadings, cutoff=.4, sort=T)

#Scores

scores <- training_p2$scores
scores_1 <- scores[,1]
summary(scores_1)
scores_2 <- scores[,2]
summary(scores_2)
scores_3 <- scores[,3]
summary(scores_3)
scores_4 <- scores[,4]
summary(scores_4)
################################################################################


library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations


fit = factanal(training_datasets2, 2)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)

#Using Factoextra
library(factoextra)

p3 <- prcomp(training_datasets2, scale = TRUE) 
fviz_eig(p3)

#PCA Individuals
pI<-fviz_pca_ind(p3,
                 col.ind = "cos2", # Color by the quality of representation
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE     # Avoid text overlapping
)
pI

#PCA Variables
pca_var<-fviz_pca_var(p3,
                      col.var = "contrib", # Color by contributions to the PC
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)

pca_var

#Biplot
bi_plot<-fviz_pca_biplot(p3, repel = TRUE,
                         col.var = "#2E9FDF", # Variables color
                         col.ind = "#696969"  # Individuals color
)

bi_plot

################################################################################

library("FactoMineR")
p4 <- PCA(training_datasets2, graph = FALSE)
#IF graph is set to true, it will provide the individual and variable maps

#Shows all the objects or functions available in PCA
print(p4)

#Options for providing screeplot
fviz_eig(p4, addlabels = TRUE, ylim = c(0, 35))
fviz_screeplot(p4, addlabels = TRUE, ylim = c(0, 35))

variables <- get_pca_var(p4)

#Which variables contibute the most to the PCs?
#there are ll variables
head(variables$contrib, 20)

library("corrplot")
corrplot(variables$contrib, is.corr=FALSE)    

# Contributions of variables to PC1
fviz_contrib(p4, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(p4, choice = "var", axes = 2, top = 10)


library(ade4)
p5 <- dudi.pca(training_datasets2,
               scannf = FALSE,   # Hide scree plot
               nf = 3         # Number of components kept in the results
)
fviz_screeplot(p5, addlabels = TRUE, ylim = c(0, 35))

variables2 <- get_pca_var(p5)



