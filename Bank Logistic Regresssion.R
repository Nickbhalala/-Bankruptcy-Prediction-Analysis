

library(dplyr)
library(glmnet)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(DescTools) #VIF Function
library(leaps)

##########################################################################

#Read in Dataset

#Set Working Directory
setwd('C:/Users/josep/Downloads')

bank <- read.csv(file="data.csv", header=TRUE, sep=",")

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(bank))
#0 Missing Values

names(bank)
############################################################################
#Creating Training and Testing Samples
require(caTools)  # loading caTools library
library(caTools)
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(bank,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train =subset(bank,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test=subset(bank, sample==FALSE)

###########################################################################

#Using the Manual Method

bank2 <- bank[, c(1,2,3,20,34,81,82,90)]

logistic <- glm(Bankrupt. ~ ., data = bank2, family = "binomial")

summary(logistic)
###############################################################################
#Check for multicollinearity 

car::vif(logistic)

###############################################################################
#which variables to choose
fit = glmnet(train,test)  # note there is no ~ here and y comes after the x's.
plot(fit, label=T)
#################################################################################
#ANOVA function used to analyze the table of deviance

anova(logistic, test="Chisq")

#MCFadden R^2 index used to assess model fit
library(pscl)
pR2(logistic)

#Strange output for McFadden R^2 index 
################################################################################
#Assessing the predictive ability of the model

fitted.results <- predict(logistic,newdata=bank2,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != bank2$Bankrupt.)
print(paste('Accuracy',1-misClasificError))

#Accuracy of .9601941747

#Plotting ROC curve and Area Under The Curve
install.packages("ROCR")
library(ROCR)
p <- predict(logistic, newdata=bank2, type="response")
pr <- prediction(p, bank2$Bankrupt.)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

#curve seems to be 45 degrees, suggesting a non-accurate test 

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#auc value of .58312, which suggests that the model does not display the best predictive ability 
################################################################################
#Coefficients in Exponential Form 
library(dplyr)
logistic %>% 
  gtsummary::tbl_regression(exp = TRUE) 
################################################################################
#Graphing Forest Plots
#Forest Plots
install.package("forestplot")
library(forestplot)

#AR and Strain Forest Plots for Model Beta Coefficients
clrs <- fpColors(box="royalblue",line="darkblue", summary="royalblue")
tabletext<-cbind(
  c("Characteristics","basebp", "baseEF", "hxofHT", "hxofDM", "hxofMI","hxofCABG","Age Tertiles","Gender","hxofCig"),
  c("Odds Ratios", "1.00 (0.99,1.01), P=0.96", "0.96 (0.94-0.98), P<0.001", "2.05 (1.12-3.93), P=0.024","1.77 (1.08-2.92), P=0.024","1.89 (1.10-3.22), P=0.021","0.85 (0.43-1.60), P=0.62", "1.32 (0.97-1.82), P=0.08", "1.14 (0.67-1.90), P=0.63", "1.16 (0.86-1.56), P=0.32"))

xticks <- seq(from = -3, to = 3, by = 1)
forestplot(tabletext, 
           title = " ",
           zero = 1,
           boxsize = .10, # We set the box size to better visualize the type
           line.margin = .05, # We need to add this to avoid crowding
           mean  = c(NA, 1, 0.96, 2.05, 1.77, 1.89, 0.85, 1.32, 1.14, 1.16), 
           lower = c(NA,0.99, 0.94, 1.12, 1.08, 1.10, 0.43, 0.97, 0.67, 0.86),
           upper = c(NA, 1.01, 0.98, 3.93, 2.92, 3.22, 1.60, 1.82, 1.90, 1.56),
           fn.ci_norm = fpDrawCircleCI,
           clip =c(-1.00, 7.00),
           col=clrs,
           vertices=TRUE,
           xticks = xticks,
           txt_gp = fpTxtGp(label = gpar(fontface="bold"), ticks = gpar(fontface="bold"), xlab  = gpar(fontfamily = "", cex = 1)),
           xlab=expression(bold("Odds Ratios")))
