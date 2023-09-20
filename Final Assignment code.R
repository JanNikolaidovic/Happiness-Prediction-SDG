
##################################
##################################
##########PREPARATION#############
##################################
##################################
setwd("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment")
data2015.PCR<-na.omit(read.csv("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment/657983in_Happiness_2015.csv"))
#PCR data
data2015.PCR<-data2015.PCR[,-c(1,3)]
colnames(data2015.PCR)[1]<- "Year" #renaming basic variables
colnames(data2015.PCR)[2]<- "Country"
colnames(data2015.PCR)[3]<- "Code"

data2015.PCR[,-c(1,2,3,22)]<-scale(data2015.PCR[,-c(1,2,3,22)]) # scaling numeric values except the response
data2015.PCR<-data2015.PCR[,-c(1:3)]
#LASSO data
data2015.2<-na.omit(read.csv("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment/657983in_Happiness_2015.csv"))
data2015.exp<-read.csv("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment/657983in_Happiness_2015_expanded.csv")
#prep 1
data2015.2<-data2015.2[,-c(1,3)]
colnames(data2015.2)[1]<- "Year" 
colnames(data2015.2)[2]<- "Country"
colnames(data2015.2)[3]<- "Code"
#prep 2
data2015.exp<-data2015.exp[,-c(1,3)]
colnames(data2015.exp)[1]<- "Year" 
colnames(data2015.exp)[2]<- "Country"
colnames(data2015.exp)[3]<- "Code"

data2015.LASSO<-merge(data2015.2,data2015.exp, by=c("Country","Year","Happiness_score"))
data2015.LASSO[,-c(1,2,3,4,23)]<-scale(data2015.LASSO[,-c(1,2,3,4,23)])
data2015.LASSO<-data2015.LASSO[,-c(2,4,23)]
data2015.LASSO<-data2015.LASSO[,-c(1)]

for(i in 3:100){ #Replacing every NA with the mean (0 after scaling)
  rows<-which(is.na(data2015.LASSO[,i]))
  data2015.LASSO[rows,i]<- 0
}
sum(is.na(data2015.LASSO[,3:101])) 

###############################################################################
library(Metrics)
library(reshape2)    
library(ggplot2) # visuals
library(Hmisc) # describe function
library(boot)
library(plotrix)
library(corrplot)
source("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment/my_biplot.R")
source("C:/Users/ioann/Desktop/Intro to Data Science/Final Assignment/permtestPCA.R")

#PCA###########################################################################

colnames(data2015.PCR)
cor(as.matrix(data2015.PCR))


set.seed(299)
fitpca<-princomp(data2015.PCR[,-c(19)],cor=TRUE)  #pca , choosing columns if interest, scaling
summary(fitpca)

print(summary(fitpca, loadings = T,cutoff=0.2), digits=2) 


#BIPLOT AND SCREEPLOT ##########################################################

#standard deviation is equal to the eigenvalue , variance explained by each component
#total components = total amount of original variables 
#if we include all of the components the variances would be 1
plot(fitpca$sdev^2, type="b",pch=18,cex=1,lwd=1,
     xlab = "",
     ylab="Eigenvalue")

#Following Kaisers Rule we can pick eigenvalues higher than 1,
#so we can choose the first 3 components
screeplot(fitpca, main="") 

biplot(fitpca, pc.biplot = T,scale=1,choices=1:2, col=c("blue","black"),  
       
       asp=1,cex=c(0.5,1),main="Biplot of PC1 & PC2") 

#FIT PER VARIABLE##############################################################

#Diagnostics : fit per variable , which is the square of correlations 
#between original values and scores
corpca<-cor(data2015.PCR[,-c(19)],fitpca$scores)[,1:3] #First three components
#If we took all of the components then the variations would be equal to 1 
#as all of them would be explained by the analysis
rowSums(corpca^2) #how much variation is explained for each variable
#by the first 2 components
temp<-cbind(corpca,rowSums(corpca^2)) #combining correlations with variation
#explained
colnames(temp)<-c("Dim1","Dim2","Dim3","Fit") #renaming 

#adding row with columns totals (equals eigenvalues)
temp<-round(temp,digits = 2)
temp #correlations between components (dim) and variables 

#PERMUTATION TEST##############################################################
perm_range<-permtestPCA(data2015.PCR[,-c(19)])

#BOOTSTRAP######################################################################

#Define Function that Runs PCA and Returns
boot_pca<-function(x,ind){
  res<-princomp(x[ind, ],cor=T)
  return(res$sdev^2)
}

#Run bootstrap
fit.boot<-boot(data=data2015.PCR, statistic = boot_pca, R=1000)

#Store the bootstrapped stat(all eigenvalues)
eigs.boot<-fit.boot$t
head(eigs.boot)
fitpca$sdev^2

###variance explained of the first 3 components###
var.expl<-rowSums(eigs.boot[,1:3])/rowSums(eigs.boot)

hist(var.expl,xlab="Variance Explained",las=1,col="blue",
     main="Bootstrap Confidence Intervals", breaks=20,
     border="white")

perc.alpha<-quantile(var.expl,c(0.025,1-0.025))

perc.alpha


abline(v=perc.alpha, col="green",lwd=2)
abline(v=fitpca$sdev^2, col="red",lwd=2)

##############################################################################


#PCR
#Splitting data to train and test, 75% - 25%
set.seed(299)
idx<-floor(0.85 * nrow(data2015.PCR))

#setting seed 
train_index<-sample(seq_len(nrow(data2015.PCR)),size = idx)
train<-data2015.PCR[train_index,]
test<-data2015.PCR[-train_index,]

set.seed(299)
library(pls)
pcr_model<-pcr(Happiness_score~.,        #composing PCR model
               data=train,       #applied to the training set
               scale=F,       #scaling data
               validation="CV")  #k-fold cross validation

summary(pcr_model)#the amount of improvement in the RMSE doesn't gets better after the 2 components , so we will choose the first two PC's
#RMSE 
pcr_pred<-predict(pcr_model,test,ncomp = 2) 
#If we chose the same number of components with the total variables
#in the linear model (ncomp=14) then we would get the same exact RMSE (because there would be no difference from linear to PCA)
validationplot(pcr_model)
validationplot(pcr_model, val.type = "R2") 
rmse(actual=test$Happiness_score, predicted = pcr_pred) 

###############################################################################

#LASSO
set.seed(299)
library(caret)
#split data , 80-20
trainRowNumbers<-createDataPartition(data2015.LASSO$Happiness_score,
                                     p=0.85,list=F)
#create train and test data
trainData<-data2015.LASSO[trainRowNumbers,]
testData<-data2015.LASSO[-trainRowNumbers,]
#Store X and Y for later use
x<- trainData[,c(2:100)]
y<-trainData$Happiness_score
#cross validation (custom control parameters)
fitControl<-trainControl(method="repeatedcv",
                         number = 10, #10 folds
                         repeats=5,
                         verboseIter = T) #don't see repeats in the console
set.seed(299)
#Caret
lasso<-train(Happiness_score~.,
             trainData,
             method="glmnet",
             tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=100)),
             trControl=fitControl)
lasso
plot(lasso)
plot(lasso$finalModel,xvar="lambda",label=T)
round(coef(lasso$finalModel,lasso$bestTune$lambda),3)

lasso_pred<- predict(lasso, newdata=testData)
rmse(actual=testData$Happiness_score, predicted = lasso_pred)
R2(lasso_pred,testData$Happiness_score)

#Predict using PCR model as it has lower RMSE
predictions <- read.csv("C:/Users/ioann/Desktop/Intro to Data Science/Final assignment/Happiness_2019.csv")
predictions[,6:104]<- scale(predictions[,6:104])

set.seed(299)
pcr_pred_1<-predict(pcr_model,predictions,ncomp = 3)
       


