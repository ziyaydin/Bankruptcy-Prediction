############# DATA BUSTERS --> THE ENTIRE CODE ##############

#import necessary libraries
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

if (!require("rpart")) {
  install.packages("rpart")
  library("rpart")
}

if (!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}

if (!require("Metrics")) {
  install.packages("Metrics")
  library("Metrics")
}

if (!require("psych")) {
  install.packages("psych")
  library("psych")
}

# This package is needed for calculating the area under the curve (AUC) for Lift/ROC curves
if (!require("pROC")) {
  install.packages("pROC")
  library("pROC")
}

if (!require("e1071")) {
  install.packages("e1071")
  library("e1071")
}

#Compute kNN
###########################
if (!require("FNN")) {
  install.packages("FNN")
  library("FNN")
}

#read in data; separate into training and test data
##############################################################

#Read the Data
setwd("C:/Users/Student/Desktop/QTM2000/Data")
myData <- read.csv("Bankruptcy7.csv")

#Change target variable to a category
myData$Bankrupt. <- as.factor(as.character(myData$Bankrupt.))

#training and test sets
trainSetSize <- floor(0.75 * nrow(myData))   
RNGkind(sample.kind = "Rejection")
set.seed(50)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myData[trainInd, ]               
myDataTest <- myData[-trainInd, ] 

#Finding individual p values and AICs of the variables
dimnames(myData)

#use the glm function
logRegrModelInc <- glm(Bankrupt. ~ Working.Capital.to.Total.Assets, 
                       data = myData,
                       family ="binomial")
#summarize logistic regression output 
summary(logRegrModelInc)

##############################################################

##############  CLUSTERS ##################

#specify the number of clusters
k = 3

#run k-means algorithm
kmOut <- kmeans(myData,k)

#show cluster membership
kmOut$cluster

#show centroids
kmOut$centers

#within-cluster sum of squares
kmOut$withinss

#print distances between cluster centroids
dist(kmOut$centers)

#cluster size
kmOut$size

#create elbow plot
#thanks to Sunny Anand
#compute and plot wss for number of clusters from 1 to kMax = 20
kMax <- 20
wss <- sapply(1:kMax, 
              function(iCluster){kmeans(myData, iCluster)$tot.withinss})
wss
plot(1:kMax, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Plots
#######################
#visual representation of cluster centroids
#plot an empty scatter plot
plot(c(0),xaxt = "n", ylab = "", type = "l",
     ylim = c(min(kmOut$centers), max(kmOut$centers)), xlim = c(0,ncol(myData)))
#label x axes
axis(1, at = c(1:ncol(myData)), labels = names(myData))
#plot centroids 
for (i in c(1:k)) {
  lines(kmOut$centers[i,], lty = i, lwd = 2)
}
#label clusters
text(x = 0.5, y = kmOut$centers[,1], labels = paste("Cluster", c(1:k)))

#summarize original data by cluster
myDataClusteredK <- data.frame(myData,kmOut$cluster)

#specify the number of clusters
k = 3

#run k-means algorithm
kmOut <- kmeans(myDataScaled,k)

#show cluster membership
kmOut$cluster

#output cluster assignments to a file
myDataClusteredK <- data.frame(myData,kmOut$cluster)
write.csv(myDataClusteredK, file = "../ROutput/clusteredUtilKMeans.csv")

#Decide how many neighbors to use based on which number results in highest model accuracy 
#################################
#Initiate a data frame (table) with first column = k and second column = Error
#The number of neighbors and the associated model error will be stored there
errTable <- data.frame(k = c(1:7),  err = rep(0, 7))
actualTestClass <- myDataTest$Bankrupt.

####################################

###########  LOGISTICS w 7 variables ############

#LOOK AT THE NAMES OF EACH COLUMN, SO YOU CAN COPY PASTE
dimnames(myData)

#use the glm function
logRegrModelInc7 <- glm(Bankrupt. ~ ., 
                        data = myData,
                        family ="binomial")
#summarize logistic regression output 
summary(logRegrModelInc7)

predTestScores <- predict(logRegrModelInc7, type="response", newdata=myDataTest) 


##Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores >= cutoff] <- 1

actualTestClass <- myDataTest$Bankrupt
actualTestClass <- as.numeric(as.character(actualTestClass))

#Simply using tables
confMx <- table(predTestClass, actualTestClass) 

confMx

confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")


simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClass,predTestScores)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

pROCData <- pROC::roc(myDataTest$Bankrupt.,predTestScores) 
plot(pROCData) # Gets a smoother version of the curve for calculating AUC; axes labels a bit confusing
pROCData[9]


################### LOGISTICS w 25 variables ################3#

myData <- read.csv("Bankruptcy25.csv")

#use the glm function
logRegrModelInc25 <- glm(Bankrupt. ~ ., 
                         data = myData,
                         family ="binomial")
#summarize logistic regression output 
summary(logRegrModelInc25)

predTestScores <- predict(logRegrModelInc25, type="response", newdata=myDataTest) 


##Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores >= cutoff] <- 1

actualTestClass <- myDataTest$Bankrupt
actualTestClass <- as.numeric(as.character(actualTestClass))

#Simply using tables
confMx <- table(predTestClass, actualTestClass) 

confMx

confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")


simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClass,predTestScores)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

pROCData <- pROC::roc(myDataTest$Bankrupt.,predTestScores) 
plot(pROCData) # Gets a smoother version of the curve for calculating AUC; axes labels a bit confusing
pROCData[9]

################## LOGISTICS w 85 variables ######################

myData <- read.csv("Bankruptcy85.csv")

#use the glm function
logRegrModelInc85 <- glm(Bankrupt. ~ ., 
                         data = myData,
                         family ="binomial")
#summarize logistic regression output 
summary(logRegrModelInc85)

predTestScores <- predict(logRegrModelInc85, type="response", newdata=myDataTest) 


##Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores >= cutoff] <- 1

actualTestClass <- myDataTest$Bankrupt
actualTestClass <- as.numeric(as.character(actualTestClass))

#Simply using tables
confMx <- table(predTestClass, actualTestClass) 

confMx

confusionMatrix(as.factor(predTestClass), as.factor(actualTestClass), positive = "1")


simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClass,predTestScores)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

pROCData <- pROC::roc(myDataTest$Bankrupt.,predTestScores) 
plot(pROCData) # Gets a smoother version of the curve for calculating AUC; axes labels a bit confusing
pROCData[9]

################# kNN #####################

#K=3 ####################################################
predTestClass <- knn(train = myDataTrain[,2:7], test = myDataTest[,2:7], cl = myDataTrain[, 1], k = 3)
#Compute confusion matrix for prediction model
################################################de
actualTestClass <- myDataTest$Bankrupt.
#Calculate all relevant statistics for confusion matrix
#############################################
confMx3 <- confusionMatrix(predTestClass, actualTestClass, positive = "1")
#print confusion matrix
confMx3
#calculate total accuracy
totAcc <- confMx3$overall[1]
totAcc

#K=5 ####################################################
predTestClass <- knn(train = myDataTrain[,2:7], test = myDataTest[,2:7], cl = myDataTrain[, 1], k = 5)
#Compute confusion matrix for prediction model
################################################de
actualTestClass <- myDataTest$Bankrupt.
#Calculate all relevant statistics for confusion matrix
#############################################
confMx5 <- confusionMatrix(predTestClass, actualTestClass, positive = "1")
#print confusion matrix
confMx5
#calculate total accuracy
totAcc <- confMx5$overall[1]
totAcc


#K=7 ####################################################
predTestClass <- knn(train = myDataTrain[,2:7], test = myDataTest[,2:7], cl = myDataTrain[, 1], k = 7)
#Compute confusion matrix for prediction model
################################################de
actualTestClass <- myDataTest$Bankrupt.
#Calculate all relevant statistics for confusion matrix
#############################################
confMx7 <- confusionMatrix(predTestClass, actualTestClass, positive = "1")
#print confusion matrix
confMx7
#calculate total accuracy
totAcc <- confMx7$overall[1]
totAcc

#K=9 ####################################################
predTestClass <- knn(train = myDataTrain[,2:7], test = myDataTest[,2:7], cl = myDataTrain[, 1], k = 9)
#Compute confusion matrix for prediction model
################################################de
actualTestClass <- myDataTest$Bankrupt.
#Calculate all relevant statistics for confusion matrix
#############################################
confMx9 <- confusionMatrix(predTestClass, actualTestClass, positive = "1")
#print confusion matrix
confMx9
#calculate total accuracy
totAcc <- confMx9$overall[1]
totAcc

#########################################  CLASS TREE  ###########################################

#Create the full classification tree
RNGkind(sample.kind = "Rejection")
set.seed(50)   
classTree1 <- rpart(Bankrupt. ~ ROA.C..before.interest.and.depreciation.before.interest + Borrowing.dependency + Working.Capital.to.Total.Assets + Cash.Total.Assets + Working.Capital.Equity + Retained.Earnings.to.Total.Assets + Net.Income.to.Total.Assets,
                    data=myDataTrain,
                    method = "class",
                    control = rpart.control(minsplit = 1,cp = 0))
#              control = rpart.control(minsplit = 20,cp = 0))
#              method = "class")

#visualize the tree
rpart.plot(classTree1, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 4, tweak = 0.5, cex = 0.8)
classTree1 #print tree rules
printcp(classTree1) #print cp table (useful for pruning)
plotcp(classTree1) #plot graph of how xerror changes with cp

#Prune the tree
#The argument cp = fullTree$cptable[which.min(fullTree$cptable[,"xerror"]
#prunes the tree at the point with minimum cross-validation error
RNGkind(sample.kind = "Rejection")
set.seed(50)
classTree2 <- prune(classTree1, cp=
                      classTree1$cptable[which.min(classTree1$cptable[,"xerror"]), "CP"])
#prunedTree <- prune(fullTree, cp=0.0025510)
printcp(classTree2)
rpart.plot(classTree2, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 5, tweak = 0.8, cex = 0.9)
rpart.rules(classTree2, cover = TRUE)
classTree2

#Fit the rules to new data (test set)
predTestClass <- predict(classTree2, newdata = myDataTest, type="class")
confMx <- confusionMatrix(predTestClass, as.factor(myDataTest$Bankrupt.), positive = "1")
confMx

##########################################  BALANCING DATA  ########################################

#Downsample - i.e., reduce the number of the rows of the large class
#to match the number of rows of the small class
#Only do this when you have a lot of data. Not in this case!
downTrain <- downSample(x = myDataTrain[, -ncol(myDataTrain)],
                        y = myDataTrain$Bankrupt.)
table(downTrain$x)   
colnames(downTrain)[colnames(downTrain)=="Class"] <- "Bankrupt."

#Export predicted prices for observations in the test set to a csv file
#named predictedCarsCART.csv
dfToExport <- data.frame(downTrain)
write.csv(dfToExport, file = "../ROutput/downtrain.csv")

#Upsample - i.e., increase the number of the rows of the small class
#to match the number of rows of the large class by resampling from its distribution
upTrain <- upSample(x = myDataTrain[, -ncol(myDataTrain)],
                    y = myDataTrain$Bankrupt.)                         
table(upTrain$x) 
colnames(upTrain)[colnames(upTrain)=="Class"] <- "Bankrupt."

dfToExport <- data.frame(upTrain)
write.csv(dfToExport, file = "../ROutput/uptrain.csv")

####################################  DOWNSAMPLE BALANCED TREES  #########################################

DownTree1 <- rpart(Bankrupt. ~ ROA.C..before.interest.and.depreciation.before.interest + Borrowing.dependency + Working.Capital.to.Total.Assets + Cash.Total.Assets + Working.Capital.Equity + Retained.Earnings.to.Total.Assets,
                   data=downTrain,
                   method = "class",
                   control = rpart.control(minsplit = 1,cp = 0))
#              control = rpart.control(minsplit = 20,cp = 0))
#              method = "class")

#visualize the tree
rpart.plot(DownTree1, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 5, tweak = 0.9, cex = 0.9)
DownTree1 #print tree rules
printcp(DownTree1) #print cp table (useful for pruning)
plotcp(DownTree1) #plot graph of how xerror changes with cp

#Prune the tree
DownTree2 <- prune(DownTree1, cp=
                     DownTree1$cptable[which.min(DownTree1$cptable[,"xerror"]), "CP"])
#prunedTree <- prune(fullTree, cp=0.0025510)
printcp(DownTree2)
rpart.plot(DownTree2, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 5, tweak = 0.9, cex = 0.9)
rpart.rules(DownTree2, cover = TRUE)
DownTree2

#check accuracy of model built on downsampled data on (unchanged) test set
predTestClass <- predict(DownTree2, newdata = myDataTest, type="class")
confMxDown <- confusionMatrix(predTestClass, as.factor(myDataTest$Bankrupt.), positive = "1")
confMxDown

####################################  UPSAMPLE BALANCED TREES  #########################################

UpTree1 <- rpart(Bankrupt. ~ ROA.C..before.interest.and.depreciation.before.interest + Borrowing.dependency + Working.Capital.to.Total.Assets + Cash.Total.Assets + Working.Capital.Equity + Retained.Earnings.to.Total.Assets,
                 data=upTrain,
                 method = "class",
                 control = rpart.control(minsplit =1,cp = 0))
#              control = rpart.control(minsplit = 20,cp = 0))
#              method = "class")

#visualize the tree
rpart.plot(UpTree1, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 5, tweak = 0.9, cex = 0.9)
UpTree1 #print tree rules
printcp(UpTree1) #print cp table (useful for pruning)
plotcp(UpTree1) #plot graph of how xerror changes with cp

#Prune the tree
UpTree2 <- prune(UpTree1, cp=
                   UpTree1$cptable[which.min(UpTree1$cptable[,"xerror"]), "CP"])
#prunedTree <- prune(fullTree, cp=0.0025510)
printcp(UpTree2)
rpart.plot(UpTree2, type=3, extra=2, fallen.leaves = FALSE,
           varlen = 5, tweak = 0.9, cex = 0.9)
rpart.rules(UpTree2, cover = TRUE)
UpTree2

#check accuracy of model built on upsampled data on (unchanged) test set
predTestClass <- predict(UpTree2, newdata = myDataTest, type="class")
confMxUp <- confusionMatrix(predTestClass, as.factor(myDataTest$Bankrupt.), positive = "1")
confMxUp

## ---------------------------------------------  END  ------------------------------------------------------------ ##

