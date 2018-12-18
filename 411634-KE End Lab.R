
#Loading 'train' & 'test' data files of Big Mart Sales Prediction downloaded from the Big MArt Sales Practice Problem competition from Analytics Vidhya
train<-read.csv("C:/Users/Phani Rohitha Kaza/Desktop/BigMart/Train.csv")
test<-read.csv("C:/Users/Phani Rohitha Kaza/Desktop/BigMart/Test.csv")

#Data Description
str(train)

#Summary of train data
summary(train)

#Pre-processing the train data
table(train[,3])
fattrain<- train[,3]

#Recoding the Low Fat variables to 1 & Regular variables to 2 of Item_Fat_Content Attribute in train data
library(car)
fattrainencode <-  recode(fattrain,"'LF'=1;'low fat'=1;'Low Fat'=1;'reg'=2;'Regular'=2" )
table(fattrainencode)
trainclass<- train['Item_Outlet_Sales']

#Recoding the Tier 1 variables to 1,Tier 2 variables to 2 & Tier 3 variables to 3 of Outlet_Location_Type Attribute in train data
table(train[,10])
outlocationtypetrainencode<- recode(train[,10],"'Tier 1'=1; 'Tier 2'=2; 'Tier 3'=3")
table(outlocationtypetrainencode)

#Recoding the High variables to 1,Medium variables to 2,Small variables to 3 & Empty Values(' ') are replaced by 'NA' of Outlet_Size Attribute in train data
table(train[,9])
outsizetrainencode <- recode(train[,9],"'High'=1; 'Medium'=2;  'Small'=3; ''=NA")
table(outsizetrainencode)

#Converting the categorical type variables to numeric type
table(train[,5])
typetrainnum <- as.numeric(train[,5])
table(typetrainnum)

table(train[,7])
identrainnum <- as.numeric(train[,7])
table(identrainnum)

table(train[,8])
outoldtrainnum<-2018-as.numeric(train[,8])
table(outoldtrainnum)

table(train[,11])
outtypenum <- as.numeric(train[,11])
table(outtypenum)

#Combine the modified attributes into a frame
trainready <- data.frame(cbind(train[,2],fattrainencode,train[,4],typetrainnum,train[,6],identrainnum,outoldtrainnum,outsizetrainencode,outlocationtypetrainencode,outtypenum))

#Finding whether the column contains 'NA' values in train data
sum(is.na(trainready[,1] ))
sum(is.na(fattrainencode))
sum(is.na(trainready[,3]))
sum(is.na(typetrainnum))
sum(is.na(trainready[,5]))
sum(is.na(identrainnum))
sum(is.na(outoldtrainnum))
sum(is.na(outsizetrainencode))
sum(is.na(outlocationtypetrainencode))
sum(is.na(outtypenum))

#Some the values of 1,8 columns in trainready are not available;they are replaced by mean and median of the attributes respectively
trainready[is.na(trainready[,1]), 1] <- mean(trainready[,1], na.rm = TRUE)
trainready[is.na(trainready[,8]), 8] <- median(trainready[,8], na.rm = TRUE)
trainreadyfinal<-data.frame(cbind(trainready[,1],fattrainencode,train[,4],typetrainnum,train[,6],identrainnum,outoldtrainnum,trainready[,8],outlocationtypetrainencode,outtypenum))

#Pre-processing the test data
summary(test)
table(test[,3])
fattrain<- test[,3]

#Recoding the Low Fat variables to 1 & Regular variables to 2 of Item_Fat_Content Attribute in test data
library(car)
fattrainencode <-  recode(fattrain,"'LF'=1;'low fat'=1;'Low Fat'=1;'reg'=2;'Regular'=2" )
table(fattrainencode)

#Recoding the Tier 1 variables to 1,Tier 2 variables to 2 & Tier 3 variables to 3 of Outlet_Location_Type Attribute in test data
table(test[,10])
outlocationtypetrainencode<- recode(test[,10],"'Tier 1'=1; 'Tier 2'=2; 'Tier 3'=3")
table(outlocationtypetrainencode)

#Recoding the High variables to 1,Medium variables to 2,Small variables to 3 & Empty Values(' ') are replaced by 'NA' of Outlet_Size Attribute in test data
table(test[,9])
outsizetrainencode <- recode(test[,9],"'High'=1; 'Medium'=2;  'Small'=3; ''=NA")
table(outsizetrainencode)

#Converting the categorical type variables to numeric type
table(test[,5])
typetrainnum <- as.numeric(test[,5])
table(typetrainnum)

table(test[,8])
outoldtrainnum<-2018-as.numeric(test[,8])
table(outoldtrainnum)

table(test[,7])
identrainnum <- as.numeric(test[,7])
table(identrainnum)

table(test[,11])
outtypenum <- as.numeric(test[,11])
table(outtypenum)

#Combine the modified attributes into a frame
testready <- data.frame(cbind(test[,2],fattrainencode,test[,4],typetrainnum,test[,6],identrainnum,outoldtrainnum,outsizetrainencode,outlocationtypetrainencode,outtypenum))

#Finding whether the column contains 'NA' values in test data
sum(is.na(testready[,1] ))
sum(is.na(fattrainencode))
sum(is.na(testready[,3]))
sum(is.na(typetrainnum))
sum(is.na(testready[,5]))
sum(is.na(identrainnum))
sum(is.na(outoldtrainnum))
sum(is.na(outsizetrainencode))
sum(is.na(outlocationtypetrainencode))
sum(is.na(outtypenum))

#Some the values of 1,8 columns in testready are not available;they are replaced by mean and median of the attributes respectively
testready[is.na(testready[,1]), 1] <- mean(testready[,1], na.rm = TRUE)
testready[is.na(testready[,8]), 8] <- median(testready[,8], na.rm = TRUE)
testreadyfinal<-data.frame(cbind(testready[,1],fattrainencode,test[,4],typetrainnum,test[,6],identrainnum,outoldtrainnum,testready[,8],outlocationtypetrainencode,outtypenum))

#Plots
#Boxplots
par(mfrow=c(2,2))
boxplot(train$Item_Visibility,horizontal = TRUE,main="Item Visibility",col="Green")
boxplot(train$Item_Weight,horizontal = TRUE,main="Item Weight",col="Pink")
boxplot(train$Item_MRP,horizontal = TRUE,main="Item MRP",col="Yellow")
boxplot(train$Item_Outlet_Sales,horizontal = TRUE,main="Item Output Sales",col="Blue")

#Histograms
par(mfrow=c(2,2))
hist(train$Item_Visibility,main="Item Visibility",col="Blue",xlab = "Visiblity")
hist(train$Item_Weight,main="Item Weight",col="Yellow",xlab = "Weight")
hist(train$Item_MRP,main="Item MRP",col="Pink",xlab = "MRP")
hist(train$Item_Outlet_Sales,main="Item Output Sales",col="Green",xlab = "Outlet Sales")

#Scatter Plot
scatterplotMatrix(~train$Item_Visibility+train$Item_Weight+train$Item_MRP+train$Item_Outlet_Sales,col="Purple")

#Correalation Matrix
Corr_Matrix <- train[,c(2,4,6,8,12)]
cor(Corr_Matrix)

#Visualization of Correlation Matrix
library(corrplot)
corrplot(corr=cor(Corr_Matrix),method="ellipse")

#Random Forest Classification
library(randomForest)
modelrf<-randomForest(trainreadyfinal,train[,12],importance=TRUE, ntree=100)
Item_Outlet_Sales<-predict(modelrf,testreadyfinal)
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(Item_Outlet_Sales))
write.csv(soln,file="C:/Users/Phani Rohitha Kaza/Desktop/BigMart/SampleSubmission.csv")
#Score:1171.81

#Decision Tree Classification
library(caret)
library(onehot)
library(ggplot2)
library(Metrics)
library(tree)
library(rpart)
library(h2o)

dtc = rpart.control(maxdepth = 4,minsplit = 20,minbucket = 7) 
dt = rpart(train[,12]~. , data = trainreadyfinal,parms =  c(split = "gini"),control = dtc)
Item_Outlet_Sales = predict(dt,newdata = testreadyfinal)
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(Item_Outlet_Sales))
write.csv(soln,file="C:/Users/Phani Rohitha Kaza/Desktop/BigMart/SampleSubmission1.csv")
#Score:1174.33


dtc = rpart.control(maxdepth = 4,minsplit = 20,minbucket = 7) 
dt = rpart(train[,12]~. , data = trainreadyfinal,parms =  c(split = "entropy"),control = dtc)
Item_Outlet_Sales = predict(dt,newdata = testreadyfinal)
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(Item_Outlet_Sales))
write.csv(soln,file="C:/Users/Phani Rohitha Kaza/Desktop/BigMart/SampleSubmission3.csv")
#Score:1273.88

#Linear Regression with multiple variables
lr =lm(train[,12]~ . ,data =trainreadyfinal)
Item_Outlet_Sales <- predict(lr,newdata = testreadyfinal)
soln<-cbind(test[,'Item_Identifier'],test[,'Outlet_Identifier'],data.frame(Item_Outlet_Sales))
write.csv(soln,file="C:/Users/Phani Rohitha Kaza/Desktop/BigMart/SampleSubmission2.csv")
#Score:1265.90

