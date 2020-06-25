# Project 1#
setwd("C:/Users/abayas/Desktop/R proj/project 1")
#-----------------------------Data Preprocessing------------------------------#

Data <- read.csv("census-income.csv" )

#------------------------------------------------------------------------------#
library(dplyr)

# replace all missing values wth NA
## Exclude the missing observations
set.seed(123)

str(Data)
complete.cases(Data)

x <- Data[complete.cases(Data), ]
x

Data <- x


complete.cases(Data)
which(complete.cases(Data)) -> D

which(!complete.cases(Data))

Data1 <- na.omit(Data)

Data1

D
#------------------------------------------------------------------------------------#
#data Manupulation 
#-------------------------------------------------------------------------------------#


#extract Education coloumn and store it in to census_ed. 

census_ed <- data.frame(Data$education)

#extract all colouna from age to relationship and store it in census_seq

census_seq <- data.frame(Data$age, Data$workclass,Data$fnlwgt, Data$education, Data$education.num, Data$marital.status, Data$occupation, Data$relationship)

census_seq
census_ed 
# OR below way 
C<- data.frame(Data[ ,1:8])
C

#extract coloun number 5, 8  and 11 and store it in to census_col

census_col <- data.frame( Data[, c(5,8,11)])

census_col

# Extract all Male employee who work in state goverment and store it in state_gov


GovMale <- filter(Data,workclass == " State-gov" & sex == " Male")
GovMale
View(GovMale)


#extract all 39 years old who either have bachelor degree or native of USA store this in census_us

census_us <- filter(Data, age == 39 &  education == " Bachelors" |  native.country == " United-States")
census_us


#extract 200 random rws from and stroe them in census_200

census_200 <- sample_n(Data, 200)
census_200
View(census_200)

#get count of diffrent levels of worklevel

count(Data, workclass )

#calculate mean of capital gain and group according to work class;

summarise( group_by(Data,workclass ), mean( capital.gain))
summarise(group_by(Data, sex), mean(capital.gain))
summarise(group_by(Data,native.country),mean(capital.gain) )


#================================================================================================================#
#Data Visualisation
#================================================================================================================#

#build bar plot for " relationship" column and fill the bars according to the "race" coloumn
library("tidyverse")
library(ggplot2)
library(tidyverse)


count(Data, race,sex) -> c

C1 <- data.frame(C)

C1

View(C1)

DataP <- C1


barplot(table(Data$sex, Data$race), beside = TRUE, cex.axis = 0.7,main = " Distribution of Relationships by Sex")

ggplot(Data , aes(x= race, fill = sex), position_dodge(1)) + geom_bar(position="dodge") -> GGPLOT

GGPLOT

#v.	Set the title of plot to be 'Distribution of Relationships by Sex"

GGPLOT + ggtitle( " Distribution of Relationships by Sex") -> GGPLOT

#i.	Set x-axis label to 'Categories of Relationships'
#ii.	Set y-axis label to 'Count of Categories'

GGPLOT + xlab(" Categories of Relationships") + ylab("Count of Categories")


#=========================================================================================#
#b)	Build a Histogram for the "age" column with number of bins equal to 50.

#i)	Fill the bars of the histogram according to yearly income column i.e., "X"
#ii)	Set the title of the plot to "Distribution of Age". 
#iii)Set the legend title to "Yearly income".
#iv) Set the theme of the plot to black and white.
#=========================================================================================#

Histogram <- ggplot( Data, aes(x= age, fill =YearlyIncome)) + geom_histogram(bins = 50)
Histogram

ggplot( Data, aes(x= age)) + geom_histogram(bins = 50) + ggtitle("Distribution of Age") + theme(legend.title = element_text("Yearly income") ) + theme_bw()


#==========================================================================================#

#c)	Build a scatter-plot between "capital.gain" and "hours.per.week". Map "capital.gain" on the x- axis and "hours.per.week" on the y-axis.

plot(Data$capital.gain, Data$hours.per.week)

ggplot(Data, aes(x=capital.gain, y=hours.per.week, col= "blue" )) + geom_point()


#i)	Set the transparency of the points to 40% and size as 2.
ggplot(Data, aes(x=capital.gain, y=hours.per.week, col= "blue",  size = 20 , alpha = 0.4)) + geom_point()

#ii)	Set the color of the points according to the "X" (yearly income) column. *************************************************************************************************************need help 

ggplot(Data, aes(x=capital.gain, y=hours.per.week, col = "blue" , size = 20 , alpha = 0.4)) + geom_point( colour="#FF9999")

#iii)Set the x-axis label to "Capital Gain", y-axis label to "Hours per Week", title to "Capital Gain vs Hours per Week by Income", 
#and legend label to "Yearly Income".


ggplot(Data, aes(x=capital.gain, y=hours.per.week, col = "blue" , size = 20 , alpha = 0.4)) + geom_point( colour="#FF9999") + xlab("Capital Gain ") + ylab("ours per Week") + ggtitle("Capital Gain vs Hours per Week by Income")


#--------------------------------------------------------------------------------------------#

#======================================================================================================#
#d)	Build a box-plot between "education" and "age" column.Map "education" on the x-axis and
#"age" on the y-axis

ggplot(Data, aes(x= education, y =age )) + geom_boxplot()

#i)	Fill the box-plots according to the "sex" column.

ggplot(Data, aes(x= education, y =age, fill =  sex )) + geom_boxplot()

#ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(Data, aes(x= education, y =age, fill =  sex )) + geom_boxplot(size = .5) + ggtitle("Box-Plot of age by Education and Sex")

#===========================================================================================================#
#4.	Linear Regression:
#===============================================================================================================#

#a)	Build a simple linear regression model as follows:

# i)	Divide the dataset into training and test sets in 70:30 ratio.
library(dplyr)
library(caTools)

ST  <- sample.split(Data$hours.per.week, SplitRatio = 0.70)

Training <- subset(Data, ST == T)
Test <- subset(Data, ST == F)

nrow(Training)
nrow(Test)


#ii)	Build a linear model on the test set where the dependent variable is
#"hours.per.week" and independent variable is "education.num".

Linear_model <- lm(hours.per.week ~ education.num, data = Test )
summary(Linear_model)



#iii)	Predict the values on the train set and find the error in prediction. iv)Find the root-mean-square error (RMSE).
Predicted_model <- predict(Linear_model, newdata = Training)
summary(Predicted_model)



#education.num

final_data <- cbind(Actual = Training$hours.per.week, Predicted = Predicted_model )

class(final_data)
FD<- as.data.frame(final_data)

Error <- FD$Actual - FD$Predicted
FD<- cbind(FD, Error)

head(FD)

# root mean square :
RMS <- sqrt(mean(FD$Error)^2)

RMS


# mdel plot:
library(ggplot2)

ggplot( Data, aes(x = hours.per.week, y = education.num )) + geom_point(size = 1)+geom_smooth(method = "lm")


#plot of predicted Vs error

ggplot(FD, aes(x = Predicted, y =Error)) + geom_point()

qqnorm(FD$Error)
#================================================================================================================#
#================================================================================================================#
#5.	Logistic Regression:
library(InformationValue)
library(caTools)  
#  a)	Build a simple logistic regression model as follows:
#  i)	Divide the dataset into training and test sets in 65:35 ratio.

ST2 <- sample.split(Data$YearlyIncome, SplitRatio = 0.65 )

TST <- subset( Data, ST2== F)
TRN <- subset(Data, ST2 == T)

#ii)	Build a logistic regression model where the dependent variable is "X"(yearly income) and independent variable is "occupation".

Data$fnlwgt
Data$occupation

Log_Model <- glm( YearlyIncome ~ occupation, data = Data, family = "binomial")
summary(Log_Model)


#iii)	Predict the values on the test set.

Predictlog <- predict(Log_Model,newdata = TST, type = 'response')
Predictlog
summary(Predictlog)
range(Predictlog)

ConfusionMatrix <- table( TST$YearlyIncome, Predictlog >0.1)

#iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
library(ROCR)

pred_log <- prediction( Predictlog, TST$YearlyIncome)

performance(pred_log,"acc")->acc
plot(acc)


performance(pred_log, "tpr", "fpr") -> ROC_curve

ROC_curve
plot(ROC_curve , col = "orange", main = " ROC CURVE OF LOGISTIC REGRESSION")

#Plot AUC

table(TST$YearlyIncome, Predictlog > 0.28 )

performance(pred_log,"auc")-> AUC
AUC
summary(AUC)
AUC

#An object of class "performance"
#Slot "x.name":
#  [1] "None"

#Slot "y.name":
#  [1] "Area under the ROC curve"

#Slot "alpha.name":
#  [1] "none"

#Slot "x.values":
#  list()

#Slot "y.values":
#  [[1]]
#[1] 0.7177789


#Slot "alpha.values":
# list()

#===============================================================================================================================#
#===============================================================================================================================#
#6.	Decision Tree:
  
library(tree)

#a)	Build a decision tree model as follows:
  
# i)	Divide the dataset into training and test sets in 70:30 ratio.

set.seed(123)
STT <- sample.split(Data$YearlyIncome, SplitRatio = 0.70)
  
D_TRN <- subset(Data, STT == T)   
D_TST <- subset(Data, STT == F)  

count(D_TST)
nrow(D_TRN)  
nrow(D_TST)  

#ii)	Build a decision tree model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables.

Tree_Model1 <- tree(YearlyIncome ~ age + workclass + fnlwgt + education + education.num +marital.status+
                      occupation +relationship + race+sex+capital.gain +capital.loss+sex, data = D_TRN )

Tree_Model1

#TM2 <- tree(YearlyIncome ~ hours.per.week+ native.country, data = D_TRN) : unable to add this two coloumn.

#iii)	Plot the decision tree.

plot(Tree_Model1) 
text(Tree_Model1)

#iv)	Predict the values on the test set.
set.seed(33)
D_predict <- predict(Tree_Model1, newdata = D_TST, type = "class" )

D_predict



table(D_TST$YearlyIncome, D_predict) -> CM


v)	Build a confusion matrix and calculate the accuracy.
table(D_TST$YearlyIncome, D_predict) -> CM

D_predict
<=50K  >50K
<=50K   6442   354
>50K    1080  1172

Accuracy <- (6442+1172 )/(6442+1172+354+1080 )
Accuracy

library(ROCR)

D_plog <- 

#-------------------------------------------------------------------------------------------------------------------------------#

#===============================================================================================================================#
#RANDOM FOREST
library(randomForest)
library(dplyr)
library(ggraph)
library(igraph)

#  a)	Build a random forest model as follows:
  
# i)	Divide the dataset into training and test sets in 80:20 ratio.

R_ST <- sample.split(Data$YearlyIncome, SplitRatio = 0.80 )

table(R_ST)
R_TST <- subset( Data, R_ST == F)
R_TRN <- subset(Data, R_ST == T)


#ii)	Build a random forest model where the dependent variable is "X"(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
Data
R_Model1<- randomForest(YearlyIncome ~  sex +capital.gain+ capital.loss+ hours.per.week+ native.country+ age+workclass+fnlwgt+education+education.num +marital.status+occupation +relationship +race, data =  R_TRN,ntree = 300)

R_Model1

importance(R_Model1)
varImpPlot(R_Model1)
varImpPlot(R_Model1, col="palegreen4")

#iii)	Predict values on the test set

R_predict <- predict(R_Model1, newdata = R_TST, type = "class")
head(R_predict)

#confusion matroix
table(R_TST$YearlyIncome, R_predict)

#iv)	Build a confusion matrix and calculate the accuracy

table(R_TST$YearlyIncome, R_predict) 

table(R_TST$YearlyIncome, R_predict) -> R_CM

R_predict
<=50K  >50K
<=50K   4253   278
>50K     530   972

Accuracy_R <- (4253+972)/(4253+972+530+278)

Accuracy_R

plot(R_Model1)
text(R_Model1)

#============================================================================================================================#










