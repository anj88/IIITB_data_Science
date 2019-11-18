#installing neccessary libraries
#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#loading the packages
library("caret")
library("kernlab")
library("dplyr")
library("readr")
library("ggplot2")
library("gridExtra")
#setting the directory
setwd("E:/Blue Prism/upgrad/course 4/svm/SVM Dataset")
train_data<-read.csv("mnist_train.csv", header = F) 
# 60000 obs of 785 variables
test_data<-read.csv("mnist_test.csv", header = F)
# 10000 obs of 785 variables
names(test_data) #to know column names
names(train_data)  
#To know if column names are same for both data sets
coltrian<-names(train_data)
coltest<-names(test_data)
setdiff(coltrian ,coltest) #returns 0

#checking NA value
sum(is.na(train_data)) #returns 0  
sum(is.na(test_data)) #retunrs 0 

#checking duplicates
which(duplicated(train_data)) #returns 0 which means no duplicates
which(duplicated(carprice)) #returns 0 ,ie means no duplicates

str(train_data) # Analysing Column V1
train_data$V1<-as.factor(train_data$V1) #converting V1 into factors 
levels(train_data$V1) #This denotes digits from 0-9

test_data$V1<-as.factor(test_data$V1)
levels(test_data$V1)  #digts from 0-9



#choosing 10% data for modelling  ie 6000 observations  
set.seed(100)
train_data_index<-sample(1:nrow(train_data), 0.10*nrow(train_data))
train_data_1<-train_data[train_data_index, ]
# we are using the complete test data set for evaluation.


#modelling
#1.Using Linear Kernel
Model_LK <- ksvm(V1~ ., data = train_data_1, scale = FALSE, kernel = "vanilladot")
Eval_LK<- predict(Model_LK, test_data)

#Confusion matrix building
confusionMatrix(Eval_LK,test_data$V1) #Accuracy : 0.9126
#There are variations in Senstivity and Specificity for certain classes
####################################################################################333

#Using RBF Kernel 
Model_RBF <- ksvm(V1~ ., data = train_data_1, scale = FALSE, kernel = "rbfdot")
Model_RBF  #we get sigma as sigma =  1.63176955642165e-07 
Eval_RBF<- predict(Model_RBF, test_data)

#Confusion Matrix
confusionMatrix(Eval_RBF,test_data$V1)
#Accuracy has increased to 0.952    which indicates that this is better than ;linear kernel ,hence we need to tune 
#Sensitivity and  Specificity has shown some improvement as well 

##########################################################################################333
#HYPER-PARAMETER AND CROSS VALIDATION  in RBF model
#setting up the grid 
Train_CTRL<-trainControl(method="cv", number=5, verboseIter = TRUE)
metric_no<-"Accuracy"
set.seed(7)
#trying different values for sigma 
#grid_no<-expand.grid(.sigma =c(0.025 ,0.05), .C=c(0.1,0.5,1,2)) #the accuracy is very less =0.10 approx
#using sigma values close to what we got for rbf model
grid_no<-expand.grid(.sigma =c(0.00000026,0.00000026 ,0.00000030), .C=c(2,2.5,2.75))

#Performing 5 fold cross validation
fit.svm <- train(V1~., data=train_data_1, method="svmRadial", metric=metric_no, 
                 tuneGrid=grid_no, trControl=Train_CTRL)

print(fit.svm)
#Best values have been obtained at sigma =3.0e-07 and C=2.00 ,Accuracy=0.9640029

plot(fit.svm)


#Validating results on test data
evaluate_test1<- predict(fit.svm, test_data)
confusionMatrix(evaluate_test1, test_data$V1)
#Accuracy of model when tried on test data is 0.963

#using linear kernel we got accuracy as 0.9126 but using rbf we got accuracy as 0.96