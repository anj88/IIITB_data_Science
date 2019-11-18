
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(GGally)
library(reshape)
library(reshape2)
library(Rcpp)
library(lattice)
library(ROCR)

#assiging data to dataframes
emp_data<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
manager_data<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
general_data<-read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

#structure of the data frames
str(emp_data)    
str(manager_data)    
str(general_data)    
str(in_time)    
str(out_time)    

#Data Preparation #################################
#Renaming the column names in in_time and out_time files
#__________________________________________________________colnames(in_time)[1]<-"EmployeeID"
#__________________________________________________________colnames(out_time)[1]<-"EmployeeID"

#Reordering  the column name for "EmployeeID" in general data 
general_data=general_data[,c(9,1:8,10:24)]

#Checking for duplicates in order to prepare for merge 
sum(duplicated(general_data$EmployeeID))    #Return 0
sum(duplicated(emp_data$EmployeeID)) #Return 0
sum(duplicated(manager_data$EmployeeID))  #Return 0
sum(duplicated(in_time$EmployeeID))         #Return 0
sum(duplicated(out_time$EmployeeID))        #Return 0
#Conclusion -There are no duplicates in EmployeeID field

#checking if there are NA's in EmployeeID
sum(is.na(general_data$EmployeeID))
sum(is.na(emp_data$EmployeeID))
sum(is.na(manager_data$EmployeeID))
sum(is.na(in_time$EmployeeID))
sum(is.na(out_time$EmployeeID))
#returns 0 which means no na's are present in the column EmployeeID ,hence we can merge the data on this


###################################################################################### .
######################################################################################

#treatment of intime and outtime
# we will get average number of hours and total number fo leaves from IN_tim and out_time data


in1<-sapply(in_time, function(x)all(is.na(x)))
in1<-as.data.frame(in1)
in1<-in_time[which(in1$in1=="TRUE"),0]                                       
in1<-as.data.frame(in1)
# 12 obs have been returned 
in_time<-in_time[, colSums(is.na(in_time)) != nrow(in_time)]#removing colums with all NA
#data contains inout time of 261 days
#lets see which values have all NA which indicates there was a holiday.
in_time <- melt(in_time, id.vars = c("X"))
in_time$variable <- str_sub(in_time$variable, start = 2L)
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time)[2] <- "date"
colnames(in_time)[3] <- "in_date_hour"

in_time$in_date_hour = as.POSIXlt(in_time$in_date_hour, format = "%Y-%m-%d %H:%M:%S")
in_time$date = as.POSIXlt(in_time$date, format = "%Y.%m.%d")
in_time$in_hour = as.POSIXlt(in_time$in_date_hour,format =  "%H:%M")


in_time <- in_time[,-3]

str(in_time)

out1<-sapply(out_time, function(x)all(is.na(x)))
out1<-as.data.frame(out1)
out1<-out_time[which(out1$out1=="TRUE"),0]                                       
out1<-as.data.frame(out1)
# 12 obs have been returned 
out_time<-out_time[, colSums(is.na(out_time)) != nrow(out_time)]#removing colums with all NA
#data contains inout time of 261 days
#lets see which values have all NA which indicates there was a holiday.
out_time <- melt(out_time, id.vars = c("X"))
out_time$variable <- str_sub(out_time$variable, start = 2L)
colnames(out_time)[1] <- "EmployeeID"
colnames(out_time)[2] <- "date"
colnames(out_time)[3] <- "out_date_hour"

out_time$out_date_hour = as.POSIXlt(out_time$out_date_hour, format = "%Y-%m-%d %H:%M:%S")
out_time$date = as.POSIXlt(out_time$date, format = "%Y.%m.%d")
out_time$out_hour = as.POSIXlt(out_time$out_date_hour, format = "%H:%M")


out_time <- out_time[,-3]


time_sheet <- merge(in_time,out_time,by = c("EmployeeID","date"))
time_sheet$hour_spent_in_office <- time_sheet$out_hour - time_sheet$in_hour
time_sheet$hour_spent_in_office <- round(time_sheet$hour_spent_in_office, digits = 1)
time_sheet <- time_sheet[,-c(3,4)]
time_sheet$EmployeeID <- as.factor(time_sheet$EmployeeID)
time_sheet$date <- as.Date.factor(time_sheet$date)

attendance <- aggregate(hour_spent_in_office ~ EmployeeID, time_sheet, mean)
attendance$hour_spent_in_office <- round(attendance$hour_spent_in_office, digits = 1)
leave <- aggregate(hour_spent_in_office ~ EmployeeID, time_sheet, function(x) {sum(is.na(x))}, na.action = NULL)
colnames(leave)[2] <- "no_of_leaves"

attendance <- merge(attendance,leave,by = "EmployeeID")

##################################################################################


# merging all the relevant data sets together############################## 


#checking for unique Ids now
length(unique(tolower(emp_data$EmployeeID))) #4100  
length(unique(tolower(manager_data$EmployeeID))) #4410
length(unique(tolower(general_data$EmployeeID)))#4410
length(unique(tolower(in_time$EmployeeID))) #4410
length(unique(tolower(out_time$EmployeeID))) #4410 
length(unique(tolower(time_sheet$EmployeeID))) #4410 
length(unique(tolower(attendance$EmployeeID))) #4410 


#Reconfirming "EmployeeID" is the key
setdiff(emp_data$EmployeeID,emp_data$EmployeeID)#returns 0 ,implies EmployeeID has same value in both datasets
setdiff(emp_data$EmployeeID,general_data$EmployeeID)#returns 0 ,implies EmployeeID has same value in both datasets
setdiff(emp_data$EmployeeID,manager_data$EmployeeID)#returns 0 ,implies EmployeeID has same value in both datasets
setdiff(emp_data$EmployeeID,attendance$EmployeeID)#returns 0 ,implies EmployeeID has same value in both datasets

#Hence EmployeeID can be used to merge data 
  

###########Merging#############################################
merged_data<-merge(emp_data,manager_data,by="EmployeeID" ,all=F)
merged_data<-merge(merged_data ,general_data,by="EmployeeID" ,all=F)
merged_data<-merge(merged_data ,attendance,by="EmployeeID" ,all=F)

#NA Treament#######################################################3 
sum(is.na(merged_data))#111  which is 2 % of total data.Hence can be ignored

#******************************************************************
colSums(is.na(merged_data))
#no of NAs present in Columns
#EnvironmentSatisfaction -25
#$JobSatisfaction-20
#WorkLifeBalance -38
#NumCompaniesWorked-19
#TotalWorkingYears-9
#removing rows having NA
 
merged_data <- merged_data[!(is.na(merged_data$EnvironmentSatisfaction)),]
merged_data <- merged_data[!(is.na(merged_data$JobSatisfaction)),]
merged_data <- merged_data[!(is.na(merged_data$NumCompaniesWorked)),]
merged_data <- merged_data[!(is.na(merged_data$TotalWorkingYears)),]
merged_data <- merged_data[!(is.na(merged_data$WorkLifeBalance)),]
#now left with 4300 observations


str(merged_data)
#Converting the categorical data into factors#############################3
merged_data$EnvironmentSatisfaction<-as.factor(merged_data$EnvironmentSatisfaction)
merged_data$JobSatisfaction<-as.factor(merged_data$JobSatisfaction)
merged_data$WorkLifeBalance<-as.factor(merged_data$WorkLifeBalance)
merged_data$JobInvolvement<-as.factor(merged_data$JobInvolvement)
merged_data$PerformanceRating<-as.factor(merged_data$PerformanceRating)
merged_data$Attrition<-as.factor(merged_data$Attrition)
merged_data$BusinessTravel<-as.factor(merged_data$BusinessTravel)
merged_data$Department<-as.factor(merged_data$Department)
merged_data$EducationField<-as.factor(merged_data$EducationField)
merged_data$Education<-as.factor(merged_data$Education)
merged_data$Gender<-as.factor(merged_data$Gender)
merged_data$JobLevel<-as.factor(merged_data$JobLevel)

merged_data$JobRole<-as.factor(merged_data$JobRole)
merged_data$MaritalStatus<-as.factor(merged_data$MaritalStatus)
merged_data$Over18<-as.factor(merged_data$Over18)
merged_data$StockOptionLevel<-as.factor(merged_data$StockOptionLevel)
merged_data$JobLevel<-as.factor(merged_data$JobLevel)
str(merged_data)

#validating if the  categories are valid and there are no typos##########33
table(factor(merged_data$EnvironmentSatisfaction))
table(factor(merged_data$JobSatisfaction))
table(factor(merged_data$WorkLifeBalance))
table(factor(merged_data$JobInvolvement))
table(factor(merged_data$PerformanceRating))
table(factor(merged_data$Attrition))
table(factor(merged_data$BusinessTravel))
table(factor(merged_data$Department))
table(factor(merged_data$EducationField))
table(factor(merged_data$Education))

table(factor(merged_data$Gender))
table(factor(merged_data$JobLevel))
table(factor(merged_data$JobRole))
table(factor(merged_data$MaritalStatus))
table(factor(merged_data$Over18)) #all employees are over 18 ,we can remove this row .
table(factor(merged_data$StockOptionLevel))
table(factor(merged_data$JobLevel))
table(factor(merged_data$Education))
str(merged_data) #on analysing the EmployeeCount ,its found that it has only one value ,
#hence could be removed
#removing "Over18" ,"EmployeeCount",
table(merged_data$StandardHours) #all entries have 8
# StandardHours also has same variable throughout ,hence removing this as well
merged_data$StandardHours<-NULL
merged_data$Over18<-NULL
merged_data$EmployeeCount<-NULL
#Conclusion:-All unwanted variables have been removed





#***************************EDA categorical VAriables********************************88

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(merged_data, aes(x=(merged_data$EnvironmentSatisfaction),fill=(merged_data$Attrition)))+ geom_bar(position = "fill"), 
          ggplot(merged_data, aes(x=(merged_data$JobSatisfaction),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$WorkLifeBalance),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$JobInvolvement),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$PerformanceRating),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$BusinessTravel),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")  
#People with very low environment  and job satisfaction tend to leave company more.
#People with low work life balance tend to leave company which is understandable.

#Performance rating -3 have high attrition
#TRavel  - hpeople with frequent travel have high atrition that can be because of travel stress and more independency to look other option as on travel

plot_grid(
          ggplot(merged_data, aes(x=(merged_data$EducationField),fill=(merged_data$Attrition)))+ geom_bar(position = "fill"),
          
          ggplot(merged_data, aes(x=(merged_data$Gender),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          
          ggplot(merged_data, aes(x=(merged_data$JobRole),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")  
# No clear attrition based on gender. although it looks like people from human resources have high attrition. 
# reason is unclear at this point of time though


ggplot(merged_data, aes(x=(merged_data$Department),fill=factor(merged_data$Attrition)))+ geom_bar(position = "fill")
#high Attriion is found in Human and resources, which is in line our finding from job role.
#Looks like job role and department can be correlated all together.

plot_grid(ggplot(merged_data, aes(x=(merged_data$MaritalStatus),fill=(merged_data$Attrition)))+ geom_bar(position = "fill"), 
          ggplot(merged_data, aes(x=(merged_data$StockOptionLevel),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$JobLevel),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(merged_data, aes(x=(merged_data$Education),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")  
# Marital status seems to be strong factor behind high attrition. People having single status tends to leave job more.
# there is a reason for it because Single people are more risk taker, mostly independent. if they dont like the job they don't 
#hesitate to look for other option whereas married and divorce people have more responsibilities.
#Stockopton level 0,1 ,Education - Level 3  ('Bachelor') & job leve;l does not reveal any noticable difference in attrition.

###########################Derived Variables###########################################
#1.Please note the variable avg_hrs has been derived from in_time and out_time 
#avg_hrs-  is the average time spent by an employee in the office.
#2. Standard working hours is 8 ,the difference between standard hours and average hours of an employee 
#can give us an idea if the employee works more or less than standard working hours.
merged_data$standard_avg_hrs<-ifelse(merged_data$hour_spent_in_office>8,"greater than 8" ,
                                     ifelse(merged_data$hour_spent_in_office==8,"8hrs","less 8"))
#1 implies employee spends more than or equal to 8 hrs on an average everyday 0 implies he does not spend 8 hours  
merged_data$standard_avg_hrs<-as.factor(merged_data$standard_avg_hrs)
#######################################################################################


ggplot(merged_data, aes(x=(merged_data$standard_avg_hrs),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")
ggplot(merged_data, aes(x=(merged_data$hour_spent_in_office),fill=(merged_data$Attrition)))+ geom_histogram(position = "fill", binwidth = 0.5)
# Attrition is higher in when employee spends greater than 8 hrs
#from which it seems that overworked employees tend to leave company faster.



#######################################################################################################3
#Creating some more graphs to analyse Numeric Varibales ***********************************8
ggplot(merged_data, aes(merged_data$Age))+ geom_histogram(binwidth = 5 ,breaks=seq(18, 60, by = 2), 
                                                          col="red", 
                                                          fill="green", 
                                                          alpha = .2) +facet_grid(~merged_data$Attrition)+theme_bw()

#most of the attrition happens are between 30 and 40 
#note -0 indicates no attrition
ggplot(merged_data, aes(merged_data$DistanceFromHome))+ geom_histogram(binwidth = 10 ,breaks=seq(0, 30, by = 1), 
                                                          col="red", 
                                                          fill="green", 
                                                          alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
#Attrition rate more for the people liver nearer to office


min(merged_data$MonthlyIncome)#10090
max(merged_data$MonthlyIncome)#137756

ggplot(merged_data, aes(merged_data$MonthlyIncome))+ geom_histogram(binwidth = 100 ,breaks=seq(10090, 137756, by = 50000), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
#Lower income people tend to leave company more. Which is expected.

ggplot(merged_data, aes(merged_data$NumCompaniesWorked))+ geom_histogram(binwidth = 10 ,breaks=seq(0, 5, by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$NumCompaniesWorked),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")
#although at first look it looks like people who have not worked in other companies are leaving more. 
#attrition rate is almost similar in other categories too. 
# attrition is high due to the fact that there are more number of employees in 0 num companies worked.


ggplot(merged_data, aes(merged_data$PercentSalaryHike))+ geom_histogram(binwidth = 10 ,breaks=seq(min(merged_data$PercentSalaryHike), max(merged_data$PercentSalaryHike), by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$PercentSalaryHike),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")

# there also attrition percentage is not high however total attritions are high in lower percent salary hike.

ggplot(merged_data, aes(merged_data$TotalWorkingYears))+ geom_histogram(binwidth = 10 ,breaks=seq(min(merged_data$TotalWorkingYears, na.rm = T), max(merged_data$TotalWorkingYears, na.rm = T), by = 2), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$TotalWorkingYears),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")

# people with less number of years have high attrition.

ggplot(merged_data, aes(merged_data$TrainingTimesLastYear))+ geom_histogram(binwidth = 1 ,breaks=seq(1, 5, by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$TrainingTimesLastYear),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")

# Attrition of people who have no training last year has highest attrition rate amongst others with 6 times training having lowest.

ggplot(merged_data, aes(merged_data$YearsAtCompany))+ geom_histogram(binwidth = 10 ,breaks=seq(1, 50, by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()

ggplot(merged_data, aes(x=(merged_data$YearsAtCompany),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")
# people who have spent less number of years at the company tends to leave company more not in terms of number
#but also in terms of percentage, This is evident from graph. 

ggplot(merged_data, aes(merged_data$YearsSinceLastPromotion))+ geom_histogram(binwidth = 10 ,breaks=seq(min(merged_data$YearsSinceLastPromotion, na.rm = T), max(merged_data$YearsSinceLastPromotion, na.rm = T), by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)
ggplot(merged_data, aes(x=(merged_data$YearsSinceLastPromotion),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")
#no clear attrition rate trend as we increase the yearssince last promotion


ggplot(merged_data, aes(merged_data$YearsWithCurrManager))+ geom_histogram(binwidth = 10 ,breaks=seq(min(merged_data$YearsWithCurrManager), max(merged_data$YearsWithCurrManager), by = 1), 
                                                                       col="red", 
                                                                       fill="green", 
                                                                       alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$YearsWithCurrManager),fill=(merged_data$Attrition)))+ geom_bar(position = "fill")

# people whose manager is just appointed recently, attrition rate is high, 
# first year is very critical to the attrition once employees cross that 1 year threshold with a manager, attrition rate lower down by almost half


ggplot(merged_data, aes(as.numeric(merged_data$hour_spent_in_office)))+ geom_histogram(binwidth = 10 ,breaks=seq(min(as.numeric(merged_data$hour_spent_in_office)), max(as.numeric(merged_data$hour_spent_in_office)), by = 1), 
                                                                           col="red", 
                                                                           fill="green", 
                                                                           alpha = .2)+facet_grid(~merged_data$Attrition)+theme_bw()
ggplot(merged_data, aes(x=(merged_data$hour_spent_in_office),fill=(merged_data$Attrition)))+ geom_histogram(position = "fill", binwidth = 0.5)
# this is also very evident from the second plot that as soon as average number of hour increase from 8 hours,
#people feel overworked and tend to leave company more.

################Outlier removal ############################################################################3
#Analysing numeric values -Checking for outliers 
boxplot(merged_data$ Age) # no outlier
boxplot(merged_data$DistanceFromHome)
boxplot(merged_data$MonthlyIncome)#outliers are present  in MonthlyIncome
#analysing the MonthlyIncome
quantile(merged_data$MonthlyIncome,seq(0,1,0.01))
merged_data$MonthlyIncome[which(merged_data$MonthlyIncome>137756.0)]<-137756.0

boxplot(merged_data$PercentSalaryHike) # no outlier


boxplot(merged_data$TotalWorkingYears) #outliers are present  ,hence analysing and removing outliers 
quantile(merged_data$TotalWorkingYears,seq(0,1,0.01))
merged_data$TotalWorkingYears[which(merged_data$TotalWorkingYears>28)]<-28

boxplot(merged_data$TrainingTimesLastYear) #outliers are present  ,hence analysing and removing outliers 
quantile(merged_data$TrainingTimesLastYear,seq(0,1,0.01))
merged_data$TrainingTimesLastYear[which(merged_data$TrainingTimesLastYear>4)]<-4


boxplot(merged_data$YearsAtCompany)#outliers are present  ,hence analysing and removing outliers 
quantile(merged_data$YearsAtCompany,seq(0,1,0.01))
merged_data$YearsAtCompany[which(merged_data$YearsAtCompany>17.08)]<-17.08


boxplot(merged_data$YearsSinceLastPromotion)#outliers are present  ,hence analysing and removing outliers 
quantile(merged_data$YearsSinceLastPromotion,seq(0,1,0.01))
merged_data$YearsSinceLastPromotion[which(merged_data$YearsSinceLastPromotion>8)]<-8



boxplot(merged_data$YearsWithCurrManager)#outliers are present  ,hence analysing and removing outliers 
quantile(merged_data$YearsWithCurrManager,seq(0,1,0.01))
merged_data$YearsWithCurrManager[which(merged_data$YearsWithCurrManager>13)]<-13



boxplot(as.numeric(merged_data$hour_spent_in_office)) 
quantile(as.numeric(merged_data$hour_spent_in_office),seq(0,1,0.01))
merged_data$hour_spent_in_office[which(merged_data$hour_spent_in_office>10.600 )]<-10.600
#All outliers have been removed  

#coorelation among numeric variables
#Coorelation 

Numeric<-merged_data[,c(7,11,18,19,20,22,23,24,25,26,27,28)]
Numeric$hour_spent_in_office <- as.numeric(Numeric$hour_spent_in_office)
Categorical_data<-merged_data[,-c(7,11,18,19,20,22,23,24,25,26,27,28)] #this will used later to create dummy 
#variables
# converting categorical attributes to factor
Categorical_data<- data.frame(sapply(Categorical_data, function(x) factor(x)))
Categorical_data<- Categorical_data[,-1] # removing Employee ID as that is irrelvant for prediction


Corelation<-cor(Numeric)
Corelation<-round(Corelation,2)

#highest coorelation is found between yearsatcompany and yearswith manager  

#Attrition Rate *************************************************************************
merged_data$Attrition<-ifelse(merged_data$Attrition=="Yes",1,0)
attrition<-sum(merged_data$Attrition)/nrow(merged_data)
attrition<-attrition*100
#16.20 % attrition rate
#******************************************************************************************8
#Normalising continous variables
merged_data$ Age<-scale(merged_data$ Age)
merged_data$DistanceFromHome<-scale(merged_data$DistanceFromHome)
merged_data$MonthlyIncome<-scale(merged_data$MonthlyIncome)
merged_data$Permerged_data$YearsAtCompanycentSalaryHike<-scale(merged_data$PercentSalaryHike)
merged_data$TotalWorkingYears<-scale(merged_data$TotalWorkingYears)
merged_data$TrainingTimesLastYear<-scale(merged_data$TrainingTimesLastYear)
merged_data$YearsAtCompany<-scale(merged_data$YearsAtCompany)
merged_data$YearsSinceLastPromotion<-scale(merged_data$YearsSinceLastPromotion)
merged_data$YearsWithCurrManager<-scale(merged_data$YearsWithCurrManager)

##############################################################################################3
#creating dummy variables 
dummies<- data.frame(sapply(Categorical_data, 
                            function(x) data.frame(model.matrix(~x-1,data =Categorical_data))[,-1]))
################################################################################################
# Final dataset
attrition_final<-cbind(merged_data[,c(7,11,18,19,20,22,23,24,25,26,27,28)],dummies)
View(attrition_final) #4099 obs. of  59 variables
################################################################################################
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(attrition_final$Attrition, SplitRatio = 0.7)

train = attrition_final[indices,]

test = attrition_final[!(indices),]

########################################################################
########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 1976.1....58 coeff..nullDev 2542.5...resDev 1860.1
vif(model_1)
# Stepwise selection (automatically selecting a reduced number of predictor variables for building the best performing logistic regression model)
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#removing EducationField.xLife.Sciences because of high VIF
#it has been high VIF because of since model_1


model_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                hour_spent_in_office + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 +  
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_3)

vif(model_3)

#removing hour_spent_in_office because of high VIF and high insignificance

model_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Education.x5 +  
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_4)

vif(model_4)

# good decision to remove hours_spent_office as AIC did not hcange much.

#removing BusinessTravel.xTravel_Rarely  because of high VIF of 3.557704 and high insignificance, it has been high VIF since model_1


model_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_5)

vif(model_5)

#both WorkLifeBalance.x3 and WorkLifeBalance.x2 but WorkLifeBalance.x2 has lower significance than WorkLifeBalance.x3
# seems both variable is highly correlated. We can check that 
cor(attrition_final$WorkLifeBalance.x2,attrition_final$WorkLifeBalance.x3)
#correlation is -0.68 which shows high collinearity 

# we can remove worklife.balance.x2 from the data because of collinearity



#removing WorkLifeBalance.x2 because of high VIF of 3.035984, it has been high VIF since model_1


model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_6)

vif(model_6)


#now all variable have VIF below 2.5
#looking at high insignificance of 0.46  we will remove EducationField.xMarketing

model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_7)

vif(model_7)

#looking at high insignificance of 0.453  we will remove EducationField.xMedical

model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_8)

vif(model_8)

#looking at high insignificance of 0.171  we will remove WorkLifeBalance.x4

model_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_9)

vif(model_9)


#looking at high insignificance of 0.157  we will remove EducationField.xTechnical.Degree

model_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                BusinessTravel.xTravel_Frequently + 
                Education.x5 +  
                EducationField.xOther + 
                JobLevel.x5 + JobRole.xLaboratory.Technician + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
              family = "binomial", data = train)

summary(model_10)

vif(model_10)

#looking at high insignificance of 0.184  we will remove EducationField.xOther

model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Education.x5 +  
                 JobLevel.x5 + JobRole.xLaboratory.Technician + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_11)

vif(model_11)

#looking at high insignificance of 0.121  we will remove JobLevel.x5

model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Education.x5 +  
                 JobRole.xLaboratory.Technician + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_12)

vif(model_12)

#looking at high insignificance of 0.083895  we will remove JobRole.xLaboratory.Technician

model_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Education.x5 +
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_13)

vif(model_13)

#looking at high insignificance of 0.071647  we will remove Education.x5

model_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_14)

vif(model_14)


#looking at high insignificance of 0.0866  we will remove JobRole.xResearch.Scientist

model_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_15)

vif(model_15)

#looking at high insignificance of 0.045264  we will remove StockOptionLevel.x1

model_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_16)

vif(model_16)

#looking at high insignificance of 0.011240  we will remove JobRole.xResearch.Director

model_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_17)

vif(model_17)

#looking at high insignificance of 0.017454  we will remove TrainingTimesLastYear

model_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_18)

vif(model_18)

#looking at high insignificance of 0.015578  we will remove JobRole.xSales.Executive

model_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_19)

vif(model_19)

#looking at high insignificance of 0.002215  we will remove JobSatisfaction.x2

model_20<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_20)

vif(model_20)

#looking at high insignificance of 0.052719  we will remove JobSatisfaction.x3

model_21<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_21)

vif(model_21)

#Although all the variables are loking decently significant. however worklifebalance.x3 
#is on the verge of insignificance(p = 0.000826) lets check removing that
# wel will keep removing it until we get P value very low(~10e-4) and use all commbinations to see which gives us better result.

model_22<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director +
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_22)

vif(model_22)

# we will remove JobRole.xManufacturing.Director because of slight insignificance of 0.000186

model_23<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4 + 
                 BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle + standard_avg_hrs.xgreater.than.8, 
               family = "binomial", data = train)

summary(model_23)

vif(model_23)
# removing the variable increased AIC drastically . it not advisable to remove JobRole.xManufacturing.Director
# our final model is model_22

final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-26])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition,test_actual_attrition)

# Let's use the probability cutoff of 40%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_pred_attrition,test_actual_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#########################################################################################
# Let's Choose the cutoff value. 
# we would be using optimal cut-off by using the value which has equal values for accuracy, specificity, and sensitivity
# because we cannot treat any type of error less. becaue false yes 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
summary(test_pred)

# Creating cutoff values from 0.000881 to 0.874239 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.001,.874,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
}


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

#chosen cutoff is 0.159

# Let's choose a cutoff value of 0.159 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.159, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #accuracy - 74.42%
sens #Sensitivity - 75.11%
spec #Specificity - 74.28%

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart



lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

# as per gain table, top 4th decile contain 78.9% of attrition.
# till 4th decile, lift is more than 1.97

Attrition_decile$non_attrition <- Attrition_decile$total - Attrition_decile$totalresp
Attrition_decile$cum_non_attrition <- cumsum(Attrition_decile$non_attrition)
Attrition_decile$cum_non_attrition_percent <- (Attrition_decile$cum_non_attrition/sum(Attrition_decile$non_attrition))*100
Attrition_decile$KS_statistic_percent <- Attrition_decile$Gain - Attrition_decile$cum_non_attrition_percent

#KS-statistics is 52.7% in 3rd decile which shows very good model prediction. 
