#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#																BFS DOMAIN - CAPSTONE PROJECT
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# 1.Business Understanding and Problem Statement :
#	a. Identify the strategy to mitigate credit risk to acquire the right customers doing acquisition risk analytics.
#	b. Help CredX identify the right customers using predictive models. 
#	c. Determine the factors affecting credit risk,and assess the financial benefit of your project.

# 2.Data Understanding 
#	a. Demographic 			Information provided by the applicants at the time of credit card application
#	b. Credit bureau data	Information from the credit bureau containing the customer the repayment status.
	
# 3.Data cleaning and preparation
#	a. Create a master file with all the relevant variables and conduct the necessary data quality checks and cleaning. 
#	b. Identify the significant varibales using WOE and IV analysis and also impute missing values from the data
#	c. Create a seperate file(e.g. woe_data) by replace the actual/missing values of all the variables by the corresponding WOE value for further analysis.  
# d. The variables in the CB data are zero and credit card utilisation is missing indicating that there is a no-hit in the credit bureau. 
#	e. Data indicating the missing of credit card utilisation info indicates applicant does not have any other credit card.

# 4.Model Building
# 5.Model evaluation
#	a. Evaluate the models using relevant metrics and report the results.
#      Discriminatory power 	: ROC curve, sensitivity, rank-ordering, KS statistic, Gini Index etc.
#	   Accuracy/Calibration		: The difference between the predicted and actual odds of default. Odds & Probability 
#	b. As a part of model validation, predict the likelihood of default for the rejected  candidates as well
#	c. Assess whether the results correspond to your expectations.
	
# 6.Application scorecard
#	a. Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.  
#	b. For the rejected population, calculate the application scores and assess the results.  
#	c. Compare the scores of the rejected population with the approved candidates and comment on the observations.
#	d. On the basis of the scorecard, identify the cut-off score below which you would not grant credit cards to applicants.
	
# 7.Assessing the financial benefit of your project
#	a. You need to assess and explain the potential financial benefit of your project to the management of the bank.
#	b. From a P&L perspective, identify the metrics you are trying to optimise,explain (in simple terms) how the analysis and the model works, and share
#	   the results of the model. 

# 8.Finally, assess the financial benefit of the model and report the following:
#	a. The implications of using the model for auto approval or rejection, i.e. how many applicants on an average would the model automatically approve or reject
#	b. The potential credit loss avoided with the help of the model
#	c. Assumptions based on which the model has been built 	
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("gridExtra")
install.packages("car")
install.packages("caTools")
install.packages("corrplot")
install.packages("caret")
install.packages("Amelia")
install.packages("dummies")
install.packages("e1071")
install.packages("Information")
install.packages("knitr")
install.packages("rattle")
install.packages("randomForest")
install.packages("sqldf")
install.packages("ROSE")
install.packages("mcc")
install.packages("DMwR")
install.packages("ggcorrplot")

 
library(lubridate)                      #For Processing the Dates
library(dplyr)                          #For Cleaning, Processing, & Manipulating Data
library(tidyr)                          #For Cleaning, Processing, & Manipulating Data
library(reshape2)                       #For Reshape your data from long to wide, split a column, aggregate
library(ggplot2)                        #For Ploting the Data
library(readr)                          #For Friendly way to read rectangular text data (like 'csv', 'tsv', and 'fwf')
library(gridExtra)                      #For multiple plots on the same page
library(stringr)                        #For Character Manipulation,White Spaces,Pattern Matching
library(e1071)                          #For Statistics
library(cowplot)                        #For Streamlined Plot Theme and Plot Annotations for 'ggplot2
library(caTools)                        #For Data Split ROC AUC,
library(caret)                          #For Classification and Regression Training data splitting,feature selection,confusion matrix
library(MASS)							              #calculate confidence intervals from logistic regression models
library(car)							              #Companion to Applied Regression
library(GGally)                         #Extension to ggplot2
library(corrplot)                       #Graphical display of a correlation matrix
library(Amelia)                         #A Program for Missing Data. A tool that "multiply imputes" missing data in a single cross-section
library(dummies)                        #For flexibly and efficiently creates dummy variables for a variety of structures.
library(Information)                    #For calculating the IV and WOE
library(ROCR)                           #Library for ROC Curve, AUC
library(knitr)                          #Fast dynamic report generation with R
library(rpart)                          #Recursive Partitioning And Regression Tree used for Decision Tress
library(rattle)                         #Graphical User Interface for Data Science in R
library(randomForest)                   #Breiman and Cutler's Random Forests for Classification and. Regression
library(ROSE)                           #Generation of synthetic data by Randomly Over Sampling Examples 
library(mcc)                            #MCC is a method which can measure association between rows of a matrix with a single response vector.
library(DMwR)                           #Data Mining with R
library(sqldf)                          #R package for running SQL statements on R data frames
library(ggcorrplot)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                        Data Understanding
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Data Dictionary of Demographic Data 	:	This is obtained from the information provided by the applicants at the time of credit card application
#Application ID						          Unique ID of the customers
#Age								                Age of customer
#Gender								              Gender of customer
#Marital Status						          Marital status of customer (at the time of application)
#No of dependents					          No. of childrens of customers
#Income								              Income of customers
#Education							            Education of customers
#Profession							            Profession of customers
#Type of residence					        Type of residence of customers
#No of months in current residence	No of months in current residence of customers
#No of months in current company	  No of months in current company of customers
#Performance Tag					          Status of customer performance (" 1 represents "Default")

#Credit Bureau data This is taken from the credit bureau constains the customer loan payment information
#Application ID														                        Customer application ID
#No of times 90 DPD or worse in last 6 months						          Number of times customer has not payed dues since 90days in last 6 months
#No of times 60 DPD or worse in last 6 months						          Number of times customer has not payed dues since 60 days last 6 months
#No of times 30 DPD or worse in last 6 months						          Number of times customer has not payed dues since 30 days days last 6 months
#No of times 90 DPD or worse in last 12 months						        Number of times customer has not payed dues since 90 days days last 12 months
#No of times 60 DPD or worse in last 12 months						        Number of times customer has not payed dues since 60 days days last 12 months
#No of times 30 DPD or worse in last 12 months						        Number of times customer has not payed dues since 30 days days last 12 months
#Avgas CC Utilization in last 12 months								            Average utilization of credit card by customer
#No of trades opened in last 6 months								              Number of times the customer has done the trades in last 6 months
#No of trades opened in last 12 months								            Number of times the customer has done the trades in last 12 months
#No of PL trades opened in last 6 months							            No of PL trades in last 6 month  of customer
#No of PL trades opened in last 12 months							            No of PL trades in last 12 month  of customer
#No of Inquiries in last 6 months (excluding home & auto loans)		Number of times the customers has inquired in last 6 months
#No of Inquiries in last 12 months (excluding home & auto loans)	Number of times the customers has inquired in last 12 months
#Presence of open home loan											                  Is the customer has home loan (1 represents "Yes")
#Outstanding Balance												                      Outstanding balance of customer
#Total No of Trades													                      Number of times the customer has done total trades
#Presence of open auto loan											                  Is the customer has auto loan (1 represents "Yes")
#Performance Tag													                        Status of customer performance (" 1 represents "Default")

#Read the two data files into 2 data frames
credit_bureau_data <- read.csv("Credit Bureau data.csv", 		  header = TRUE, na.strings = c("", "NA"))
demographic_data   <- read.csv("Demographic data.csv", header = TRUE, na.strings = c("", "NA"))

#Check the structure of the data 
str(credit_bureau_data) #71295 obs. of  19 variables
str(demographic_data)   #71295 obs. of  12 variables

#Check the First 10 rows of the data
head(sample(credit_bureau_data), 10)
head(sample(demographic_data)  , 10)

#Summary of dataset
summary(credit_bureau_data)
summary(demographic_data)

#71295 rows in both datasets
nrow(credit_bureau_data)
nrow(demographic_data)

#Renaming the column names in Demographic & Credit Bureau dataset 
names(demographic_data)[1]    <- "Application_ID"
names(demographic_data)[4]    <- "Marital_Status"
names(demographic_data)[5]    <- "No_of_Dependents"
names(demographic_data)[9]    <- "Type_of_Residence"
names(demographic_data)[10]   <- "Mnths_in_current_residence"
names(demographic_data)[11]   <- "Mnths_in_current_company"
names(demographic_data)[12]   <- "Performance_Tag"

names(credit_bureau_data)[1]   <- "Application_ID"
names(credit_bureau_data)[2]   <- "DPD_90_6_months"
names(credit_bureau_data)[3]   <- "DPD_60_6_months"
names(credit_bureau_data)[4]   <- "DPD_30_6_months"
names(credit_bureau_data)[5]   <- "DPD_90_12_months"
names(credit_bureau_data)[6]   <- "DPD_60_12_months"
names(credit_bureau_data)[7]   <- "DPD_30_12_months"
names(credit_bureau_data)[8]   <- "Avg_CC_Util_12"
names(credit_bureau_data)[9]   <- "Trade_Open_6months"
names(credit_bureau_data)[10]  <- "Trade_Open_12months"
names(credit_bureau_data)[11]  <- "PLtrade_Open_6months"
names(credit_bureau_data)[12]  <- "PLtrade_Open_12months"
names(credit_bureau_data)[13]  <- "Enquiries_6months"
names(credit_bureau_data)[14]  <- "Enquiries_12months"
names(credit_bureau_data)[15]  <- "Open_homeloan"
names(credit_bureau_data)[16]  <- "Outstanding_Balance"
names(credit_bureau_data)[17]  <- "Total_No_of_Trades"
names(credit_bureau_data)[18]  <- "Open_autoloan"
names(credit_bureau_data)[19]  <- "Performance_Tag"

#Plot the Response Variable
prop.table(table(credit_bureau_data$Performance_Tag)) %>% barplot(col = "darkred", xlab = "Response", ylab = "Percentage of response",
                                          ylim = c(0, 1), beside = TRUE, main = "Response Percentage")
prop.table(table(credit_bureau_data$Performance_Tag))
#     0          1 
# 0.95780736 0.04219264

prop.table(table(demographic_data$Performance_Tag)) %>% barplot(col = "darkred", xlab = "Response", ylab = "Percentage of response",
                                          ylim = c(0, 1), beside = TRUE, main = "Response Percentage")
prop.table(table(demographic_data$Performance_Tag))
#     0          1 
# 0.95780736 0.04219264

#Checking the NA values in data and there are #3028 in credit_bureau_data #1577 in demographic_data
sum(is.na(credit_bureau_data)) 
sum(is.na(demographic_data))

# Checking the duplicate rows,and there are 0 Duplicated Rows
sum(duplicated(credit_bureau_data))
sum(duplicated(demographic_data))

#Gives the  Application IDs that are duplicated #765011468,#653287861,#67198918
credit_bureau_data[duplicated(credit_bureau_data$Application_ID)==TRUE,]$Application_ID
demographic_data[duplicated(demographic_data$Application_ID)==TRUE,]$Application_ID

#Displays the rows which has duplicated application ID's)
credit_bureau_data[(duplicated(credit_bureau_data$Application_ID)==T),] 
demographic_data[(duplicated(demographic_data$Application_ID)==T),]

#Select the data without duplicated Application_ID
credit_bureau_data <- sqldf("select * from credit_bureau_data where Application_ID not in ('765011468', '653287861', '671989187');")
demographic_data   <- sqldf("select * from demographic_data where Application_ID not in ('765011468', '653287861', '671989187');")

#Check if there are any blank values and there are no blank values in the data set
sapply(credit_bureau_data, function(x)  length(which(x == "")))
sapply(demographic_data, function(x)  length(which(x == "")))

# Removing the duplicate rows
credit_bureau_data <- unique(credit_bureau_data)
demographic_data   <- unique(demographic_data)


#Function to Identify missing and NA's
ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_hue(name = "",
                    labels = c("Present","NA's")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
ggplot_missing(demographic_data)
ggplot_missing(credit_bureau_data)

#Find the NAs present w.r.to each variable which are total 3028 in credit_bureau_data
sort(sapply(credit_bureau_data, function(x) { sum(is.na(x)) }), decreasing=TRUE)

#Performance.Tag              1425 
#Avg_CC_Util_12               1058 
#Open_homeloan                272 
#Outstanding_Balance          272 
#Trade_Open_6months           1 


#Find the NAs present w.r.to each variable which are total 1577 in demographic_data
sort(sapply(demographic_data,   function(x) { sum(is.na(x)) }), decreasing=TRUE)

#Performance.Tag  	        1425                                 								
#Education  			          119
#Profession 			          14                          
#Type_of_Residence 	        8                                                        
#Marital_Status             6                        
#No_of_Dependents 	        3 
#Gender    			            2                      

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                        Data Cleaning and Preperation
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Check whether application Id can be used for merging the Datasets. #0 means both have same application_Ids
setdiff(demographic_data$Application_ID,credit_bureau_data$Application_ID) 
setdiff(credit_bureau_data$Application_ID,demographic_data$Application_ID)


#Merge based on Application Id and Create a master file with all the relevant variables for conducting the necessary data quality checks and cleaning.
combined_data <- merge(credit_bureau_data, demographic_data, by="Application_ID")
#71289 obs. of  30 variables
str(combined_data)
summary(combined_data)

#Performance_Tag variable on both dataset so lets remove after checking its value
#1                 0 66917
#2                 1  2947
#3                NA  1425
combined_data%>%group_by(Performance_Tag.x)%>%summarise(n())
combined_data%>%group_by(Performance_Tag.y)%>%summarise(n())


#Check whether null value rows match w.r.to Performance Tag in both data sets.1425 rows matches with NA values
null_value_match 	<- sum(which(is.na(combined_data$Performance_Tag.x)) == which(is.na(combined_data$Performance_Tag.y)))

#Check whether non null value rows match w.r.to Performance in both data sets. 69874 rows matches with non NA values
value_match 			<- sum(which(!is.na(combined_data$Performance_Tag.x)) == which(!is.na(combined_data$Performance_Tag.y)))

#TRUE Conclusion: All null and non-null values are matching w.r.to performance tags from 2 data sets
null_value_match + value_match == nrow(combined_data)
 
#As the performance variable matches with NA's and Non NA's, hence remove one of the performance variable
combined_data <- combined_data[, -19]

#Set the name of the Performance column
names(combined_data)[29] <- "Performance_Tag"

#Get the column names of comnined data
colnames(combined_data)


#There are 1425 rows with Performance being NULL values
null_value_rows    <- which(is.na(combined_data$Performance_Tag)) 

#Null values in Performance indicate those are rejected applicants get the rejected applicants
rejected_customers <- combined_data[null_value_rows,]

#Application Rejected Rate 1.998906
(nrow(rejected_customers)/nrow(combined_data))*100

#Application Approval Rate 98.00109
(1-(nrow(rejected_customers)/nrow(combined_data)))*100

#Remove the rows which dont have performance Tag as they are the customers where the card is rejected.
final_cust_data <- combined_data[-null_value_rows,]

#Convert categorical variables to factor variables.They are mainly present in demographic data
FACTOR_VARIABLES <- c("Gender", "Marital_Status", "Education", "Profession", "Type_of_Residence")
final_cust_data[, FACTOR_VARIABLES] <- as.data.frame(sapply(final_cust_data[, FACTOR_VARIABLES], as.factor))


#The following plot gives an overview of the Credit Card Applicant data, i.e.; 'Non-Default' or 'Default'.
ggplot(final_cust_data, aes(x = as.factor(final_cust_data$Performance_Tag))) + 
  theme_bw() + 
  geom_bar(fill ="blue") + 
  labs(x = "Performance_Tag (0 = Non-Default, 1 = Default)", 
       y  = "Count",
       title = "Credit Card Applicants") +
  geom_text(stat='count',aes(y = (..count..), 
                             label = ifelse((..count..)==0,"",
                                            scales::percent((..count..)/sum(..count..)))),vjust=-0.4)

#Check the structure of final data. 69864 obs. of  29 variables
str(final_cust_data)

#Checking the unique values present in the Dataset.
sapply(final_cust_data, function(x) length(unique(x)))

#Check the missing values in the final data frame.There are no missing values
sapply(final_cust_data, function(x) length(which(x == "")))
											
#Check the NA values in the final data frame against each varibale
sort(sapply(final_cust_data,   function(x) { sum(is.na(x)) }), decreasing=TRUE)

# Avg_CC_Util_12   		    1023 
# Open_homeloan           272 
# Outstanding_Balance     272 
# Education               118
# Profession              13 
# Type_of_Residence       8 
# Marital_Status   	      6 
# No_of_Dependents        3 
# Gender                  2 
# Trade_Open_6months      1


#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#												                        Treating Missing Values and Outliers
# Approach for Treating Outliers: -
# we could cap it by replacing those observations outside the lower limit with the value of 5th %ile 
# and those that lie above the upper limit, with the value of 95th/99th %ile. 
# Will remove the rows that contains NA values and for missing values and Observation beyond Lower 
# and upper limits will be capped at 5th & 95th %ile.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

##------- Exploring Avg_CC_Util_12 column -------##
summary(final_cust_data$Avg_CC_Util_12) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00    8.00   15.00   29.27   45.00  113.00    1023 

#Remove the missing values as they are only 1023
final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Avg_CC_Util_12)),]

#Check for upper and Lower Limits
boxplot (final_cust_data$Avg_CC_Util_12, col = "brown", main = "CC-UTIL")
quantile(final_cust_data$Avg_CC_Util_12, seq(0, 1, 0.01),na.rm = T)

#Cap the value at 95%Quantile 104
final_cust_data$Avg_CC_Util_12[which(final_cust_data$Avg_CC_Util_12>104)] <- 104

##------- Exploring Open_homeloan column THere are no NA's -------##
summary(final_cust_data$Open_homeloan)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2623  1.0000  1.0000

#Analyze Education,replace other with Masters and remove the NA's as they are only 117 
summary(final_cust_data$Education)
# Bachelor      Masters       Others          Phd    Professional         NA's 
# 17036          23136          115           4401        24036           117

levels(final_cust_data$Education)[3] <- "Masters"
final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Education)),]

##------- Exploring Profession column there are only 13 NA's and hence remove the same -------##
summary(final_cust_data$Profession)
# SAL      SE     SE_PROF    NA's 
# 39035   13700   15976      13
final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Profession)),]

##------- Exploring Type_of_Residence column there are only 8 NA's and hence remove the same -------##
summary(final_cust_data$Type_of_Residence) 
#Company provided Living with Parents    Others             Owned              Rented                NA's 
#  1579                1744               195               13781               51404                   8

final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Type_of_Residence)),]

##------- Exploring Marital_Status column there are only 5 NA's and hence remove the same -------##
summary(final_cust_data$Marital_Status) 
# Married  Single    NA's 
#  58536   10162       5

final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Marital_Status)),]

##------- Exploring Gender column there are only 2 NA's and hence remove the same -------##
summary(final_cust_data$Gender)
#  F      M       NA's 
# 16235 52461      2

final_cust_data <- final_cust_data[-which(is.na(final_cust_data$Gender)),]


##------- Exploring Outstanding_Balance column and cap the value at 99% Qauntile -------##
summary(final_cust_data$Outstanding_Balance) 
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
#  0    213474  777990   1266528 2930274   5218801
boxplot (final_cust_data$Outstanding_Balance, col = "brown", main = "OUT_STD_BAL")
quantile(final_cust_data$Outstanding_Balance, seq(0,1,0.01),na.rm = T)
final_cust_data$Outstanding_Balance[which(final_cust_data$Outstanding_Balance>4251463.70)] <- 4251463.70

##------- Exploring No_of_Dependents column there are only 3 NA's and hence remove the same -------##
 
summary(final_cust_data$No_of_Dependents)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
# 1.000   2.000   3.000   2.859   4.000     5.000       3 
final_cust_data <- final_cust_data[-which(is.na(final_cust_data$No_of_Dependents)),]
boxplot (final_cust_data$No_of_Dependents, col = "brown", main = "NUM-DEPENDENTS") #no outlier

##------- Exploring Age column the min age is negative,which is not right,the age should be >18 to avail a credit car -------##
summary(final_cust_data$Age)
# Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
# -3.00   38.00   45.00   45.02   53.00   65.00
quantile(final_cust_data$Age,seq(0,1,0.1))
boxplot (final_cust_data$Age, col = "brown", main = "Age")
final_cust_data <- final_cust_data[-which(final_cust_data$Age<18),]

##------- Exploring Income column there are no NA's -------##
summary(final_cust_data$Income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.50   14.00   27.00   27.39   40.00   60.00 
quantile(final_cust_data$Income,seq(0,1,0.1))
boxplot (final_cust_data$Income, col = "brown", main = "INCOME")
final_cust_data$Income[which(final_cust_data$Income<0)] <- 0

##------- Exploring No_of_months_in_current_residence column there are no outliers -------##
summary(final_cust_data$Mnths_in_current_residence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.0     6.0    10.0    34.6    61.0   126.0
quantile(final_cust_data$Mnths_in_current_residence,seq(0,1,0.01))
boxplot (final_cust_data$Mnths_in_current_residence, col = "brown", main = "MONS_IN_RESIDENCE")

##------- Exploring No_of_months_in_current_company column and cap the value at 99% Quantile -------##
summary(final_cust_data$Mnths_in_current_company) #no NA value
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   17.00   34.00   34.24   51.00  133.00
quantile(final_cust_data$Mnths_in_current_company,seq(0,1,0.01))
boxplot (final_cust_data$Mnths_in_current_company, col = "brown", main = "MONS_IN_COMPANY")
final_cust_data$Mnths_in_current_company[which(final_cust_data$Mnths_in_current_company>74)] <- 74

##------- Exploring Trade_Open_6months column there are outliers and cap the value at 6 -------##
summary(final_cust_data$Trade_Open_6months)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   2.000   2.319   3.000  12.000 
quantile(final_cust_data$Trade_Open_6months,seq(0,1,0.01))
boxplot (final_cust_data$Trade_Open_6months, col = "brown", main = "TRDS_IN_6_MNTS")
final_cust_data$Trade_Open_6months[which(final_cust_data$Trade_Open_6months>6)] <- 6

##------- Exploring Trade_Open_12months column and cap the value at 99% Quantile -------##
summary(final_cust_data$Trade_Open_12months) #no NA value                         
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    2.00    5.00    5.87    9.00   28.00
quantile(final_cust_data$Trade_Open_12months,seq(0,1,0.01))
boxplot (final_cust_data$Trade_Open_12months, col = "brown", main = "TRDS_IN_12_MNTS")
final_cust_data$Trade_Open_12months[which(final_cust_data$Trade_Open_12months>21)] <- 21

##------- Exploring PLtrade_Open_6months column and cap the value at 99% Quantile -------##
summary(final_cust_data$PLtrade_Open_6months) #no NA value
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.207   2.000   6.000
quantile(final_cust_data$PLtrade_Open_6months,seq(0,1,0.01))
boxplot (final_cust_data$PLtrade_Open_6months, col = "brown", main = "PL_TRDS_IN_6_MNTS")
final_cust_data$PLtrade_Open_6months[which(final_cust_data$PLtrade_Open_6months>5)] <- 5

##------- Exploring PLtrade_Open_12months column no NA value and  cap the value at 99% Quantile -------##
summary(final_cust_data$PLtrade_Open_12months)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   2.000   2.397   4.000  12.000
quantile(final_cust_data$PLtrade_Open_12months,seq(0,1,0.01))
boxplot (final_cust_data$PLtrade_Open_12months, col = "brown", main = "PL_TRDS_IN_12_MNTS")
final_cust_data$PLtrade_Open_12months[which(final_cust_data$PLtrade_Open_12months>9)] <- 9

##------- Exploring Enquiries_6months column and no NA value,cap the value at 99% Quantile -------##
summary(final_cust_data$Enquiries_6months)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.783   3.000  10.000 
quantile(final_cust_data$Enquiries_6months,seq(0,1,0.01))
boxplot (final_cust_data$Enquiries_6months, col = "brown", main = "INQRY_IN_6_MONTS")
final_cust_data$Enquiries_6months[which(final_cust_data$Enquiries_6months>8)] <- 8

##------- Exploring Enquiries_12months column and cap the value at 99% Quantile -------##

summary(final_cust_data$Enquiries_12months)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   3.000   3.571   5.000  20.000
quantile(final_cust_data$Enquiries_12months,seq(0,1,0.01))
boxplot (final_cust_data$Enquiries_12months, col = "brown", main = "INQRY_IN_12_MONTS")
final_cust_data$Enquiries_12months[which(final_cust_data$Enquiries_12months>15)] <- 15

# Checking the NA values after treating the data.And there are no NA's
sum(is.na(final_cust_data))

str(final_cust_data)
# 68636 obs. of  29 variables (5 Factor 24 Numerical)
# Application_ID            : int  100450 128993 142768 176721 197956 203973 210394 223052 237197 247959 
# DPD_90_6_months           : int  0 1 0 0 0 0 0 1 0 0 
# DPD_60_6_months           : int  0 1 1 0 0 0 0 1 0 0 
# DPD_30_6_months           : int  0 2 1 0 0 0 0 1 0 0 
# DPD_90_12_months          : int  0 1 1 0 0 0 0 1 0 0 
# DPD_60_12_months          : int  0 1 1 0 0 0 0 1 0 0 
# DPD_30_12_months          : int  0 2 1 0 0 0 0 1 0 0 
# Avg_CC_Util_12            : num  104 9 34 18 3 
# Trade_Open_6months        : num  2 1 2 0 6 1 1 2 1 2 
# Trade_Open_12months       : num  8 6 7 1 13 2 2 5 3 4 
# PLtrade_Open_6months      : int  2 1 2 0 5 0 0 2 0 1 
# PLtrade_Open_12months     : num  5 4 4 1 5 0 1 3 1 1 
# Enquiries_6months         : num  1 4 1 2 6 0 1 2 2 1 
# Enquiries_12months        : num  3 7 2 3 11 0 3 5 4 1 
# Open_homeloan             : int  1 0 0 0 0 0 0 0 0 0 
# Outstanding_Balance       : num  3903438 741058 815325 209593 992024 
# Total_No_of_Trades        : int  9 8 9 3 25 5 4 6 5 7 
# Open_autoloan             : int  0 0 0 0 0 0 0 0 0 0 
# Age                       : int  52 36 55 55 28 43 42 51 44 59 
# Gender                    : Factor w/ 2 levels "F","M": 2 2 2 2 1 1 2 2 2 2 
# Marital_Status            : Factor w/ 2 levels "Married","Single": 1 1 1 1 1 2 1 1 1 1 
# No_of_Dependents          : int  4 4 1 3 3 1 2 3 4 5 
# Income                    : num  32 13 29 53 35 35 43 45 5 40 
# Education                 : Factor w/ 4 levels "Bachelor","Masters",: 4 4 3 4 2 4 1 2 4 2 
# Profession                : Factor w/ 3 levels "SAL","SE","SE_PROF": 3 1 1 3 1 1 3 1 1 1 
# Type_of_Residence         : Factor w/ 5 levels "Company provided",: 5 5 5 5 5 5 5 5 5 4 
# Mnths_in_current_residence: int  79 6 46 6 6 6 6 83 6 6 
# Mnths_in_current_company  : num  3 21 3 27 43 52 3 48 38 5 
# Performance_Tag           : int  0 1 0 0 0 0 0 0 0 0 

#Seperate the Continuous Varibales to bin then
#Avg_CC_Util_12,Trade_Open_6months,Trade_Open_12months,PLtrade_Open_6months,PLtrade_Open_12months,Enquiries_6months     
#Enquiries_12months,Outstanding_Balance,Total_No_of_Trades,Age,Income,Mnths_in_current_residence,Mnths_in_current_company

continous_variable <- c("Avg_CC_Util_12","Outstanding_Balance","Total_No_of_Trades"
                        ,"Age","Income","Mnths_in_current_residence","Mnths_in_current_company",
                        "Trade_Open_6months","Trade_Open_12months","PLtrade_Open_6months",                           
                        "PLtrade_Open_12months","Enquiries_6months","Enquiries_12months")

continuous_data <- final_cust_data[,(colnames(final_cust_data) %in% continous_variable)]
                                                                      
categorical_data <- final_cust_data[,!(colnames(final_cust_data) %in% continous_variable)]

#Remove application_ID column
categorical_data <- categorical_data[,-1]

colnames(categorical_data)
# "DPD_90_6_months"   "DPD_60_6_months"   "DPD_30_6_months"   "DPD_90_12_months"  "DPD_60_12_months"  "DPD_30_12_months" 
# "Open_homeloan"     "Open_autoloan"     "Gender"            "Marital_Status"    "No_of_Dependents"  "Education"        
# "Profession"        "Type_of_Residence" "Performance_Tag"

colnames(continuous_data)
# "Avg_CC_Util_12"             "Trade_Open_6months"         "Trade_Open_12months"        "PLtrade_Open_6months"      
# "PLtrade_Open_12months"      "Enquiries_6months"          "Enquiries_12months"         "Outstanding_Balance"       
# "Total_No_of_Trades"         "Age"                        "Income"                     "Mnths_in_current_residence"
# "Mnths_in_current_company"  

#There are no NA's and 68648 Rows available in both sets
sum(is.na(continuous_data))
nrow(continuous_data)

sum(is.na(categorical_data))
nrow(categorical_data)


#Check the correlation among the continuous variables
corrs = round(cor(continuous_data, use = "pairwise.complete.obs"), 2)
ggcorrplot(corrs)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                       Exploratory Data Analysis
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Perform Binning of the continuous variables
categorical_data$CC_Utili           <- as.factor(cut(continuous_data$Avg_CC_Util_12,include.lowest = TRUE, breaks = c(0,6,8,11,15,25,35,45,55,65,85,110)))
categorical_data$Outstanding        <- as.factor(cut(continuous_data$Outstanding_Balance,include.lowest = TRUE, breaks = c(0,106737,213474,388940,777880,1465024,2609400,5218801)))
categorical_data$TotalTrades        <- as.factor(cut(continuous_data$Total_No_of_Trades,include.lowest = TRUE,  breaks = c(0,4,8,16,24,32,40,48)))
categorical_data$Age                <- as.factor(cut(continuous_data$Age,include.lowest = TRUE, breaks = c(18,30,40,50,65)))
categorical_data$Income             <- as.factor(cut(continuous_data$Income,include.lowest = TRUE, breaks = c(0,7, 11, 17, 22, 27, 32, 37, 43, 49,74)))
categorical_data$MonthsInResidence  <- as.factor(cut(continuous_data$Mnths_in_current_residence,include.lowest = TRUE, breaks = c(6,10,29,50,73,98,126)))
categorical_data$MonthsInCompany    <- as.factor(cut(continuous_data$Mnths_in_current_company,include.lowest = TRUE, breaks = c(3,6,13,20,27,34,41,48,54,62,74)))
categorical_data$TradesIn12Mnths    <- as.factor(cut(continuous_data$Trade_Open_12months,include.lowest = TRUE, breaks = c(0,1,3,5,7,10,15,21)))
categorical_data$InquiriesIn12Mnths <- as.factor(cut(continuous_data$Enquiries_12months,include.lowest = TRUE, breaks = c(0,1,3,5,10,15)))

categorical_data$TradesIn6Mnths     <- as.factor(continuous_data$Trade_Open_6months)
categorical_data$PLTradesIn6Mnths   <- as.factor(continuous_data$PLtrade_Open_6months)
categorical_data$PLTradesIn12Mnths  <- as.factor(continuous_data$PLtrade_Open_12months)
categorical_data$InquiriesIn6Mnths  <- as.factor(continuous_data$Enquiries_6months)

categorical_data$Numdependents      <- as.factor(final_cust_data$No_of_Dependents )
categorical_data$DPD90_6_MNTHS      <- as.factor(final_cust_data$DPD_90_6_months)
categorical_data$DPD60_6_MNTHS      <- as.factor(final_cust_data$DPD_60_6_months)
categorical_data$DPD30_6_MNTHS      <- as.factor(final_cust_data$DPD_30_6_months)
categorical_data$DPD90_12_MNTHS     <- as.factor(final_cust_data$DPD_90_12_months)
categorical_data$DPD60_12_MNTHS     <- as.factor(final_cust_data$DPD_60_12_months)
categorical_data$DPD30_12_MNTHS     <- as.factor(final_cust_data$DPD_30_12_months)
categorical_data$HomeLoan		        <- as.factor(final_cust_data$Open_homeloan)
categorical_data$AutoLoan		        <- as.factor(final_cust_data$Open_autoloan)                  
categorical_data$Performance	      <- as.factor(final_cust_data$Performance_Tag)                            

#Remove the Numerical variables which are already converted into factor variables
categorical_data<-categorical_data[,-c(1:8,11,15)]

#Chek if there are any NA Values there are no NA values
sum(is.na(categorical_data))

#Final Column Names after Data Processing
colnames(categorical_data)
# "Gender"             "Marital_Status"     "Education"          "Profession"         "Type_of_Residence" 
# "CC_Utili"           "Outstanding"        "TotalTrades"        "Age"                "Income"            
# "MonthsInResidence"  "MonthsInCompany"    "TradesIn12Mnths"    "InquiriesIn12Mnths" "TradesIn6Mnths"    
# "PLTradesIn6Mnths"   "PLTradesIn12Mnths"  "InquiriesIn6Mnths"  "Numdependents"      "DPD90_6_MNTHS"     
# "DPD60_6_MNTHS"      "DPD30_6_MNTHS"      "DPD90_12_MNTHS"     "DPD60_12_MNTHS"     "DPD30_12_MNTHS"    
# "HomeLoan"           "AutoLoan"           "Performance" 

#The dataset is with 68636 rows and  28 factor variables
str(categorical_data)

#----------------------------------------------- UNI-VARIATE ANALYSIS-DEMOGRAPHIC -------------------------------------------

# Age Gender  Marital_Status Numdependents  Income Education Profession Type_of_Residence  MonthsInResidence  MonthsInCompany 

gdm_1 =  ggplot(categorical_data, aes(x = Age)) +  theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Age", y  = "Count", title = "AgeVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.1) +facet_wrap( ~ Performance)

gdm_2 =  ggplot(categorical_data, aes(x = Gender)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Gender", y  = "Count", title = "GenderVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.1)+facet_wrap( ~ Performance)

gdm_3 =  ggplot(categorical_data, aes(x = Marital_Status)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Marital_Status", y  = "Count", title = "MartialVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.1)+facet_wrap( ~ Performance)

gdm_4 =  ggplot(categorical_data, aes(x = Numdependents)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Numdependents", y  = "Count", title = "NumDepVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_5 =  ggplot(categorical_data, aes(x = Income)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Income", y  = "Count", title = "IncomeVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_6 =  ggplot(categorical_data, aes(x = Education)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Education", y  = "Count", title = "EdueVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_7 =  ggplot(categorical_data, aes(x = Profession)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Profession", y  = "Count", title = "ProfVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_8 =  ggplot(categorical_data, aes(x = Type_of_Residence)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Type_of_Residence", y  = "Count", title = "ResiVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_9 =  ggplot(categorical_data, aes(x = MonthsInResidence)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "MonthsInResidence", y  = "Count", title = "MnthsInResiVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gdm_10 =  ggplot(categorical_data, aes(x = MonthsInCompany)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "MonthsInCompany", y  = "Count", title = "MnthsInCmpVsPerformance") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)


grid.arrange(gdm_1, gdm_2, gdm_3)
grid.arrange(gdm_4, gdm_5, gdm_6)  
grid.arrange(gdm_7, gdm_8, gdm_9, gdm_10)

#----------------------------------------------- UNI-VARIATE ANALYSIS-CREDIT BUREAU ----------------------------------

gcb_1 =  ggplot(categorical_data, aes(x = DPD90_6_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD90_6_MNTHS", y  = "Count", title = "DPD90_6_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_2 =  ggplot(categorical_data, aes(x = DPD60_6_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD60_6_MNTHS", y  = "Count", title = "DPD60_6_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_3 =  ggplot(categorical_data, aes(x = DPD30_6_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD30_6_MNTHS", y  = "Count", title = "DPD30_6_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_4 =  ggplot(categorical_data, aes(x = DPD90_12_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD90_12_MNTHS", y  = "Count", title = "DPD90_12_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_5 =  ggplot(categorical_data, aes(x = DPD60_12_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD60_12_MNTHS", y  = "Count", title = "DPD60_12_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_6 =  ggplot(categorical_data, aes(x = DPD30_12_MNTHS)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "DPD30_12_MNTHS", y  = "Count", title = "DPD30_12_MNTHSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_7 =  ggplot(categorical_data, aes(x = PLTradesIn6Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "PLTradesIn6Mnths", y  = "Count", title = "PLTradesIn6MnthsvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_8 =  ggplot(categorical_data, aes(x = PLTradesIn12Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "PLTradesIn12Mnths", y  = "Count", title = "PLTradesIn12MnthsvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_9 =  ggplot(categorical_data, aes(x = TradesIn6Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "TradesIn6Mnths", y  = "Count", title = "TradesIn6MnthsvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_10 =  ggplot(categorical_data, aes(x = TradesIn12Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "TradesIn12Mnths", y  = "Count", title = "TradesIn12MnthsvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_11 =  ggplot(categorical_data, aes(x = TotalTrades)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "TotalTrades", y  = "Count", title = "TotalTradesvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_12 =  ggplot(categorical_data, aes(x = Outstanding)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "Outstanding", y  = "Count", title = "OSvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_13 =  ggplot(categorical_data, aes(x = InquiriesIn12Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "InquiriesIn12Mnths", y  = "Count", title = "12MnthsInqvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_14 =  ggplot(categorical_data, aes(x = InquiriesIn6Mnths)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "InquiriesIn6Mnths", y  = "Count", title = "6MnthInqvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_15 =  ggplot(categorical_data, aes(x = HomeLoan)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "HomeLoan", y  = "Count", title = "HomeLoanvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

gcb_16 =  ggplot(categorical_data, aes(x = AutoLoan)) + theme_bw() + 
  geom_bar(fill ="red") + labs(x = "AutoLoan", y  = "Count", title = "AutoLoanvsPerf") +
  geom_text(stat='count',aes(y = (..count..), label = ifelse((..count..)==0,"",
  scales::percent((..count..)/sum(..count..)))),vjust=-0.4)+facet_wrap( ~ Performance)

grid.arrange(gcb_1,gcb_2, gcb_3)
grid.arrange(gcb_4,gcb_5, gcb_6)
grid.arrange(gcb_7, gcb_8,gcb_9,gcb_10)
grid.arrange(gcb_11, gcb_12)
grid.arrange(gcb_13,gcb_14)
grid.arrange(gcb_15, gcb_16)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                      MULTI_VARIATE ANALYSIS
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Age & Income correlation for Performance_Tag
ggplot(final_cust_data, aes(x = Age,y = Income,col = factor(Performance_Tag))) + theme_bw() +
  facet_wrap(~ Performance_Tag) + geom_smooth(method = 'gam',formula = y ~ s(x, bs = "cs"),se = TRUE) +
  labs(x = "Age",y  = "Income",title = "Age & Income correlation for Performance_Tag")

#No_of_months_in_current_company & No_of_months_in_current_residence correlation for Performance_Tag
ggplot(final_cust_data, aes(x = Mnths_in_current_company,y = Mnths_in_current_residence,col = factor(Performance_Tag))) + theme_bw() +
  facet_wrap(~ Performance_Tag) + geom_smooth(method = 'gam',formula = y ~ s(x, bs = "cs"),se = TRUE) +
  labs(x = "months_in_current_company", 
       y  = "months_in_current_residence",
       title = "months_in_current_company & months_in_current_residence correlation for Performance_Tag")

#Visualizing relationship among Income, Profession and Performance_Tag
ggplot(final_cust_data, aes(x = Income,col = factor(Performance_Tag))) + theme_bw() +
  facet_wrap(~ Performance_Tag) + geom_density(position="stack",aes(fill=Profession),alpha = 0.5) +
  labs(x = "Income",y  = "Density distribution of Profession",
  title = "Density distribution of Profession by Income for Performance_Tag")

ggplot(final_cust_data, aes(x = Outstanding_Balance,col = factor(Performance_Tag))) + theme_bw() +
  facet_wrap(~ Performance_Tag) + geom_density(position="stack",aes(fill=Profession),alpha = 0.5) +
  labs(x = "Outstanding_Balance",y  = "Density distribution of OS Balance",
  title = "Density distribution of OutStandingBalanceAndProfession for Performance_Tag")

ggplot(final_cust_data, aes(x = Avg_CC_Util_12,y = Outstanding_Balance,col = factor(Performance_Tag))) + theme_bw() +
  facet_wrap(~ Performance_Tag) + geom_smooth( method = 'gam',formula = y ~ s(x, bs = "cs")) +
  labs(x = "Avg_CC_Util_12",y  = "Outstanding_Balance",title = "CC Util & OB correlation for Performance_Tag")

AC_OB_OHL_0 <- ggplot(subset(final_cust_data, Performance_Tag == 0), aes(x=Avg_CC_Util_12, y=(Outstanding_Balance), col = factor(Open_homeloan))) +
  geom_smooth() + labs(x = "Average utilization of credit card", 
                       y  = "Outstanding Balance (in Millions)",
                       title = "Non Default")

AC_OB_OHL_1 <- ggplot(subset(final_cust_data, Performance_Tag == 1), aes(x=Avg_CC_Util_12, y=(Outstanding_Balance), col = factor(Open_homeloan))) +
  geom_smooth() +   labs(x = "Average utilization of credit card",y  = "Outstanding Balance (in Millions)",title = "Default")

plot_grid(AC_OB_OHL_0, AC_OB_OHL_1)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                       EDA CONCLUSION
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Based on the exploratory data analysis the below variables appear to be significant 
# 1. DPD_30_12_months 
# 2. DPD_60_12_months
# 3. DPD_90_12_months
# 4. DPD_30_6_months 
# 5. DPD_60_6_months
# 6. DPD_90_6_months
# 7. PLTradesIn6Mnths
# 8. InquiriesIn12Mnths

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                       WOE and IV Analysis
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
final_cust_data_Perf_changed<-final_cust_data

# Change the levels to "good " and "bad"
final_cust_data_Perf_changed$Performance_Tag <- ifelse(final_cust_data_Perf_changed$Performance_Tag==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
final_cust_data_Perf_changed$Performance_Tag <- ifelse(final_cust_data_Perf_changed$Performance_Tag=="good",1,0)

#Returns WOE or NWOE tables (as data.frames), and a data.frame with IV or NIV values for all predictive variables
IV_Tables <- create_infotables(final_cust_data_Perf_changed[,-1],y="Performance_Tag",ncore = 2)

#Summary gives the IV value of each variable
IV_dataframe <- IV_Tables$Summary

#Arrange the variables with decreasing order of IV
plotFrame <- IV_Tables$Summary[order(-IV_Tables$Summary$IV), ]

plotFrame$Variable <- factor(plotFrame$Variable,levels = plotFrame$Variable[order(-plotFrame$IV)])

#Plot the informatin values in the decreasing order of IV Values
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", fill = "yellow") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

#Categorize the Variables in terms of Strength based on the IV value range
for(i in 1:nrow(IV_dataframe)){
  
  if (IV_dataframe$IV[i]<0.02){
    IV_dataframe$feedback[i] = "Useless"
    
  } else if(IV_dataframe$IV[i]>=0.02& IV_dataframe$IV[i]<0.1){
    IV_dataframe$feedback[i] = "Weak"
    
  } else if(IV_dataframe$IV[i]>=0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Medium"
    
  }else if(IV_dataframe$IV[i]>=0.3 & IV_dataframe$IV[i]<0.5){
    IV_dataframe$feedback[i] = "Strong"
    
  }else if(IV_dataframe$IV[i]>0.1 & IV_dataframe$IV[i]<0.3){
    IV_dataframe$feedback[i] = "Suspicious"
  }
}
#Factor the feedback and Varibles
IV_dataframe$Variable <- as.factor(IV_dataframe$Variable)
IV_dataframe$feedback <- as.factor(IV_dataframe$feedback)

#Get the varibles that can  impact the Performance based on IV value. Choose strong and Medium.Generally IV value <0.05 has less impact
impact_varibles <- which(IV_dataframe$feedback=="Strong"|IV_dataframe$feedback=="Medium")

#Get the column names of the same
impact_var_names <- IV_dataframe[impact_varibles, 1]

#Get the Cloumn Indexes 
column_indexes <- which(colnames(final_cust_data_Perf_changed) %in% impact_var_names)

#Get the data frame with required variables based in IV values for Modified Performance Tag
final_cust_data_Perf_changed_iv <- final_cust_data_Perf_changed[,c(1,column_indexes,29)]

#Get the data frame with required variables based in IV values for Original Performance Tag
final_cust_data_iv <- final_cust_data[,c(1,column_indexes,29)]

colnames(final_cust_data_Perf_changed_iv)
colnames(final_cust_data_iv)
# "Application_ID"        "DPD_90_6_months"       "DPD_60_6_months"       "DPD_30_6_months"       "DPD_90_12_months"     
# "DPD_60_12_months"      "DPD_30_12_months"      "Avg_CC_Util_12"        "Trade_Open_6months"    "Trade_Open_12months"  
# "PLtrade_Open_6months"  "PLtrade_Open_12months" "Enquiries_6months"     "Enquiries_12months"    "Outstanding_Balance"  
# "Total_No_of_Trades"    "Performance_Tag"

#The IV of a variable measures its predictive power, and variables with IV < .05 are generally considered to have a low predictive power.
IV_Tables <- create_infotables(final_cust_data_Perf_changed_iv[,-1],y="Performance_Tag",ncore = 2)

knitr::kable(head(IV_Tables$Summary))
#  |   |Variable              |        IV|
#  |:--|:---------------------|---------:|
#  |13 |Enquiries_12months    | 0.3170205|
#  |7  |Avg_CC_Util_12        | 0.3141667|
#  |11 |PLtrade_Open_12months | 0.3124460|
#  |9  |Trade_Open_12months   | 0.3117646|
#  |14 |Outstanding_Balance   | 0.2561862|
#  |16 |Total_No_of_Trades    | 0.2511140|

#Data Frame for Adding WOE values for Data Frame with Original Performance Tag Values
final_cust_data_iv_woe <-final_cust_data_iv

# Let's replace the variables with woe value for model building 
final_cust_data_woe_list <- list()

typeof(IV_Tables$Tables)
length(IV_Tables$Tables)

# Extracting the bins and corresponding WOE values for each variable from IV_Tables
for(i in 1:length(IV_Tables$Tables)) {
  final_cust_data_woe_list[[i]] = cbind(IV_Tables$Tables[[i]][1],IV_Tables$Tables[[i]][4])
}
head(final_cust_data_woe_list)

# woe_list is a list of dataframes with each df having the following structure:
#    DPD_90_6_months        WOE
#1           [0,0]  0.2664232
#2           [1,3] -0.6237097
final_cust_data_woe_list

#The below function takes the bins of each variable in to range and replace the actual values of variable in dataframe with corresponding WOE value
add_woe_values <- function(df, variable) {
  for(i in 1:nrow(df)){
    s <- df[i,1]
    if(s=="NA"){
      replace_by = df[i,2]
      variable[which(is.na(variable))] = replace_by
    } else {
      s <- str_replace_all(s, fixed(" "), "")
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      replace_by = df[i,2]
      
      variable[which(variable %in% range)] = replace_by
    }
  }
  return(variable)
}

#Ge the column names of the variables for which the original values to be replaced with WOE value
col_replace <- which(!colnames(final_cust_data_iv_woe) %in% c("Application_ID","Performance_Tag") )

for(i in col_replace){
  final_cust_data_iv_woe[, i] = add_woe_values(df=final_cust_data_woe_list[[i-1]], variable=final_cust_data_iv_woe[,i])  
}

#let's do it for rejected customers dataset as well
for(i in col_replace){
  rejected_customers[, i] = add_woe_values(df=final_cust_data_woe_list[[i-1]], variable=rejected_customers[,i])  
}


#The final dataframe contains all the impact variables replaced with WOE Values
colnames(final_cust_data_iv_woe)
# "Application_ID"        "DPD_90_6_months"       "DPD_60_6_months"       "DPD_30_6_months"       "DPD_90_12_months"     
# "DPD_60_12_months"      "DPD_30_12_months"      "Avg_CC_Util_12"        "Trade_Open_6months"    "Trade_Open_12months"  
# "PLtrade_Open_6months"  "PLtrade_Open_12months" "Enquiries_6months"     "Enquiries_12months"    "Outstanding_Balance"  
# "Total_No_of_Trades"    "Performance_Tag"  

#Get the names of impact variables without Application_ID Performance_Tag
names <- names(final_cust_data_iv_woe[,-c(1,17)])

#Store all the graphs in list
woe_plots <- list()

#Plot the WOE values only for the impact variables
for (i in 1:length(names)){
  woe_plots[[i]] <- plot_infotables(IV_Tables, names[i])
}

#Plot the WOE values for all bins across all variables
grid.arrange(woe_plots[[1]],woe_plots[[2]],woe_plots[[3]],woe_plots[[4]])
grid.arrange(woe_plots[[5]],woe_plots[[6]],woe_plots[[7]],woe_plots[[8]])
grid.arrange(woe_plots[[9]],woe_plots[[10]],woe_plots[[11]],woe_plots[[12]])
grid.arrange(woe_plots[[13]],woe_plots[[14]],woe_plots[[15]])

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                                    MODEL BUILDING AND EVALUATION-UNBALANCED DATA
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

#Copy the final data frame for the model.
final_data_for_model<-final_cust_data_iv_woe[,-1]

# Splitting the data into train and test dataset
set.seed(1)
split_indices <- sample.split(final_data_for_model$Performance_Tag, SplitRatio = 0.70)

train <- final_data_for_model[split_indices, ]
test <-  final_data_for_model[!split_indices, ]

# 0.6999971
nrow(train)/nrow(final_data_for_model)

# 0.3000029
nrow(test)/nrow(final_data_for_model)

#---------------------------------------------------------Model 1: Logistic Regression---------------------------------------------------------------------------    

#Perform the Inital model with all variables
logistic_1 <- glm(Performance_Tag ~ ., family = "binomial", data = train)
summary(logistic_1)

#Run Step AIC function to get the sg=ignificant variables
logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)

vif(logistic_2)

logistic_3 <- glm(formula = Performance_Tag ~DPD_90_12_months+ PLtrade_Open_12months+ DPD_30_12_months+ 
                    Trade_Open_6months + Total_No_of_Trades + PLtrade_Open_6months + Enquiries_6months + 
                    DPD_90_6_months+ DPD_60_12_months + DPD_60_6_months,family = "binomial", data = train)
summary(logistic_3)
sort(vif(logistic_3))

#PLtrade_Open_6months has highest p-value 0.87319
logistic_4 <- glm(formula = Performance_Tag ~DPD_90_12_months+ PLtrade_Open_12months+ DPD_30_12_months+ 
                    Trade_Open_6months + Total_No_of_Trades + Enquiries_6months + 
                    DPD_90_6_months+ DPD_60_12_months + DPD_60_6_months,family = "binomial", data = train)
summary(logistic_4)
sort(vif(logistic_4))

#DPD_60_12_months has highest p-value 0.82364
logistic_5 <- glm(formula = Performance_Tag ~DPD_90_12_months+ PLtrade_Open_12months+ DPD_30_12_months+ 
                    Trade_Open_6months + Total_No_of_Trades + Enquiries_6months + 
                    DPD_90_6_months+DPD_60_6_months,family = "binomial", data = train)
summary(logistic_5)
sort(vif(logistic_5))

#Trade_Open_6months has highest p-value 0.74050
logistic_6 <- glm(formula = Performance_Tag ~DPD_90_12_months+ PLtrade_Open_12months+ DPD_30_12_months+ 
                    Total_No_of_Trades + Enquiries_6months + 
                    DPD_90_6_months+DPD_60_6_months,family = "binomial", data = train)
summary(logistic_6)
sort(vif(logistic_6))

#Total_No_of_Trades has highest p-value 0.11910
logistic_7 <- glm(formula = Performance_Tag ~DPD_90_12_months+ PLtrade_Open_12months+ DPD_30_12_months+ 
                    + Enquiries_6months + DPD_90_6_months+DPD_60_6_months,family = "binomial", data = train)
summary(logistic_7)
sort(vif(logistic_7))


#Considering the logistic_7 as the final model as all the varibales has less p-value and are significant
final_model <- logistic_7

#---------------------------------------------------------Model 1: Logistic Regression Evaluation ---------------------------------------------------------------------------    

#1 Accuracy, Sensitivity, Specificity

  #Predicted probabilities of response for the test data
  test_pred = predict(final_model, type = "response",newdata = test[,-16])
  summary(test_pred)
  
  #Get the Probabilities of the test data
  test$prob <- test_pred
  View(test)
  
  summary(test$prob)
  
  # Let's use the probability cutoff of at 50% which is  0.0417
  test_pred_response   <- as.factor(ifelse(test_pred >=  0.042, "1", "0"))
  test$Performance_Tag <- as.factor(test$Performance_Tag)
  
  test_conf <- confusionMatrix(test_pred_response, test$Performance_Tag, positive = "1")
  #Accuracy : 0.572 Sensitivity : 0.70046  Specificity : 0.56639
  #              Reference
  #  Prediction    0      1
  #    0         11171   260
  #    1          8552   608
  
  #Accuracy is low hence need to find the suitable cut-off for the probabilities
  test_conf
  
  ## As we can see that the Sensitivity is quite low, so we need to find the optimal cutoff to get balanced Accuracy, Sensitivity & Specificity 
  perform_fn <- function(cutoff) 
  {
    predicted_response <- factor(ifelse(test_pred >= cutoff, "1", "0"))
    conf <- confusionMatrix(predicted_response, test$Performance_Tag, positive = "1")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
  }
  # Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 100 X 3.
  s = seq(0.01355,0.12437,length=100)
  
  OUT = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i])
  } 
  
  #Plot all the cut-offs
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0.01,0.15,length=50),seq(0.01,0.15,length=50),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0.08,.70,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.025)]
  
  #Apply the cut-off for the prediction
  predicted_response <- factor(ifelse(test_pred >= cutoff, "1", "0"))
  conf_final  <- confusionMatrix(predicted_response, test$Performance_Tag, positive = "1")
  
  #             Reference
  #Prediction     0      1
  #0            12497   331
  #1             7226   537
  
  #Get the accuracy, Sensitivity and Specificity  
  Accuracy    <- conf_final$overall[1]  # Accuracy    : 0.633
  Accuracy
  Sensitivity <- conf_final$byClass[1]  # Sensitivity : 0.61866
  Sensitivity
  Specificity <- conf_final$byClass[2]  # Specificity : 0.63363
  Specificity
  
#2 Calculating ROC Curve AUC  and KS Statistic for both train and test data
  
  #Predict the test data with the final Model
  testPrediction = predict(final_model, type = "response",newdata = test[,-16])
 
  #Set the cut-off
  predicted_response <- as.factor(ifelse(testPrediction >= cutoff, 1, 0))
  
  #Get the actual and Predicted Performance
  actual_test<- ifelse(test$Performance_Tag =="1",1,0)
  predicted_response <- ifelse(predicted_response=="1",1,0)
  
  #Function to create prediction objects
  pred<-prediction(predicted_response,actual_test)
  
  #Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
  perf <- performance(pred,"tpr","fpr")
  
  #precision/recall curve (x-axis: recall, y-axis: precision)
  perf1 <- performance(pred, "prec", "rec")
  plot(perf1)
  
  # sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
  perf2 <- performance(pred, "sens", "spec")
  plot(perf2)

  #KS Statistics KS is the maximum difference between the cumulative true positive and cumulative false positive rate. 0.2562527
  ks= max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
  
  #Plot the ROC Cure for TPR and FPR
  plot(perf,col = "red",main=paste0(' KS=',round(ks*100,1),'%'))
  lines(x = c(0,1),y=c(0,1))
  print(ks);
  
  #Calculate the AUC Value
  auc <- performance(pred, measure = "auc")@y.values
  print(auc);
  #KS Statistics for the Test  data.KS :  0.2522893  AUC : 0.6261447
 
#3 Gain and Lift Chart
  
  #Create New Data Frame for gain and lift charts
  gainLiftModel<-final_data_for_model
  
  #Factor the Performance varibale as good and bad customer
  gainLiftModel$Performance_Tag <- as.factor(ifelse(gainLiftModel$Performance_Tag == 1, "bad", "good"))
 
  # Finding the probaility of Performance_Tag all original data
  gainLiftModel$prob <- predict(final_model, newdata = gainLiftModel[,-16], type = "response")
  
  #Making new data frame with Actual Response and Predicted Probability
  new_df <-dplyr::select(gainLiftModel, Actual_Resp = Performance_Tag, Prob = prob)
  
  #Create the predicted Response column based on probability cutoff
  new_df$Predicted_Resp <- factor(ifelse(new_df$Prob >= cutoff, "yes", "no")) 
  
  # Sorting final_df in descending order of probability oe response
  new_df_dec_prob <- arrange(new_df, desc(Prob))
  
  #Creating Lift and Gain chart
  lift <- function(labels , predicted_prob, groups=10) {
    
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
  
  # Creating a table of cumulative gain and lift 
  predicted_response <- as.factor(ifelse(new_df_dec_prob$Actual_Resp=="good",1,0))
  liftChart = lift(predicted_response, new_df_dec_prob$Prob, groups = 10)

  # Lift Chart and its intrepretation
  plot(liftChart$bucket,liftChart$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
 	
#-----------------------------------GLM MODEL PARAMETERS FOR  IMBALANCED DATA------------------------------------------

#1. Accuracy                                                          63.3
#2. Sensitivity (True positive rate)                                  61.8 
#3. Specificity (True negative rate)                                  63.363
#4. Precision/Specificity: how many selected instances are relevant.   0.042
#5. Recall/Sensitivity: how many relevant instances are selected.      1.000
#6. F1 score: harmonic mean of precision and recall.                   0.040
#7. AUC: relation between true-positive rate and false positive rate.  0.6261447
#8. KS-Static Paramaer												   0.2522893

#---------------------------------------------------------Model 2: Random Forest ---------------------------------------------------------------------------    

#Get the Data
data_For_randomForest <- final_cust_data_iv_woe[,-1]
data_For_randomForest$Performance_Tag <- as.factor(ifelse(data_For_randomForest$Performance_Tag==1,"yes","no"))

#Split the Indices and extract train and test datasets
set.seed(1010)
split_indices <- sample.split(data_For_randomForest$Performance_Tag, SplitRatio = 0.70)
train_rf <- data_For_randomForest[split_indices, ]
test_rf  <- data_For_randomForest[!split_indices, ]
  
#Apply the random forest model
randomForest_model <- randomForest(Performance_Tag~.,data = train_rf, proximity = F, do.trace = T, mtry = 5,ntree=500)

summary(randomForest_model)

#Dotchart of variable importance as measured by a Random Forest
varImpPlot(randomForest_model,sort = T,main="Variable Importance",n.var=5)
  
#Node Purity based Variable Importance
imp_var_by_np  <- data.frame(importance(randomForest_model,type=2))

imp_var_by_np$Variables <- row.names(imp_var_by_np)
imp_var_by_np[order(imp_var_by_np$MeanDecreaseGini,decreasing = T),]
# Variables               MeanDecreaseGini             
# Avg_CC_Util_12               342.92330 
# Enquiries_12months           308.96799 
# Outstanding_Balance          292.74329 
# Enquiries_6months            241.82113 
# Total_No_of_Trades           231.41334 
# Trade_Open_6months           210.89148 
# PLtrade_Open_12months        198.76549 
# Trade_Open_12months          165.88994 
# PLtrade_Open_6months         157.08826 
# DPD_90_12_months             114.90658 
# DPD_60_12_months              95.84075 
# DPD_30_6_months               63.78867 
# DPD_30_12_months              55.27529 
# DPD_90_6_months               37.56561 
# DPD_60_6_months               21.49712 

predtest_rf = predict(randomForest_model ,test_rf[,-16],type = "prob")
test$predictProb<-predict(randomForest_model ,test_rf[,-16],type = "prob")
summary(predtest_rf)

# calculate the sens, spec and acc for different cutoff values
perform_fn <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(predtest_rf[,2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance_Tag, positive = "yes")
  acc  <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out  <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)
OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values
for(i in 1:100)
{
  OUT_rf[i,] = perform_fn(s[i])
} 

#Plot the Sensitivity SPecificity Accuracy
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0.073,.90,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]

#best_cutoff @ 0.01989899
predicted_response_22 <- factor(ifelse(predtest_rf[, 2] >= best_cutoff, "yes", "no"))

#Get the Accuracy : 0.6168,Sensitivity : 0.59908 and Specifity : 0.61760
confusionMatrix(predicted_response_22, test_rf[, 16], positive = "yes")

#Get Precision : 0.042 ,Recall: 1.000,F1 : 0.040
accuracy.meas(test_rf[, 16], predicted_response_22)

#Calculate the Area under ROC 0.608
roc.curve(test_rf[, 16], predicted_response_22, plotit = F)

#-------------------------------------RF MODEL PARAMETERS FOR  IMBALANCED DATA------------------------------------------

#1. Accuracy                                                          61.68
#2. Sensitivity (True positive rate)                                  59.908 
#3. Specificity (True negative rate)                                  61.760
#4. Precision/Specificity: how many selected instances are relevant.   0.042
#5. Recall/Sensitivity: how many relevant instances are selected.      1.000
#6. F1 score: harmonic mean of precision and recall.                   0.040
#7. AUC: relation between true-positive rate and false positive rate.  0.608


#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#													                       MODEL BUILDING AND EVALUATION-BALANCED
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Get the Data
data_to_balance <- final_cust_data_iv_woe[,-1]
#data_to_balance$Performance_Tag <- as.factor(ifelse(data_to_balance$Performance_Tag==1,"yes","no"))

#Split the Indices and extract train and test datasets
set.seed(1010)
split_indices <- sample.split(data_to_balance$Performance_Tag, SplitRatio = 0.70)
train_balance <- data_to_balance[split_indices, ]
test_balance  <- data_to_balance[!split_indices, ]

#Show the count of the Defaulters and non Defaulters  #no :46020 yes : 2025
table(train_balance$Performance_Tag)            

#Oversampling, as defaulters are having less occurrence,this method will increase the defaulters untill matches default records
over_sampling_data  <- ovun.sample(Performance_Tag ~ .,  data = train_balance, method = "over", N = 48045*2, seed = 1)$data

#Undersampling,as defaulters are having less occurrence,this method will descrease the non defaulters untill matches default records, 
under_sampling_data <- ovun.sample(Performance_Tag ~ .,  data = train_balance, method = "under", N = 6000, seed = 1)$data

# Mixed Sampling, apply both under sampling and over sampling on this imbalanced data
both_sampling_data  <- ovun.sample(Performance_Tag ~ .,  data = train_balance, method = "both",N=48045, p=0.5, seed=222)$data

#ROSE Sampling, this helps us to generate data synthetically. It generates artificial data instead of dulicate data.
rose_sampling_data  <- ROSE(Performance_Tag ~ ., data = train_balance, seed = 111)$data

TraindataForSmote <- train_balance
TraindataForSmote$Performance_Tag <- as.factor(TraindataForSmote$Performance_Tag)
TestdataForSmote <- test_balance
TestdataForSmote$Performance_Tag <- as.factor(TestdataForSmote$Performance_Tag)

#SMOTE(Synthetic Minority Over-sampling Technique) Sampling Since Data has less defaulters, increasing it with 2000 and keeping the non-defaulters class to 100.
smote_sample_data <- SMOTE(Performance_Tag ~ ., data = TraindataForSmote, perc.under=100,perc.over = 2000)

table(rose_sampling_data$Performance_Tag)       #no :24064  yes : 23981
table(both_sampling_data$Performance_Tag)       #no :24032  yes : 24013
table(over_sampling_data$Performance_Tag)       #no :46020  yes : 50070
table(under_sampling_data$Performance_Tag)      #no : 3975  yes :  2025
table(smote_sample_data$Performance_Tag)        #no : 40500 yes :  42525

# Apply logistic Regression on different sampling data
over_classifier  = glm(formula = Performance_Tag ~ ., family = binomial, data = over_sampling_data)
under_classifier = glm(formula = Performance_Tag ~ ., family = binomial, data = under_sampling_data)
both_classifier  = glm(formula = Performance_Tag ~ ., family = binomial, data = both_sampling_data)
rose_classifier  = glm(formula = Performance_Tag ~ ., family = binomial, data = rose_sampling_data)
smote_classifier = glm(formula = Performance_Tag ~ ., family = binomial, data = smote_sample_data)

# Predicting the test set using Over sampling classifier Acc : 0.5545 Sens : 0.73157  Speci: 0.54667 AUC 0.639
over_probability_predict = predict(over_classifier, type = 'response', newdata = test_balance[-16])
y_pred_over = as.factor(ifelse(over_probability_predict >=  0.5, "1", "0"))
confusionMatrix(y_pred_over, as.factor(test_balance$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_over)

#Predicting the test set using Under sampling classifier Accuracy : 0.8389 Sensitivity : 0.26037 Specificity : 0.86437 AUC 0.562
under_probability_predict = predict(under_classifier, type = 'response', newdata = test_balance[-16])
y_pred_under = as.factor(ifelse(under_probability_predict >=  0.5, "1", "0"))
confusionMatrix(y_pred_under, as.factor(test_balance$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_under)

#Predicting the test set using Mixed sampling classifier Accuracy : 0.5748 Sensitivity : 0.69240  Specificity : 0.56964 AUC 0.631
both_probability_predict = predict(both_classifier, type = 'response', newdata = test_balance[-16])
y_pred_both = as.factor(ifelse(both_probability_predict >=  0.5, "1", "0"))
confusionMatrix(y_pred_both, as.factor(test_balance$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_both)

#Predicting the test set using ROSE classifier Accuracy : 0.5982 Sensitivity : 0.66820 Specificity : 0.59509 AUC 0.632
rose_probability_predict = predict(rose_classifier, type = 'response', newdata = test_balance[-16])
y_pred_rose = as.factor(ifelse(rose_probability_predict >=  0.5, "1", "0"))
confusionMatrix(y_pred_rose, as.factor(test_balance$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_rose)

#Predicting the test set using SMOTE classifier Accuracy : 0.5619 Sensitivity : 0.70968  Specificity : 0.55539  AUC 0.636
smote_probability_predict = predict(smote_classifier, type = 'response', newdata = test_balance[-16])
y_pred_smote = as.factor(ifelse(smote_probability_predict >=  0.5, "1", "0"))
confusionMatrix(y_pred_smote, as.factor(TestdataForSmote$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_smote)

#Among all ROSE balancing are giving better accuracy and sensitivity
rose_balanced_step_model = step(rose_classifier, direction = "both")
summary(rose_balanced_step_model)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(rose_balanced_step_model)

#Taking the Last Parameters of the STEP function
rose_balanced_model_2 <- glm(formula = Performance_Tag ~ DPD_60_6_months + DPD_30_6_months + DPD_90_12_months + 
                               DPD_30_12_months + Avg_CC_Util_12 + Trade_Open_12months + 
                               PLtrade_Open_6months + PLtrade_Open_12months + Enquiries_6months + 
                               Enquiries_12months + Outstanding_Balance + Total_No_of_Trades,family = "binomial", 
                               data = rose_sampling_data)
summary(rose_balanced_model_2)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(rose_balanced_model_2)

#DPD_60_6_months has high p-value 0.016829 compared to others
rose_balanced_model_3 <- glm(formula = Performance_Tag ~ DPD_30_6_months + DPD_90_12_months + 
                               DPD_30_12_months + Avg_CC_Util_12 + Trade_Open_12months + 
                               PLtrade_Open_6months + PLtrade_Open_12months + Enquiries_6months + 
                               Enquiries_12months + Outstanding_Balance + Total_No_of_Trades,family = "binomial", 
                             data = rose_sampling_data)
summary(rose_balanced_model_3)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(rose_balanced_model_3)

#Outstanding_Balance has high p-value  0.011052 compared to others
rose_balanced_model_4 <- glm(formula = Performance_Tag ~ DPD_30_6_months + DPD_90_12_months + 
                               DPD_30_12_months + Avg_CC_Util_12 + Trade_Open_12months + 
                               PLtrade_Open_6months + PLtrade_Open_12months + Enquiries_6months + 
                               Enquiries_12months + Total_No_of_Trades,family = "binomial", 
                             data = rose_sampling_data)
summary(rose_balanced_model_4)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(rose_balanced_model_4)

#Total_No_of_Trades has high p-value  0.011219 compared to others
rose_balanced_model_5 <- glm(formula = Performance_Tag ~ DPD_30_6_months + DPD_90_12_months + 
                               DPD_30_12_months + Avg_CC_Util_12 + Trade_Open_12months + 
                               PLtrade_Open_6months + PLtrade_Open_12months + Enquiries_6months + 
                               Enquiries_12months,family = "binomial", 
                             data = rose_sampling_data)
summary(rose_balanced_model_5)

# Checking the variance inflation factor to detect the highly correlated independent variable.
vif(rose_balanced_model_5)

rose_balanced_final_model <- rose_balanced_model_5

#Accuracy : 0.5937 Sensitivity : 0.66935  Specificity : 0.59038 AUC 0.630
rose_balanced_prediction = predict(rose_balanced_final_model, type = 'response', newdata = test_balance[-16])
y_pred_rose = as.factor(ifelse(rose_balanced_prediction >=  0.5, "1", "0"))
confusionMatrix(y_pred_rose, as.factor(test_balance$Performance_Tag), positive = "1")
roc.curve(test_balance$Performance_Tag, y_pred_rose)

#function to calculate acc, sens, spec for different cutoff
rose_balance_perform_fn <- function(cutoff) {
    y_pred = as.factor(ifelse(rose_balanced_prediction >=  cutoff, "1", "0"))
    conf <- confusionMatrix(y_pred, as.factor(test_balance$Performance_Tag), positive = "1")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc)))
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
}
summary(rose_balanced_prediction)

s = seq(0.24,0.75 ,length=100)
OUT_rose = matrix(0,100,3)

for(i in 1:100){
  OUT_rose[i,] = rose_balance_perform_fn(s[i])
}

#Plot the accuracy sensitivity and specificity for all cut-off value to determine the actual cut-off
plot(s, OUT_rose[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.25,0.7,length=5),seq(0.25,0.7,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rose[,2],col="darkgreen",lwd=2)
lines(s,OUT_rose[,3],col=4,lwd=2)
box()
legend(0.5,.70, col=c(1,"darkgreen", 1,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

best_cutoff <- s[which(abs(OUT_rose[,1]-OUT_rose[,2])== min(abs(OUT_rose[,1]-OUT_rose[,2])))]

predicted_response_sampling <- factor(ifelse(rose_balanced_prediction >= best_cutoff, "1", "0"))

#Get the accuracy : 0.6229,Sensitivity : 0.63364 and Specifity : 0.62242
confusionMatrix(predicted_response_sampling, factor(test_balance[, 16]), positive = "1")

#Function to create prediction objects
pred<-prediction(as.numeric(predicted_response_sampling),as.numeric(test_balance$Performance_Tag))

#Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

# sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
perf2 <- performance(pred, "sens", "spec")
plot(perf2)

#KS Statistics KS is the maximum difference between the cumulative true positive and cumulative false positive rate. 0.2560611
ks = max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# AUC : 0.628
roc.curve(test_balance$Performance_Tag, predicted_response_sampling)

#Get Precision : 0.042 ,Recall: 1.000,F1 : 0.040
accuracy.meas(test_balance[, 16], predicted_response_sampling)


#-----------------------------GLM MODEL PARAMETERS FOR ROSE BALANCED DATA----------------------------------------------
#1. Accuracy                                                          62.29
#2. Sensitivity (True positive rate)                                  63.364 
#3. Specificity (True negative rate)                                  62.242
#4. Precision/Specificity: how many selected instances are relevant.   0.042
#5. Recall/Sensitivity: how many relevant instances are selected.      1.000
#6. F1 score: harmonic mean of precision and recall.                   0.040
#7. AUC: relation between true-positive rate and false positive rate.  0.628

#------------------------------------------------- APPLYING RANDOM FOREST MODEL ON BALANCED DATA----------------------

#Get the Data
data_For_RF_Sampling <- final_cust_data_iv_woe[,-1]
data_For_RF_Sampling$Performance_Tag <- as.factor(ifelse(data_For_RF_Sampling$Performance_Tag==1,"yes","no"))

#ROSE Sampling, this helps us to generate data synthetically. It generates artificial data instead of dulicate data.
rose_sampling_data  <- ROSE(Performance_Tag ~ ., data = data_For_RF_Sampling, seed = 111)$data

#Split the Indices and extract train and test datasets
set.seed(1010)
split_indices <- sample.split(rose_sampling_data$Performance_Tag, SplitRatio = 0.70)
train_rf_rose_sampling <- rose_sampling_data[split_indices, ]
test_rf_rose_sampling  <- rose_sampling_data[!split_indices, ]

rForest_RoseSampling_model <- randomForest(Performance_Tag~.,data = train_rf_rose_sampling, proximity = F,mtry = 5,ntree=200)
summary(rForest_RoseSampling_model)

#Dotchart of variable importance as measured by a Random Forest
varImpPlot(rForest_RoseSampling_model,sort = T,main="Variable Importance",n.var=5)
  
#Node Purity based Variable Importance
imp_var_by_np  <- data.frame(importance(rForest_RoseSampling_model,type=2))
imp_var_by_np$Variables <- row.names(imp_var_by_np)
imp_var_by_np[order(imp_var_by_np$MeanDecreaseGini,decreasing = T),]
#Variables                   MeanDecreaseGini
#DPD_90_6_months               2112.085
#DPD_60_6_months               1998.652
#Enquiries_12months            1981.827
#Outstanding_Balance           1977.526
#DPD_30_6_months               1877.072
#DPD_90_12_months              1861.771
#PLtrade_Open_12months         1778.904
#DPD_30_12_months              1538.262
#DPD_60_12_months              1522.739
#Avg_CC_Util_12                1511.084
#Trade_Open_12months           1234.227
#Enquiries_6months             1192.998
#Trade_Open_6months            1146.718
#PLtrade_Open_6months          1144.573
#Total_No_of_Trades            1143.526

predtest_rose_sampling_rf = predict(rForest_RoseSampling_model ,test_rf_rose_sampling[,-16],type = "prob")
summary(predtest_rose_sampling_rf)

#Accuracy 0.718747  Sensitivity :0.7368267 Specificity : 0.7007653
predicted_response <- as.factor(ifelse(predtest_rose_sampling_rf[,2] >= 0.5, "yes", "no"))
conf <- confusionMatrix(predicted_response, test_rf_rose_sampling$Performance_Tag, positive = "yes")
acc  <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]

# calculate the sens, spec and acc for different cutoff values
perform_fn_rose_sampling_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(predtest_rose_sampling_rf[,2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf_rose_sampling$Performance_Tag, positive = "yes")
  acc  <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out  <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#Creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)
OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values
for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rose_sampling_rf(s[i])
} 

#Plot the Sensitivity SPecificity Accuracy
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0.073,.90,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]

predicted_response_rf_sampling <- factor(ifelse(predtest_rose_sampling_rf[,2] >= best_cutoff_rf, "yes", "no"))

#Accuracy : 0.719 Sensitivity : 0.7171 Specificity : 0.7209
confusionMatrix(predicted_response_rf_sampling, test_rf_rose_sampling[, 16], positive = "yes")

#Get the actual and Predicted Performance
sampling_actual_test_rf<- ifelse(test_rf_rose_sampling$Performance_Tag =="yes",1,0)
sampling_predicted_response_rf <- ifelse(predicted_response_rf_sampling=="yes",1,0)

pred_rf_sampling<-prediction(sampling_actual_test_rf,sampling_predicted_response_rf)

#Computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf <- performance(pred_rf_sampling,"tpr","fpr")
plot(perf)

#precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred_rf_sampling, "prec", "rec")
plot(perf1)

# sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
perf2 <- performance(pred_rf_sampling, "sens", "spec")
plot(perf2)

#KS Statistics KS is the maximum difference between the cumulative true positive and cumulative false positive rate. 0.2562527
ks = max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# AUC : 0.719
roc.curve(test_rf_rose_sampling$Performance_Tag, predicted_response_rf_sampling)

#Get Precision : 0.499 ,Recall: 1.000,F1 : 0.332
accuracy.meas(test_rf_rose_sampling[, 16], predicted_response_rf_sampling)

#--------------------------RF MODEL PARAMETERS FOR ROSE BALANCED DATA-------------------------------------
#1. Accuracy                                                            0.719                                 
#2. Sensitivity (True positive rate)                                    0.7171
#3. Specificity (True negative rate)                                    0.7209
#4. Precision/Specificity: how many selected instances are relevant.     0.496   
#5. Recall/Sensitivity: how many relevant instances are selected.        1.000
#6. F1 score: harmonic mean of precision and recall.                     0.332   
#7. AUC: relation between true-positive rate and false positive rate.    0.718
#8. KS- Static                                                           0.4356921

# CONCLUSION : RF Model with ROSE BALANCED DATA Has given better Model Evaluation Parameter 

#---------------------------------------------------------------------------------------------------------------------
#                                         BUILDING APPLICATION SCORE CARD
#---------------------------------------------------------------------------------------------------------------------
# Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.
str(final_cust_data_iv_woe)
colnames(final_cust_data_iv_woe)

final_cust_data_iv_woe$probability <- predict(rose_balanced_final_model, newdata = final_cust_data_iv_woe[,-c(1,17)], type = "response")
str(final_cust_data_iv_woe$probability)

# Created a function to create application score card
Cal_application_Score <-
  function(App = NULL,prob = NULL,perf = NULL,score = NULL,score_points = NULL,Odd = NULL) {
    good <- 1 - prob
    oddgood <- good / prob
    odd <- log(oddgood)
    factor <- (score_points / log(2))
    offset <- score - (factor * log(Odd))
    y <- offset + (factor * odd)
    x <-
      as.data.frame(cbind.data.frame(App, perf, prob, good, oddgood, odd, y))
    colnames(x) <-
      c("Applicant","Performance","probability","pgood","OddsGood","ln_Odds","ApplicatioScores")
    z <- x[order(-x[, 5]), ]
    return(z)
  }


Applicant_ScoreCard<- Cal_application_Score(final_cust_data_iv_woe$Application_ID,final_cust_data_iv_woe$probability,final_cust_data_iv_woe$Performance_Tag,400,20,10)

View(Applicant_ScoreCard)
str(Applicant_ScoreCard)

#Plot score and ln odds of app scorecard
ggplot(Applicant_ScoreCard,aes(x = Applicant_ScoreCard$ApplicatioScores,y=Applicant_ScoreCard$ln_Odds)) + geom_line()

table(Applicant_ScoreCard$Performance)

#Calculate the threshold scores
summary(Applicant_ScoreCard$ApplicatioScores)

#Probability cutoff of 50.
Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$probability >= 0.50, "1", "0"))

# Confusion matrix.
conf <- confusionMatrix(Applicant_ScoreCard$Pred, as.factor(Applicant_ScoreCard$Performance), positive = "1")
conf
#Accuracy : 0.5985
#Sensitivity : 0.67093         
#Specificity : 0.59527

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$probability >= cutoff, "1", "0"))
  conf <- confusionMatrix(Applicant_ScoreCard$Pred, as.factor(Applicant_ScoreCard$Performance), positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy")
  return(OUT)
}

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- as.data.frame(cbind.data.frame(s,OUT))

colnames(cutoff) <- c("cutoff","sensitivity", "specificity", "accuracy")

cutoff$diff <- abs(cutoff$sensitivity - cutoff$specificity)

x <- min(cutoff$diff)

y <- which(cutoff$diff == x)

cutoff[y,1] #0.5148485

# lets find out the cut value of our application score
Applicant_ScoreCard$ApplicatioScores[which(abs(Applicant_ScoreCard$probability) >= abs(cutoff[y,1]))][1]

# cut off value 331.8453

Applicant_ScoreCard$Pred <- factor(ifelse(Applicant_ScoreCard$probability >= cutoff[y,1], "1", "0"))

cmfinal <- confusionMatrix(Applicant_ScoreCard$Pred, as.factor(Applicant_ScoreCard$Performance), positive = "1")
cmfinal
acc <- cmfinal$overall[1]
sens <- cmfinal$byClass[1]
spec <- cmfinal$byClass[2]

acc #63.1%
sens #63.6%
spec #63.1%


table(Applicant_ScoreCard$Pred)
appscore_default_percent <- round((table(Applicant_ScoreCard$Pred)[2]/(table(Applicant_ScoreCard$Pred)[1] + table(Applicant_ScoreCard$Pred)[2]))*100, digits = 1)
appscore_default_percent
# 38.2%


table(final_cust_data_iv_woe$Performance_Tag)
Actual_default_percent <- round((table(final_cust_data_iv_woe$Performance_Tag)[2]/(table(final_cust_data_iv_woe$Performance_Tag)[1] + table(final_cust_data_iv_woe$Performance_Tag)[2]))*100, digits = 1)
Actual_default_percent

## 4.2%

## OBSERVATION: There is a wide gap in the actual and predicted response.

###############################################################
## Application scorecard for Rejected data
###############################################################
str(rejected_customers)
rejected_customers$probability <- predict(rose_balanced_final_model, newdata = rejected_customers[,-c(1)], type = "response")

# Created a function to create application score card for Rejected data
Cal_application_Score_rejected <-
  function(App = NULL,prob = NULL,score = NULL,score_points = NULL,Odd = NULL) {
    good <- 1 - prob
    oddgood <- good / prob
    odd <- log(oddgood)
    factor <- (score_points / log(2))
    offset <- score - (factor * log(Odd))
    p <- offset + (factor * odd)
    q <- as.data.frame(cbind.data.frame(App, prob, good, oddgood, odd, p))
    colnames(q) <-
      c("Applicant","probability","pgood","OddsGood","ln_Odds","ApplicationScores")
    r <- q[order(-q[, 6]), ]
    return(r)
  }

Rejected_Applicant_ScoreCard<- round(Cal_application_Score_rejected(rejected_customers$Application_ID,rejected_customers$probability,400,20,10), digits = 0)

View(Rejected_Applicant_ScoreCard)
str(Rejected_Applicant_ScoreCard)

Rejected_Applicant_ScoreCard$Pred <- factor(ifelse(Rejected_Applicant_ScoreCard$probability >= cutoff[y,1], "1", "0"))

table(Rejected_Applicant_ScoreCard$Pred)

# predicted response using probability cutoff
table(Rejected_Applicant_ScoreCard$Pred)
reject_appscore_default_percent <- round((table(Rejected_Applicant_ScoreCard$Pred)[2]/(table(Rejected_Applicant_ScoreCard$Pred)[1] + table(Rejected_Applicant_ScoreCard$Pred)[2]))*100, digits = 1)
reject_appscore_default_percent
# 99.9%

# predicted response using score card cutoff
Rejected_Applicant_ScoreCard$predicted_response <- factor(ifelse(Rejected_Applicant_ScoreCard$ApplicationScores >= 332.6703, "0", "1"))
table(Rejected_Applicant_ScoreCard$predicted_response)
model_default_percent <- round((table(Rejected_Applicant_ScoreCard$predicted_response)[2]/(table(Rejected_Applicant_ScoreCard$predicted_response)[1] + table(Rejected_Applicant_ScoreCard$predicted_response)[2]))*100, digits = 1)
model_default_percent
# 99.9%

## OBSERVATION: The predicted response from probability cutoff and the response from score card cutoff is nearly equal.
# Bank has correctly classified these applicants as Rejected.
##--------------------------------------------------End----------------------------------------------------------------##