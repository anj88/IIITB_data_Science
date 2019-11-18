#---Checking and installing required packages -------#

packages = c("ggplot2","dplyr","stringr","readxl","tidyr","stats","zoo")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# ---- Data understanding and cleaning ----- #
loan_data <- read.csv("loan.csv")
#39717 obs of 111 varibales

#find out the columns containing all Not Applicable (NA) values (just for reference )
Loan_na<-sapply(loan_data, function(x)all(is.na(x)))
Loan_na<-as.data.frame(Loan_na)
Loan_na<-Loan_na[which(Loan_na$Loan_na=="TRUE"),0]
Loan_na<-as.data.frame(Loan_na)
#54 observations have been returned which have NA values

#Removing all the 54 values at first 
loan_data<-loan_data[, colSums(is.na(loan_data)) != nrow(loan_data)]
#now loan_data contains only 57 variables ,which implies the 54 columns 
#which contained all "NA" have been removed

#Cross verfiying if any column is still left having all "NA"s
all((is.na(loan_data[,c(1:57)])))
#Returns FALSE

########################################################################

#checking NA's from the rows -Loan Amount and Loan Status as these are the rows which are crucial for analysis
sum(is.na(loan_data$annual_inc)) #no NA values in annual income field
sum(is.na(loan_data$funded_amnt)) #no NA values in funded amount field
sum(is.na(loan_data$loan_status)) #no NA values status

#checking for NA values in other columns
NAloan<-as.data.frame(sapply(loan_data, function(x) sum(is.na(x))))
#its clear that NA va;lues are present in "mths_since_last_record" , mths_since_last_delinq" ,"pub_rec_bankruptcies" 
# "last_pymnt_d" , "last_pymnt_month" ,"last_pymnt_year" , "revol_util" .



#__________Checking for outliers__________________________________________________________________


boxplot(loan_data) # outliers are present in the annual income field .
boxplot(loan_data$annual_inc)
# finding out max and min values in annaul income field
max(loan_data$annual_inc) #6000000 (univariate analysis)
min(loan_data$annual_inc) #4000 (univariate analysis)

#__________identifying and removing the outliers_______________________________________________

outlier_values <- boxplot.stats(loan_data$annual_inc)$out #finds out the outlier values
max(outlier_values) #6000000
min(outlier_values)#145596
#removing the outliers
#loan_data<-loan_data[-which(loan_data$annual_inc>145596),]
###########################################################################################

typeof(loan_data$annual_inc)


####splittng the annual income into buckets

loan_data$income_in_k<-ifelse(loan_data$annual_inc<200000 ,'0-199999',
                              ifelse((loan_data$annual_inc>=200000 & loan_data$annual_inc<=399999),'200000-399999',
                                     ifelse((loan_data$annual_inc>=400000 & loan_data$annual_inc<599999),'400000-599999',                                  
                                            ifelse((loan_data$annual_inc>=600000 & loan_data$annual_inc<799999),'600000-799999',
                                                   ifelse((loan_data$annual_inc>=800000 & loan_data$annual_inc<799999),'800000-799999', 
                                                          ifelse((loan_data$annual_inc>=1000000 & loan_data$annual_inc<1199999),'1000000-1199999',
                                                                 ifelse((loan_data$annual_inc>=1200000 & loan_data$annual_inc<1399999),'1200000-1399999',   
                                                                        ifelse((loan_data$annual_inc>=1200000 & loan_data$annual_inc<=1299999),'1200000-1299999',   
                                                                               ifelse((loan_data$annual_inc>=1300000 & loan_data$annual_inc<1399999),'1300000-1399999',  
                                                                                      ifelse((loan_data$annual_inc>=1400000 & loan_data$annual_inc<=1500000),'1400000-1500000',  
                                                                                             
                                                                                             "0"
                                                                                      ))))))))))
##################################
#Checking if there are Inconsistent column names
names(loan_data)
length(names(loan_data))
#returns 58
##################################

#Checking if there is any duplicated entries for loans
sum(duplicated(loan_data$id))
#returns 0 that means no duplicate loan entries

###########################################
#Removal and Analysis of other rows 
attach(loan_data)
loan_data <- select(loan_data, -acc_now_delinq, -application_type, -chargeoff_within_12_mths, -collections_12_mths_ex_med, -delinq_amnt, - initial_list_status, -last_credit_pull_d, -open_acc, -policy_code, -pymnt_plan, -tax_liens, -title, -url ,-member_id ,-loan_amnt
,-funded_amnt_inv,-emp_title ,-pymnt_plan ,-url , -desc ,-title ,-zip_code ,-application_type ,-policy_code)
#pymnt_plan is having comon value of n across all rows 
#45 variables left now

#checking the data types
str(loan_data)

#Data Transformations For Analysis
loan_data$id <- as.factor(loan_data$id)

#- Converting Int_rate to number for calculation purpose
loan_data$int_rate <- as.numeric(str_sub(as.character(loan_data$int_rate),1,str_length(loan_data$int_rate)-1))

#Issue_date data formatting
loan_data$issue_d <- as.character(loan_data$issue_d)
loan_data$issue_d <- as.yearmon(loan_data$issue_d, "%b-%y")
loan_data$issue_month <- months(loan_data$issue_d)
loan_data$issue_quarter <- quarters(loan_data$issue_d)
loan_data$issue_year <- format(loan_data$issue_d, "%Y")


# annual income number formatting

loan_data$annual_inc <- as.integer(loan_data$annual_inc)

#- Converting revol_util to number for calculation purpose

loan_data$revol_util <- as.numeric(str_sub(as.character(loan_data$revol_util),1,str_length(loan_data$revol_util)-1))


#last_paymnt_d data formatting
loan_data$last_pymnt_d <- as.character(loan_data$last_pymnt_d)
loan_data$last_pymnt_d <- as.yearmon(loan_data$last_pymnt_d, "%b-%y") 
loan_data$last_pymnt_month <- months(loan_data$last_pymnt_d)
loan_data$last_pymnt_quarter <- quarters(loan_data$last_pymnt_d)
loan_data$last_pymnt_year <- format(loan_data$last_pymnt_d, "%Y")

#__________________________________________#
#last_pymnt_d 
#next_pymnt_d
#earliest_cr_line
#issue_d
#__________________________________________#

#___________________Univariate Analysis___________________________#

#_________________________Analysing Loan Amount___________________________#

loan_amnt_aggregate <- aggregate(id ~ loan_amnt, loan_data, length)
summary(loan_data$loan_amnt)
ggplot(data = loan_data, aes(y = loan_amnt)) + geom_boxplot()
ggplot(data = loan_data, aes(x = loan_amnt)) + geom_histogram(binwidth = 1000)

#most of the loan amount are in between 5,500 and 15,000 USD with some outlier of loan amount more than 30,000

#_________________________Analysing Funded Amount___________________________#

ggplot(data = loan_data, aes(x = funded_amnt)) + geom_histogram(binwidth = 1000)
#funded amount sees off same pattern as loan amount having unusual high quantity of funded amount of rounded off.

#_________________________Analysing Term__________________________________#

term_counts <- aggregate(id ~ term, loan_data, length)
#Visualizing through plot
ggplot(loan_data, aes(term, fill= term)) + geom_bar(stat = "count")

# With above visualizations, it seems either people are taking short term loan more than long term loan of 60 months
# it might be possible in future analysis that short term loan leads to higher monthly installment so it might be useful
# to see if short term loan are charging off more than long term.

#________________________Analysing Interest Rates_________________________#

summary(loan_data$int_rate)
ggplot(data = loan_data, aes(x = int_rate)) + geom_histogram(binwidth = 1)
#   Min.   1st Qu.  Median    Mean   3rd Qu.    Max. 
#   5.42    9.25    11.86     12.02   14.59    24.59 


#________________________Analyzing grades_________________________________#

grade_counts <- aggregate(id ~ grade, loan_data, length)
#Visualizing through plot
ggplot(loan_data, aes(grade, fill= grade)) + geom_bar(stat = "count")
#Insteresting graphs is seen from the loan grade.
# A grade count is less than grade B loan grade counts
# Probability of people taking loan grade B are the ones who comes in sweet spot where they need the money and also suitable to pay
# the money back. Whereas people whose loans are graded are A is lesser because of less loan application. 
# On the other hand, low loan for lower grades are because of high rejection of loan application.

#________________________Analyzing sub-grades_________________________________#

sub_grade_counts <- aggregate(id ~ sub_grade, loan_data, length)
sub_grade_avg_int_rate <- aggregate(int_rate ~ sub_grade, loan_data, mean)
#Visualizing through plot
ggplot(loan_data, aes(sub_grade, fill = grade)) + geom_bar(stat = "count")
ggplot(sub_grade_avg_int_rate, aes(x = sub_grade,y = int_rate)) + geom_bar(stat = "identity")
# when we dig deeper into subgrade , considering the fact that loan acceptance rate is high for A and B grade loan application
# It is understandable that lower section of higher grades will have more demand for loan application which is driving the loan application higher.
# However, what is fascinating is subgrade A4 & A5 loan applications are lower than B1 & B2 sub grade.

# It looks like we might be seeing some people who are in A4 & A5 grade are either not well off spending wisely and 

# also, it is interesting to see average interest rates for each subgrade, there is a strong correlation in interest rates.
# higher the subgrade is lower the interest rates are.

#________________________Analyzing employee_length_________________________________#

emp_length_counts <- aggregate(id ~ emp_length, loan_data, length)
#Visualizing through plot
ggplot(loan_data, aes(emp_length, fill= emp_length)) + geom_bar(stat = "count")
#shows employees with 10+ years of experience have the highest count of loan application
# There could be many reasons for it. people with more than 10 years of experience are bread-earner for the family.
# people with 10+ years of experience not only takes loan for themselves they also take loan for their children also.
# leading to high application.

#________________________Home_ownership wise analysis____________________#
Home_ownership_counts <- aggregate(id ~ home_ownership, loan_data, length)
ggplot(loan_data, aes(home_ownership, fill= home_ownership)) + geom_bar(stat = "count")
# This is usual graph showing people who does not have their own house have greater need for loans
# This is great driving factor to check if someone will need a loan or not. we can deep dive more into this in later plots

#________________________Annual Income wise analysis____________________#
#Visualizing through plot
ggplot(data = loan_data, aes(x = annual_inc)) + geom_histogram(binwidth = 50000)
#income graph is a natural skewed normal distribution with with a skewness towards lower income.
# it means there are very few number of people with very high annual income.

#________________________Verification Status wise analysis______________#
verification_status_wise_counts <- aggregate(id ~ verification_status, loan_data, length)
ggplot(data = loan_data, aes(x = verification_status)) + geom_histogram(stat = "count")
#Visualizing through plot
ggplot(loan_data, aes(verification_status, fill= verification_status)) + geom_bar(stat = "count")
# no proper insight has been shown here other than that there are very big number of people whose employment status has not been
#verified. we need to check if this leads to high charged off loans


#________________________Purpose wise analysis___________________________#
loan_purpose_counts <- aggregate(id ~ purpose, loan_data, length)

#Visualizing through plot
ggplot(loan_data, aes(x=purpose, fill=purpose)) + geom_bar(stat="count")
#shows debt_consolidation and credit card debt has highest count with others category third highest which may be for the various reasons

#_________________________Issue_date_trends______________________________#


Date_wise_loan_count <- aggregate(id ~ issue_d, loan_data, length)
ggplot(data = loan_data, aes(x = issue_d, fill = issue_d)) + geom_histogram(stat = "count")
# we can see that loan given by the organization is steadily growing with a very high rate.

Quarter_wise_loan_count <- aggregate(id ~ issue_quarter, loan_data, length)
ggplot(data = loan_data, aes(x = issue_quarter, fill = issue_quarter)) + geom_histogram(stat = "count")
# It shows that most the loans were issues during 4th quarter of the year.
# but this is part because of our previous graph, no of loans month on month is increasing steadily

month_wise_loan_count <- aggregate(id ~ issue_month, loan_data, length)
ggplot(data = loan_data, aes(x = issue_month, fill = issue_month)) + geom_histogram(stat = "count")
#this aligns with our quarterly data with highest number of loan counts in october, november & december


year_wise_loan_count <- aggregate(id ~ issue_year, loan_data, length)
ggplot(data = loan_data, aes(x = issue_year, fill = issue_year)) + geom_histogram(stat = "count")
#this aligns with our issue_d data with loan 


# Loan industry is booming with a very strong growth month by month. 
# There also should also be steady number of increase in charged off loans

#________________________Loan Status wise analysis______________#
Loan_status_wise_counts <- aggregate(id ~ loan_status, loan_data, length)
View(Loan_status_wise_counts)

#Visualizing through plot
ggplot(loan_data, aes(loan_status, fill= loan_status)) + geom_bar(stat = "count")
#Charged Off=5627
#Current=1140
#Fully Paid=32950



#________________________Zip Code wise analysis___________________________#           #--------------NOtRequired


#________________________Area(Address State) wise analysis________________#
Area_wise_counts <- aggregate(id ~ addr_state, loan_data, length)

#Visualizing through plot
ggplot(loan_data, aes(x=addr_state, fill=addr_state)) + geom_bar(stat="count")
#shows California, florida, new york & texas has highest number of loans.
# which is in turn because these states are more well to do states than others.
#california because of their tech industry, florida because of tourism, new york being the financial hub & texas being the manufacturing hub.

#________________________Analysis of dti____________________________#

dti_counts <- aggregate(id ~ dti, loan_data, length)

#Observed highest count of dti Between 12.5 and 17.5

#splittng the annual income into buckets


loan_data$dti_bucketing<-ifelse(loan_data$dti<05.00 ,'0-04.99',
                              ifelse((loan_data$dti>=05.00 & loan_data$dti<=09.99),'05.00-09.99',
                                     ifelse((loan_data$dti>=10.00 & loan_data$dti<15.00),'10.00-14.99',                                  
                                            ifelse((loan_data$dti>=15.00 & loan_data$dti<20.00),'15.00-19.99',
                                                   ifelse((loan_data$dti>=20.00 & loan_data$dti<25),'20.00-24.99', 
                                                          ifelse((loan_data$dti>=25.00 & loan_data$dti<30),'25.00-29.99',
                                                                 ifelse((loan_data$dti>=30.00 & loan_data$dti<35),'30.00-34.99',   
                                                                        ifelse((loan_data$dti>=35.00 & loan_data$dti<40),'35.00-39.99',   
                                                                               ifelse((loan_data$dti>=40.00 & loan_data$dti<45),'40.00-44.99',  
                                                                                      ifelse((loan_data$dti>=50 & loan_data$dti<55),'50.00-54.99',  
                                                                                             
                                                                                             "0"
                                                                                      ))))))))))

dti_bucket_wise_count <- aggregate(id ~ dti_bucketing, loan_data, length)

#Visualizing through plot
ggplot(data = loan_data, aes(x = dti_bucketing, fill = dti_bucketing)) + geom_histogram(stat = "count")

#highest count for 10-14.99 bucket

#________________________delinq_2years______________________________#

delinq_2yrs_counts <- aggregate(id ~ delinq_2yrs, loan_data, length)
#Visualizing through plot
ggplot(data = loan_data, aes(x = delinq_2yrs, fill = delinq_2yrs)) + geom_bar(stat = "count")
#Observed highest count for 0

#________________________inq_last_6mths______________________________#

inq_last_6mths_counts <- aggregate(id ~ inq_last_6mths, loan_data, length)
#Visualizing through plot
ggplot(data = loan_data, aes(x = inq_last_6mths, fill = inq_last_6mths)) + geom_bar(stat = "count")
#Observed 0 for most loans

#---- Bi-Variate Analysis ####

#-- we will check against categorical variables and figure out if any category has more probability of charged off loans

term_vs_loan_default <- aggregate(id ~ term+loan_status, loan_data, length)
term_vs_loan_default$term <- as.character(term_vs_loan_default$term)
term_vs_loan_default_36_months <- dplyr::filter(term_vs_loan_default, str_detect(term, "36 months"))
term_vs_loan_default_36_months$percentage <- (term_vs_loan_default_36_months$id/sum(term_vs_loan_default_36_months$id))*100
term_vs_loan_default_60_months <- dplyr::filter(term_vs_loan_default, str_detect(term, "60 months"))
term_vs_loan_default_60_months$percentage <- (term_vs_loan_default_60_months$id/sum(term_vs_loan_default_60_months$id))*100
term_vs_loan_default <- rbind(term_vs_loan_default_36_months,term_vs_loan_default_60_months)
ggplot(data = term_vs_loan_default, aes(x = term, y = id, fill = loan_status)) + geom_bar(stat = "identity", position = "fill")
# however the number of default loans are similar in both the terms but due to less number of 60 months tenure,
# we can say the there is greater share of loan defaulter share in 60 months bracket. giving us the clue that
# people taking longer duration loan are more likely to default on loan

# Loan grade and sub grade analysis
grade_vs_loan_default <- aggregate(id ~ grade+loan_status, loan_data, length)
sub_grade_vs_loan_default <- aggregate(id ~ grade+sub_grade+loan_status, loan_data, length)
ggplot(data = grade_vs_loan_default, aes(x = grade, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = grade_vs_loan_default, aes(x = grade, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")

ggplot(data = sub_grade_vs_loan_default, aes(x = sub_grade, y = id, fill = loan_status)) + geom_bar(stat = "identity")+ylab("% of loan applications")
ggplot(data = sub_grade_vs_loan_default, aes(x = sub_grade, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
# here is the interesting insight. There seems to be a strong correlation of likelihood of default as we go down the Loan grade or the loan subgrade
# total number of charged off loans are high in the Loan grade B but total percentage of defaulter are not so high
# which is an expected result

# Employee years of experience 

emp_len_analysis <- aggregate(id ~ emp_length+loan_status, loan_data, length)
ggplot(data = emp_len_analysis, aes(x = emp_length, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = emp_len_analysis, aes(x = emp_length, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
# No direct relation ship of likelihood of default with increasing or decreasing experience.

# Verification_status

ver_stat_analysis <- aggregate(id ~ verification_status+loan_status, loan_data, length)
ggplot(data = ver_stat_analysis, aes(x = verification_status, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = ver_stat_analysis, aes(x = verification_status, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
# No direct relation ship of likelihood of default with any verification status by count or percentage terms.

#home_ownership
home_own_analysis <- aggregate(id ~ home_ownership+loan_status, loan_data, length)
ggplot(data = home_own_analysis, aes(x = home_ownership, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = home_own_analysis, aes(x = home_ownership, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
# No direct relation ship of likelihood of default with home ownership.

# Income analysis
income_analysis <- aggregate(id ~ income_in_k+loan_status, loan_data, length)
ggplot(data = loan_data, aes(x = annual_inc, fill = loan_status)) + geom_histogram(binwidth = 50000)
ggplot(data = loan_data, aes(x = annual_inc, fill = loan_status)) + geom_histogram(binwidth = 50000,position = "fill")+ylab("% of loan applications")
ggplot(data = income_analysis, aes(x = income_in_k, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
# There seems to be no relation between that income affect people propensity to pay off loans or not.
#it might be because even though a person is earning enough, he or she might be spending a lot on other things.

#purpose analysis
purpose_analysis <- aggregate(id ~ purpose+loan_status, loan_data, length)
ggplot(data = purpose_analysis, aes(x = purpose, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = purpose_analysis, aes(x = purpose, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
#although maximum people are taking loan for debt consolidation and credit card bill payout.
#however small business have highest number of percentage of loan defaulters.
# Small business comes with a inherent risk of loss that increases the propensity of loan defaulters.

#State Wide analysis
state_analysis <- aggregate(id ~ addr_state+loan_status, loan_data, length)
ggplot(data = state_analysis, aes(x = addr_state, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = state_analysis, aes(x = addr_state, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")
#no further insights are created as we go and check the data state wide


#time-series analysis issue_d

issue_date_analysis <- aggregate(id ~ issue_d+loan_status, loan_data, length)
ggplot(data = issue_date_analysis, aes(x = issue_d, y = id, fill = loan_status)) + geom_bar(stat = "identity")
ggplot(data = issue_date_analysis, aes(x = issue_d, y = id, fill = loan_status)) + geom_bar(stat = "identity",position = "fill")+ylab("% of loan applications")

ggplot(data = issue_date_analysis, aes(x = issue_d, y = id, fill = loan_status)) + geom_line(aes(colour = loan_status))

# line graph shows us that there is a more steep rise in fully paid loans.
# however from 2010 loan default rate is increasing more than its previous years
# after 2011 there are some current loans are also there which means some of these loans will also contribute to Default loans.


# No. of installments_ analysis

# we can check at what stage people are starting to default on loan by calculating no of installment given

loan_data$no_of_installment_paid <- ceiling((loan_data$total_rec_prncp + loan_data$total_rec_int)/loan_data$installment)
ggplot(data = loan_data, aes(x = no_of_installment_paid, fill = loan_status)) + geom_histogram()
ggplot(data = loan_data, aes(x = no_of_installment_paid, fill = loan_status)) + geom_histogram(position = "fill")+ylab("% of loan applications")

#this is probably the most interesting graph on whole analysis.
# it says that the people who are defaulters make less than 30 installment.
# It means the risk of default stays high till a person has made 30 installment and then risk of default greatly reduce after 30 payments. 


#DTI vs Int_rate vs loan grade distribution
ggplot(data = loan_data, aes(x = dti, y = int_rate, colour = loan_status)) + geom_point()
# this above graph does not show any correlation or insights between debt to income ration vs interest rate offered
ggplot(data = loan_data, aes(x = loan_amnt, y = int_rate, colour = grade)) + geom_point() + facet_grid(.~loan_status)
# this above graph does not show any additional correlation or insights between debt to income ration vs interest rate offered


ggplot(data = loan_data, aes(x = loan_amnt, y = int_rate, colour = grade)) + geom_point() + facet_grid(.~issue_year)
# this is also very important insight. We can infer multiple information from it. 

#1. till 2010 there are no loan accepted beyond 25000. However in year 2011, 
# it seems that banks have been quite lenient in their loan acceptance procedure. organization is earn high profits by chargin high interest rate
# to lower grade customer.
#2. This is a dangerous line bank might be crossing because we are also seeing that with most of the high loan amount accepted have been accepted
#   for loan grade c, D, E, & F
#3. People who are much more capable to paying off the loan does not take high loan amount, which suggest they are smart enough to know just about how much money they need
#   and these people stay away from over lending.


ggplot(data = loan_data, aes(x = loan_amnt, y = int_rate, colour = grade)) + geom_point() + facet_grid(.~purpose)
# this graph gives a one more important insight 
# Most popular purpose for lower grade loans are Debt_consolidation, Credit card, small business & Home_improvement
# Whereas lower grade (D-G) loans are much more diversified.
# This suggest that Debt_consolidation, Credit card, small business & Home_improvement might have a future inherent risk of default.