#loading all the packages .assuming the packages have been pre installed
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(car)
library(MASS)
        
#loading dataset
r<-read.csv("CarPrice_Assignment.csv" ,stringsAsFactors = FALSE)
str(r)#checking the dataset
############Data Cleaning #################################
sum(is.na(r)) #no na present 
sum(duplicated(r))#all rows unique ,as it returns 0 
r<-separate(r,col=CarName ,into = c("car_name" ,"model"),sep=" ") #separating car names and models 
r<-r[,-4] #removal of the model column as it is not needed in analysis

sum(is.na(r$car_name)) #checking na in car names
#conversion of categorical varibales into factors____________________
r$car_name<-as.factor(r$car_name)
r$fueltype<-as.factor(r$fueltype)
r$aspiration<-as.factor(r$aspiration)
r$doornumber<-as.factor(r$doornumber)
r$carbody<-as.factor(r$carbody)
r$drivewheel<-as.factor(r$drivewheel)
r$enginelocation<-as.factor(r$enginelocation)
r$enginetype<-as.factor(r$cylindernumber)
r$cylindernumber<-as.factor(r$cylindernumber)
r$fuelsystem<-as.factor(r$fuelsystem)
r$symboling<-as.factor(r$symboling)
str(r) #checking to see if there are any character variables left

####################################################################################
#After initial analysis it seems there are spelling mistakes in car names
levels(r$car_name)
#correcting spelling mistakes
r$car_name<-gsub("maxda","mazda",r$car_name)
r$car_name<-gsub("nissan","Nissan",r$car_name)
r$car_name<-gsub("porcshce","porsche",r$car_name)
r$car_name<-gsub("toyouta","toyota",r$car_name)
r$car_name<-gsub("vokswagen","volkswagen",r$car_name)
r$car_name<-gsub("vw","volkswagen",r$car_name)
str(r) #car names converted to character 
r$car_name<-as.factor(r$car_name)

############Univariate and bivariate analysis##############################

#Checking for  outliers by considering few variables at a time   
boxplot(r[,1:5]) #no outliers in first 5 variables
boxplot(r[,6:10]) # wheelbase has outliers
boxplot(r[,11:15])# no considerable outliers
boxplot(r[,16:26])#peakrpm and price have outliers 

#removing outliers one by one (outlier treatment based on the previous box plots)
quantile(r$wheelbase,seq(0,1,0.01)) #jump between 99% to 100 %
r$wheelbase[which(r$wheelbase>115.544)]<-115.544

#outlier treatment on peakrpm and price based on the previous box plots
quantile(r$peakrpm,seq(0,1,0.01))
r$peakrpm[which(r$peakrpm>5556)]<-5556

#price
quantile(r$price,seq(0,1,0.01))
r$price[which(r$price>22563.00)]<-22563.00
##############################################################################

#Deriving varibales
r$wheel_length<-(r$wheelbase/r$carlength)*100  #this indicates the interior room inside the car 
r$area<-(r$carlength*r$carwidth)/1000  #indicates the surface area of a  car  


#checking plots__________________________________________________________________-

ggplot(r ,aes(r$car_name ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$symboling ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$aspiration ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$doornumber ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$carbody ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$carbody ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$drivewheel ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$enginelocation ,r$price))+geom_bar(stat="identity")
ggplot(r ,aes(r$wheelbase ,r$price))+geom_line()
ggplot(r ,aes(r$carlength ,r$price))+geom_line()#
ggplot(r ,aes(r$carwidth ,r$price))+geom_line()#
ggplot(r ,aes(r$carheight ,r$price))+geom_line()
ggplot(r ,aes(r$curbweight ,r$price))+geom_line()#
ggplot(r ,aes(r$enginesize ,r$price))+geom_line()
ggplot(r ,aes(r$boreratio ,r$price))+geom_line()
ggplot(r ,aes(r$stroke ,r$price))+geom_line()
ggplot(r ,aes(r$compressionratio ,r$price))+geom_line()
ggplot(r ,aes(r$horsepower ,r$price))+geom_line()#
ggplot(r ,aes(r$peakrpm ,r$price))+geom_line()
ggplot(r ,aes(r$citympg ,r$price))+geom_line()#*
ggplot(r ,aes(r$highwaympg ,r$price))+geom_line()#*
ggplot(r ,aes(r$wheel_length ,r$price))+geom_line()#*
ggplot(r ,aes(r$area ,r$price))+geom_line()#*

#gettingv cues from plots its clear that some varibales are correlated 
#checking correlation based on the graphs analysed
cor(r$citympg ,r$highwaympg) #-very high coorelation 0.971337
cor(r$carlength ,r$carwidth) #0.84113
cor(r$carheight ,r$carlength)#0.4910
cor(r$carlength ,r$area)#0.98 ,we are loose the varibale area 
cor(r$carwidth ,r$area)#0.92
cor(r$carheight ,r$area)#0.44 
cor(r$carlength ,r$wheel_length) #-0.55
cor(r$wheelbase ,r$wheel_length)#-0.080
#############################################################################################
#creating dummy varibles for all categorical variables ,variables with 2 levels have been coverted to
dummy<-model.matrix(~r$symboling-1 ,data = r)
dummy<-dummy[,-1]
rnew<-cbind(r[,-2], dummy)

########################

dummy1<-model.matrix(~r$car_name-1 ,data = r)
dummy1<-dummy1[,-1]
rnew<-cbind(rnew[,-2], dummy1)
#############################
dummy2<-model.matrix(~r$fueltype-1 ,data = r)
rnew<-cbind(rnew[,-2], dummy2)
####################################3
dummy3<-model.matrix(~r$aspiration-1 ,data = r)
rnew<-cbind(rnew[,-2], dummy3)
#########################################
dummy4<-model.matrix(~r$doornumber-1 ,data = r)
rnew<-cbind(rnew[,-2], dummy4)
####################################################3
dummy5<-model.matrix(~r$carbody-1 ,data = r)
dummy5<-dummy5[,-1]
rnew<-cbind(rnew[,-2], dummy5)
######################################################
dummy6<-model.matrix(~r$drivewheel-1 ,data = r)
rnew<-cbind(rnew[,-2], dummy6)
########################################################3
dummy7<-model.matrix(~r$enginelocation-1 ,data = r)
rnew<-cbind(rnew[,-2], dummy7)
##########################################################3
dummy8<-model.matrix(~r$enginetype-1 ,data = r)
dummy8<-dummy8[,-1]
rnew<-cbind(rnew[,-7], dummy8)
##############################################################3
dummy9<-model.matrix(~r$cylindernumber-1 ,data = r)
dummy9<-dummy9[,-1]
rnew<-cbind(rnew[,-7], dummy9)
##############################################################3
dummy10<-model.matrix(~r$fuelsystem-1 ,data = r)
dummy10<-dummy10[,-1]
rnew<-cbind(rnew[,-8], dummy10)
##################################################################
str(rnew)#no factors are present
##################################################################
#divide data into test and train************************************************
set.seed(100)
r1<-sample.int(n=nrow(rnew),size=floor(0.75*nrow(rnew)),replace =F)
train <- rnew[r1, ]
test  <- rnew[-r1, ] # Data has been divided into training and test sets .    
###############################################################################
#considering the whole data set as there are only very few observations so its not ideal to split
#created test data set to test . 
model1<-lm(price~.,data=rnew)
summary(model1)

step <- stepAIC(model1, direction="both")
step
model2<-lm(formula = price ~ car_ID + wheelbase + carlength + curbweight + 
             stroke + horsepower + highwaympg + wheel_length + `r$symboling0` + 
             `r$symboling1` + `r$symboling2` + `r$symboling3` + `r$car_namebuick` + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesaab` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` + `r$carbodyhardtop` + `r$carbodyhatchback` + 
             `r$carbodywagon` + `r$drivewheel4wd` + `r$drivewheelfwd` + 
             `r$enginelocationfront` + `r$enginetypefive` + `r$enginetypesix` + 
             `r$enginetypetwelve` + `r$enginetypetwo` + `r$fuelsystem2bbl` + 
             `r$fuelsystemmfi` + `r$fuelsystemmpfi` + `r$fuelsystemspdi`, 
           data = rnew)
summary(model2)

vif(model2)# car_ID ,#no-r$car_nametoyota ,no-car_nameNissan ,no-car_namevolkswagen ,yes-drivewheelfwd ,no-car_namesubaru
summary(model2)  # removing stroke ,car_nameaudi,carbodyconvertible,drivewheelfwd,enginetypefive,fuelsystemspfi based on p value
#car_id was removed as it was not an important metric


#model3
model3<-lm(formula = price ~  wheelbase + carlength + curbweight + 
                             horsepower + highwaympg + `r$symboling0` + `r$symboling1` + 
                             `r$symboling2` + `r$symboling3` +  `r$car_namebuick` + 
                             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
                             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
                             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
                             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
                             `r$car_namerenault` + `r$car_namesaab` + `r$car_namesubaru` + 
                             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
                             `r$aspirationstd` +  `r$carbodysedan` + 
                             `r$drivewheelrwd` +  
                             `r$enginetypesix` + `r$enginetypetwelve` + `r$enginetypetwo` + 
                             `r$fuelsystem2bbl` + `r$fuelsystemmfi` + `r$fuelsystemmpfi` + 
                             `r$fuelsystemspdi`, data = rnew)
summary(model3) #R-squared:  0.9493,	Adjusted R-squared:  0.9377 
#removing horsepower ,highwaympg ,symboling0 ,symboling1,symboling3 ,
#car_namesaab,enginetypetwo ,fuelsystemmfi,fuelsystemspdi based on p value
model4<-lm(formula = price ~  wheelbase + carlength + curbweight + 
              
             `r$symboling2`  +  `r$car_namebuick` + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` +  `r$carbodysedan` + 
             `r$drivewheelrwd` +  
             `r$enginetypesix` + `r$enginetypetwelve`  + 
             `r$fuelsystem2bbl`  + `r$fuelsystemmpfi` , data = rnew)
summary(model4)#R-squared:  0.9416,	Adjusted R-squared:  0.9319

#enginetypetwelve ,fuelsystem2bbl removed due to low significance 

model5<-lm(formula = price ~  wheelbase + carlength + curbweight + 
             
             `r$symboling2`  +  `r$car_namebuick` + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` +  `r$carbodysedan` + 
             `r$drivewheelrwd` +  
             `r$enginetypesix` +  `r$fuelsystemmpfi` , data = rnew)

summary(model5)#R-squared:  0.9416,	Adjusted R-squared:  0.9319

vif(model5)#vif is less for all variables  ,hence removing less significant varibles and checking
#enginetypesix ,symboling2 ,car_namebuick has been removed

model6<-lm(formula = price ~  wheelbase + carlength + curbweight  + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` +  `r$carbodysedan` + 
             `r$drivewheelrwd` +  
             `r$fuelsystemmpfi` , data = rnew)
summary(model6)#R-squared:  0.9307,	Adjusted R-squared:  0.9215 
vif(model6) #high values are for carlength ,curbweight ,but cannot remove curbweight due to high significance
#removing  carlength and checking

model7<-lm(formula = price ~  wheelbase + curbweight  + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu` + `r$car_namejaguar` + `r$car_namemazda` + 
             `r$car_namemercury` + `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` +  `r$carbodysedan` + 
             `r$drivewheelrwd` +  
             `r$fuelsystemmpfi` , data = rnew)
summary(model7)#R-squared:  0.9272,	Adjusted R-squared:  0.9179 
#observed changes in p values ,checking vif again
vif(model7)#wheelbase can be removed ,car_namejaguar ,car_namemercury ,carbodysedan

model8<-lm(formula = price ~  curbweight  + 
             `r$car_namechevrolet` + `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu`  + `r$car_namemazda` + 
              `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen` + `r$car_namevolvo` + 
             `r$aspirationstd` + 
             `r$drivewheelrwd` +  
             `r$fuelsystemmpfi` , data = rnew)
summary(model8)#R-squared:  0.9177,	Adjusted R-squared:  0.9092 
vif(model8)#car_namechevrolet,car_namevolvo removed
model9<-lm(formula = price ~  curbweight  + 
             `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu`  + `r$car_namemazda` + 
             `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen`  + 
             `r$aspirationstd` + 
             `r$drivewheelrwd` +  
             `r$fuelsystemmpfi` , data = rnew)
summary(model9)# R-squared:  0.9103,	Adjusted R-squared:  0.9021 
vif(model9)#car_namemazda has been removed

model10<-lm(formula = price ~  curbweight  + 
             `r$car_namedodge` + `r$car_namehonda` + 
             `r$car_nameisuzu`  + 
             `r$car_namemitsubishi` + `r$car_nameNissan` + 
             `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
             `r$car_namerenault` + `r$car_namesubaru` + 
             `r$car_nametoyota` + `r$car_namevolkswagen`  + 
             `r$aspirationstd` + 
             `r$drivewheelrwd` +  
             `r$fuelsystemmpfi` , data = rnew)
summary(model10)#car_namehonda ,car_nameisuzu has been removed
#R-squared:  0.9074,	Adjusted R-squared:  0.8995

model11<-lm(formula = price ~  curbweight  + 
              `r$car_namedodge` +  
              `r$car_namemitsubishi` + `r$car_nameNissan` + 
              `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche` + 
              `r$car_namerenault` + `r$car_namesubaru` + 
              `r$car_nametoyota` + `r$car_namevolkswagen`  + 
              `r$aspirationstd` + 
              `r$drivewheelrwd` +  
              `r$fuelsystemmpfi` , data = rnew)

summary(model11)#R-squared:  0.9074,	Adjusted R-squared:  0.8995
vif(model11)

#car_nameNissan ,car_namerenault ,car_namevolkswagen
model12<-lm(formula = price ~  curbweight  + 
              `r$car_namedodge` +  
              `r$car_namemitsubishi` + 
              `r$car_namepeugeot` + `r$car_nameplymouth` + `r$car_nameporsche`+ `r$car_namesubaru` + 
              `r$car_nametoyota` + 
              `r$aspirationstd` + 
              `r$drivewheelrwd` +  
              `r$fuelsystemmpfi` , data = rnew)

summary(model12)#car_nameplymouth ,car_namemitsubishi ,car_namedodge
#R-squared:  0.891,	Adjusted R-squared:  0.8848 
model13<-lm(formula = price ~  curbweight  + 
               
              
              `r$car_namepeugeot`  + `r$car_nameporsche`+ `r$car_namesubaru` + 
              `r$car_nametoyota` + 
              `r$aspirationstd` + 
              `r$drivewheelrwd` +  
              `r$fuelsystemmpfi` , data = rnew)
summary(model13)#R-squared:  0.8805,	Adjusted R-squared:  0.8757 
#car_namesubaru
vif(model13)
model14<-lm(formula = price ~  curbweight  + 
              
              
              `r$car_namepeugeot`  + `r$car_nameporsche` + 
              `r$car_nametoyota` + 
              `r$aspirationstd` + 
              `r$drivewheelrwd` +  
              `r$fuelsystemmpfi` , data = rnew)
summary(model14)#Multiple R-squared:  0.8762,	Adjusted R-squared:  0.8718 
#left with all significant varibles
vif(model14)

###############_______________________
#checking correlatoin of other variables
COR<-select(rnew ,curbweight ,`r$car_namepeugeot` ,`r$car_nameporsche` , `r$car_nametoyota` ,`r$aspirationstd` ,`r$drivewheelrwd`,`r$fuelsystemmpfi`)
View(COR)
View(COR)
CORR1<-cor(COR)
View(CORR1)

##################################################################3
#drivewheelrwd ,curbweight  seem to correlated ,removing drivewheelrwd as it is less significant 
model15<-lm(formula = price ~  curbweight  + 
             
              
              `r$car_namepeugeot`  + `r$car_nameporsche` + 
              `r$car_nametoyota` + 
              `r$aspirationstd` + 
              `r$fuelsystemmpfi` , data = rnew)
summary(model15)
#r values decreased so retaining them back 
# R-squared:  0.8546,	Adjusted R-squared:  0.8502 
##model15 is the final one

###################################################################################_________________
#testing with test data .
Predict_1 <- predict(model14, test[-15])
test$test_sales <- Predict_1
r <- cor(test$price,test$test_sales)
rsquared <- r^2
rsquared #0.8574253
##################################################################################__________________r